-- | Rust MIR -> Frankenstein Core Translation
--
-- Translates rustc's MIR (post-borrow-check) into Frankenstein Core.
-- MIR is a CFG of basic blocks — lower level than GHC Core or Mercury HLDS.
-- The key advantage: ownership is already resolved by rustc.
--
-- MIR operations -> Core:
--   Move(_x)           -> EVar x + EDrop x (consumed, Perceus release)
--   Copy(_x)           -> EVar x + ERetain x (refcount increment)
--   Drop(_x)           -> EDrop x (explicit destructor)
--   StorageLive(_x)    -> (scope marker, allocate)
--   StorageDead(_x)    -> EDrop x (scope exit, release)
--   _x = Rvalue        -> ELet binding
--   Call(fn, args)      -> EApp
--   SwitchInt           -> ECase
--   Return              -> result variable
--   Goto(bbN)           -> (control flow, handled by CFG->expression conversion)
--
-- The main challenge: converting CFG (basic blocks) back to structured
-- expressions (let/case/lambda). We use a simple dominator-based algorithm.

module Frankenstein.RustBridge.CoreTranslate
  ( translateMir
  , translateBody
  ) where

import Frankenstein.Core.Types
import Frankenstein.RustBridge.MirParse

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Bits ((.&.))
import Data.List (find)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import qualified Data.Set as Set
import Text.Printf (printf)

-- | Translate a full MIR program to Frankenstein Core
--
-- Note: progData is left empty because MIR does not directly expose ADT
-- declarations. By the time rustc emits MIR, data types have been lowered to
-- layout information (field offsets, discriminant values, etc.) spread across
-- individual MIR bodies. Recovering the original enum/struct declarations
-- would require either:
--   (a) Parsing the rustc_private TyKind::Adt from the rustc shim (preferred —
--       the shim already has access to TyCtxt), or
--   (b) Reconstructing ADTs from MIR aggregate rvalues and discriminant reads,
--       which is lossy and fragile.
-- TODO: extend rustc-shim to emit ADT definitions alongside MIR bodies.
-- (Requires modifying rustc-shim/src/main.rs to walk TyCtxt::adt_def()
-- and emit enum/struct declarations as JSON alongside MIR bodies.)
translateMir :: MirProgram -> Either Text Program
translateMir prog = do
  -- Filter out derive(Debug)-generated `<impl at ...>::fmt` bodies
  -- before translation.  Their bodies reference
  -- core::fmt::Formatter::debug_struct_field*_finish helpers that the
  -- runtime doesn't shim, and our dispatch doesn't actually invoke
  -- them (we handle `{:?}` directly in kk_rust_print_one_arg's
  -- positional ADT fallback).  Compiling these would just create
  -- unresolved-symbol link failures.
  let interestingBodies =
        [ b | b <- mirBodies prog, not (isDerivedFmt (mirName b)) ]
  defs <- mapM translateBody interestingBodies
  Right $ Program
    { progName = QName "rust" (Name "main" 0)
    , progDefs = defs
    , progData = []     -- See note above: MIR lacks direct ADT declarations
    , progEffects = []  -- Rust has no user-defined effects (IO is implicit)
    }
  where
    isDerivedFmt n =
      "<impl " `T.isPrefixOf` n && "::fmt" `T.isSuffixOf` n

-- | Translate a single MIR function body to a Frankenstein definition
translateBody :: MirBody -> Either Text Def
translateBody body = do
  let rawName = mirName body
      -- Strip crate name prefix if present (e.g. "double::double" → "double")
      -- Also strip the module_ prefix that MIR sometimes adds
      cleanName = case T.breakOnEnd "::" rawName of
                    ("", n)  -> n    -- no :: found, use as-is
                    (_, n)   -> n    -- take part after last ::
      name = QName "rust" (Name cleanName 0)

      -- Build argument types from local declarations
      -- In MIR, _0 is the return place, _1.._N are arguments
      argLocals = take (mirArgCount body) (drop 1 (mirLocals body))
      argTypes = [(Affine, localTypeToType l) | l <- argLocals]

      -- Return type from _0
      retType = case mirLocals body of
        (ret:_) -> localTypeToType ret
        []      -> unitType

      -- Rust functions have IO effect (they can do arbitrary side effects)
      effRow = EffectRowExtend (QName "std" (Name "io" 0)) EffectRowEmpty

      funType = TFun argTypes effRow retType

      -- Build argument names
      argNames = [(Name ("_" <> T.pack (show (localIndex l))) 0, localTypeToType l)
                 | l <- argLocals]

      -- Translate basic blocks to expression tree
      bodyExpr = translateBlocks body

      -- Wrap in lambda if there are arguments
      expr = if null argNames
             then bodyExpr
             else ELam argNames bodyExpr

  Right $ Def
    { defName = name
    , defType = funType
    , defExpr = expr
    , defSort = DefFun
    , defVisibility = Public
    }

-- | Convert a MIR type string to Frankenstein Type
localTypeToType :: MirLocalDecl -> Type
localTypeToType decl = rustTypeToType (localType decl)

-- | Convert a Rust type string to Frankenstein Type
rustTypeToType :: Text -> Type
rustTypeToType ty
  | ty == "i64"  = intType
  | ty == "i32"  = intType
  | ty == "i128" = intType
  | ty == "u64"  = intType
  | ty == "u32"  = intType
  | ty == "usize" = intType
  | ty == "bool" = boolType
  | ty == "()"   = unitType
  | "(" `T.isPrefixOf` ty && ")" `T.isSuffixOf` ty = -- tuple
      TCon (TypeCon (QName "rust" (Name ty 0)) KindValue)
  | otherwise =
      TCon (TypeCon (QName "rust" (Name ty 0)) KindValue)

-- Common types
intType :: Type
intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

boolType :: Type
boolType = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

unitType :: Type
unitType = TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)

------------------------------------------------------------------------
-- CFG -> Expression translation
------------------------------------------------------------------------

-- | Convert MIR basic blocks (CFG) into a structured expression.
--
-- Strategy:
-- 1. Start at bb0
-- 2. For each block, translate statements as let-bindings
-- 3. Translate terminators:
--    - Return -> EVar _0
--    - Goto -> inline target block (with visited set to avoid loops)
--    - SwitchInt -> ECase with branches
--    - Call -> EApp, then continue to return block
--    - Assert -> translate as let-binding, continue to success block
translateBlocks :: MirBody -> Expr
translateBlocks body =
  case mirBlocks body of
    [] -> ELit (LitInt 0)  -- empty function
    _  -> translateBlockAt body Set.empty 0

-- | Translate a specific basic block by index, with a visited set for loop detection
translateBlockAt :: MirBody -> Set.Set Int -> Int -> Expr
translateBlockAt body visited idx
  | idx `Set.member` visited =
      -- Loop detected: emit a placeholder
      EApp (EVar (Name "loop" 0)) [ELit (LitString ("bb" <> T.pack (show idx)))]
  | otherwise =
      case lookupBlock body idx of
        Just bb -> translateBlockExpr body (Set.insert idx visited) bb
        Nothing -> EApp (EVar (Name "unreachable" 0)) [ELit (LitString ("bb" <> T.pack (show idx)))]

-- | Translate a single basic block to an expression
translateBlockExpr :: MirBody -> Set.Set Int -> MirBasicBlock -> Expr
translateBlockExpr body visited bb =
  let -- Parse and translate statements
      stmtBinds = concatMap (translateStmtToBinds body) (bbStatements bb)
      -- Translate terminator
      termExpr = case bbTerminator bb of
        Just termStr -> translateTermExpr body visited (parseTerminator termStr) termStr
        Nothing      -> EVar (Name "_0" 0)  -- implicit return
      -- Chain statements as let-bindings wrapping the terminator
  in wrapWithBinds stmtBinds termExpr

-- | Wrap an expression with a list of let-bindings
wrapWithBinds :: [BindGroup] -> Expr -> Expr
wrapWithBinds [] e = e
wrapWithBinds bgs e = ELet bgs e

-- | Translate a MIR statement into let-binding groups
translateStmtToBinds :: MirBody -> Text -> [BindGroup]
translateStmtToBinds body stmtStr =
  case parseStmt stmtStr of
    StmtAssign dest rv ->
      let destName = Name ("_" <> T.pack (show dest)) 0
          destTy = case findLocal body dest of
                     Just l  -> localTypeToType l
                     Nothing -> unitType
          rvalExpr = translateRvalue body rv
      in [[Bind destName destTy rvalExpr DefVal]]
    StmtRaw _ ->
      -- Skip unparseable statements (StorageLive, etc.)
      []

-- | Translate an rvalue to a Core expression
translateRvalue :: MirBody -> MirRvalue -> Expr
translateRvalue body rv = case rv of
  RvUse op -> translateOperand body op

  RvBinOp op a b ->
    let opName = mirBinOpToName op
    in EApp (EVar (Name opName 0))
            [translateOperand body a, translateOperand body b]

  RvWithOverflow op a b ->
    -- Overflow-checking ops: we treat them as regular ops
    -- (the overflow check is in the assert terminator)
    let opName = mirBinOpToName op
    in EApp (EVar (Name opName 0))
            [translateOperand body a, translateOperand body b]

  RvAggregate ops ->
    -- Tuple/array construction
    EApp (ECon (QName "std" (Name "tuple" 0)))
         (map (translateOperand body) ops)

  RvStruct name fields ->
    -- Named struct construction: pass type name + comma-separated
    -- field names alongside the values into a runtime helper that
    -- builds a KK_RUST_STRUCT_TAG cell.  The Debug printer reads
    -- the metadata from the cell.
    --
    -- Three shapes:
    --   * Named-field structs: field names = "x,y,…" (non-empty).
    --   * Positional tuple variants: field names = "" (empty entries),
    --     joined to a sequence of commas (e.g. ",," for 3-tuple).
    --     The printer detects "no real names" by checking if the
    --     metadata string is empty after splitting and renders parens.
    --   * Unit variants (0 fields): rust_struct_0 returns a cell
    --     with just the name and no values.
    let n = length fields
        fieldNames = T.intercalate "," (map fst fields)
        vals = map (translateOperand body . snd) fields
        metaArgs = ELit (LitString name) : ELit (LitString fieldNames) : vals
    in if n == 0
       then EApp (EVar (Name "rust_struct_0" 0))
                 [ELit (LitString name), ELit (LitString "")]
       else if n >= 1 && n <= 8
       then EApp (EVar (Name ("rust_struct_" <> T.pack (show n)) 0)) metaArgs
       else EApp (ECon (QName "std" (Name "tuple" 0))) vals

  RvRef idx ->
    ERetain (EVar (Name ("_" <> T.pack (show idx)) 0))

  RvFieldAccess baseIdx fieldIdx _ty ->
    -- Field projection.  Two cases:
    --  - CheckedAdd/Mul/Sub WithOverflow-style tuples are pre-flattened
    --    by the bridge to plain arithmetic (the base is already the
    --    result value, not a heap tuple).  Field 0 should be the base.
    --  - Regular RvAggregate-constructed tuples ARE heap cells; field 0
    --    needs kk_field at runtime to extract the contained value.
    -- We dispatch via the runtime helper `rust_field_safe` which
    -- inspects the base: a heap pointer is treated as a tuple cell;
    -- otherwise the base is returned as-is (matching the
    -- WithOverflow-flattened shape).
    EApp (EVar (Name "rust_field_safe" 0))
         [EVar (Name ("_" <> T.pack (show baseIdx)) 0),
          ELit (LitInt (fromIntegral fieldIdx))]

  RvRaw t -> ELit (LitString t)

-- | Translate a MIR operand to a Core expression
translateOperand :: MirBody -> MirOperand -> Expr
translateOperand _body op = case op of
  OpMove idx ->
    -- Move: use the variable. The variable is consumed.
    EVar (Name ("_" <> T.pack (show idx)) 0)

  OpCopy idx ->
    -- Copy: retain (increment refcount) then use
    ERetain (EVar (Name ("_" <> T.pack (show idx)) 0))

  OpConst t -> parseConstLit t

  OpFieldAccess baseIdx fieldIdx _ty ->
    if fieldIdx == 0
    then EVar (Name ("_" <> T.pack (show baseIdx)) 0)
    else EApp (EVar (Name ("field_" <> T.pack (show fieldIdx)) 0))
              [EVar (Name ("_" <> T.pack (show baseIdx)) 0)]

  OpRef idx ->
    ERetain (EVar (Name ("_" <> T.pack (show idx)) 0))

-- | Parse a MIR constant literal into a Core expression
parseConstLit :: Text -> Expr
parseConstLit t
  | "true" `T.isPrefixOf` t = ECon (QName "std" (Name "True" 0))
  | "false" `T.isPrefixOf` t = ECon (QName "std" (Name "False" 0))
  | Just bits <- parseFloatBits t = ELit (LitInt bits)
  | otherwise =
      -- Try to parse as integer: "0_i64", "1_i64", "10_i64", etc.
      let numPart = T.takeWhile (\c -> c == '-' || isDigit c) t
      in case reads (T.unpack numPart) of
           [(n, _)] -> ELit (LitInt n)
           _        -> ELit (LitString (stripQuotes t))
  where
    -- MIR prints string literals with surrounding double quotes; strip
    -- them so kk_str_len returns the byte count of the actual string
    -- content, not including the source-syntax quote characters.
    -- Rust byte-string literals carry a leading `b` prefix
    -- (`b"…"`).  Plain Rust string literals (no `b` prefix) hold
    -- UTF-8 text and round-trip cleanly through Text/UTF-8.  Byte
    -- strings can contain raw bytes ≥ 0x80 that don't round-trip
    -- through UTF-8 encoding, so we materialise them as ASCII hex
    -- strings of the form `__RBYTES:HHHH…` — the runtime
    -- kk_str_from_hex_marker decodes them back to raw bytes when
    -- it sees the marker prefix, and treats other strings as
    -- ordinary UTF-8.
    stripQuotes s0 = case T.uncons s0 of
      Just ('b', rest) -> case T.uncons rest of
        Just ('"', inner) -> case T.unsnoc inner of
          Just (content, '"') -> "__RBYTES:" <> hexEncodeBytes content
          _                   -> s0
        _ -> s0
      _ -> case T.uncons s0 of
             Just ('"', rest) -> case T.unsnoc rest of
               Just (inside, '"') -> unescapeRust inside
               _                  -> s0
             _ -> s0
    -- Hex-encode a byte-string source-form (with Rust escapes intact)
    -- to a flat sequence of two-hex-digit pairs.  Two passes:
    -- (1) unescape Rust-style \n/\t/\xHH into Char codepoints,
    -- (2) for each Char codepoint c, emit printf "%02X" (c & 0xff).
    -- Codepoints > 0xff are clamped (Rust byte strings only allow
    -- 0x00-0xff by syntax).
    hexEncodeBytes content =
      let unescaped = unescapeRust content
          bytes = map (\c -> fromEnum c .&. 0xff) (T.unpack unescaped)
      in T.pack (concatMap (printf "%02X") bytes)
    unescapeRust = T.pack . go . T.unpack
    go []             = []
    go ('\\':'n':xs)  = '\n' : go xs
    go ('\\':'t':xs)  = '\t' : go xs
    go ('\\':'r':xs)  = '\r' : go xs
    go ('\\':'0':xs)  = '\0' : go xs
    go ('\\':'"':xs)  = '"'  : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    -- \xHH: two-hex-digit byte escape (Rust byte-string syntax).
    go ('\\':'x':h1:h2:xs)
      | Just b <- twoHexDigits h1 h2 = toEnum b : go xs
    go (c:xs)         = c    : go xs
    twoHexDigits h1 h2 =
      case (hexVal h1, hexVal h2) of
        (Just v1, Just v2) -> Just (v1 * 16 + v2)
        _                  -> Nothing
    hexVal c
      | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = Just (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'F' = Just (fromEnum c - fromEnum 'A' + 10)
      | otherwise            = Nothing

-- | Recognise MIR float literals like "3.1415899999999999f64" or
-- "1.5f32" (also "-2.5f64", "0e0f64").  The float value is bit-cast
-- to a 64-bit integer pattern (f32 bits are zero-extended into the
-- low 32 bits of the i64) so the rest of Core can carry it through as
-- a normal LitInt.  The bridge wraps the value with
-- rust_arg_f64 / rust_arg_f32, which tags the heap cell so the
-- runtime knows to reinterpret the bits as a float when printing.
parseFloatBits :: Text -> Maybe Integer
parseFloatBits t
  | Just numStr <- T.stripSuffix "f64" t
  , looksLikeFloat numStr
  , Just d <- readFloatStr (T.unpack numStr)
  = Just (toInteger (castDoubleToWord64 d))
  | Just numStr <- T.stripSuffix "f32" t
  , looksLikeFloat numStr
  , Just f <- readFloatStr (T.unpack numStr)
  = Just (toInteger (castFloatToWord32 f))
  | otherwise = Nothing
  where
    looksLikeFloat s = T.any (== '.') s
                    || T.any (== 'e') (T.toLower s)
    readFloatStr :: Read a => String -> Maybe a
    readFloatStr s = case reads s of
      [(x, "")] -> Just x
      _         -> Nothing

-- | Map MIR binary operator names to Core operator names
mirBinOpToName :: Text -> Text
mirBinOpToName "Eq"  = "=="
mirBinOpToName "Ne"  = "!="
mirBinOpToName "Lt"  = "<"
mirBinOpToName "Le"  = "<="
mirBinOpToName "Gt"  = ">"
mirBinOpToName "Ge"  = ">="
mirBinOpToName "Add" = "+"
mirBinOpToName "Sub" = "-"
mirBinOpToName "Mul" = "*"
mirBinOpToName "Div" = "/"
mirBinOpToName "Rem" = "%"
mirBinOpToName op    = op

-- | Translate a parsed terminator into a Core expression
translateTermExpr :: MirBody -> Set.Set Int -> MirTerminator -> Text -> Expr
translateTermExpr body visited term _raw = case term of
  TermReturn ->
    -- Return: the result is in _0.  For functions returning `()` the
    -- return slot is never assigned by the MIR, so emit a literal 0
    -- (the runtime's unit representation) instead of a dangling EVar.
    let retLocal = case mirLocals body of
          (l:_) -> Just l
          []    -> Nothing
        returnsUnit = case retLocal of
          Just l -> localType l == "()"
          Nothing -> False
    in if returnsUnit
       then ELit (LitInt 0)
       else EVar (Name "_0" 0)

  TermGoto target ->
    -- Goto: inline the target block
    translateBlockAt body visited target

  TermSwitchInt op targets ->
    -- switchInt -> case expression
    let scrutExpr = translateOperand body op
        branches = map (mkSwitchBranch body visited) targets
    in ECase scrutExpr branches

  TermCallSimple funcName argStrs retBb ->
    -- Function call: translate to EApp, then continue to return block.
    -- Known Rust intrinsics are remapped to Frankenstein runtime names
    -- (e.g. core::str::<impl str>::len → str_len) so the emitter routes
    -- them through the kk_* runtime without applying the rust_ module
    -- prefix or arity suffix.  Arguments::from_str is a thin wrapper —
    -- elide it entirely and use the first arg directly so the
    -- subsequent print_str call sees the string literal.
    let argExprs = map parseCallArg argStrs
        callExpr = case (funcName, argExprs) of
          ("Arguments::<'_>::from_str", (a:_)) -> a
          ("Arguments::<'_>::from_str_nonconst", (a:_)) -> a
          ("Arguments::<'_>::new_const", (a:_)) -> a
          -- core::fmt::rt::Argument::<'_>::new_display::<T>(value)
          -- is a thin wrapper around the value — elide it so the
          -- argument's raw i64 reaches the Arguments::new args array.
          -- new_display::<T>: i64 is the natural raw type, so elide.
          -- For other integer widths the bridge wraps with a per-type
          -- runtime tag so the dispatcher can mask / interpret the
          -- bits correctly when printing.
          (_, (a:_)) | Just wrapper <- displayTypeWrapper funcName ->
            EApp (EVar (Name wrapper 0)) [a]
          (_, (a:_)) | "Argument::<'_>::new_display" `T.isInfixOf` funcName -> a
          -- Debug format: wrap with a marker cell so the runtime
          -- picks the debug formatter (quotes around strings,
          -- escape special chars, etc.).  For Int the formatter
          -- happens to match Display, but the runtime still
          -- handles the wrapped case correctly via tag dispatch.
          (_, (a:_)) | "Argument::<'_>::new_debug" `T.isInfixOf` funcName ->
            EApp (EVar (Name "rust_arg_debug" 0)) [a]
          -- Radix-format args ({:x}, {:X}, {:o}, {:b}) — Rust selects
          -- via separate Argument constructors; the format template
          -- byte is still plain 0xc0 (no field-spec bytes).  Wrap
          -- with a per-radix runtime marker.
          (_, (a:_)) | "Argument::<'_>::new_lower_hex" `T.isInfixOf` funcName ->
            EApp (EVar (Name "rust_arg_lower_hex" 0)) [a]
          (_, (a:_)) | "Argument::<'_>::new_upper_hex" `T.isInfixOf` funcName ->
            EApp (EVar (Name "rust_arg_upper_hex" 0)) [a]
          (_, (a:_)) | "Argument::<'_>::new_octal" `T.isInfixOf` funcName ->
            EApp (EVar (Name "rust_arg_octal" 0)) [a]
          (_, (a:_)) | "Argument::<'_>::new_binary" `T.isInfixOf` funcName ->
            EApp (EVar (Name "rust_arg_binary" 0)) [a]
          -- `Result::<T, E>::unwrap(r)` — the bridge represents the
          -- result of fallible I/O calls (read_file, write_file, …)
          -- as the raw success value (empty string on error, etc.),
          -- so unwrap is identity.  Same for `Result::expect` and the
          -- `?` operator's `Try::branch` which lowers to a similar
          -- shape.
          (_, (a:_))
            | "Result::<" `T.isPrefixOf` funcName
            , "::unwrap" `T.isSuffixOf` funcName
            -> a
          (_, (a:_))
            | "Result::<" `T.isPrefixOf` funcName
            , "::expect" `T.isInfixOf` funcName
            -> a
          -- Arguments::<'_>::new::<N, M>(template, args) builds a
          -- packed (template, args) cell at runtime.  std::io::_print
          -- below dispatches: if the value is a kk_string the
          -- existing print_str path runs, else it's the packed cell
          -- and rust_print_args walks the template substituting args.
          (_, (template:argsArr:_))
            | "Arguments::<'_>::new" `T.isPrefixOf` funcName ->
              EApp (EVar (Name "rust_args_pack" 0)) [template, argsArr]
          -- Stdin::read_line(self, &mut buf) — fold the call into a
          -- direct kk_read_line() with no args.  The buf-rebind side
          -- effect is materialised separately below by emitting a
          -- second let-binding for the variable behind the &mut.
          (_, _) | funcName == "Stdin::read_line"
                 || "io::Stdin::read_line" `T.isSuffixOf` funcName ->
            EApp (EVar (Name "read_line" 0)) []
          -- stdin() / stdout() — these return handle values that our
          -- runtime doesn't use (the read_line/write/_print helpers
          -- talk to libc stdin/stdout directly).  Return a sentinel.
          (_, _) | funcName == "stdin" || "io::stdin" `T.isSuffixOf` funcName
                 || "::stdin" `T.isSuffixOf` funcName ->
            ELit (LitInt 0)
          (_, _) | "String::new" `T.isSuffixOf` funcName
                 || "string::String::new" `T.isSuffixOf` funcName ->
            EApp (EVar (Name "string_empty" 0)) []
          _ -> EApp (EVar (Name (remapRustIntrinsic funcName) 0)) argExprs
        -- Find the destination variable: look at the raw terminator
        -- _N = func(args) -> [return: bbM, ...]
        destName = findCallDest _raw
        destTy = case destName of
                   Just n -> case findLocalByName body n of
                     Just l  -> localTypeToType l
                     Nothing -> unitType
                   Nothing -> unitType
        -- Continue translating from the return block
        contExpr = translateBlockAt body visited retBb
        -- For Stdin::read_line(_, &mut _N), rebind the underlying
        -- buffer variable _N to the kk_read_line() result so that
        -- subsequent reads of _N see the line content.  Use the body's
        -- statement scan to chase argStrs[1] (e.g. "copy _6") through
        -- a prior `_6 = &mut _N` statement.
        bufRebind =
          case (funcName, argStrs) of
            (fn, _:secondArg:_)
              | fn == "Stdin::read_line"
                || "io::Stdin::read_line" `T.isSuffixOf` fn
              , Just refIdx <- argIndex secondArg
              , Just target <- findMutRefTarget body refIdx
              -> Just target
            _ -> Nothing
    in case (bufRebind, destName) of
      (Just bufIdx, Just n) ->
        let bufName = Name ("_" <> T.pack (show bufIdx)) 0
            bufTy   = case findLocal body bufIdx of
                        Just l  -> localTypeToType l
                        Nothing -> unitType
        in ELet [[ Bind bufName bufTy (EApp (EVar (Name "read_line" 0)) []) DefVal
                 , Bind (Name n 0) destTy (ELit (LitInt 0)) DefVal
                 ]] contExpr
      (_, Just n) ->
        ELet [[Bind (Name n 0) destTy callExpr DefVal]] contExpr
      (_, Nothing) ->
        ELet [[Bind (Name "_call" 0) unitType callExpr DefVal]] contExpr

  TermAssert _msg successBb ->
    -- Assert: just continue to success block (we ignore the assertion for now)
    translateBlockAt body visited successBb

  TermDrop idx target ->
    -- Drop then goto
    let dropExpr = EDrop (EVar (Name ("_" <> T.pack (show idx)) 0))
    in ELet [[Bind (Name "_drop" 0) unitType dropExpr DefVal]]
            (translateBlockAt body visited target)

  TermCall {} ->
    ELit (LitString "unhandled_call")

  TermRaw t ->
    EApp (EVar (Name "mir_term" 0)) [ELit (LitString t)]

-- | Parse a call argument string to an expression
parseCallArg :: Text -> Expr
parseCallArg t =
  case parseOperand t of
    Just op -> translateOperand undefined op  -- body not needed for operand translation
    Nothing -> ELit (LitString t)

-- | Find the destination variable name from a call terminator string
-- e.g. "_3 = factorial(move _4) -> ..." -> Just "_3"
findCallDest :: Text -> Maybe Text
findCallDest t =
  let stripped = T.strip t
  in case T.uncons stripped of
    Just ('_', rest) ->
      let (digits, after) = T.span isDigit rest
      in if not (T.null digits) && " = " `T.isPrefixOf` after
         then Just ("_" <> digits)
         else Nothing
    _ -> Nothing

-- | Extract the local index from an MIR operand text fragment like
-- "move _6" or "copy _6" or bare "_6".  Returns Nothing for
-- non-local args.
argIndex :: Text -> Maybe Int
argIndex t =
  let stripped = T.strip t
      bare = case T.words stripped of
        [_, w] -> w     -- "move _N" / "copy _N"
        [w]    -> w     -- "_N"
        _      -> T.empty
  in case T.stripPrefix "_" bare of
       Just digits ->
         let (d, r) = T.span isDigit digits
         in if not (T.null d) && T.null r
            then case reads (T.unpack d) of
                   [(n, _)] -> Just n
                   _        -> Nothing
            else Nothing
       Nothing -> Nothing

-- | Given a local index N, scan the body's basic blocks for a
-- statement `Assign((_N, &mut _M))` or `Assign((_N, &_M))` and
-- return M.  The text-MIR pipeline normalises statements into
-- `Assign(...)` form by the time they reach `bbStatements`.
findMutRefTarget :: MirBody -> Int -> Maybe Int
findMutRefTarget body localIdx =
  let nText = T.pack (show localIdx)
      needle1 = "Assign((_" <> nText <> ", &mut _"
      needle2 = "Assign((_" <> nText <> ", &_"
      stmts = concatMap bbStatements (mirBlocks body)
      tryMatch s
        | Just rest <- T.stripPrefix needle1 (T.stripStart s) = parseTail rest
        | Just rest <- T.stripPrefix needle2 (T.stripStart s) = parseTail rest
        | otherwise = Nothing
      parseTail rest =
        let (d, _) = T.span isDigit rest
        in case reads (T.unpack d) of
             [(n, _)] -> Just n
             _        -> Nothing
  in case [n | s <- stmts, Just n <- [tryMatch s]] of
    (n:_) -> Just n
    []    -> Nothing

-- | Find a local variable by name
findLocalByName :: MirBody -> Text -> Maybe MirLocalDecl
findLocalByName body varName =
  case T.uncons varName of
    Just ('_', digits) ->
      case reads (T.unpack digits) of
        [(idx, _)] -> findLocal body idx
        _          -> Nothing
    _ -> Nothing

-- | Make a branch for a switch target
mkSwitchBranch :: MirBody -> Set.Set Int -> MirSwitchTarget -> Branch
mkSwitchBranch body visited target =
  let bodyExpr = translateBlockAt body visited (switchBlock target)
      pat = case switchVal target of
              Just 0  -> PatLit (LitInt 0)  -- "0: bbN" means value is 0 (false for bool)
              Just n  -> PatLit (LitInt n)
              Nothing -> PatWild boolType   -- "otherwise"
  in Branch
    { branchPattern = pat
    , branchGuard = Nothing
    , branchBody = bodyExpr
    }

-- | Find a local declaration by index
findLocal :: MirBody -> Int -> Maybe MirLocalDecl
findLocal body idx =
  find (\l -> localIndex l == idx) (mirLocals body)

-- | Look up a block by index
lookupBlock :: MirBody -> Int -> Maybe MirBasicBlock
lookupBlock body idx =
  find (\bb -> bbIndex bb == idx) (mirBlocks body)

-- | Remap Rust stdlib symbols to Frankenstein runtime intrinsics.
-- Known names get rewritten to the runtime equivalents that the MLIR
-- emitter recognises and routes through the kk_* runtime.  Unknown
-- names pass through unchanged (the emitter applies the rust_ module
-- prefix and arity suffix, leaving them as unresolved externs that
-- the user is expected to provide via FFI shim).
-- | If `funcName` is `Argument::<'_>::new_display::<T>` for a numeric
-- type T that needs special formatting (anything that isn't i64),
-- return the corresponding runtime wrapper name.  Returns Nothing for
-- i64 (the natural raw type) so the bridge falls through to the elide
-- path.
displayTypeWrapper :: Text -> Maybe Text
displayTypeWrapper funcName
  | not ("Argument::<'_>::new_display" `T.isInfixOf` funcName) = Nothing
  | "<u32>"   `T.isInfixOf` funcName = Just "rust_arg_u32"
  | "<i32>"   `T.isInfixOf` funcName = Just "rust_arg_i32"
  | "<u64>"   `T.isInfixOf` funcName = Just "rust_arg_u64"
  | "<u16>"   `T.isInfixOf` funcName = Just "rust_arg_u16"
  | "<i16>"   `T.isInfixOf` funcName = Just "rust_arg_i16"
  | "<u8>"    `T.isInfixOf` funcName = Just "rust_arg_u8"
  | "<i8>"    `T.isInfixOf` funcName = Just "rust_arg_i8"
  | "<f64>"   `T.isInfixOf` funcName = Just "rust_arg_f64"
  | "<f32>"   `T.isInfixOf` funcName = Just "rust_arg_f32"
  | "<usize>" `T.isInfixOf` funcName = Just "rust_arg_u64"
  | "<isize>" `T.isInfixOf` funcName = Nothing  -- isize on x86-64 = i64
  | otherwise = Nothing

remapRustIntrinsic :: Text -> Text
remapRustIntrinsic n = case n of
  "core::str::<impl str>::len"      -> "str_len"
  "core::str::len"                  -> "str_len"
  -- println!("...") expands to two MIR calls:
  --   Arguments::<'_>::from_str(const "...") creates a thin Arguments
  --     wrapper — elided at the bridge level (see TermCallSimple).
  --   std::io::_print(args) does the actual print; remap to print_str
  --     (no trailing newline — Rust's println! source already includes
  --     it in the format string).
  "std::io::_print"                 -> "rust_print_dispatch"
  -- File I/O.  read_to_string/write/read both come from std::fs and
  -- read_to_string/write also have monomorphic <&str> specialisations
  -- — match all the variants we've observed.  Our bridge represents
  -- the Result as the raw value (empty kk_string on failure for
  -- reads, return code for writes), so the user's `.unwrap()` is
  -- elided by the Result::unwrap pattern above.
  "std::fs::read_to_string"                 -> "read_file"
  "std::fs::read_to_string::<&str>"         -> "read_file"
  "std::fs::write::<&str, &str>"            -> "write_file"
  "std::fs::write::<&str, std::string::String>" -> "write_file"
  "std::fs::write"                          -> "write_file"
  _                                 -> n


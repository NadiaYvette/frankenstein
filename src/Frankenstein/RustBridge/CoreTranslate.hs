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
import Data.List (find)
import qualified Data.Set as Set

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
translateMir :: MirProgram -> Either Text Program
translateMir prog = do
  defs <- mapM translateBody (mirBodies prog)
  Right $ Program
    { progName = QName "rust" (Name "main" 0)
    , progDefs = defs
    , progData = []     -- See note above: MIR lacks direct ADT declarations
    , progEffects = []  -- Rust has no user-defined effects (IO is implicit)
    }

-- | Translate a single MIR function body to a Frankenstein definition
translateBody :: MirBody -> Either Text Def
translateBody body = do
  let name = QName "rust" (Name (mirName body) 0)

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

  RvRef idx ->
    ERetain (EVar (Name ("_" <> T.pack (show idx)) 0))

  RvFieldAccess baseIdx fieldIdx _ty ->
    -- Field projection: translate as application of a field accessor
    EApp (EVar (Name ("field_" <> T.pack (show fieldIdx)) 0))
         [EVar (Name ("_" <> T.pack (show baseIdx)) 0)]

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
    EApp (EVar (Name ("field_" <> T.pack (show fieldIdx)) 0))
         [EVar (Name ("_" <> T.pack (show baseIdx)) 0)]

  OpRef idx ->
    ERetain (EVar (Name ("_" <> T.pack (show idx)) 0))

-- | Parse a MIR constant literal into a Core expression
parseConstLit :: Text -> Expr
parseConstLit t
  | "true" `T.isPrefixOf` t = ECon (QName "std" (Name "True" 0))
  | "false" `T.isPrefixOf` t = ECon (QName "std" (Name "False" 0))
  | otherwise =
      -- Try to parse as integer: "0_i64", "1_i64", "10_i64", etc.
      let numPart = T.takeWhile (\c -> c == '-' || isDigit c) t
      in case reads (T.unpack numPart) of
           [(n, _)] -> ELit (LitInt n)
           _        -> ELit (LitString t)

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
    -- Return: the result is in _0
    EVar (Name "_0" 0)

  TermGoto target ->
    -- Goto: inline the target block
    translateBlockAt body visited target

  TermSwitchInt op targets ->
    -- switchInt -> case expression
    let scrutExpr = translateOperand body op
        branches = map (mkSwitchBranch body visited) targets
    in ECase scrutExpr branches

  TermCallSimple funcName argStrs retBb ->
    -- Function call: translate to EApp, then continue to return block
    let funcExpr = EVar (Name funcName 0)
        argExprs = map parseCallArg argStrs
        callExpr = EApp funcExpr argExprs
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
    in case destName of
      Just n ->
        ELet [[Bind (Name n 0) destTy callExpr DefVal]] contExpr
      Nothing ->
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

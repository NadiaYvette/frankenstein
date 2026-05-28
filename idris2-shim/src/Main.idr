module Main

import Idris.Driver
import Compiler.Common
import Compiler.CompileExpr
import Core.CompileExpr
import Core.Context
import Core.Core
import Core.Name
import Core.Name.Namespace
import Core.TT.Primitive
import Idris.Syntax

import Data.List
import Data.String
import Data.Vect
import System
import System.File

-- ---------------------------------------------------------------- JSON utils

jsonEscChar : Char -> String
jsonEscChar '"'  = "\\\""
jsonEscChar '\\' = "\\\\"
jsonEscChar '\n' = "\\n"
jsonEscChar '\r' = "\\r"
jsonEscChar '\t' = "\\t"
jsonEscChar c    = cast c

jsonEsc : String -> String
jsonEsc = fastConcat . map jsonEscChar . unpack

jsonStr : String -> String
jsonStr s = "\"" ++ jsonEsc s ++ "\""

jsonArr : List String -> String
jsonArr [] = "[]"
jsonArr xs = "[" ++ fastConcat (intersperse ", " xs) ++ "]"

jsonObj : List (String, String) -> String
jsonObj [] = "{}"
jsonObj fs = "{" ++ fastConcat (intersperse ", " (map field fs)) ++ "}"
  where
    field : (String, String) -> String
    field (k, v) = jsonStr k ++ ": " ++ v

jsonInt : Integer -> String
jsonInt = show

jsonDouble : Double -> String
jsonDouble = show

isEmpty : List a -> Bool
isEmpty []        = True
isEmpty (_ :: _)  = False

-- ---------------------------------------------------------------- Names

-- | Convert an Idris2 Name to (moduleText, localText) for OrganIR QName.
-- Preserves MN/PV/Nested/Resolved indices so distinct definitions stay distinct.
splitName : Name -> (String, String)
splitName (NS ns n) =
    let (m', t) = splitName n
        nsStr   = show ns
    in (if m' == "" then nsStr else nsStr ++ "." ++ m', t)
splitName (UN un)              = ("", displayUserName un)
splitName (MN n i)             = ("", n ++ "_" ++ show i)
splitName (PV n i)             = let (m, t) = splitName n in (m, "pv" ++ show i ++ "_" ++ t)
splitName (DN _ n)             = splitName n
splitName (Nested (a, b) n)    = let (m, t) = splitName n in (m, "n" ++ show a ++ "_" ++ show b ++ "_" ++ t)
splitName (CaseBlock outer i)  = ("", "case_" ++ outer ++ "_" ++ show i)
splitName (WithBlock outer i)  = ("", "with_" ++ outer ++ "_" ++ show i)
splitName (Resolved i)         = ("", "res_" ++ show i)

mkName : String -> String
mkName t = jsonObj [("text", jsonStr t), ("unique", jsonInt 0)]

mkQName : String -> String -> String
mkQName m t = jsonObj [("module", jsonStr m), ("name", mkName t)]

idrisQName : Name -> String
idrisQName n = let (m, t) = splitName n in mkQName m t

-- | Used inside EVar/EApp references.  The Linker treats "/" as the
-- module-separator (GHC convention) — pick it so cross-module names
-- resolve.  Dots inside the module portion stay (Idris2 namespaces
-- are dot-joined).
showFullName : Name -> String
showFullName n =
    let (m, t) = splitName n
    in if m == "" then t else m ++ "/" ++ t

anyTy : String
anyTy = jsonObj [("con", jsonObj [("qname", mkQName "" "any")])]

-- ---------------------------------------------------------------- Primitives

constLit : Constant -> String
constLit (I x)    = jsonObj [("int", jsonInt (cast x))]
constLit (I8 x)   = jsonObj [("int", jsonInt (cast x))]
constLit (I16 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (I32 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (I64 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (BI x)   = jsonObj [("int", jsonInt x)]
constLit (B8 x)   = jsonObj [("int", jsonInt (cast x))]
constLit (B16 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (B32 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (B64 x)  = jsonObj [("int", jsonInt (cast x))]
constLit (Str s)  = jsonObj [("string", jsonStr s)]
constLit (Ch c)   = jsonObj [("int", jsonInt (cast (ord c)))]
constLit (Db d)   = jsonObj [("float", jsonDouble d)]
constLit (PrT _)  = jsonObj [("int", jsonInt 0)]
constLit WorldVal = jsonObj [("int", jsonInt 0)]

-- | Map an Idris2 PrimFn to a name the Frankenstein emitter already lowers.
-- Short tokens like "+", "-", "==" are inlined to arith.* by MlirEmit;
-- string ops route to runtime helpers registered in externalRuntimeFns.
-- Names with no current mapping pass through under an "idris_" prefix so
-- they show up as obvious unresolved symbols rather than silent failures.
-- Double-typed arithmetic and comparisons need to route through runtime
-- helpers that do floating-point operations on the i64 bit-pattern ABI.
-- The bare "+", "-", "==" tokens are lowered by MlirEmit to integer
-- arith ops, which silently produces garbage for Double operands.
primFnName : {arity : Nat} -> PrimFn arity -> String
primFnName (Add DoubleType)  = "idris_double_add"
primFnName (Sub DoubleType)  = "idris_double_sub"
primFnName (Mul DoubleType)  = "idris_double_mul"
primFnName (Div DoubleType)  = "idris_double_div"
primFnName (Neg DoubleType)  = "idris_double_neg"
primFnName (LT  DoubleType)  = "idris_double_lt"
primFnName (LTE DoubleType)  = "idris_double_lte"
primFnName (EQ  DoubleType)  = "idris_double_eq"
primFnName (GTE DoubleType)  = "idris_double_gte"
primFnName (GT  DoubleType)  = "idris_double_gt"
primFnName (Add _)       = "+"
primFnName (Sub _)       = "-"
primFnName (Mul _)       = "*"
primFnName (Div _)       = "/"
primFnName (Mod _)       = "mod"
primFnName (Neg _)       = "negate"
primFnName (ShiftL _)    = "idris_shl"
primFnName (ShiftR _)    = "idris_shr"
primFnName (BAnd _)      = "andI#"
primFnName (BOr _)       = "orI#"
primFnName (BXOr _)      = "xorI#"
primFnName (LT _)        = "<"
primFnName (LTE _)       = "<="
primFnName (EQ _)        = "=="
primFnName (GTE _)       = ">="
primFnName (GT _)        = ">"
primFnName StrLength     = "str_len"
primFnName StrHead       = "idris_str_head"
primFnName StrTail       = "idris_str_tail"
primFnName StrIndex      = "idris_str_index"
primFnName StrCons       = "idris_str_cons"
primFnName StrAppend     = "str_concat"
primFnName StrReverse    = "idris_str_reverse"
primFnName StrSubstr     = "idris_str_substr"
primFnName DoubleExp     = "idris_double_exp"
primFnName DoubleLog     = "idris_double_log"
primFnName DoublePow     = "idris_double_pow"
primFnName DoubleSin     = "idris_double_sin"
primFnName DoubleCos     = "idris_double_cos"
primFnName DoubleTan     = "idris_double_tan"
primFnName DoubleASin    = "idris_double_asin"
primFnName DoubleACos    = "idris_double_acos"
primFnName DoubleATan    = "idris_double_atan"
primFnName DoubleSqrt    = "idris_double_sqrt"
primFnName DoubleFloor   = "idris_double_floor"
primFnName DoubleCeiling = "idris_double_ceiling"
primFnName (Cast x y)    = "cast-" ++ show x ++ "-" ++ show y
primFnName BelieveMe     = "idris_believe_me"
primFnName Crash         = "idris_crash"

-- | True if a Cast preserves the i64 bit-representation we use for every
-- integral / Char type — i.e. it's a no-op once values reach MLIR.
isIdentityCast : PrimType -> PrimType -> Bool
isIdentityCast StringType _      = False
isIdentityCast _ StringType      = False
isIdentityCast DoubleType _      = False
isIdentityCast _ DoubleType      = False
isIdentityCast WorldType _       = False
isIdentityCast _ WorldType       = False
isIdentityCast _ _               = True

-- | Map non-identity casts to a runtime helper name where one exists.
castFnName : PrimType -> PrimType -> String
castFnName IntegerType StringType  = "show_int"
castFnName IntType     StringType  = "show_int"
castFnName Int8Type    StringType  = "show_int"
castFnName Int16Type   StringType  = "show_int"
castFnName Int32Type   StringType  = "show_int"
castFnName Int64Type   StringType  = "show_int"
castFnName Bits8Type   StringType  = "show_int"
castFnName Bits16Type  StringType  = "show_int"
castFnName Bits32Type  StringType  = "show_int"
castFnName Bits64Type  StringType  = "show_int"
castFnName CharType    StringType  = "show_int"
castFnName IntegerType DoubleType  = "cast_Integer_Double"
castFnName IntType     DoubleType  = "cast_Integer_Double"
castFnName Int8Type    DoubleType  = "cast_Integer_Double"
castFnName Int16Type   DoubleType  = "cast_Integer_Double"
castFnName Int32Type   DoubleType  = "cast_Integer_Double"
castFnName Int64Type   DoubleType  = "cast_Integer_Double"
castFnName Bits8Type   DoubleType  = "cast_Integer_Double"
castFnName Bits16Type  DoubleType  = "cast_Integer_Double"
castFnName Bits32Type  DoubleType  = "cast_Integer_Double"
castFnName Bits64Type  DoubleType  = "cast_Integer_Double"
castFnName DoubleType  IntType     = "cast_Double_Int"
castFnName DoubleType  IntegerType = "cast_Double_Int"
castFnName DoubleType  Int8Type    = "cast_Double_Int"
castFnName DoubleType  Int16Type   = "cast_Double_Int"
castFnName DoubleType  Int32Type   = "cast_Double_Int"
castFnName DoubleType  Int64Type   = "cast_Double_Int"
castFnName DoubleType  Bits8Type   = "cast_Double_Int"
castFnName DoubleType  Bits16Type  = "cast_Double_Int"
castFnName DoubleType  Bits32Type  = "cast_Double_Int"
castFnName DoubleType  Bits64Type  = "cast_Double_Int"
castFnName DoubleType  StringType  = "cast_Double_String"
castFnName x           y           = "cast_" ++ show x ++ "_" ++ show y

vectToList : Vect n a -> List a
vectToList []        = []
vectToList (x :: xs) = x :: vectToList xs

-- | Map an Idris2 %extern primitive (NmExtPrim's Name) to a runtime
-- stub name.  %extern decls have no body — the backend is expected to
-- provide them — so the shim must rewrite the use-site reference to
-- something the Frankenstein runtime will link against.  Unknown
-- prims fall through under their original showFullName so they show
-- up as obvious unresolved symbols.
externPrimName : Name -> String
externPrimName n =
    case showFullName n of
        "Data.IORef/prim__newIORef"     => "idris2_newIORef"
        "Data.IORef/prim__readIORef"    => "idris2_readIORef"
        "Data.IORef/prim__writeIORef"   => "idris2_writeIORef"
        other                            => other

-- | Parse Idris2's foreign-call descriptor list.
-- A CCS entry has the form "backend:opt1,opt2,opt3" — for the C
-- target the first opt is the function name (see Compiler.Common.parseCC).
-- Try "C:" then "RefC:" prefixes; return the basename only.
--
-- IMPORTANT: the returned name MUST NOT collide with the Idris2
-- declaration's basename (e.g. Prelude.Types.fastUnpack defined via
-- `%foreign "RefC:fastUnpack"`), because Frankenstein's emitter does
-- suffix-matching on unqualified names — a call to "fastUnpack" would
-- otherwise resolve to the enclosing definition itself, compiling to
-- self-recursion (clang lowers the tail-call to a `jmp $` infinite
-- loop).  RefC:foo therefore becomes `idris2_foo` so the call resolves
-- to a Frankenstein runtime shim instead.  Plain C:foo names are
-- assumed to refer to user-provided C functions whose names are already
-- under the user's control, so we leave them unchanged.
parseCName : List String -> String
parseCName []          = ""
parseCName (s :: rest) =
    if isPrefixOf "C:" s     then takeUntilComma (substr 2 (length s) s)
    else if isPrefixOf "RefC:" s then "idris2_" ++ takeUntilComma (substr 5 (length s) s)
    else parseCName rest
  where
    takeUntilComma : String -> String
    takeUntilComma str = pack (takeWhile (/= ',') (unpack str))

-- ---------------------------------------------------------------- Pattern helpers

patWild : String -> String
patWild body =
    jsonObj
      [ ("pattern", jsonObj [("pat_wild", jsonObj [])])
      , ("body", body)
      ]

-- The first argument is a pre-rendered QName JSON (use 'idrisQName' so
-- pattern and construction sites agree on the {module, name} split —
-- 'Frankenstein.Core.ConTags.conKey' looks up tags by 'qnameName.text'
-- alone, so any split-vs-flat mismatch produces inconsistent tags and
-- pattern matches that never fire.
patCon : String -> List String -> String -> String
patCon qnameJson binders body =
    let bs = map (\b => jsonObj [("name", mkName b)]) binders
        patObj = jsonObj $
                   ("name", qnameJson)
                   :: (if isEmpty bs then [] else [("args", jsonArr bs)])
    in jsonObj
          [ ("pattern", jsonObj [("pat_con", patObj)])
          , ("body", body)
          ]

patLit : String -> String -> String
patLit lit body =
    jsonObj
      [ ("pattern", jsonObj [("pat_lit", lit)])
      , ("body", body)
      ]

-- ---------------------------------------------------------------- Expressions

mutual
  renderExpr : NamedCExp -> String
  renderExpr (NmLocal _ n) =
      let (_, t) = splitName n
      in jsonObj [("evar", mkName t)]
  renderExpr (NmRef _ n) =
      jsonObj [("evar", mkName (showFullName n))]
  renderExpr (NmLam _ x body) =
      let (_, xt) = splitName x
          param   = jsonObj [("name", mkName xt)]
      in jsonObj
            [ ("elam", jsonObj
                          [ ("params", jsonArr [param])
                          , ("body", renderExpr body)
                          ])
            ]
  renderExpr (NmLet _ x val body) =
      let (_, xt) = splitName x
          bind    = jsonObj [("name", mkName xt), ("expr", renderExpr val)]
      in jsonObj
            [ ("elet", jsonObj
                          [ ("binds", jsonArr [bind])
                          , ("body", renderExpr body)
                          ])
            ]
  renderExpr (NmApp _ f args) =
      jsonObj
        [ ("eapp", jsonObj
                      [ ("fn", renderExpr f)
                      , ("args", jsonArr (map renderExpr args))
                      ])
        ]
  renderExpr (NmCon _ name _ _ args) =
      let argJs = map renderExpr args
          obj   = jsonObj $
                    ("name", idrisQName name)
                    :: (if isEmpty argJs then [] else [("args", jsonArr argJs)])
      in jsonObj [("econ", obj)]
  renderExpr (NmOp _ (Cast from to) [x]) =
      if isIdentityCast from to
          then renderExpr x
          else jsonObj
                  [ ("eapp", jsonObj
                                [ ("fn", jsonObj [("evar", mkName (castFnName from to))])
                                , ("args", jsonArr [renderExpr x])
                                ])
                  ]
  renderExpr (NmOp _ BelieveMe [_, _, x]) =
      renderExpr x
  renderExpr (NmOp _ op args) =
      jsonObj
        [ ("eapp", jsonObj
                      [ ("fn", jsonObj [("evar", mkName (primFnName op))])
                      , ("args", jsonArr (vectToList (map renderExpr args)))
                      ])
        ]
  renderExpr (NmExtPrim _ p args) =
      jsonObj
        [ ("eapp", jsonObj
                      [ ("fn", jsonObj [("evar", mkName (externPrimName p))])
                      , ("args", jsonArr (map renderExpr args))
                      ])
        ]
  renderExpr (NmForce _ _ e) =
      jsonObj [("eforce", renderExpr e)]
  renderExpr (NmDelay _ _ e) =
      jsonObj [("edelay", renderExpr e)]
  renderExpr (NmConCase _ sc alts def) =
      let altsJs = map renderConAlt alts
          defJs  = case def of
                     Just d  => [patWild (renderExpr d)]
                     Nothing => []
      in jsonObj
            [ ("ecase", jsonObj
                          [ ("scrutinee", renderExpr sc)
                          , ("branches", jsonArr (altsJs ++ defJs))
                          ])
            ]
  renderExpr (NmConstCase _ sc alts def) =
      let altsJs = map renderConstAlt alts
          defJs  = case def of
                     Just d  => [patWild (renderExpr d)]
                     Nothing => []
      in jsonObj
            [ ("ecase", jsonObj
                          [ ("scrutinee", renderExpr sc)
                          , ("branches", jsonArr (altsJs ++ defJs))
                          ])
            ]
  renderExpr (NmPrimVal _ c) =
      jsonObj [("elit", constLit c)]
  renderExpr (NmErased _) =
      jsonObj [("elit", jsonObj [("int", jsonInt 0)])]
  renderExpr (NmCrash _ msg) =
      jsonObj
        [ ("eperform", jsonObj
                          [ ("effect", mkQName "" "exn")
                          , ("op", jsonStr "raise")
                          , ("args", jsonArr [jsonObj [("elit", jsonObj [("string", jsonStr msg)])]])
                          ])
        ]

  renderConAlt : NamedConAlt -> String
  renderConAlt (MkNConAlt name _ _ args body) =
      let qnameJs = idrisQName name
          binders = map (\a => snd (splitName a)) args
      in patCon qnameJs binders (renderExpr body)

  renderConstAlt : NamedConstAlt -> String
  renderConstAlt (MkNConstAlt c body) =
      patLit (constLit c) (renderExpr body)

-- ---------------------------------------------------------------- Definitions

renderDef : Name -> NamedDef -> String
renderDef name (MkNmFun args body) =
    let (m, t)  = splitName name
        -- Strip the %World arg for whichever module declares `main`.
        -- Idris2's `executable` directive can name any module (Main,
        -- Test, surd's Demo.TrigTable etc.) as the entry point; the
        -- def name is always "<that-module>.main".  Match on basename.
        isMain  = t == "main"
        bodyJs  = renderExpr body
        -- Idris2's `main : IO ()` compiles to arity-1 taking a %World
        -- token; Frankenstein's MLIR wrapper expects nullary `main`.
        -- Strip the world arg and bind it to 0 (the runtime's null world).
        paramNames : List String
        paramNames =
            if isMain
               then []
               else map (\a => snd (splitName a)) args
        finalBody : String
        finalBody =
            if isMain
               then wrapMainBody args bodyJs
               else bodyJs
        paramJs = map (\nm => jsonObj [("name", mkName nm)]) paramNames
        wrapped = if isEmpty paramJs
                     then finalBody
                     else jsonObj
                            [ ("elam", jsonObj
                                          [ ("params", jsonArr paramJs)
                                          , ("body", finalBody)
                                          ])
                            ]
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", wrapped)
          , ("sort", jsonStr "fun")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt (cast (length paramNames)))
          ]
  where
    -- Bind each Idris2 main arg (world tokens) to literal 0 around the body.
    wrapMainBody : List Name -> String -> String
    wrapMainBody []        body = body
    wrapMainBody (a :: as) body =
        let argText = snd (splitName a)
            inner   = wrapMainBody as body
            bind    = jsonObj
                        [ ("name", mkName argText)
                        , ("expr", jsonObj [("elit", jsonObj [("int", jsonInt 0)])])
                        ]
        in jsonObj
              [ ("elet", jsonObj
                            [ ("binds", jsonArr [bind])
                            , ("body", inner)
                            ])
              ]
renderDef name (MkNmCon _ arity _) =
    let (m, t) = splitName name
        body   = jsonObj [("econ", jsonObj [("name", mkQName m t)])]
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", body)
          , ("sort", jsonStr "con")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt (cast arity))
          ]
renderDef name (MkNmForeign ccs fargs _) =
    let (m, t)  = splitName name
        cname   = parseCName ccs
        arity   = length fargs
        argIdxs = if arity == 0 then [] else [0 .. natToInteger arity - 1]
        params  = map (\i => "ffi_arg_" ++ show i) argIdxs
        paramJs = map (\nm => jsonObj [("name", mkName nm)]) params
        argRefs = map (\nm => jsonObj [("evar", mkName nm)]) params
        callee  = if cname == "" then "_idris_unresolved_foreign" else cname
        callExp = jsonObj
                    [ ("eapp", jsonObj
                                  [ ("fn", jsonObj [("evar", mkName callee)])
                                  , ("args", jsonArr argRefs)
                                  ])
                    ]
        body    = if isEmpty params
                     then callExp
                     else jsonObj
                            [ ("elam", jsonObj
                                          [ ("params", jsonArr paramJs)
                                          , ("body", callExp)
                                          ])
                            ]
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", body)
          , ("sort", jsonStr "fun")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt (cast arity))
          ]
  where
    natToInteger : Nat -> Integer
    natToInteger n = cast n
renderDef name (MkNmError body) =
    let (m, t) = splitName name
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", renderExpr body)
          , ("sort", jsonStr "fun")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt 0)
          ]

-- ---------------------------------------------------------------- Top-level

renderOrganIR : List (Name, FC, NamedDef) -> String
renderOrganIR defs =
    let defsJs = map (\(n, _, d) => renderDef n d) defs
        meta = jsonObj
                  [ ("source_language", jsonStr "idris2")
                  , ("shim_version", jsonStr "0.1.0")
                  ]
        modObj = jsonObj
                    [ ("name", jsonStr "main")
                    , ("definitions", jsonArr defsJs)
                    ]
    in jsonObj
          [ ("schema_version", jsonStr "0.1.0")
          , ("metadata", meta)
          , ("module", modObj)
          ]

-- ---------------------------------------------------------------- Codegen

compileExpr : Ref Ctxt Defs -> Ref Syn SyntaxInfo ->
              (tmpDir : String) -> (outputDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c s tmpDir outputDir tm outfile
    = do cdata <- getCompileData False Cases tm
         let defs = namedDefs cdata
         let json = renderOrganIR defs
         let outPath = outputDir ++ "/" ++ outfile ++ ".organ.json"
         Right () <- coreLift $ writeFile outPath json
            | Left err => do
                coreLift_ $ putStrLn ("organir-cg: write failed: " ++ show err)
                pure Nothing
         coreLift_ $ putStrLn
            ("organir-cg: " ++ show (length defs) ++ " definitions -> " ++ outPath)
         pure (Just outPath)

executeExpr : Ref Ctxt Defs -> Ref Syn SyntaxInfo ->
              (execDir : String) -> ClosedTerm -> Core ()
executeExpr c s tmpDir tm
    = coreLift_ $ putStrLn "organir-cg: execute not implemented"

codegenOrganIR : Codegen
codegenOrganIR = MkCG compileExpr executeExpr Nothing Nothing

main : IO ()
main = mainWithCodegens [("organir", codegenOrganIR)]

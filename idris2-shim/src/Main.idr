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

primFnName : {arity : Nat} -> PrimFn arity -> String
primFnName f = show f

vectToList : Vect n a -> List a
vectToList []        = []
vectToList (x :: xs) = x :: vectToList xs

-- ---------------------------------------------------------------- Pattern helpers

patWild : String -> String
patWild body =
    jsonObj
      [ ("pattern", jsonObj [("pat_wild", jsonObj [])])
      , ("body", body)
      ]

patCon : String -> List String -> String -> String
patCon name binders body =
    let bs = map (\b => jsonObj [("name", mkName b)]) binders
        patObj = jsonObj $
                   ("name", mkQName "" name)
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
                      [ ("fn", jsonObj [("evar", mkName (showFullName p))])
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
      let patName = showFullName name
          binders = map (\a => snd (splitName a)) args
      in patCon patName binders (renderExpr body)

  renderConstAlt : NamedConstAlt -> String
  renderConstAlt (MkNConstAlt c body) =
      patLit (constLit c) (renderExpr body)

-- ---------------------------------------------------------------- Definitions

renderDef : Name -> NamedDef -> String
renderDef name (MkNmFun args body) =
    let (m, t) = splitName name
        paramJs = map (\a => jsonObj [("name", mkName (snd (splitName a)))]) args
        bodyJs  = renderExpr body
        wrapped = if isEmpty args
                     then bodyJs
                     else jsonObj
                            [ ("elam", jsonObj
                                          [ ("params", jsonArr paramJs)
                                          , ("body", bodyJs)
                                          ])
                            ]
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", wrapped)
          , ("sort", jsonStr "fun")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt (cast (length args)))
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
renderDef name (MkNmForeign ccs _ _) =
    let (m, t) = splitName name
        ccsStr = fastConcat (intersperse "," ccs)
        body   = jsonObj [("evar", mkName ccsStr)]
    in jsonObj
          [ ("name", mkQName m t)
          , ("type", anyTy)
          , ("expr", body)
          , ("sort", jsonStr "external")
          , ("visibility", jsonStr "public")
          , ("arity", jsonInt 0)
          ]
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

module Main

import Idris.Driver
import Compiler.Common
import Compiler.CompileExpr
import Core.Context
import Core.Core
import Core.Name
import Idris.Syntax

import System

compileExpr : Ref Ctxt Defs -> Ref Syn SyntaxInfo ->
              (tmpDir : String) -> (outputDir : String) ->
              ClosedTerm -> (outfile : String) -> Core (Maybe String)
compileExpr c s tmpDir outputDir tm outfile
    = do cdata <- getCompileData False Cases tm
         let defs = namedDefs cdata
         coreLift_ $ putStrLn ("organir-cg: " ++ show (length defs) ++ " definitions")
         pure (Just outfile)

executeExpr : Ref Ctxt Defs -> Ref Syn SyntaxInfo ->
              (execDir : String) -> ClosedTerm -> Core ()
executeExpr c s tmpDir tm
    = coreLift_ $ putStrLn "organir-cg: execute not implemented"

codegenOrganIR : Codegen
codegenOrganIR = MkCG compileExpr executeExpr Nothing Nothing

main : IO ()
main = mainWithCodegens [("organir", codegenOrganIR)]

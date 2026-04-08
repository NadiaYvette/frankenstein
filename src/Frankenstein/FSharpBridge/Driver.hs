-- | Run an F# AST dumper (@dotnet fsi@ + bundled @fsdump.fsx@) that
-- uses @FSharp.Compiler.Service@ to parse the source and walk the
-- untyped syntax tree ('FSharp.Compiler.Syntax'), emitting a tiny
-- s-expression representation. The s-expression is then read by the
-- shared Scheme reader and translated in 'CoreTranslate'.
module Frankenstein.FSharpBridge.Driver
  ( readFSharpFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Frankenstein.SchemeBridge.Reader (SExpr, readScheme)

-- | The F# script that walks the parsetree and emits s-expressions.
--
-- The shape is:
--
-- >  (module <name> (let <name> (<args...>) <body>) ...)
--
-- where expressions are @(int N)@, @(var sym)@, @(app f a)@,
-- @(if c t e)@, @(letin (<binds...>) <body>)@, @(seq e1 e2)@.
--
-- The script references @FSharp.Compiler.Service.dll@ from the .NET
-- SDK directly — no NuGet resolution or network access required.
dumpFsx :: Text
dumpFsx = T.unlines
  [ "#r \"/usr/lib64/dotnet/sdk/10.0.104/FSharp/FSharp.Compiler.Service.dll\""
  , "open FSharp.Compiler.CodeAnalysis"
  , "open FSharp.Compiler.Syntax"
  , "open FSharp.Compiler.Text"
  , "open FSharp.Compiler.SyntaxTrivia"
  , "open System.Text"
  , ""
  , "let sb = StringBuilder()"
  , "let emit (s: string) = sb.Append(s) |> ignore"
  , ""
  , "let rec emitExpr (e: SynExpr) ="
  , "  match e with"
  , "  | SynExpr.Const(SynConst.Int32 n, _) -> emit (sprintf \"(int %d)\" n)"
  , "  | SynExpr.Const(SynConst.Unit, _) -> emit \"(unit)\""
  , "  | SynExpr.Const(SynConst.String(s, _, _), _) -> emit (sprintf \"(str %d)\" (hash s))"
  , "  | SynExpr.Ident id -> emit (sprintf \"(var %s)\" id.idText)"
  , "  | SynExpr.LongIdent(_, SynLongIdent(ids, _, trivia), _, _) ->"
  , "      let name ="
  , "        match trivia with"
  , "        | [Some(IdentTrivia.OriginalNotation op)] -> op"
  , "        | _ -> ids |> List.map (fun i -> i.idText) |> String.concat \".\""
  , "      emit (sprintf \"(var %s)\" name)"
  , "  | SynExpr.Paren(inner, _, _, _) -> emitExpr inner"
  , "  | SynExpr.Typed(inner, _, _) -> emitExpr inner"
  , "  | SynExpr.App(_, _, f, a, _) ->"
  , "      emit \"(app \""
  , "      emitExpr f"
  , "      emit \" \""
  , "      emitExpr a"
  , "      emit \")\""
  , "  | SynExpr.IfThenElse(ifExpr = c; thenExpr = t; elseExpr = eOpt) ->"
  , "      emit \"(if \""
  , "      emitExpr c"
  , "      emit \" \""
  , "      emitExpr t"
  , "      emit \" \""
  , "      (match eOpt with"
  , "       | Some e -> emitExpr e"
  , "       | None -> emit \"(int 0)\")"
  , "      emit \")\""
  , "  | SynExpr.LetOrUse(bindings = bs; body = body) ->"
  , "      emit \"(letin (\""
  , "      for b in bs do emitBinding b; emit \" \""
  , "      emit \") \""
  , "      emitExpr body"
  , "      emit \")\""
  , "  | SynExpr.Sequential(_, _, e1, e2, _, _) ->"
  , "      emit \"(seq \""
  , "      emitExpr e1"
  , "      emit \" \""
  , "      emitExpr e2"
  , "      emit \")\""
  , "  | other -> emit (sprintf \"(unk %s)\" (other.GetType().Name))"
  , ""
  , "and emitBinding (SynBinding(headPat = hp; expr = body)) ="
  , "  let name, args ="
  , "    match hp with"
  , "    | SynPat.LongIdent(longDotId = SynLongIdent(ids,_,_); argPats = SynArgPats.Pats pats) ->"
  , "        let nm = ids |> List.map (fun i -> i.idText) |> String.concat \".\""
  , "        nm, pats"
  , "    | SynPat.Named(ident = SynIdent(id,_)) -> id.idText, []"
  , "    | _ -> \"_unknown\", []"
  , "  let rec patNames (p: SynPat) : string list ="
  , "    match p with"
  , "    | SynPat.Named(ident = SynIdent(id,_)) -> [id.idText]"
  , "    | SynPat.Paren(p2, _) -> patNames p2"
  , "    | SynPat.Typed(p2, _, _) -> patNames p2"
  , "    | SynPat.Const(SynConst.Unit, _) -> []"
  , "    | _ -> [\"_\"]"
  , "  let argNames = args |> List.collect patNames"
  , "  emit \"(let \""
  , "  emit name"
  , "  emit \" (\""
  , "  for a in argNames do emit (sprintf \"%s \" a)"
  , "  emit \") \""
  , "  emitExpr body"
  , "  emit \")\""
  , ""
  , "let emitDecl (d: SynModuleDecl) ="
  , "  match d with"
  , "  | SynModuleDecl.Let(_, bs, _) ->"
  , "      for b in bs do emitBinding b; emit \"\\n\""
  , "  | SynModuleDecl.Expr(e, _) ->"
  , "      emit \"(do \"; emitExpr e; emit \")\\n\""
  , "  | _ -> ()"
  , ""
  , "let args = System.Environment.GetCommandLineArgs()"
  , "let path = args |> Array.last"
  , "let src = System.IO.File.ReadAllText path"
  , "let sourceText = SourceText.ofString src"
  , "let po = { FSharpParsingOptions.Default with SourceFiles = [|path|] }"
  , "let checker = FSharpChecker.Create()"
  , "let pres = checker.ParseFile(path, sourceText, po) |> Async.RunSynchronously"
  , "match pres.ParseTree with"
  , "| ParsedInput.ImplFile(ParsedImplFileInput(contents = ms)) ->"
  , "    for m in ms do"
  , "      match m with"
  , "      | SynModuleOrNamespace(longId = longId; decls = decls) ->"
  , "          let nm = longId |> List.map (fun i -> i.idText) |> String.concat \".\""
  , "          emit (sprintf \"(module %s\\n\" nm)"
  , "          for d in decls do emitDecl d"
  , "          emit \")\\n\""
  , "| _ -> ()"
  , "printfn \"%s\" (sb.ToString())"
  ]

-- | Invoke @dotnet fsi@ on the bundled script and parse the emitted
-- s-expressions.
readFSharpFile :: FilePath -> IO (Either Text [SExpr])
readFSharpFile path = withSystemTempDirectory "frankenstein-fsharp" $ \tmpDir -> do
  let scriptPath = tmpDir </> "fsdump.fsx"
  TIO.writeFile scriptPath dumpFsx
  (ec, out, err) <- readProcessWithExitCode "dotnet" ["fsi", scriptPath, path] ""
  case ec of
    ExitSuccess   -> pure (readScheme (T.pack out))
    ExitFailure _ ->
      pure (Left ("dotnet fsi failed:\n" <> T.pack err <> "\n" <> T.pack out))

-- | Run an escript that scans and parses an Erlang source file and
-- prints the resulting AST as an Erlang term.
module Frankenstein.ErlangBridge.Driver
  ( readErlangFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Frankenstein.ErlangBridge.AstParse

-- | The escript source that dumps an Erlang file's AST. It:
--
--   1. Reads the file
--   2. Tokenizes via @erl_scan:string/1@
--   3. Splits on dots into forms
--   4. Parses each form with @erl_parse:parse_form/1@
--   5. Prints the resulting list with @~p@ (no location lines)
dumpAstScript :: Text
dumpAstScript = T.unlines
  [ "#!/usr/bin/env escript"
  , "main([InFile]) ->"
  , "    {ok, Bin} = file:read_file(InFile),"
  , "    Src = binary_to_list(Bin),"
  , "    {ok, Toks, _} = erl_scan:string(Src),"
  , "    Forms = split_forms(Toks, [], []),"
  , "    ParsedForms = [begin"
  , "        {ok, F} = erl_parse:parse_form(FTs),"
  , "        F"
  , "    end || FTs <- Forms],"
  , "    io:format(\"~p~n\", [ParsedForms])."
  , ""
  , "split_forms([], _Acc, Out) -> lists:reverse(Out);"
  , "split_forms([{dot,_}=Dot|Rest], Acc, Out) ->"
  , "    split_forms(Rest, [], [lists:reverse([Dot|Acc])|Out]);"
  , "split_forms([T|Rest], Acc, Out) ->"
  , "    split_forms(Rest, [T|Acc], Out)."
  ]

readErlangFile :: FilePath -> IO (Either Text ETerm)
readErlangFile path = withSystemTempDirectory "frankenstein-erlang" $ \tmpDir -> do
  let scriptPath = tmpDir </> "dumpast.escript"
  TIO.writeFile scriptPath dumpAstScript
  (ec, out, err) <- readProcessWithExitCode "escript" [scriptPath, path] ""
  case ec of
    ExitSuccess   -> pure (parseErlangTerms (T.pack out))
    ExitFailure _ -> pure (Left ("escript failed:\n" <> T.pack err))

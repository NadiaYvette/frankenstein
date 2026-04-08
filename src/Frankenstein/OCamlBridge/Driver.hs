-- | Shell out to @ocamlc -stop-after parsing -dparsetree@ and return
-- the parsed tree.
module Frankenstein.OCamlBridge.Driver
  ( readOCamlFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Frankenstein.OCamlBridge.AstParse

readOCamlFile :: FilePath -> IO (Either Text OLine)
readOCamlFile path = do
  -- ocamlc writes the parsetree to stderr when given -dparsetree.
  (ec, out, err) <- readProcessWithExitCode
                      "ocamlc" ["-stop-after", "parsing", "-dparsetree", path] ""
  let text = if null out then err else out
  case ec of
    ExitSuccess   -> pure (parseOCamlParsetree (T.pack text))
    ExitFailure _ -> pure (Left ("ocamlc -dparsetree failed:\n" <> T.pack err))

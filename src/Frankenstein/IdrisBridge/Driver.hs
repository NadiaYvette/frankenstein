-- | Read an @.idr@ source file and parse it with our tiny in-tree
-- source parser. Unlike most other bridges we don't shell out to the
-- donor compiler at all: Idris 2 has no public parsetree dump CLI,
-- and reaching for the internal library would dwarf the rest of the
-- bridge. See 'Frankenstein.IdrisBridge.Parse' for what we accept.
module Frankenstein.IdrisBridge.Driver
  ( readIdrisFile
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import Frankenstein.IdrisBridge.Parse

readIdrisFile :: FilePath -> IO (Either Text [IDecl])
readIdrisFile path = do
  src <- TIO.readFile path
  pure (parseIdris src)

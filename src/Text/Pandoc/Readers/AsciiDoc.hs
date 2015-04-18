module Text.Pandoc.Readers.AsciiDoc (
  readAsciiDoc
) where

import qualified Data.Map as Map
import Text.Pandoc.Definition
import Text.Pandoc.Options

readAsciiDoc :: ReaderOptions
             -> String
             -> Pandoc
readAsciiDoc opts s = Pandoc (Meta (Map.singleton "foo" (MetaString "bar"))) []

module Text.Pandoc.Readers.AsciiDoc (
  readAsciiDoc
) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import qualified Text.Pandoc.Builder as B

type AsciiDocParser = Parser String ParserState

readAsciiDoc :: ReaderOptions
             -> String
             -> Pandoc
-- readAsciiDoc opts s = Pandoc (Meta (Map.singleton "foo" (MetaString "bar"))) []
readAsciiDoc opts s =
  (readWith parseAsciiDoc) def{ stateOptions = opts } (s ++ "\n\n")

parseAsciiDoc :: AsciiDocParser Pandoc
parseAsciiDoc = do
  -- markdown allows raw HTML
  let meta = (Meta (Map.singleton "foo" (MetaString "bar")))
  return $ B.doc (B.Many Seq.empty)

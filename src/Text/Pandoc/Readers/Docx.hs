module Text.Pandoc.Readers.Docx (readDocx) where

import Text.Pandoc.Parsing
import Text.Pandoc.Definition
import Data.ByteString.Lazy.Char8 (ByteString)

-- | Convert OpenXML-formatted string to 'Pandoc' document.
readDocx :: ParserState   -- ^ Parser state
         -> ByteString    -- ^ ByteString to parse
         -> Pandoc
readDocx _ _ = Pandoc (Meta [] [] []) [Para [Str "hello world"]]

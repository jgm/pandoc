module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Text.Pandoc.Parsing (ParserState(..), defaultParserState)
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Data.Monoid
import Data.Char (isSpace)

readDocBook :: ParserState -> String -> Pandoc
readDocBook st inp = Pandoc (Meta [] [] []) $ toList blocks
  where blocks = mconcat $ map (parseBlock st) $ parseXML inp

parseBlock :: ParserState -> Content -> Blocks
parseBlock st (Text (CData _ s _)) = if all isSpace s
                                        then mempty
                                        else plain $ text s
parseBlock st (Elem e) =
  case qName (elName e) of
        "para" -> para $ trimInlines $ mconcat
                       $ map (parseInline st) $ elContent e
        _      -> mconcat $ map (parseBlock st) $ elContent e
parseBlock st (CRef _) = mempty

parseInline :: ParserState -> Content -> Inlines
parseInline st (Text (CData _ s _)) = text s
parseInline st (Elem e) =
  case qName (elName e) of
        "emphasis" -> case lookupAttrBy (\attr -> qName attr == "role")
                           (elAttribs e) of
                             Just "strong" -> strong innerInlines
                             _             -> emph innerInlines
        _          -> innerInlines
   where innerInlines = trimInlines . mconcat . map (parseInline st)
                        $ elContent e
parseInline st (CRef _) = mempty


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.XMLFormat
  ( decodeAttrName,
    encodeAttrName,
    atNameAlignment,
    atNameApiVersion,
    atNameCitationHash,
    atNameCitationMode,
    atNameCitationNoteNum,
    atNameColspan,
    atNameColWidth,
    atNameFormat,
    atNameImageUrl,
    atNameLevel,
    atNameLinkUrl,
    atNameMathType,
    atNameMetaBoolValue,
    atNameMetaMapEntryKey,
    atNameNumberDelim,
    atNameNumberStyle,
    atNameQuoteType,
    atNameRowHeadColumns,
    atNameRowspan,
    atNameSpaceCount,
    atNameStart,
    atNameStrContent,
    atNameTitle,
    tgNameBodyBody,
    tgNameBodyHeader,
    tgNameCitations,
    tgNameCitationPrefix,
    tgNameCitationSuffix,
    tgNameColspecs,
    tgNameDefListDef,
    tgNameDefListItem,
    tgNameDefListTerm,
    tgNameLineItem,
    tgNameListItem,
    tgNameMetaMapEntry,
    tgNameShortCaption,
  )
where

import Data.Char (chr, digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

-- | Encode an attribute name so that it is a valid XML name.
-- Characters that are not allowed in XML names -- and colons, which
-- XML parsers treat as namespace separators -- are encoded as
-- _xHHHH_, where HHHH is the hexadecimal code of the character
-- (uppercase, at least four digits).  An underscore that introduces
-- a literal "_x" is encoded as _x005F_, so that decoding is
-- unambiguous.  For example, "typst:property" is encoded as
-- "typst_x003A_property".
encodeAttrName :: Text -> Text
encodeAttrName name =
  case T.unpack name of
    [] -> name
    c : cs
      | isNameStartChar c && all isNameChar cs && not ("_x" `T.isInfixOf` name) ->
          name
      | otherwise ->
          T.pack $ concat $ go isNameStartChar (c : cs)
  where
    go _ [] = []
    go ok (c : cs)
      | c == '_' && take 1 cs == "x" = encodeChar '_' : go isNameChar cs
      | ok c = [c] : go isNameChar cs
      | otherwise = encodeChar c : go isNameChar cs
    encodeChar c = "_x" ++ replicate (4 - length h) '0' ++ h ++ "_"
      where
        h = map toUpper $ showHex (ord c) ""

-- | Decode an attribute name encoded by 'encodeAttrName': _xHHHH_
-- sequences (four to six hexadecimal digits) are decoded to the
-- character with the given code.
decodeAttrName :: Text -> Text
decodeAttrName name
  | "_x" `T.isInfixOf` name = T.concat $ go name
  | otherwise = name
  where
    go t =
      case T.breakOn "_x" t of
        (pre, rest)
          | T.null rest -> [pre]
          | otherwise ->
              let body = T.drop 2 rest
                  digits = T.takeWhile isHexDigit body
                  n = T.length digits
                  code = T.foldl' (\acc d -> 16 * acc + digitToInt d) 0 digits
               in if n >= 4
                    && n <= 6
                    && "_" `T.isPrefixOf` T.drop n body
                    && validChar code
                    then pre : T.singleton (chr code) : go (T.drop (n + 1) body)
                    else pre : "_x" : go body
    validChar code = code <= 0x10FFFF && not (code >= 0xD800 && code <= 0xDFFF)

-- the XML NameStartChar production, without the colon (which XML
-- parsers treat as a namespace separator)
isNameStartChar :: Char -> Bool
isNameStartChar c =
  isAsciiUpper c
    || isAsciiLower c
    || c == '_'
    || (c >= '\xC0' && c <= '\xD6')
    || (c >= '\xD8' && c <= '\xF6')
    || (c >= '\xF8' && c <= '\x2FF')
    || (c >= '\x370' && c <= '\x37D')
    || (c >= '\x37F' && c <= '\x1FFF')
    || (c >= '\x200C' && c <= '\x200D')
    || (c >= '\x2070' && c <= '\x218F')
    || (c >= '\x2C00' && c <= '\x2FEF')
    || (c >= '\x3001' && c <= '\xD7FF')
    || (c >= '\xF900' && c <= '\xFDCF')
    || (c >= '\xFDF0' && c <= '\xFFFD')
    || (c >= '\x10000' && c <= '\xEFFFF')

-- the XML NameChar production, without the colon
isNameChar :: Char -> Bool
isNameChar c =
  isNameStartChar c
    || isDigit c
    || c == '-'
    || c == '.'
    || c == '\xB7'
    || (c >= '\x300' && c <= '\x36F')
    || (c >= '\x203F' && c <= '\x2040')

-- the attribute carrying the API version of pandoc types in the main Pandoc element
atNameApiVersion :: Text
atNameApiVersion = "api-version"

-- the element of a <meta> or <MetaMap> entry
tgNameMetaMapEntry :: Text
tgNameMetaMapEntry = "entry"

-- the attribute carrying the key name of a <meta> or <MetaMap> entry
atNameMetaMapEntryKey :: Text
atNameMetaMapEntryKey = "key"

-- the attribute carrying the boolean value ("true" or "false") of a MetaBool
atNameMetaBoolValue :: Text
atNameMetaBoolValue = "value"

-- level of a Header
atNameLevel :: Text
atNameLevel = "level"

-- start number of an OrderedList
atNameStart :: Text
atNameStart = "start"

-- number delimiter of an OrderedList
atNameNumberDelim :: Text
atNameNumberDelim = "number-delim"

-- number style of an OrderedList
atNameNumberStyle :: Text
atNameNumberStyle = "number-style"

-- target title in Image and Link
atNameTitle :: Text
atNameTitle = "title"

-- target url in Image
atNameImageUrl :: Text
atNameImageUrl = "src"

-- target url in Link
atNameLinkUrl :: Text
atNameLinkUrl = "href"

-- QuoteType of a Quoted
atNameQuoteType :: Text
atNameQuoteType = "quote-type"

-- MathType of a Math
atNameMathType :: Text
atNameMathType = "math-type"

-- format of a RawInline or a RawBlock
atNameFormat :: Text
atNameFormat = "format"

-- alignment attribute in a ColSpec or in a Cell
atNameAlignment :: Text
atNameAlignment = "alignment"

-- ColWidth attribute in a ColSpec
atNameColWidth :: Text
atNameColWidth = "col-width"

-- RowHeadColumns attribute in a TableBody
atNameRowHeadColumns :: Text
atNameRowHeadColumns = "row-head-columns"

-- RowSpan attribute in a Cell
atNameRowspan :: Text
atNameRowspan = "row-span"

-- ColSpan attribute in a Cell
atNameColspan :: Text
atNameColspan = "col-span"

-- the citationMode of a Citation
atNameCitationMode :: Text
atNameCitationMode = "mode"

-- the citationHash of a Citation
atNameCitationHash :: Text
atNameCitationHash = "hash"

-- the citationNoteNum of a Citation
atNameCitationNoteNum :: Text
atNameCitationNoteNum = "note-num"

-- the number of consecutive spaces of the <Space> element
atNameSpaceCount :: Text
atNameSpaceCount = "count"

-- the content of the <Str> element
atNameStrContent :: Text
atNameStrContent = "content"

-- container of Citation elements in Cite inlines
tgNameCitations :: Text
tgNameCitations = "citations"

-- element around the prefix inlines of a Citation
tgNameCitationPrefix :: Text
tgNameCitationPrefix = "prefix"

-- element around the suffix inlines of a Citation
tgNameCitationSuffix :: Text
tgNameCitationSuffix = "suffix"

-- list item for BulletList and OrderedList
tgNameListItem :: Text
tgNameListItem = "item"

-- list item for DefinitionList
tgNameDefListItem :: Text
tgNameDefListItem = "item"

-- element around the inlines of the term of a DefinitionList item
tgNameDefListTerm :: Text
tgNameDefListTerm = "term"

-- element around the blocks of a definition in a DefinitionList item
tgNameDefListDef :: Text
tgNameDefListDef = "def"

-- optional element of the ShortCaption
tgNameShortCaption :: Text
tgNameShortCaption = "ShortCaption"

-- element around the ColSpec of a Table
tgNameColspecs :: Text
tgNameColspecs = "colspecs"

-- element around the header rows of a TableBody
tgNameBodyHeader :: Text
tgNameBodyHeader = "header"

-- element around the body rows of a TableBody
tgNameBodyBody :: Text
tgNameBodyBody = "body"

-- element around the inlines of a line in a LineBlock
tgNameLineItem :: Text
tgNameLineItem = "line"

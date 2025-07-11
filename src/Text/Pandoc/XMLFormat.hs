{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.XMLFormat
  ( atNameAlignment,
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

import Data.Text (Text)

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

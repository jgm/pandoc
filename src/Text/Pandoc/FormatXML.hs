{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.FormatXML
  ( attrAlignment,
    attrApiVersion,
    attrCitationHash,
    attrCitationMode,
    attrCitationNoteNum,
    attrColspan,
    attrColWidth,
    attrFormat,
    attrImageUrl,
    attrLevel,
    attrLinkUrl,
    attrMathType,
    attrMetaBoolValue,
    attrMetaMapEntryKey,
    attrNumberDelim,
    attrNumberStyle,
    attrQuoteType,
    attrRowHeadColumns,
    attrRowspan,
    attrStart,
    attrTitle,
    tagBodyBody,
    tagBodyHeader,
    tagCitations,
    tagCitationPrefix,
    tagCitationSuffix,
    tagColspecs,
    tagDefListDef,
    tagDefListItem,
    tagDefListTerm,
    tagLineItem,
    tagListItem,
    tagMetaMapEntry,
    tagShortCaption
  )
where

import Data.Text (Text)

-- the attribute carrying the API version of pandoc types in the main Pandoc element
attrApiVersion :: Text
attrApiVersion = "api-version"

-- the element of a <meta> or <MetaMap> entry
tagMetaMapEntry :: Text
tagMetaMapEntry = "entry"

-- the attribute carrying the key name of a <meta> or <MetaMap> entry
attrMetaMapEntryKey :: Text
attrMetaMapEntryKey = "text"

-- the attribute carrying the boolean value ("true" or "false") of a MetaBool
attrMetaBoolValue :: Text
attrMetaBoolValue = "value"

-- level of a Header
attrLevel :: Text
attrLevel = "level"

-- start number of an OrderedList
attrStart :: Text
attrStart = "start"

-- number delimiter of an OrderedList
attrNumberDelim :: Text
attrNumberDelim = "number-delim"

-- number style of an OrderedList
attrNumberStyle :: Text
attrNumberStyle = "number-style"

-- target title in Image and Link
attrTitle :: Text
attrTitle = "title"

-- target url in Image
attrImageUrl :: Text
attrImageUrl = "src"

-- target url in Link
attrLinkUrl :: Text
attrLinkUrl = "href"

-- QuoteType of a Quoted
attrQuoteType :: Text
attrQuoteType = "quote-type"

-- MathType of a Math
attrMathType :: Text
attrMathType = "math-type"

-- format of a RawInline or a RawBlock
attrFormat :: Text
attrFormat = "format"

-- alignment attribute in a ColSpec or in a Cell
attrAlignment :: Text
attrAlignment = "alignment"

-- ColWidth attribute in a ColSpec
attrColWidth :: Text
attrColWidth = "col-width"

-- RowHeadColumns attribute in a TableBody
attrRowHeadColumns :: Text
attrRowHeadColumns = "row-head-columns"

-- rowspan attribute in a Cell
attrRowspan :: Text
attrRowspan = "rowspan"

-- colpan attribute in a Cell
attrColspan :: Text
attrColspan = "colspan"

-- the citationMode of a Citation
attrCitationMode :: Text
attrCitationMode = "mode"

-- the citationHash of a Citation
attrCitationHash :: Text
attrCitationHash = "hash"

-- the citationNoteNum of a Citation
attrCitationNoteNum :: Text
attrCitationNoteNum = "note-num"

-- container of Citation elements in Cite inlines
tagCitations :: Text
tagCitations = "citations"

-- element around the prefix inlines of a Citation
tagCitationPrefix :: Text
tagCitationPrefix = "prefix"

-- element around the suffix inlines of a Citation
tagCitationSuffix :: Text
tagCitationSuffix = "suffix"

-- list item for BulletList and OrderedList
tagListItem :: Text
tagListItem = "item"

-- list item for DefinitionList
tagDefListItem :: Text
tagDefListItem = "item"

-- element around the inlines of the term of a DefinitionList item
tagDefListTerm :: Text
tagDefListTerm = "term"

-- element around the blocks of a definition in a DefinitionList item
tagDefListDef :: Text
tagDefListDef = "def"

-- optional element of the ShortCaption
tagShortCaption :: Text
tagShortCaption = "ShortCaption"

-- element around the ColSpec of a Table
tagColspecs :: Text
tagColspecs = "colspecs"

-- element around the header rows of a TableBody
tagBodyHeader :: Text
tagBodyHeader = "header"

-- element around the body rows of a TableBody
tagBodyBody :: Text
tagBodyBody = "body"

-- element around the inlines of a line in a LineBlock
tagLineItem :: Text
tagLineItem = "line"
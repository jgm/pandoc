{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Format.XML
  (
    attrImageUrl,
    attrLevel,
    attrLinkUrl,
    attrNumberDelim,
    attrNumberStyle,
    attrStart,
    attrTitle,
    tagBodyBody,
    tagBodyHeader,
    tagCitations,
    tagColspecs,
    tagDefListDef,
    tagDefListItem,
    tagDefListTerm,
    tagListItem,
  )
where

import Data.Text (Text)

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

-- container of Citation elements in Cite inlines
tagCitations :: Text
tagCitations = "citations"

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

-- element around the ColSpec of a Table
tagColspecs :: Text
tagColspecs = "colspecs"

-- tag around the header rows of a TableBody
tagBodyHeader :: Text
tagBodyHeader = "header"

-- tag around the body rows of a TableBody
tagBodyBody :: Text
tagBodyBody = "body"
{-# OPTIONS_GHC -fwarn-unused-imports #-}

{- |
  by xyne.archlinux.com - released under GPL 2
  # About
  This is a Pandoc writer for generating Sable output. Sable is an XML-based
  document format speech synthesis. This module can be used to generate spoken
  versions of any input format that Pandoc understands. When compiled as a
  stand-alone application it will create an executable that converts Markdown
  on STDIN to Sable on STDOUT.

  # Example

  To produce audio on the fly:

      $ festival --tts <sable file>

  To generate .wav and .mp3 files (text2wave is included with festival):

      $ text2wav <sable file>
      $ lame -V2 <wav file>

  # References
  * [Sable version 1.0](http://www.bell-labs.com/project/tts/sable.html)
-}

module Text.Pandoc.Writers.Sable
  (
    writeSable
  ) where

import Text.Pandoc.Definition hiding (Attr)
import Text.Pandoc.Options
import Text.XML.Light

------------------------------------- XML --------------------------------------

-- | Sable DOCTYPE string.
doctype :: String
doctype = "<!DOCTYPE SABLE PUBLIC \"-//SABLE//DTD SABLE speech mark up//EN\" \"Sable.v0_2.dtd\" []>"



-- | Convert the document to a string with the XML header and DOCTYPE.
pp :: [Element] -> String
pp e = unlines
  [
    xml_header,
    doctype,
    (ppElement $ node (unqual "SABLE") e)
  ]



-- | Convert a key-value pair to an unqualified Attr.
unqualAttr :: String -> String -> Attr
unqualAttr k v = Attr
  {
    attrKey = unqual k,
    attrVal = v
  }



-- | The tag used to wrap plain text.
dummyTag :: String
dummyTag = "DIV"



-- | Wrap a node type in a dummy element.
dummyElement :: (Node t) => t -> Element
dummyElement = node (unqual dummyTag)



-- | Concat plaintext elements.
concat' :: [Element] -> [Element]
concat' = (f [])
  where
    extract ~(Text (CData {cdData = s})) = s

    f :: String -> [Element] -> [Element]
    f s ((Element { elName = name, elAttribs = [], elContent = cs }):es)
      | name == (unqual dummyTag) = f (s ++ (concat $ map extract cs)) es
    f s (e:es) =
      if null s
      then e : (f [] es)
      else (naiveBreaks [] s) ++ (e : (f [] es))
    f s [] =
      if null s
      then []
      else naiveBreaks [] s



-- | Naively insert breaks in the text.
naiveBreaks :: String -> String -> [Element]
naiveBreaks s [] = (rawText s) : []
naiveBreaks s (c:' ':cs) | elem c "?!.,:;" = punctuationBreak c s (naiveBreaks [] cs)
naiveBreaks s (c:[]) | elem c "?!.,:;" = punctuationBreak c s []
naiveBreaks s (c:cs) = naiveBreaks (s ++ [c]) cs



-- | Internal function for naiveBreaks.
punctuationBreak :: Char -> String -> ([Element] -> [Element])
punctuationBreak c s =
  let
    typ =
      case c of
        ':' -> "."
        ';' -> ","
        _ -> c:[]
  in
    ((rawText s) :) .
    ((node (unqual "BREAK") [unqualAttr "LEVEL" "Medium", unqualAttr "TYPE" typ]) :)



-- | Insert raw text.
rawText :: String -> Element
rawText s = dummyElement $ CData
  {
    cdVerbatim = CDataText,
    cdData = s,
    cdLine = Nothing
  }



-- | Insert URL.
url :: String -> Element
url u = node (unqual "SAYAS") ([unqualAttr "MODE" "net", unqualAttr "MODETYPE" "URL"], u)



{- |
  Spoken metadata delimiters, such as "begin title level 1" and "end title level
  1".
-}
speakMeta :: [Element] -> [Element] -> [Element]
speakMeta tag es =
  if null es
  then
    [
      node (unqual "VOLUME") ([unqualAttr "LEVEL" "quiet"], concat' $ tag),
      node (unqual "BREAK") [unqualAttr "LEVEL" "Small", unqualAttr "TYPE" "!"]
    ]
  else
    (
      node
        (unqual "VOLUME")
        (
          [unqualAttr "LEVEL" "quiet"],
          concat' $ (rawText "begin ") : tag
        )
    ) :
    (node (unqual "BREAK") [unqualAttr "LEVEL" "Small", unqualAttr "TYPE" "!"]) :
    es ++
    (
      node
        (unqual "VOLUME")
        (
          [unqualAttr "LEVEL" "quiet"],
          concat' $ (rawText "end ") : tag
        )
    ) :
    node (unqual "BREAK") [unqualAttr "LEVEL" "Small", unqualAttr "TYPE" "!"] :
    []



{- |
  Speak all characters (including whitespace) literally and slowly. This is
  meant for code and raw data.
-}
speakAllLiteral :: String -> Element
speakAllLiteral s =
  node (unqual "RATE") ([unqualAttr "SPEED" "slow"], handleNotWhitespace s)
  where
    whitespace = " \t\r\n"

    isWhitespace = (`elem` whitespace)

    spokenWhitespace ' ' = " space "
    spokenWhitespace '\t' = " tab "
    spokenWhitespace '\r' = " carriage return "
    spokenWhitespace '\n' = " newline "
    spokenWhitespace c = ' ':c:' ':[]

    handleWhitespace :: String -> [Element]
    handleWhitespace [] = []
    handleWhitespace string@(c:rest)
      | isWhitespace c =
        (rawText $ spokenWhitespace c) :
        (node (unqual "BREAK") (unqualAttr "LEVEL" "Medium")) :
        (handleWhitespace rest)
      | otherwise = handleNotWhitespace string

    handleNotWhitespace :: String -> [Element]
    handleNotWhitespace [] = []
    handleNotWhitespace string =
      let
        (nws, ws) = break isWhitespace string
      in
        (node (unqual "SAYAS") ((unqualAttr "MODE" "literal"), nws)) :
        (handleWhitespace ws)



-- | Join a list of string with intercalated breaks.
insertBreaks
  :: String
  -- ^ Break level.
  -> (a -> ([Element] -> [Element]))
  {- ^
    A function to convert items of the given list to functions that prepend
    elements onto another list.
  -}
  -> [a]
  -> [Element]
insertBreaks _ _ [] = []
insertBreaks _ f (x:[]) = f x []
insertBreaks level f (x:xs) =
  (f x) $
  (node (unqual "BREAK") (unqualAttr "LEVEL" level)) :
  (insertBreaks level f xs)



------------------------------------ Pandoc ------------------------------------

-- | Pandoc writer for Sable.
writeSable :: WriterOptions -> Pandoc -> String
writeSable _ (Pandoc meta blocks) =
  pp $
  (formatMeta meta) ++
  (formatBlocks blocks)



-- | Extract document title, authors and date from Meta.
formatMeta :: Meta -> [Element]
formatMeta meta =
  let
    title = docTitle meta
    authors = docAuthors meta
    date = docDate meta
  in
    (
      if null title
      then id
      else ((speakMeta [rawText "document title"] $ formatInlines title) ++)
    ) $
    (
      case authors of
        [] -> id
        (a:[]) -> ((speakMeta [rawText "document author"] $ formatInlines a) ++)
        _ ->
          ((
            speakMeta [rawText "document authors"] $
            insertBreaks "Medium" (++) $
            map formatInlines authors
          ) ++)
    ) $
    (
      if null date
      then []
      else
        speakMeta
          [rawText "document date"]
          [
            node
              (unqual "SAYAS")
              -- TODO: detect date format and set MODETYPE accordingly.
              ([unqualAttr "MODE" "date", unqualAttr "MODETYPE" "YMD"], formatInlines date)
          ]
    )



-- | Format a list of blocks.
formatBlocks :: [Block] -> [Element]
formatBlocks = (insertBreaks "Large" (++)) . (map formatBlock)



-- | Format a block.
formatBlock :: Block -> [Element]
formatBlock (Plain ss) = formatInlines ss
formatBlock (Para ss) = [node (unqual "DIV") ([unqualAttr "TYPE" "paragraph"], formatInlines ss)]
formatBlock (CodeBlock _ string) = speakMeta [rawText "code"] [speakAllLiteral string]
formatBlock (RawBlock (Format t) string) = speakMeta [rawText ("raw " ++ t)] [speakAllLiteral string]
formatBlock (BlockQuote blocks) = speakMeta [rawText "quote"] (formatBlocks blocks)
formatBlock (OrderedList attr bss) = formatList (Just attr) bss
formatBlock (BulletList bss) = formatList Nothing bss
formatBlock (DefinitionList defs) = formatDefinitionList defs
formatBlock (Header n _ ss) =
  speakMeta
    [
      (rawText "header level "),
      node (unqual "SAYAS") (unqualAttr "MODE" "cardinal", show n)
    ]
    (formatInlines ss)
formatBlock (HorizontalRule) = speakMeta [rawText "horizontal rule"] []
formatBlock (Table caption _ _ headerRow rows) = formatTable caption headerRow rows
formatBlock (Div _ bs) = formatBlocks bs
formatBlock (Null) = []



-- | Format lists of inline elements.
formatInlines :: [Inline] -> [Element]
formatInlines = concat' . concat . (map formatInline)



-- | Format inline elements.
formatInline :: Inline -> [Element]
formatInline (Str s) = [rawText s]
formatInline (Emph ss) = [node (unqual "EMPH") ([unqualAttr "LEVEL" "Moderate"], formatInlines ss)]
formatInline (Strong ss) = [node (unqual "EMPH") ([unqualAttr "LEVEL" "Strong"], formatInlines ss)]
formatInline (Strikeout ss) = speakMeta [rawText "strikeout"] (formatInlines ss)
formatInline (Superscript ss) = speakMeta [rawText "superscript"] (formatInlines ss)
formatInline (Subscript ss) = speakMeta [rawText "subscript"] (formatInlines ss)
formatInline (SmallCaps ss) = speakMeta [rawText "small caps"] (formatInlines ss)
formatInline (Quoted SingleQuote ss) = speakMeta [rawText "single quote"] (formatInlines ss)
formatInline (Quoted DoubleQuote ss) = speakMeta [rawText "double quote"] (formatInlines ss)
-- TODO: implement
formatInline (Cite _ _) = speakMeta [rawText "citation"] [rawText "citations are not yet supported"]
formatInline (Code _ s) = speakMeta [rawText "code"] [speakAllLiteral s]
formatInline (Space) = [rawText " "]
formatInline (LineBreak) = speakMeta [rawText "line break"] []
formatInline (Math _ s) = [node (unqual "SAYAS") ((unqualAttr "MODE" "math"), s)]
formatInline (RawInline (Format t) s) = speakMeta [rawText ("raw " ++ t)] [speakAllLiteral s]
formatInline (Link ss (u, _)) =
  speakMeta [rawText "link"] $
  (formatInlines ss) ++
  [
    node (unqual "BREAK") (unqualAttr "LEVEL" "Medium"),
    url u
  ]
formatInline (Image alt (u, _)) =
  speakMeta [rawText "image"] $
  (formatInlines alt) ++
  [
    node (unqual "BREAK") (unqualAttr "LEVEL" "Medium"),
    url u
  ]
formatInline (Note bs) = speakMeta [rawText "note"] (formatBlocks bs)
formatInline (Span _ ss) = formatInlines ss



-- | Format a list.
formatList :: (Maybe ListAttributes) -> [[Block]] -> [Element]
formatList attr items =
  let
    f :: (Show s) => String -> s => Maybe Element
    f s x = Just $ node (unqual "SAYAS") ((unqualAttr "MODE" s), show x)

    enumeration :: [Maybe Element]
    enumeration = case attr of
      Just (_, LowerAlpha, _) -> map (f "literal") ['a'..]
      Just (_, UpperAlpha, _) -> map (f "literal") ['A'..]
      Just _ -> map (f "cardinal") [(1 :: Int) ..]
      Nothing -> map (f "ordinal") [(1 :: Int) ..]
--       Nothing -> repeat Nothing
  in
    speakMeta [rawText "list"] $
    insertBreaks "Medium" (++) (map formatListItem (zip enumeration items))



-- | Format a list item.
formatListItem :: (Maybe Element, [Block]) -> [Element]
formatListItem (Nothing, i) = formatBlocks i
formatListItem (Just e, i) =
  e :
  (node (unqual "BREAK") (unqualAttr "LEVEL" "Small")) :
  (rawText "\n") :
  (formatBlocks i)



-- | Format a definition list.
formatDefinitionList :: [([Inline], [[Block]])] -> [Element]
formatDefinitionList defs =
  speakMeta [rawText "definition list"] $
  insertBreaks "Medium" (++) $
  map formatDefinition defs



-- | Format a definition.
formatDefinition :: ([Inline], [[Block]]) -> [Element]
formatDefinition (header, body) =
  (node (unqual "EMPH") ([unqualAttr "LEVEL" "Moderate"], formatInlines header)) :
  (node (unqual "BREAK") [unqualAttr "LEVEL" "Small", unqualAttr "TYPE" "!"]) :
  (insertBreaks "Medium" (++) $ map formatBlocks body)



-- | Fromat a table.
formatTable :: [Inline] -> [TableCell] -> [[TableCell]] -> [Element]
formatTable caption headerRow rows =
  speakMeta [rawText "table"] $
  (speakMeta [rawText "caption"] (formatInlines caption)) ++
  (formatHeaderRow headerRow) ++
  (formatBodyRows rows)

formatHeaderRow :: [TableCell] -> [Element]
formatHeaderRow hcs =
  speakMeta [rawText "headers"] (concat' $ concat $ map formatHeaderCell hcs)

formatHeaderCell :: TableCell -> [Element]
formatHeaderCell hc = speakMeta [rawText "header"] (formatBlocks hc)

formatBodyRows :: [[TableCell]] -> [Element]
formatBodyRows = (insertBreaks "Small" (++)) . (map formatBodyRow)

formatBodyRow :: [TableCell] -> [Element]
formatBodyRow hcs = speakMeta [rawText "row"] (concat' $ concat $ map formatBodyCell hcs)

formatBodyCell :: TableCell -> [Element]
formatBodyCell bc = speakMeta [rawText "cell"] (formatBlocks bc)

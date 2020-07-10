{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org.Blocks
   Copyright   : Copyright (C) 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode block elements.
-}
module Text.Pandoc.Readers.Org.Blocks
  ( blockList
  , meta
  ) where

import Text.Pandoc.Readers.Org.BlockStarts
import Text.Pandoc.Readers.Org.DocumentTree (documentTree,
                                             unprunedHeadlineToBlocks)
import Text.Pandoc.Readers.Org.Inlines
import Text.Pandoc.Readers.Org.Meta (metaExport, metaKey, metaLine)
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing
import Text.Pandoc.Readers.Org.Shared (cleanLinkText, isImageFilename,
                                       originalLang, translateLang, exportsCode)

import Text.Pandoc.Builder (Blocks, Inlines)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (compactify, compactifyDL, safeRead)

import Control.Monad (foldM, guard, mplus, mzero, void)
import Data.Char (isSpace)
import Data.Default (Default)
import Data.List (foldl', intersperse)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Walk as Walk

--
-- parsing blocks
--

-- | Get a list of blocks.
blockList :: PandocMonad m => OrgParser m [Block]
blockList = do
  fHeadlineTree  <- documentTree blocks inline
  st             <- getState
  let headlineTree = runF fHeadlineTree st
  unprunedHeadlineToBlocks headlineTree st

-- | Get the meta information saved in the state.
meta :: Monad m => OrgParser m Meta
meta = do
  meta' <- metaExport
  runF meta' <$> getState

blocks :: PandocMonad m => OrgParser m (F Blocks)
blocks = mconcat <$> manyTill block (void (lookAhead headerStart) <|> eof)

block :: PandocMonad m => OrgParser m (F Blocks)
block = choice [ mempty <$ blanklines
               , table
               , orgBlock
               , figure
               , example
               , genericDrawer
               , include
               , specialLine
               , horizontalRule
               , list
               , latexFragment
               , noteBlock
               , rawOrgLine
               , paraOrPlain
               ] <?> "block"


-- | Parse a horizontal rule into a block element
horizontalRule :: Monad m => OrgParser m (F Blocks)
horizontalRule = return B.horizontalRule <$ try hline


--
-- Block Attributes
--

-- | Attributes that may be added to figures (like a name or caption).
data BlockAttributes = BlockAttributes
  { blockAttrName      :: Maybe Text
  , blockAttrLabel     :: Maybe Text
  , blockAttrCaption   :: Maybe (F Inlines)
  , blockAttrKeyValues :: [(Text, Text)]
  }

-- | Convert BlockAttributes into pandoc Attr
attrFromBlockAttributes :: BlockAttributes -> Attr
attrFromBlockAttributes BlockAttributes{..} =
  let
    ident   = fromMaybe mempty $ lookup "id" blockAttrKeyValues
    classes = case lookup "class" blockAttrKeyValues of
                Nothing     -> []
                Just clsStr -> T.words clsStr
    kv      = filter ((`notElem` ["id", "class"]) . fst) blockAttrKeyValues
  in (ident, classes, kv)

stringyMetaAttribute :: Monad m => OrgParser m (Text, Text)
stringyMetaAttribute = try $ do
  metaLineStart
  attrName <- T.toUpper <$> many1TillChar nonspaceChar (char ':')
  skipSpaces
  attrValue <- anyLine <|> ("" <$ newline)
  return (attrName, attrValue)

-- | Parse a set of block attributes. Block attributes are given through
-- lines like @#+CAPTION: block caption@ or @#+ATTR_HTML: :width 20@.
-- Parsing will fail if any line contains an attribute different from
-- those attributes known to work on blocks.
blockAttributes :: PandocMonad m => OrgParser m BlockAttributes
blockAttributes = try $ do
  kv <- many stringyMetaAttribute
  guard $ all (isBlockAttr . fst) kv
  let caption = foldl' (appendValues "CAPTION") Nothing kv
  let kvAttrs = foldl' (appendValues "ATTR_HTML") Nothing kv
  let name    = lookup "NAME" kv
  let label   = lookup "LABEL" kv
  caption' <- traverse (parseFromString inlines . (<> "\n")) caption
  kvAttrs' <- parseFromString keyValues . (<> "\n") $ fromMaybe mempty kvAttrs
  return BlockAttributes
           { blockAttrName = name
           , blockAttrLabel = label
           , blockAttrCaption = caption'
           , blockAttrKeyValues = kvAttrs'
           }
 where
   isBlockAttr :: Text -> Bool
   isBlockAttr = flip elem
                 [ "NAME", "LABEL", "CAPTION"
                 , "ATTR_HTML", "ATTR_LATEX"
                 , "RESULTS"
                 ]

   appendValues :: Text -> Maybe Text -> (Text, Text) -> Maybe Text
   appendValues attrName accValue (key, value) =
     if key /= attrName
     then accValue
     else case accValue of
            Just acc -> Just $ acc <> " " <> value
            Nothing  -> Just value

-- | Parse key-value pairs for HTML attributes
keyValues :: Monad m => OrgParser m [(Text, Text)]
keyValues = try $
  manyTill ((,) <$> key <*> value) newline
 where
   key :: Monad m => OrgParser m Text
   key = try $ skipSpaces *> char ':' *> many1Char nonspaceChar

   value :: Monad m => OrgParser m Text
   value = skipSpaces *> manyTillChar anyChar endOfValue

   endOfValue :: Monad m => OrgParser m ()
   endOfValue =
     lookAhead $ (() <$ try (many1 spaceChar <* key))
              <|> () <$ newline


--
-- Org Blocks (#+BEGIN_... / #+END_...)
--

-- | Read an org-mode block delimited by #+BEGIN_TYPE and #+END_TYPE.
orgBlock :: PandocMonad m => OrgParser m (F Blocks)
orgBlock = try $ do
  blockAttrs <- blockAttributes
  blkType <- blockHeaderStart
  ($ blkType) $
    case T.toLower blkType of
      "export"  -> exportBlock
      "comment" -> rawBlockLines (const mempty)
      "html"    -> rawBlockLines (return . B.rawBlock (lowercase blkType))
      "latex"   -> rawBlockLines (return . B.rawBlock (lowercase blkType))
      "ascii"   -> rawBlockLines (return . B.rawBlock (lowercase blkType))
      "example" -> exampleBlock blockAttrs
      "quote"   -> parseBlockLines (fmap B.blockQuote)
      "verse"   -> verseBlock
      "src"     -> codeBlock blockAttrs
      _         -> parseBlockLines $
                   let (ident, classes, kv) = attrFromBlockAttributes blockAttrs
                   in fmap $ B.divWith (ident, classes ++ [blkType], kv)
 where
   blockHeaderStart :: Monad m => OrgParser m Text
   blockHeaderStart = try $ skipSpaces *> stringAnyCase "#+begin_" *> orgArgWord

   lowercase :: Text -> Text
   lowercase = T.toLower

exampleBlock :: PandocMonad m => BlockAttributes -> Text -> OrgParser m (F Blocks)
exampleBlock blockAttrs _label = do
  skipSpaces
  (classes, kv) <- switchesAsAttributes
  newline
  content <- rawBlockContent "example"
  let id' = fromMaybe mempty $ blockAttrName blockAttrs
  let codeBlck = B.codeBlockWith (id', "example":classes, kv) content
  return . return $ codeBlck

rawBlockLines :: Monad m => (Text   -> F Blocks) -> Text -> OrgParser m (F Blocks)
rawBlockLines f blockType = ignHeaders *> (f <$> rawBlockContent blockType)

parseBlockLines :: PandocMonad m => (F Blocks -> F Blocks) -> Text -> OrgParser m (F Blocks)
parseBlockLines f blockType = ignHeaders *> (f <$> parsedBlockContent)
 where
   parsedBlockContent :: PandocMonad m => OrgParser m (F Blocks)
   parsedBlockContent = try $ do
     raw <- rawBlockContent blockType
     parseFromString blocks (raw <> "\n")

-- | Read the raw string content of a block
rawBlockContent :: Monad m => Text -> OrgParser m Text
rawBlockContent blockType = try $ do
  blkLines <- manyTill rawLine blockEnder
  tabLen <- getOption readerTabStop
  trimP <- orgStateTrimLeadBlkIndent <$> getState
  let stripIndent strs = if trimP then map (T.drop (shortestIndent strs)) strs else strs
  (T.unlines
   . stripIndent
   . map (tabsToSpaces tabLen . commaEscaped)
   $ blkLines)
   <$ updateState (\s -> s { orgStateTrimLeadBlkIndent = True })
 where
   rawLine :: Monad m => OrgParser m Text
   rawLine = try $ ("" <$ blankline) <|> anyLine

   blockEnder :: Monad m => OrgParser m ()
   blockEnder = try $ skipSpaces <* stringAnyCase ("#+end_" <> blockType)

   shortestIndent :: [Text] -> Int
   shortestIndent = foldr (min . T.length . T.takeWhile isSpace) maxBound
                    . filter (not . T.null)

   tabsToSpaces :: Int -> Text -> Text
   tabsToSpaces tabStop t =
     let (ind, suff) = T.span (\c -> c == ' ' || c == '\t') t
         tabNum      = T.length $ T.filter (== '\n') ind
         spaceNum    = T.length ind - tabNum
     in  T.replicate (spaceNum + tabStop * tabNum) " " <> suff

   commaEscaped t =
     let (ind, suff) = T.span (\c -> c == ' ' || c == '\t') t
     in  case T.uncons suff of
           Just (',', cs)
             | "*"  <- T.take 1 cs -> ind <> cs
             | "#+" <- T.take 2 cs -> ind <> cs
           _                       -> t

-- | Read but ignore all remaining block headers.
ignHeaders :: Monad m => OrgParser m ()
ignHeaders = (() <$ newline) <|> (() <$ anyLine)

-- | Read a block containing code intended for export in specific backends
-- only.
exportBlock :: Monad m => Text -> OrgParser m (F Blocks)
exportBlock blockType = try $ do
  exportType <- skipSpaces *> orgArgWord <* ignHeaders
  contents   <- rawBlockContent blockType
  returnF (B.rawBlock (T.toLower exportType) contents)

verseBlock :: PandocMonad m => Text -> OrgParser m (F Blocks)
verseBlock blockType = try $ do
  ignHeaders
  content <- rawBlockContent blockType
  fmap B.lineBlock . sequence
    <$> mapM parseVerseLine (T.lines content)
 where
   -- replace initial spaces with nonbreaking spaces to preserve
   -- indentation, parse the rest as normal inline
   parseVerseLine :: PandocMonad m => Text -> OrgParser m (F Inlines)
   parseVerseLine cs = do
     let (initialSpaces, indentedLine) = T.span isSpace cs
     let nbspIndent = if T.null initialSpaces
                      then mempty
                      else B.str $ T.map (const '\160') initialSpaces
     line <- parseFromString inlines (indentedLine <> "\n")
     return (trimInlinesF $ pure nbspIndent <> line)

-- | Read a code block and the associated results block if present.  Which of
-- boths blocks is included in the output is determined using the "exports"
-- argument in the block header.
codeBlock :: PandocMonad m => BlockAttributes -> Text -> OrgParser m (F Blocks)
codeBlock blockAttrs blockType = do
  skipSpaces
  (classes, kv)     <- codeHeaderArgs <|> (mempty <$ ignHeaders)
  content           <- rawBlockContent blockType
  resultsContent    <- option mempty babelResultsBlock
  let id'            = fromMaybe mempty $ blockAttrName blockAttrs
  let codeBlck       = B.codeBlockWith ( id', classes, kv ) content
  let labelledBlck   = maybe (pure codeBlck)
                             (labelDiv codeBlck)
                             (blockAttrCaption blockAttrs)
  return $
    (if exportsCode kv    then labelledBlck   else mempty) <>
    (if exportsResults kv then resultsContent else mempty)
 where
   labelDiv :: Blocks -> F Inlines -> F Blocks
   labelDiv blk value =
     B.divWith nullAttr <$> (mappend <$> labelledBlock value <*> pure blk)

   labelledBlock :: F Inlines -> F Blocks
   labelledBlock = fmap (B.plain . B.spanWith ("", ["label"], []))

   exportsResults :: [(Text, Text)] -> Bool
   exportsResults = maybe False (`elem` ["results", "both"]) . lookup "exports"

-- | Parse the result of an evaluated babel code block.
babelResultsBlock :: PandocMonad m => OrgParser m (F Blocks)
babelResultsBlock = try $ do
  blanklines
  resultsMarker <|>
    (lookAhead . void . try $
      manyTill (metaLineStart *> anyLineNewline) resultsMarker)
  block
 where
  resultsMarker = try . void $ stringAnyCase "#+RESULTS:" *> blankline

-- | Parse code block arguments
codeHeaderArgs :: Monad m => OrgParser m ([Text], [(Text, Text)])
codeHeaderArgs = try $ do
  language   <- skipSpaces *> orgArgWord
  (switchClasses, switchKv) <- switchesAsAttributes
  parameters <- manyTill blockOption newline
  return ( translateLang language : switchClasses
         , originalLang language <> switchKv <> parameters
         )

switchesAsAttributes :: Monad m => OrgParser m ([Text], [(Text, Text)])
switchesAsAttributes = try $ do
  switches <- skipSpaces *> try (switch `sepBy` many1 spaceChar)
  return $ foldr addToAttr ([], []) switches
 where
  addToAttr :: (Char, Maybe Text, SwitchPolarity)
            -> ([Text], [(Text, Text)])
            -> ([Text], [(Text, Text)])
  addToAttr ('n', lineNum, pol) (cls, kv) =
    let kv' = case lineNum of
                Just num -> ("startFrom", num):kv
                Nothing  -> kv
        cls' = case pol of
                 SwitchPlus  -> "continuedSourceBlock":cls
                 SwitchMinus -> cls
    in ("numberLines":cls', kv')
  addToAttr _ x = x

-- | Whether a switch flag is specified with @+@ or @-@.
data SwitchPolarity = SwitchPlus | SwitchMinus
  deriving (Show, Eq)

-- | Parses a switch's polarity.
switchPolarity :: Monad m => OrgParser m SwitchPolarity
switchPolarity = (SwitchMinus <$ char '-') <|> (SwitchPlus <$ char '+')

-- | Parses a source block switch option.
switch :: Monad m => OrgParser m (Char, Maybe Text, SwitchPolarity)
switch = try $ lineNumberSwitch <|> labelSwitch
               <|> whitespaceSwitch <|> simpleSwitch
 where
   simpleSwitch = (\pol c -> (c, Nothing, pol)) <$> switchPolarity <*> letter
   labelSwitch = genericSwitch 'l' $
     char '"' *> many1TillChar nonspaceChar (char '"')

whitespaceSwitch :: Monad m => OrgParser m (Char, Maybe Text, SwitchPolarity)
whitespaceSwitch = do
  string "-i"
  updateState $ \s -> s { orgStateTrimLeadBlkIndent = False }
  return ('i', Nothing, SwitchMinus)

-- | Generic source block switch-option parser.
genericSwitch :: Monad m
              => Char
              -> OrgParser m Text
              -> OrgParser m (Char, Maybe Text, SwitchPolarity)
genericSwitch c p = try $ do
  polarity <- switchPolarity <* char c <* skipSpaces
  arg <- optionMaybe p
  return (c, arg, polarity)

-- | Reads a line number switch option. The line number switch can be used with
-- example and source blocks.
lineNumberSwitch :: Monad m => OrgParser m (Char, Maybe Text, SwitchPolarity)
lineNumberSwitch = genericSwitch 'n' (manyChar digit)

blockOption :: Monad m => OrgParser m (Text, Text)
blockOption = try $ do
  argKey <- orgArgKey
  paramValue <- option "yes" orgParamValue
  return (argKey, paramValue)

orgParamValue :: Monad m => OrgParser m Text
orgParamValue = try $ fmap T.pack $
  skipSpaces
    *> notFollowedBy orgArgKey
    *> noneOf "\n\r" `many1Till` endOfValue
    <* skipSpaces
 where
  endOfValue = lookAhead $  try (skipSpaces <* oneOf "\n\r")
                        <|> try (skipSpaces1 <* orgArgKey)


--
-- Drawers
--

-- | A generic drawer which has no special meaning for org-mode.
-- Whether or not this drawer is included in the output depends on the drawers
-- export setting.
genericDrawer :: PandocMonad m => OrgParser m (F Blocks)
genericDrawer = try $ do
  name    <- T.toUpper <$> drawerStart
  content <- manyTill drawerLine (try drawerEnd)
  state   <- getState
  -- Include drawer if it is explicitly included in or not explicitly excluded
  -- from the list of drawers that should be exported.  PROPERTIES drawers are
  -- never exported.
  case exportDrawers . orgStateExportSettings $ state of
    _           | name == "PROPERTIES" -> return mempty
    Left  names | name `elem`    names -> return mempty
    Right names | name `notElem` names -> return mempty
    _           -> drawerDiv name <$> parseLines content
 where
  parseLines :: PandocMonad m => [Text] -> OrgParser m (F Blocks)
  parseLines = parseFromString blocks . (<> "\n") . T.unlines

  drawerDiv :: Text -> F Blocks -> F Blocks
  drawerDiv drawerName = fmap $ B.divWith (mempty, [drawerName, "drawer"], mempty)

drawerLine :: Monad m => OrgParser m Text
drawerLine = anyLine

drawerEnd :: Monad m => OrgParser m Text
drawerEnd = try $
  skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* newline


--
-- Figures
--

-- | Figures or an image paragraph (i.e. an image on a line by itself). Only
-- images with a caption attribute are interpreted as figures.
figure :: PandocMonad m => OrgParser m (F Blocks)
figure = try $ do
  figAttrs <- blockAttributes
  src <- skipSpaces *> selfTarget <* skipSpaces <* endOfParagraph
  case cleanLinkText src of
    Nothing     -> mzero
    Just imgSrc -> do
      guard (isImageFilename imgSrc)
      let isFigure = isJust $ blockAttrCaption figAttrs
      return $ imageBlock isFigure figAttrs imgSrc
 where
   selfTarget :: PandocMonad m => OrgParser m Text
   selfTarget = try $ char '[' *> linkTarget <* char ']'

   imageBlock :: Bool -> BlockAttributes -> Text -> F Blocks
   imageBlock isFigure figAttrs imgSrc =
     let
       figName    = fromMaybe mempty $ blockAttrName figAttrs
       figLabel   = fromMaybe mempty $ blockAttrLabel figAttrs
       figCaption = fromMaybe mempty $ blockAttrCaption figAttrs
       figKeyVals = blockAttrKeyValues figAttrs
       attr       = (figLabel, mempty, figKeyVals)
       figTitle   = (if isFigure then withFigPrefix else id) figName
     in
       B.para . B.imageWith attr imgSrc figTitle <$> figCaption

   withFigPrefix :: Text -> Text
   withFigPrefix cs =
     if "fig:" `T.isPrefixOf` cs
     then cs
     else "fig:" <> cs

-- | Succeeds if looking at the end of the current paragraph
endOfParagraph :: Monad m => OrgParser m ()
endOfParagraph = try $ skipSpaces *> newline *> endOfBlock


--
-- Examples
--

-- | Example code marked up by a leading colon.
example :: Monad m => OrgParser m (F Blocks)
example = try $ returnF . exampleCode =<< T.unlines <$> many1 exampleLine
 where
   exampleLine :: Monad m => OrgParser m Text
   exampleLine = try $ exampleLineStart *> anyLine

exampleCode :: Text -> Blocks
exampleCode = B.codeBlockWith ("", ["example"], [])


--
-- Comments, Options and Metadata
--

specialLine :: PandocMonad m => OrgParser m (F Blocks)
specialLine = fmap return . try $ rawExportLine <|> metaLine <|> commentLine

-- | Include the content of a file.
include :: PandocMonad m => OrgParser m (F Blocks)
include = try $ do
  metaLineStart <* stringAnyCase "include:" <* skipSpaces
  filename <- includeTarget
  includeArgs <- many (try $ skipSpaces *> many1Char alphaNum)
  params <- keyValues
  blocksParser <- case includeArgs of
      ("example" : _) -> return $ pure . B.codeBlock <$> parseRaw
      ["export"] -> return . returnF $ B.fromList []
      ["export", format] -> return $ pure . B.rawBlock format <$> parseRaw
      ("src" : rest) -> do
        let attr = case rest of
                     [lang] -> (mempty, [lang], mempty)
                     _ -> nullAttr
        return $ pure . B.codeBlockWith attr <$> parseRaw
      _ -> return $ return . B.fromList . blockFilter params <$> blockList
  insertIncludedFileF blocksParser ["."] filename
 where
  includeTarget :: PandocMonad m => OrgParser m FilePath
  includeTarget = do
    char '"'
    manyTill (noneOf "\n\r\t") (char '"')

  parseRaw :: PandocMonad m => OrgParser m Text
  parseRaw = manyChar anyChar

  blockFilter :: [(Text, Text)] -> [Block] -> [Block]
  blockFilter params blks =
    let minlvl = lookup "minlevel" params
    in case (minlvl >>= safeRead :: Maybe Int) of
         Nothing -> blks
         Just lvl -> let levels = Walk.query headerLevel blks
                         -- CAVE: partial function in else
                         curMin = if null levels then 0 else minimum levels
                     in Walk.walk (shiftHeader (curMin - lvl)) blks

  headerLevel :: Block -> [Int]
  headerLevel (Header lvl _attr _content) = [lvl]
  headerLevel _ = []

  shiftHeader :: Int -> Block -> Block
  shiftHeader shift blk =
    case blk of
      (Header lvl attr content)
       | lvl - shift > 0  -> Header (lvl - shift) attr content
       | otherwise        -> Para content
      _ -> blk

-- | Parses a meta line which defines a raw block. Currently recognized:
-- @#+LATEX:@, @#+HTML:@, @#+TEXINFO:@, and @#+BEAMER@.
rawExportLine :: PandocMonad m => OrgParser m Blocks
rawExportLine = try $ do
  metaLineStart
  key <- metaKey
  if key `elem` ["latex", "html", "texinfo", "beamer"]
    then B.rawBlock key <$> anyLine
    else mzero

-- | Parses any meta line, i.e., a line starting with @#+@, into a raw
-- org block. This should be the last resort when trying to parse
-- keywords. Leading spaces are discarded.
rawOrgLine :: PandocMonad m => OrgParser m (F Blocks)
rawOrgLine = do
  line <- metaLineStart *> anyLine
  returnF $ B.rawBlock "org" $ ("#+" <> line)

commentLine :: Monad m => OrgParser m Blocks
commentLine = commentLineStart *> anyLine *> pure mempty


--
-- Tables
--
data ColumnProperty = ColumnProperty
  { columnAlignment :: Maybe Alignment
  , columnRelWidth  :: Maybe Int
  } deriving (Show, Eq)

instance Default ColumnProperty where
  def = ColumnProperty Nothing Nothing

data OrgTableRow = OrgContentRow (F [Blocks])
                 | OrgAlignRow [ColumnProperty]
                 | OrgHlineRow

-- OrgTable is strongly related to the pandoc table ADT.  Using the same
-- (i.e. pandoc-global) ADT would mean that the reader would break if the
-- global structure was to be changed, which would be bad.  The final table
-- should be generated using a builder function.
data OrgTable = OrgTable
  { orgTableColumnProperties :: [ColumnProperty]
  , orgTableHeader           :: [Blocks]
  , orgTableRows             :: [[Blocks]]
  }

table :: PandocMonad m => OrgParser m (F Blocks)
table = do
  withTables <- getExportSetting exportWithTables
  tbl <- gridTableWith blocks True <|> orgTable
  return $ if withTables then tbl else mempty

-- | A normal org table
orgTable :: PandocMonad m => OrgParser m (F Blocks)
orgTable = try $ do
  -- don't allow a table on the first line of a list item; org requires that
  -- tables start at first non-space character on the line
  let isFirstInListItem st = orgStateParserContext st == ListItemState &&
                             isNothing (orgStateLastPreCharPos st)
  guard =<< not . isFirstInListItem <$> getState
  blockAttrs <- blockAttributes
  lookAhead tableStart
  rows <- tableRows

  let caption = fromMaybe mempty (blockAttrCaption blockAttrs)
  let orgTbl = normalizeTable <$> rowsToTable rows
  -- wrap table in div if a name or label is given
  let identMb = blockAttrName blockAttrs `mplus` blockAttrLabel blockAttrs
  let wrap = case identMb of
        Just ident -> B.divWith (ident, mempty, mempty)
        Nothing    -> id
  return . fmap wrap $ (orgToPandocTable <$> orgTbl <*> caption)

orgToPandocTable :: OrgTable
                 -> Inlines
                 -> Blocks
orgToPandocTable (OrgTable colProps heads lns) caption =
  let totalWidth = if any (isJust . columnRelWidth) colProps
                   then Just . sum $ map (fromMaybe 1 . columnRelWidth) colProps
                   else Nothing
  in B.table (B.simpleCaption $ B.plain caption)
             (map (convertColProp totalWidth) colProps)
             (TableHead nullAttr $ toHeaderRow heads)
             [TableBody nullAttr 0 [] $ map toRow lns]
             (TableFoot nullAttr [])
 where
   toRow = Row nullAttr . map B.simpleCell
   toHeaderRow l = if null l then [] else [toRow l]
   convertColProp :: Maybe Int -> ColumnProperty -> (Alignment, ColWidth)
   convertColProp totalWidth colProp =
     let
       align' = fromMaybe AlignDefault $ columnAlignment colProp
       width' = (\w t -> (fromIntegral w / fromIntegral t))
                <$> columnRelWidth colProp
                <*> totalWidth
     in (align', maybe ColWidthDefault ColWidth width')

tableRows :: PandocMonad m => OrgParser m [OrgTableRow]
tableRows = try $ many (tableAlignRow <|> tableHline <|> tableContentRow)

tableContentRow :: PandocMonad m => OrgParser m OrgTableRow
tableContentRow = try $
  OrgContentRow . sequence <$> (tableStart *> many1Till tableContentCell newline)

tableContentCell :: PandocMonad m => OrgParser m (F Blocks)
tableContentCell = try $
  fmap B.plain . trimInlinesF . mconcat <$> manyTill inline endOfCell

tableAlignRow :: Monad m => OrgParser m OrgTableRow
tableAlignRow = try $ do
  tableStart
  colProps <- many1Till columnPropertyCell newline
  -- Empty rows are regular (i.e. content) rows, not alignment rows.
  guard $ any (/= def) colProps
  return $ OrgAlignRow colProps

columnPropertyCell :: Monad m => OrgParser m ColumnProperty
columnPropertyCell = emptyOrgCell <|> propCell <?> "alignment info"
 where
   emptyOrgCell = ColumnProperty Nothing Nothing <$ try (skipSpaces *> endOfCell)
   propCell = try $ ColumnProperty
                 <$> (skipSpaces
                      *> char '<'
                      *> optionMaybe tableAlignFromChar)
                 <*> (optionMaybe (many1Char digit >>= safeRead)
                      <* char '>'
                      <* emptyOrgCell)

tableAlignFromChar :: Monad m => OrgParser m Alignment
tableAlignFromChar = try $
  choice [ char 'l' *> return AlignLeft
         , char 'c' *> return AlignCenter
         , char 'r' *> return AlignRight
         ]

tableHline :: Monad m => OrgParser m OrgTableRow
tableHline = try $
  OrgHlineRow <$ (tableStart *> char '-' *> anyLine)

endOfCell :: Monad m => OrgParser m Char
endOfCell = try $ char '|' <|> lookAhead newline

rowsToTable :: [OrgTableRow]
            -> F OrgTable
rowsToTable = foldM rowToContent emptyTable
 where emptyTable = OrgTable mempty mempty mempty

normalizeTable :: OrgTable -> OrgTable
normalizeTable (OrgTable colProps heads rows) =
  OrgTable colProps' heads rows
 where
   refRow = if heads /= mempty
            then heads
            else case rows of
                   (r:_) -> r
                   _     -> mempty
   cols = length refRow
   fillColumns base padding = take cols $ base ++ repeat padding
   colProps' = fillColumns colProps def

-- One or more horizontal rules after the first content line mark the previous
-- line as a header.  All other horizontal lines are discarded.
rowToContent :: OrgTable
             -> OrgTableRow
             -> F OrgTable
rowToContent tbl row =
  case row of
    OrgHlineRow       -> return singleRowPromotedToHeader
    OrgAlignRow props -> return . setProperties $ props
    OrgContentRow cs  -> appendToBody cs
 where
   singleRowPromotedToHeader :: OrgTable
   singleRowPromotedToHeader = case tbl of
     OrgTable{ orgTableHeader = [], orgTableRows = [b] } ->
            tbl{ orgTableHeader = b , orgTableRows = [] }
     _   -> tbl

   setProperties :: [ColumnProperty] -> OrgTable
   setProperties ps = tbl{ orgTableColumnProperties = ps }

   appendToBody :: F [Blocks] -> F OrgTable
   appendToBody frow = do
     newRow <- frow
     let oldRows = orgTableRows tbl
     -- NOTE: This is an inefficient O(n) operation.  This should be changed
     -- if performance ever becomes a problem.
     return tbl{ orgTableRows = oldRows ++ [newRow] }


--
-- LaTeX fragments
--
latexFragment :: PandocMonad m => OrgParser m (F Blocks)
latexFragment = try $ do
  envName <- latexEnvStart
  texOpt  <- getExportSetting exportWithLatex
  let envStart = "\\begin{" <> envName <> "}"
  let envEnd = "\\end{" <> envName <> "}"
  envLines <- do
    content <- manyTill anyLine (latexEnd envName)
    return $ envStart : content ++ [envEnd]
  returnF $ case texOpt of
    TeXExport -> B.rawBlock "latex" . T.unlines $ envLines
    TeXIgnore   -> mempty
    TeXVerbatim -> B.para . mconcat . intersperse B.softbreak $
                   map B.str envLines
 where
  latexEnd :: Monad m => Text -> OrgParser m ()
  latexEnd envName = try . void
     $ skipSpaces
    <* textStr ("\\end{" <> envName <> "}")
    <* blankline


--
-- Footnote definitions
--
noteBlock :: PandocMonad m => OrgParser m (F Blocks)
noteBlock = try $ do
  ref <- noteMarker <* skipSpaces <* updateLastPreCharPos
  content <- mconcat <$> many1Till block endOfFootnote
  addToNotesTable (ref, content)
  return mempty
 where
   endOfFootnote =  eof
                <|> () <$ lookAhead noteMarker
                <|> () <$ lookAhead headerStart
                <|> () <$ lookAhead (try $ blankline *> blankline)

-- Paragraphs or Plain text
paraOrPlain :: PandocMonad m => OrgParser m (F Blocks)
paraOrPlain = try $ do
  -- Make sure we are not looking at a headline
  notFollowedBy' headerStart
  ils <- inlines
  nl <- option False (newline *> return True)
  -- Read block as paragraph, except if we are in a list context and the block
  -- is directly followed by a list item, in which case the block is read as
  -- plain text.
  try (guard nl
       *> notFollowedBy (inList *> (orderedListStart <|> bulletListStart))
       *> return (B.para <$> ils))
    <|>  return (B.plain <$> ils)


--
-- list blocks
--

list :: PandocMonad m => OrgParser m (F Blocks)
list = choice [ definitionList, bulletList, orderedList ] <?> "list"

definitionList :: PandocMonad m => OrgParser m (F Blocks)
definitionList = try $ do
  indent <- lookAhead bulletListStart
  fmap (B.definitionList . compactifyDL) . sequence
    <$> many1 (definitionListItem (bulletListStart `indented` indent))

bulletList :: PandocMonad m => OrgParser m (F Blocks)
bulletList = try $ do
  indent <- lookAhead bulletListStart
  fmap (B.bulletList . compactify) . sequence
    <$> many1 (listItem (bulletListStart `indented` indent))

indented :: OrgParser m Int -> Int -> OrgParser m Int
indented indentedMarker minIndent = try $ do
  n <- indentedMarker
  guard (minIndent <= n)
  return n

orderedList :: PandocMonad m => OrgParser m (F Blocks)
orderedList = try $ do
  indent <- lookAhead orderedListStart
  fmap (B.orderedList . compactify) . sequence
    <$> many1 (listItem (orderedListStart `indented` indent))

definitionListItem :: PandocMonad m
                   => OrgParser m Int
                   -> OrgParser m (F (Inlines, [Blocks]))
definitionListItem parseIndentedMarker = try $ do
  markerLength <- parseIndentedMarker
  term <- manyTillChar (noneOf "\n\r") (try definitionMarker)
  line1 <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  cont <- T.concat <$> many (listContinuation markerLength)
  term' <- parseFromString inlines term
  contents' <- parseFromString blocks $ line1 <> blank <> cont
  return $ (,) <$> term' <*> fmap (:[]) contents'
 where
   definitionMarker =
     spaceChar *> string "::" <* (spaceChar <|> lookAhead newline)

-- | parse raw text for one list item
listItem :: PandocMonad m
         => OrgParser m Int
         -> OrgParser m (F Blocks)
listItem parseIndentedMarker = try . withContext ListItemState $ do
  markerLength <- try parseIndentedMarker
  firstLine <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  rest <- T.concat <$> many (listContinuation markerLength)
  parseFromString blocks $ firstLine <> blank <> rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: PandocMonad m => Int -> OrgParser m Text
listContinuation markerLength = try $ do
  notFollowedBy' blankline
  mappend <$> (T.concat <$> many1 (listContinuation' markerLength))
          <*> manyChar blankline
 where
   listContinuation' indentation =
      blockLines indentation <|> listLine indentation
   listLine indentation = try $ indentWith indentation *> anyLineNewline
  -- The block attributes and start must be appropriately indented,
  -- but the contents, and end do not.
   blockLines indentation =
      try $ lookAhead (indentWith indentation
                       >> blockAttributes
                       >>= (\blockAttrs ->
                              case attrFromBlockAttributes blockAttrs of
                                ("", [], []) -> countChar 1 anyChar
                                _ -> indentWith indentation))
            >> (snd <$> withRaw orgBlock)

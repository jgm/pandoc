{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder ( Inlines, Blocks )
import           Text.Pandoc.Definition
import           Text.Pandoc.Compat.Monoid ((<>))
import           Text.Pandoc.Error
import           Text.Pandoc.Options
import qualified Text.Pandoc.Parsing as P
import           Text.Pandoc.Parsing hiding ( F, unF, askF, asksF, runF
                                            , newline, orderedListMarker
                                            , parseFromString, blanklines
                                            )
import           Text.Pandoc.Readers.LaTeX (inlineCommand, rawLaTeXInline)
import           Text.Pandoc.Readers.Org.ParserState
import           Text.Pandoc.Shared (compactify', compactify'DL)
import           Text.TeXMath (readTeX, writePandoc, DisplayType(..))
import qualified Text.TeXMath.Readers.MathML.EntityMap as MathMLEntityMap

import           Control.Arrow (first)
import           Control.Monad (foldM, guard, mplus, mzero, when)
import           Control.Monad.Reader ( Reader, runReader )
import           Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import           Data.List ( foldl', intersperse, isPrefixOf, isSuffixOf )
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe, isNothing )
import           Network.HTTP (urlEncode)


-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Either PandocError Pandoc
readOrg opts s = flip runReader def $ readWithM parseOrg def{ orgStateOptions = opts } (s ++ "\n\n")

-- | The parser used to read org files.
type OrgParser = ParserT [Char] OrgParserState (Reader OrgParserLocal)

--
-- Functions acting on the parser state
--
recordAnchorId :: String -> OrgParser ()
recordAnchorId i = updateState $ \s ->
  s{ orgStateAnchorIds = i : (orgStateAnchorIds s) }

updateLastForbiddenCharPos :: OrgParser ()
updateLastForbiddenCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastForbiddenCharPos = Just p}

updateLastPreCharPos :: OrgParser ()
updateLastPreCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastPreCharPos = Just p}

pushToInlineCharStack :: Char -> OrgParser ()
pushToInlineCharStack c = updateState $ \s ->
  s{ orgStateEmphasisCharStack = c:orgStateEmphasisCharStack s }

popInlineCharStack :: OrgParser ()
popInlineCharStack = updateState $ \s ->
  s{ orgStateEmphasisCharStack = drop 1 . orgStateEmphasisCharStack $ s }

surroundingEmphasisChar :: OrgParser [Char]
surroundingEmphasisChar =
  take 1 . drop 1 . orgStateEmphasisCharStack <$> getState

startEmphasisNewlinesCounting :: Int -> OrgParser ()
startEmphasisNewlinesCounting maxNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Just maxNewlines }

decEmphasisNewlinesCount :: OrgParser ()
decEmphasisNewlinesCount = updateState $ \s ->
  s{ orgStateEmphasisNewlines = (\n -> n - 1) <$> orgStateEmphasisNewlines s }

newlinesCountWithinLimits :: OrgParser Bool
newlinesCountWithinLimits = do
  st <- getState
  return $ ((< 0) <$> orgStateEmphasisNewlines st) /= Just True

resetEmphasisNewlines :: OrgParser ()
resetEmphasisNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Nothing }

addLinkFormat :: String
              -> (String -> String)
              -> OrgParser ()
addLinkFormat key formatter = updateState $ \s ->
  let fs = orgStateLinkFormatters s
  in s{ orgStateLinkFormatters = M.insert key formatter fs }

addToNotesTable :: OrgNoteRecord -> OrgParser ()
addToNotesTable note = do
  oldnotes <- orgStateNotes' <$> getState
  updateState $ \s -> s{ orgStateNotes' = note:oldnotes }

--
-- Export Settings
--
exportSetting :: OrgParser ()
exportSetting = choice
  [ booleanSetting "^" setExportSubSuperscripts
  , ignoredSetting "'"
  , ignoredSetting "*"
  , ignoredSetting "-"
  , ignoredSetting ":"
  , ignoredSetting "<"
  , ignoredSetting "\\n"
  , ignoredSetting "arch"
  , ignoredSetting "author"
  , ignoredSetting "c"
  , ignoredSetting "creator"
  , ignoredSetting "d"
  , ignoredSetting "date"
  , ignoredSetting "e"
  , ignoredSetting "email"
  , ignoredSetting "f"
  , ignoredSetting "H"
  , ignoredSetting "inline"
  , ignoredSetting "num"
  , ignoredSetting "p"
  , ignoredSetting "pri"
  , ignoredSetting "prop"
  , ignoredSetting "stat"
  , ignoredSetting "tags"
  , ignoredSetting "tasks"
  , ignoredSetting "tex"
  , ignoredSetting "timestamp"
  , ignoredSetting "title"
  , ignoredSetting "toc"
  , ignoredSetting "todo"
  , ignoredSetting "|"
  ] <?> "export setting"

booleanSetting :: String -> ExportSettingSetter Bool -> OrgParser ()
booleanSetting settingIdentifier setter = try $ do
  string settingIdentifier
  char ':'
  value <- many nonspaceChar
  let boolValue = case value of
                    "nil" -> False
                    "{}"  -> False
                    _     -> True
  updateState $ modifyExportSettings setter boolValue

ignoredSetting :: String -> OrgParser ()
ignoredSetting s = try (() <$ string s <* char ':' <* many nonspaceChar)

--
-- Parser
--
parseOrg :: OrgParser Pandoc
parseOrg = do
  blocks' <- parseBlocks
  st <- getState
  let meta = runF (orgStateMeta' st) st
  let removeUnwantedBlocks = dropCommentTrees . filter (/= Null)
  return $ Pandoc meta $ removeUnwantedBlocks (B.toList $ runF blocks' st)

-- | Drop COMMENT headers and the document tree below those headers.
dropCommentTrees :: [Block] -> [Block]
dropCommentTrees [] = []
dropCommentTrees (b:bs) =
  maybe (b:dropCommentTrees bs)
        (dropCommentTrees . flip dropUntilHeaderAboveLevel bs)
        (commentHeaderLevel b)

-- | Return the level of a header starting a comment or :noexport: tree and
--  Nothing otherwise.
commentHeaderLevel :: Block -> Maybe Int
commentHeaderLevel blk =
   case blk of
     (Header level _ ((Str "COMMENT"):_))          -> Just level
     (Header level _ title) | hasNoExportTag title -> Just level
     _                                             -> Nothing
 where
   hasNoExportTag :: [Inline] -> Bool
   hasNoExportTag = any isNoExportTag

   isNoExportTag :: Inline -> Bool
   isNoExportTag (Span ("", ["tag"], [("data-tag-name", "noexport")]) []) = True
   isNoExportTag _ = False

-- | Drop blocks until a header on or above the given level is seen
dropUntilHeaderAboveLevel :: Int -> [Block] -> [Block]
dropUntilHeaderAboveLevel n = dropWhile (not . isHeaderLevelLowerEq n)

isHeaderLevelLowerEq :: Int -> Block -> Bool
isHeaderLevelLowerEq n blk =
  case blk of
    (Header level _ _) -> n >= level
    _                  -> False


--
-- Adaptions and specializations of parsing utilities
--

-- The version Text.Pandoc.Parsing cannot be used, as we need additional parts
-- of the state saved and restored.
parseFromString :: OrgParser a -> String -> OrgParser a
parseFromString parser str' = do
  oldLastPreCharPos <- orgStateLastPreCharPos <$> getState
  updateState $ \s -> s{ orgStateLastPreCharPos = Nothing }
  result <- P.parseFromString parser str'
  updateState $ \s -> s{ orgStateLastPreCharPos = oldLastPreCharPos }
  return result

-- | Like @Text.Parsec.Char.newline@, but causes additional state changes.
newline :: OrgParser Char
newline =
  P.newline
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Like @Text.Parsec.Char.blanklines@, but causes additional state changes.
blanklines :: OrgParser [Char]
blanklines =
  P.blanklines
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Succeeds when we're in list context.
inList :: OrgParser ()
inList = do
  ctx <- orgStateParserContext <$> getState
  guard (ctx == ListItemState)

-- | Parse in different context
withContext :: ParserContext -- ^ New parser context
            -> OrgParser a   -- ^ Parser to run in that context
            -> OrgParser a
withContext context parser = do
  oldContext <- orgStateParserContext <$> getState
  updateState $ \s -> s{ orgStateParserContext = context }
  result <- parser
  updateState $ \s -> s{ orgStateParserContext = oldContext }
  return result

--
-- parsing blocks
--

parseBlocks :: OrgParser (F Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: OrgParser (F Blocks)
block = choice [ mempty <$ blanklines
               , table
               , orgBlock
               , figure
               , example
               , drawer
               , specialLine
               , header
               , return <$> hline
               , list
               , latexFragment
               , noteBlock
               , paraOrPlain
               ] <?> "block"


--
-- Block Attributes
--

-- | Attributes that may be added to figures (like a name or caption).
data BlockAttributes = BlockAttributes
  { blockAttrName    :: Maybe String
  , blockAttrCaption :: Maybe (F Inlines)
  }

stringyMetaAttribute :: (String -> Bool) -> OrgParser (String, String)
stringyMetaAttribute attrCheck = try $ do
  metaLineStart
  attrName <- map toUpper <$> many1Till nonspaceChar (char ':')
  guard $ attrCheck attrName
  skipSpaces
  attrValue <- manyTill anyChar newline
  return (attrName, attrValue)

blockAttributes :: OrgParser BlockAttributes
blockAttributes = try $ do
  kv <- many (stringyMetaAttribute attrCheck)
  let caption = foldl' (appendValues "CAPTION") Nothing kv
  let name    = lookup "NAME" kv
  caption' <- maybe (return Nothing)
                    (fmap Just . parseFromString parseInlines)
                    caption
  return $ BlockAttributes
           { blockAttrName = name
           , blockAttrCaption = caption'
           }
 where
   attrCheck :: String -> Bool
   attrCheck attr =
     case attr of
       "NAME"    -> True
       "CAPTION" -> True
       _         -> False

   appendValues :: String -> Maybe String -> (String, String) -> Maybe String
   appendValues attrName accValue (key, value) =
     if key /= attrName
     then accValue
     else case accValue of
            Just acc -> Just $ acc ++ ' ':value
            Nothing  -> Just value


--
-- Org Blocks (#+BEGIN_... / #+END_...)
--

type BlockProperties = (Int, String)  -- (Indentation, Block-Type)

updateIndent :: BlockProperties -> Int -> BlockProperties
updateIndent (_, blkType) indent = (indent, blkType)

orgBlock :: OrgParser (F Blocks)
orgBlock = try $ do
  blockAttrs <- blockAttributes
  blockProp@(_, blkType) <- blockHeaderStart
  ($ blockProp) $
    case blkType of
      "comment" -> withRaw'   (const mempty)
      "html"    -> withRaw'   (return . (B.rawBlock blkType))
      "latex"   -> withRaw'   (return . (B.rawBlock blkType))
      "ascii"   -> withRaw'   (return . (B.rawBlock blkType))
      "example" -> withRaw'   (return . exampleCode)
      "quote"   -> withParsed (fmap B.blockQuote)
      "verse"   -> verseBlock
      "src"     -> codeBlock blockAttrs
      _         -> withParsed (fmap $ divWithClass blkType)

blockHeaderStart :: OrgParser (Int, String)
blockHeaderStart = try $ (,) <$> indentation <*> blockType
 where
  blockType = map toLower <$> (stringAnyCase "#+begin_" *> orgArgWord)

indentation :: OrgParser Int
indentation = try $ do
  tabStop  <- getOption readerTabStop
  s        <- many spaceChar
  return $ spaceLength tabStop s

spaceLength :: Int -> String -> Int
spaceLength tabStop s = (sum . map charLen) s
 where
  charLen ' '  = 1
  charLen '\t' = tabStop
  charLen _    = 0

withRaw'   :: (String   -> F Blocks) -> BlockProperties -> OrgParser (F Blocks)
withRaw'   f blockProp = (ignHeaders *> (f <$> rawBlockContent blockProp))

withParsed :: (F Blocks -> F Blocks) -> BlockProperties -> OrgParser (F Blocks)
withParsed f blockProp = (ignHeaders *> (f <$> parsedBlockContent blockProp))

ignHeaders :: OrgParser ()
ignHeaders = (() <$ newline) <|> (() <$ anyLine)

divWithClass :: String -> Blocks -> Blocks
divWithClass cls = B.divWith ("", [cls], [])

verseBlock :: BlockProperties -> OrgParser (F Blocks)
verseBlock blkProp = try $ do
  ignHeaders
  content <- rawBlockContent blkProp
  fmap B.para . mconcat . intersperse (pure B.linebreak)
    <$> mapM (parseFromString parseInlines) (map (++ "\n") . lines $ content)

exportsCode :: [(String, String)] -> Bool
exportsCode attrs = not (("rundoc-exports", "none") `elem` attrs
                         || ("rundoc-exports", "results") `elem` attrs)

exportsResults :: [(String, String)] -> Bool
exportsResults attrs = ("rundoc-exports", "results") `elem` attrs
                       || ("rundoc-exports", "both") `elem` attrs

followingResultsBlock :: OrgParser (Maybe (F Blocks))
followingResultsBlock =
       optionMaybe (try $ blanklines *> stringAnyCase "#+RESULTS:"
                                     *> blankline
                                     *> block)

codeBlock :: BlockAttributes -> BlockProperties -> OrgParser (F Blocks)
codeBlock blockAttrs blkProp = do
  skipSpaces
  (classes, kv)     <- codeHeaderArgs <|> (mempty <$ ignHeaders)
  leadingIndent     <- lookAhead indentation
  content           <- rawBlockContent (updateIndent blkProp leadingIndent)
  resultsContent    <- followingResultsBlock
  let id'            = fromMaybe mempty $ blockAttrName blockAttrs
  let includeCode    = exportsCode kv
  let includeResults = exportsResults kv
  let codeBlck       = B.codeBlockWith ( id', classes, kv ) content
  let labelledBlck   = maybe (pure codeBlck)
                             (labelDiv codeBlck)
                             (blockAttrCaption blockAttrs)
  let resultBlck     = fromMaybe mempty resultsContent
  return $ (if includeCode then labelledBlck else mempty)
           <> (if includeResults then resultBlck else mempty)
 where
   labelDiv blk value =
       B.divWith nullAttr <$> (mappend <$> labelledBlock value
                                       <*> pure blk)
   labelledBlock = fmap (B.plain . B.spanWith ("", ["label"], []))

rawBlockContent :: BlockProperties -> OrgParser String
rawBlockContent (indent, blockType) = try $
  unlines . map commaEscaped <$> manyTill indentedLine blockEnder
 where
   indentedLine = try $ ("" <$ blankline) <|> (indentWith indent *> anyLine)
   blockEnder = try $ skipSpaces *> stringAnyCase ("#+end_" <> blockType)

parsedBlockContent :: BlockProperties -> OrgParser (F Blocks)
parsedBlockContent blkProps = try $ do
  raw <- rawBlockContent blkProps
  parseFromString parseBlocks (raw ++ "\n")

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> OrgParser String
indentWith num = do
  tabStop <- getOption readerTabStop
  if num < tabStop
     then count num (char ' ')
     else choice [ try (count num (char ' '))
                 , try (char '\t' >> count (num - tabStop) (char ' ')) ]

type SwitchOption = (Char, Maybe String)

orgArgWord :: OrgParser String
orgArgWord = many1 orgArgWordChar

-- | Parse code block arguments
-- TODO: We currently don't handle switches.
codeHeaderArgs :: OrgParser ([String], [(String, String)])
codeHeaderArgs = try $ do
  language   <- skipSpaces *> orgArgWord
  _          <- skipSpaces *> (try $ switch `sepBy` (many1 spaceChar))
  parameters <- manyTill blockOption newline
  let pandocLang = translateLang language
  return $
    if hasRundocParameters parameters
    then ( [ pandocLang, rundocBlockClass ]
         , map toRundocAttrib (("language", language) : parameters)
         )
    else ([ pandocLang ], parameters)
 where hasRundocParameters = not . null

switch :: OrgParser SwitchOption
switch = try $ simpleSwitch <|> lineNumbersSwitch
 where
   simpleSwitch = (\c -> (c, Nothing)) <$> (oneOf "-+" *> letter)
   lineNumbersSwitch = (\ls -> ('l', Just ls)) <$>
                       (string "-l \"" *> many1Till nonspaceChar (char '"'))

translateLang :: String -> String
translateLang "C"          = "c"
translateLang "C++"        = "cpp"
translateLang "emacs-lisp" = "commonlisp" -- emacs lisp is not supported
translateLang "js"         = "javascript"
translateLang "lisp"       = "commonlisp"
translateLang "R"          = "r"
translateLang "sh"         = "bash"
translateLang "sqlite"     = "sql"
translateLang cs = cs

-- | Prefix used for Rundoc classes and arguments.
rundocPrefix :: String
rundocPrefix = "rundoc-"

-- | The class-name used to mark rundoc blocks.
rundocBlockClass :: String
rundocBlockClass = rundocPrefix ++ "block"

blockOption :: OrgParser (String, String)
blockOption = try $ do
  argKey <- orgArgKey
  paramValue <- option "yes" orgParamValue
  return (argKey, paramValue)

inlineBlockOption :: OrgParser (String, String)
inlineBlockOption = try $ do
  argKey <- orgArgKey
  paramValue <- option "yes" orgInlineParamValue
  return (argKey, paramValue)

orgArgKey :: OrgParser String
orgArgKey = try $
  skipSpaces *> char ':'
             *> many1 orgArgWordChar

orgParamValue :: OrgParser String
orgParamValue = try $
  skipSpaces
    *> notFollowedBy (char ':' )
    *> many1 (noneOf "\t\n\r ")
    <* skipSpaces

orgInlineParamValue :: OrgParser String
orgInlineParamValue = try $
  skipSpaces
    *> notFollowedBy (char ':')
    *> many1 (noneOf "\t\n\r ]")
    <* skipSpaces

orgArgWordChar :: OrgParser Char
orgArgWordChar = alphaNum <|> oneOf "-_"

toRundocAttrib :: (String, String) -> (String, String)
toRundocAttrib = first ("rundoc-" ++)

commaEscaped :: String -> String
commaEscaped (',':cs@('*':_))     = cs
commaEscaped (',':cs@('#':'+':_)) = cs
commaEscaped cs                   = cs

example :: OrgParser (F Blocks)
example = try $ do
  return . return . exampleCode =<< unlines <$> many1 exampleLine

exampleCode :: String -> Blocks
exampleCode = B.codeBlockWith ("", ["example"], [])

exampleLine :: OrgParser String
exampleLine = try $ skipSpaces *> string ": " *> anyLine

-- Drawers for properties or a logbook
drawer :: OrgParser (F Blocks)
drawer = try $ do
  drawerStart
  manyTill drawerLine (try drawerEnd)
  return mempty

drawerStart :: OrgParser String
drawerStart = try $
  skipSpaces *> drawerName <* skipSpaces <* P.newline
 where drawerName = try $  char ':' *> validDrawerName <* char ':'
       validDrawerName =  stringAnyCase "PROPERTIES"
                          <|> stringAnyCase "LOGBOOK"

drawerLine :: OrgParser String
drawerLine = try anyLine

drawerEnd :: OrgParser String
drawerEnd = try $
  skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* P.newline


--
-- Figures
--


-- | Figures (Image on a line by itself, preceded by name and/or caption)
figure :: OrgParser (F Blocks)
figure = try $ do
  figAttrs <- blockAttributes
  src <- skipSpaces *> selfTarget <* skipSpaces <* P.newline
  guard . not . isNothing . blockAttrCaption $ figAttrs
  guard (isImageFilename src)
  let figName    = fromMaybe mempty $ blockAttrName figAttrs
  let figCaption = fromMaybe mempty $ blockAttrCaption figAttrs
  return $ (B.para . B.image src (withFigPrefix figName) <$> figCaption)
 where
   withFigPrefix cs =
     if "fig:" `isPrefixOf` cs
     then cs
     else "fig:" ++ cs

--
-- Comments, Options and Metadata
--
specialLine :: OrgParser (F Blocks)
specialLine = fmap return . try $ metaLine <|> commentLine

metaLine :: OrgParser Blocks
metaLine = mempty <$ metaLineStart <* (optionLine <|> declarationLine)

-- The order, in which blocks are tried, makes sure that we're not looking at
-- the beginning of a block, so we don't need to check for it
metaLineStart :: OrgParser ()
metaLineStart = try $ skipSpaces <* string "#+"

commentLine :: OrgParser Blocks
commentLine = commentLineStart *> anyLine *> pure mempty

commentLineStart :: OrgParser ()
commentLineStart = try $ skipSpaces <* string "# "

declarationLine :: OrgParser ()
declarationLine = try $ do
  key <- metaKey
  inlinesF <- metaInlines
  updateState $ \st ->
    let meta' = B.setMeta <$> pure key <*> inlinesF <*> pure nullMeta
    in st { orgStateMeta' = orgStateMeta' st <> meta' }
  return ()

metaInlines :: OrgParser (F MetaValue)
metaInlines = fmap (MetaInlines . B.toList) <$> inlinesTillNewline

metaKey :: OrgParser String
metaKey = map toLower <$> many1 (noneOf ": \n\r")
                      <*  char ':'
                      <*  skipSpaces

optionLine :: OrgParser ()
optionLine = try $ do
  key <- metaKey
  case key of
    "link"    -> parseLinkFormat >>= uncurry addLinkFormat
    "options" -> () <$ sepBy spaces exportSetting
    _         -> mzero

parseLinkFormat :: OrgParser ((String, String -> String))
parseLinkFormat = try $ do
  linkType <- (:) <$> letter <*> many (alphaNum <|> oneOf "-_") <* skipSpaces
  linkSubst <- parseFormat
  return (linkType, linkSubst)

-- | An ad-hoc, single-argument-only implementation of a printf-style format
-- parser.
parseFormat :: OrgParser (String -> String)
parseFormat = try $ do
  replacePlain <|> replaceUrl <|> justAppend
 where
   -- inefficient, but who cares
   replacePlain = try $ (\x -> concat . flip intersperse x)
                     <$> sequence [tillSpecifier 's', rest]
   replaceUrl   = try $ (\x -> concat . flip intersperse x . urlEncode)
                     <$> sequence [tillSpecifier 'h', rest]
   justAppend   = try $ (++) <$> rest

   rest            = manyTill anyChar         (eof <|> () <$ oneOf "\n\r")
   tillSpecifier c = manyTill (noneOf "\n\r") (try $ string ('%':c:""))

--
-- Headers
--

-- | Headers
header :: OrgParser (F Blocks)
header = try $ do
  level <- headerStart
  title <- manyTill inline (lookAhead headerEnd)
  tags <- headerEnd
  let inlns = trimInlinesF . mconcat $ title <> map tagToInlineF tags
  st <- getState
  let inlines = runF inlns st
  attr <- registerHeader nullAttr inlines
  return $ pure (B.headerWith attr level inlines)
 where
   tagToInlineF :: String -> F Inlines
   tagToInlineF t = return $ B.spanWith ("", ["tag"], [("data-tag-name", t)]) mempty

headerEnd :: OrgParser [String]
headerEnd = option [] headerTags <* newline

headerTags :: OrgParser [String]
headerTags = try $
  skipSpaces
  *> char ':'
  *> many1 tag
  <* skipSpaces
 where tag = many1 (alphaNum <|> oneOf "@%#_")
             <* char ':'

headerStart :: OrgParser Int
headerStart = try $
  (length <$> many1 (char '*')) <* many1 (char ' ') <* updateLastPreCharPos


-- Don't use (or need) the reader wrapper here, we want hline to be
-- @show@able.  Otherwise we can't use it with @notFollowedBy'@.

-- | Horizontal Line (five -- dashes or more)
hline :: OrgParser Blocks
hline = try $ do
  skipSpaces
  string "-----"
  many (char '-')
  skipSpaces
  newline
  return B.horizontalRule

--
-- Tables
--

data OrgTableRow = OrgContentRow (F [Blocks])
                 | OrgAlignRow [Alignment]
                 | OrgHlineRow

-- OrgTable is strongly related to the pandoc table ADT.  Using the same
-- (i.e. pandoc-global) ADT would mean that the reader would break if the
-- global structure was to be changed, which would be bad.  The final table
-- should be generated using a builder function.  Column widths aren't
-- implemented yet, so they are not tracked here.
data OrgTable = OrgTable
  { orgTableAlignments :: [Alignment]
  , orgTableHeader     :: [Blocks]
  , orgTableRows       :: [[Blocks]]
  }

table :: OrgParser (F Blocks)
table = try $ do
  blockAttrs <- blockAttributes
  lookAhead tableStart
  do
    rows <- tableRows
    let caption = fromMaybe (return mempty) $ blockAttrCaption blockAttrs
    return $ (<$> caption) . orgToPandocTable . normalizeTable =<< rowsToTable rows

orgToPandocTable :: OrgTable
                 -> Inlines
                 -> Blocks
orgToPandocTable (OrgTable aligns heads lns) caption =
  B.table caption (zip aligns $ repeat 0) heads lns

tableStart :: OrgParser Char
tableStart = try $ skipSpaces *> char '|'

tableRows :: OrgParser [OrgTableRow]
tableRows = try $ many (tableAlignRow <|> tableHline <|> tableContentRow)

tableContentRow :: OrgParser OrgTableRow
tableContentRow = try $
  OrgContentRow . sequence <$> (tableStart *> many1Till tableContentCell newline)

tableContentCell :: OrgParser (F Blocks)
tableContentCell = try $
  fmap B.plain . trimInlinesF . mconcat <$> manyTill inline endOfCell

tableAlignRow :: OrgParser OrgTableRow
tableAlignRow = try $ do
  tableStart
  cells <- many1Till tableAlignCell newline
  -- Empty rows are regular (i.e. content) rows, not alignment rows.
  guard $ any (/= AlignDefault) cells
  return $ OrgAlignRow cells

tableAlignCell :: OrgParser Alignment
tableAlignCell =
  choice [ try $ emptyCell *> return AlignDefault
         , try $ skipSpaces
                   *> char '<'
                   *> tableAlignFromChar
                   <* many digit
                   <* char '>'
                   <* emptyCell
         ] <?> "alignment info"
    where emptyCell = try $ skipSpaces *> endOfCell

tableAlignFromChar :: OrgParser Alignment
tableAlignFromChar = try $
  choice [ char 'l' *> return AlignLeft
         , char 'c' *> return AlignCenter
         , char 'r' *> return AlignRight
         ]

tableHline :: OrgParser OrgTableRow
tableHline = try $
  OrgHlineRow <$ (tableStart *> char '-' *> anyLine)

endOfCell :: OrgParser Char
endOfCell = try $ char '|' <|> lookAhead newline

rowsToTable :: [OrgTableRow]
            -> F OrgTable
rowsToTable = foldM rowToContent emptyTable
 where emptyTable = OrgTable mempty mempty mempty

normalizeTable :: OrgTable -> OrgTable
normalizeTable (OrgTable aligns heads rows) = OrgTable aligns' heads rows
 where
   refRow = if heads /= mempty
            then heads
            else if rows == mempty then mempty else head rows
   cols = length refRow
   fillColumns base padding = take cols $ base ++ repeat padding
   aligns' = fillColumns aligns AlignDefault

-- One or more horizontal rules after the first content line mark the previous
-- line as a header.  All other horizontal lines are discarded.
rowToContent :: OrgTable
             -> OrgTableRow
             -> F OrgTable
rowToContent orgTable row =
  case row of
    OrgHlineRow       -> return singleRowPromotedToHeader
    OrgAlignRow as    -> return . setAligns $ as
    OrgContentRow cs  -> appendToBody cs
 where
   singleRowPromotedToHeader :: OrgTable
   singleRowPromotedToHeader = case orgTable of
     OrgTable{ orgTableHeader = [], orgTableRows = b:[] } ->
            orgTable{ orgTableHeader = b , orgTableRows = [] }
     _   -> orgTable

   setAligns :: [Alignment] -> OrgTable
   setAligns aligns = orgTable{ orgTableAlignments = aligns }

   appendToBody :: F [Blocks] -> F OrgTable
   appendToBody frow = do
     newRow <- frow
     let oldRows = orgTableRows orgTable
     -- NOTE: This is an inefficient O(n) operation.  This should be changed
     -- if performance ever becomes a problem.
     return orgTable{ orgTableRows = oldRows ++ [newRow] }


--
-- LaTeX fragments
--
latexFragment :: OrgParser (F Blocks)
latexFragment = try $ do
  envName <- latexEnvStart
  content <- mconcat <$> manyTill anyLineNewline (latexEnd envName)
  return . return $ B.rawBlock "latex" (content `inLatexEnv` envName)
 where
   c `inLatexEnv` e = mconcat [ "\\begin{", e, "}\n"
                              , c
                              , "\\end{", e, "}\n"
                              ]

latexEnvStart :: OrgParser String
latexEnvStart = try $ do
  skipSpaces *> string "\\begin{"
             *> latexEnvName
             <* string "}"
             <* blankline

latexEnd :: String -> OrgParser ()
latexEnd envName = try $
  () <$ skipSpaces
     <* string ("\\end{" ++ envName ++ "}")
     <* blankline

-- | Parses a LaTeX environment name.
latexEnvName :: OrgParser String
latexEnvName = try $ do
  mappend <$> many1 alphaNum
          <*> option "" (string "*")


--
-- Footnote defintions
--
noteBlock :: OrgParser (F Blocks)
noteBlock = try $ do
  ref <- noteMarker <* skipSpaces
  content <- mconcat <$> blocksTillHeaderOrNote
  addToNotesTable (ref, content)
  return mempty
 where
   blocksTillHeaderOrNote =
     many1Till block (eof <|> () <$ lookAhead noteMarker
                          <|> () <$ lookAhead headerStart)

-- Paragraphs or Plain text
paraOrPlain :: OrgParser (F Blocks)
paraOrPlain = try $ do
  ils <- parseInlines
  nl <- option False (newline *> return True)
  -- Read block as paragraph, except if we are in a list context and the block
  -- is directly followed by a list item, in which case the block is read as
  -- plain text.
  try (guard nl
       *> notFollowedBy (inList *> (orderedListStart <|> bulletListStart))
       *> return (B.para <$> ils))
    <|>  (return (B.plain <$> ils))

inlinesTillNewline :: OrgParser (F Inlines)
inlinesTillNewline = trimInlinesF . mconcat <$> manyTill inline newline


--
-- list blocks
--

list :: OrgParser (F Blocks)
list = choice [ definitionList, bulletList, orderedList ] <?> "list"

definitionList :: OrgParser (F Blocks)
definitionList = try $ do n <- lookAhead (bulletListStart' Nothing)
                          fmap B.definitionList . fmap compactify'DL . sequence
                            <$> many1 (definitionListItem $ bulletListStart' (Just n))

bulletList :: OrgParser (F Blocks)
bulletList = try $ do n <- lookAhead (bulletListStart' Nothing)
                      fmap B.bulletList . fmap compactify' . sequence
                        <$> many1 (listItem (bulletListStart' $ Just n))

orderedList :: OrgParser (F Blocks)
orderedList = fmap B.orderedList . fmap compactify' . sequence
              <$> many1 (listItem orderedListStart)

genericListStart :: OrgParser String
                 -> OrgParser Int
genericListStart listMarker = try $
  (+) <$> (length <$> many spaceChar)
      <*> (length <$> listMarker <* many1 spaceChar)

-- parses bullet list marker. maybe we know the indent level
bulletListStart :: OrgParser Int
bulletListStart = bulletListStart' Nothing

bulletListStart' :: Maybe Int -> OrgParser Int
-- returns length of bulletList prefix, inclusive of marker
bulletListStart' Nothing  = do ind <- length <$> many spaceChar
                               when (ind == 0) $ notFollowedBy (char '*')
                               oneOf bullets
                               many1 spaceChar
                               return (ind + 1)
 -- Unindented lists are legal, but they can't use '*' bullets
 -- We return n to maintain compatibility with the generic listItem
bulletListStart' (Just n) = do count (n-1) spaceChar
                               when (n == 1) $ notFollowedBy (char '*')
                               oneOf bullets
                               many1 spaceChar
                               return n

bullets :: String
bullets = "*+-"

orderedListStart :: OrgParser Int
orderedListStart = genericListStart orderedListMarker
  -- Ordered list markers allowed in org-mode
  where orderedListMarker = mappend <$> many1 digit <*> (pure <$> oneOf ".)")

definitionListItem :: OrgParser Int
                   -> OrgParser (F (Inlines, [Blocks]))
definitionListItem parseMarkerGetLength = try $ do
  markerLength <- parseMarkerGetLength
  term <- manyTill (noneOf "\n\r") (try definitionMarker)
  line1 <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  cont <- concat <$> many (listContinuation markerLength)
  term' <- parseFromString parseInlines term
  contents' <- parseFromString parseBlocks $ line1 ++ blank ++ cont
  return $ (,) <$> term' <*> fmap (:[]) contents'
 where
   definitionMarker =
     spaceChar *> string "::" <* (spaceChar <|> lookAhead P.newline)


-- parse raw text for one list item, excluding start marker and continuations
listItem :: OrgParser Int
         -> OrgParser (F Blocks)
listItem start = try . withContext ListItemState $ do
  markerLength <- try start
  firstLine <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  rest <- concat <$> many (listContinuation markerLength)
  parseFromString parseBlocks $ firstLine ++ blank ++ rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> OrgParser String
listContinuation markerLength = try $
  notFollowedBy' blankline
  *> (mappend <$> (concat <$> many1 listLine)
              <*> many blankline)
 where listLine = try $ indentWith markerLength *> anyLineNewline

anyLineNewline :: OrgParser String
anyLineNewline = (++ "\n") <$> anyLine


--
-- inline
--

inline :: OrgParser (F Inlines)
inline =
  choice [ whitespace
         , linebreak
         , cite
         , footnote
         , linkOrImage
         , anchor
         , inlineCodeBlock
         , str
         , endline
         , emph
         , strong
         , strikeout
         , underline
         , code
         , math
         , displayMath
         , verbatim
         , subscript
         , superscript
         , inlineLaTeX
         , smart
         , symbol
         ] <* (guard =<< newlinesCountWithinLimits)
  <?> "inline"

parseInlines :: OrgParser (F Inlines)
parseInlines = trimInlinesF . mconcat <$> many1 inline

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-,./:<=>[\\]^_{|}~"


whitespace :: OrgParser (F Inlines)
whitespace = pure B.space <$ skipMany1 spaceChar
                          <* updateLastPreCharPos
                          <* updateLastForbiddenCharPos
             <?> "whitespace"

linebreak :: OrgParser (F Inlines)
linebreak = try $ pure B.linebreak <$ string "\\\\" <* skipSpaces <* newline

str :: OrgParser (F Inlines)
str = return . B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")
      <* updateLastStrPos

-- | An endline character that can be treated as a space, not a structural
-- break.  This should reflect the values of the Emacs variable
-- @org-element-pagaraph-separate@.
endline :: OrgParser (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy' exampleLine
  notFollowedBy' hline
  notFollowedBy' noteMarker
  notFollowedBy' tableStart
  notFollowedBy' drawerStart
  notFollowedBy' headerStart
  notFollowedBy' metaLineStart
  notFollowedBy' latexEnvStart
  notFollowedBy' commentLineStart
  notFollowedBy' bulletListStart
  notFollowedBy' orderedListStart
  decEmphasisNewlinesCount
  guard =<< newlinesCountWithinLimits
  updateLastPreCharPos
  return . return $ B.softbreak

cite :: OrgParser (F Inlines)
cite = try $ do
  guardEnabled Ext_citations
  (cs, raw) <- withRaw normalCite
  return $ (flip B.cite (B.text raw)) <$> cs

normalCite :: OrgParser (F [Citation])
normalCite = try $  char '['
                 *> skipSpaces
                 *> citeList
                 <* skipSpaces
                 <* char ']'

citeList :: OrgParser (F [Citation])
citeList = sequence <$> sepBy1 citation (try $ char ';' *> skipSpaces)

citation :: OrgParser (F Citation)
citation = try $ do
  pref <- prefix
  (suppress_author, key) <- citeKey
  suff <- suffix
  return $ do
    x <- pref
    y <- suff
    return $ Citation{ citationId      = key
                     , citationPrefix  = B.toList x
                     , citationSuffix  = B.toList y
                     , citationMode    = if suppress_author
                                            then SuppressAuthor
                                            else NormalCitation
                     , citationNoteNum = 0
                     , citationHash    = 0
                     }
 where
   prefix = trimInlinesF . mconcat <$>
            manyTill inline (char ']' <|> (']' <$ lookAhead citeKey))
   suffix = try $ do
     hasSpace <- option False (notFollowedBy nonspaceChar >> return True)
     skipSpaces
     rest <- trimInlinesF . mconcat <$>
             many (notFollowedBy (oneOf ";]") *> inline)
     return $ if hasSpace
              then (B.space <>) <$> rest
              else rest

footnote :: OrgParser (F Inlines)
footnote = try $ inlineNote <|> referencedNote

inlineNote :: OrgParser (F Inlines)
inlineNote = try $ do
  string "[fn:"
  ref <- many alphaNum
  char ':'
  note <- fmap B.para . trimInlinesF . mconcat <$> many1Till inline (char ']')
  when (not $ null ref) $
       addToNotesTable ("fn:" ++ ref, note)
  return $ B.note <$> note

referencedNote :: OrgParser (F Inlines)
referencedNote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF orgStateNotes'
    case lookup ref notes of
      Nothing   -> return $ B.str $ "[" ++ ref ++ "]"
      Just contents  -> do
        st <- askF
        let contents' = runF contents st{ orgStateNotes' = [] }
        return $ B.note contents'

noteMarker :: OrgParser String
noteMarker = try $ do
  char '['
  choice [ many1Till digit (char ']')
         , (++) <$> string "fn:"
                <*> many1Till (noneOf "\n\r\t ") (char ']')
         ]

linkOrImage :: OrgParser (F Inlines)
linkOrImage = explicitOrImageLink
              <|> selflinkOrImage
              <|> angleLink
              <|> plainLink
              <?> "link or image"

explicitOrImageLink :: OrgParser (F Inlines)
explicitOrImageLink = try $ do
  char '['
  srcF   <- applyCustomLinkFormat =<< possiblyEmptyLinkTarget
  title  <- enclosedRaw (char '[') (char ']')
  title' <- parseFromString (mconcat <$> many inline) title
  char ']'
  return $ do
    src <- srcF
    if isImageFilename title
      then pure $ B.link src "" $ B.image title mempty mempty
      else linkToInlinesF src =<< title'

selflinkOrImage :: OrgParser (F Inlines)
selflinkOrImage = try $ do
  src <- char '[' *> linkTarget <* char ']'
  return $ linkToInlinesF src (B.str src)

plainLink :: OrgParser (F Inlines)
plainLink = try $ do
  (orig, src) <- uri
  returnF $ B.link src "" (B.str orig)

angleLink :: OrgParser (F Inlines)
angleLink = try $ do
  char '<'
  link <- plainLink
  char '>'
  return link

selfTarget :: OrgParser String
selfTarget = try $ char '[' *> linkTarget <* char ']'

linkTarget :: OrgParser String
linkTarget = enclosedByPair '[' ']' (noneOf "\n\r[]")

possiblyEmptyLinkTarget :: OrgParser String
possiblyEmptyLinkTarget = try linkTarget <|> ("" <$ string "[]")

applyCustomLinkFormat :: String -> OrgParser (F String)
applyCustomLinkFormat link = do
  let (linkType, rest) = break (== ':') link
  return $ do
    formatter <- M.lookup linkType <$> asksF orgStateLinkFormatters
    return $ maybe link ($ drop 1 rest) formatter

-- | Take a link and return a function which produces new inlines when given
-- description inlines.
linkToInlinesF :: String -> Inlines -> F Inlines
linkToInlinesF linkStr =
  case linkStr of
    ""      -> pure . B.link mempty ""       -- wiki link (empty by convention)
    ('#':_) -> pure . B.link linkStr ""      -- document-local fraction
    _       -> case cleanLinkString linkStr of
                 (Just cleanedLink) -> if isImageFilename cleanedLink
                                       then const . pure $ B.image cleanedLink "" ""
                                       else pure . B.link cleanedLink ""
                 Nothing -> internalLink linkStr  -- other internal link

-- | Cleanup and canonicalize a string describing a link.  Return @Nothing@ if
-- the string does not appear to be a link.
cleanLinkString :: String -> Maybe String
cleanLinkString s =
  case s of
    '/':_                  -> Just $ "file://" ++ s  -- absolute path
    '.':'/':_              -> Just s                 -- relative path
    '.':'.':'/':_          -> Just s                 -- relative path
    -- Relative path or URL (file schema)
    'f':'i':'l':'e':':':s' -> Just $ if ("//" `isPrefixOf` s') then s else s'
    _ | isUrl s            -> Just s                 -- URL
    _                      -> Nothing
 where
   isUrl :: String -> Bool
   isUrl cs =
     let (scheme, path) = break (== ':') cs
     in all (\c -> isAlphaNum c || c `elem` (".-"::String)) scheme
          && not (null path)

isImageFilename :: String -> Bool
isImageFilename filename =
  any (\x -> ('.':x)  `isSuffixOf` filename) imageExtensions &&
  (any (\x -> (x++":") `isPrefixOf` filename) protocols ||
   ':' `notElem` filename)
 where
   imageExtensions = [ "jpeg" , "jpg" , "png" , "gif" , "svg" ]
   protocols = [ "file", "http", "https" ]

internalLink :: String -> Inlines -> F Inlines
internalLink link title = do
  anchorB <- (link `elem`) <$> asksF orgStateAnchorIds
  if anchorB
    then return $ B.link ('#':link) "" title
    else return $ B.emph title

-- | Parse an anchor like @<<anchor-id>>@ and return an empty span with
-- @anchor-id@ set as id.  Legal anchors in org-mode are defined through
-- @org-target-regexp@, which is fairly liberal.  Since no link is created if
-- @anchor-id@ contains spaces, we are more restrictive in what is accepted as
-- an anchor.

anchor :: OrgParser (F Inlines)
anchor =  try $ do
  anchorId <- parseAnchor
  recordAnchorId anchorId
  returnF $ B.spanWith (solidify anchorId, [], []) mempty
 where
       parseAnchor = string "<<"
                     *> many1 (noneOf "\t\n\r<>\"' ")
                     <* string ">>"
                     <* skipSpaces

-- | Replace every char but [a-zA-Z0-9_.-:] with a hypen '-'.  This mirrors
-- the org function @org-export-solidify-link-text@.

solidify :: String -> String
solidify = map replaceSpecialChar
 where replaceSpecialChar c
           | isAlphaNum c    = c
           | c `elem` ("_.-:" :: String) = c
           | otherwise       = '-'

-- | Parses an inline code block and marks it as an babel block.
inlineCodeBlock :: OrgParser (F Inlines)
inlineCodeBlock = try $ do
  string "src_"
  lang <- many1 orgArgWordChar
  opts <- option [] $ enclosedByPair '[' ']' inlineBlockOption
  inlineCode <- enclosedByPair '{' '}' (noneOf "\n\r")
  let attrClasses = [translateLang lang, rundocBlockClass]
  let attrKeyVal  = map toRundocAttrib (("language", lang) : opts)
  returnF $ B.codeWith ("", attrClasses, attrKeyVal) inlineCode

enclosedByPair :: Char          -- ^ opening char
               -> Char          -- ^ closing char
               -> OrgParser a   -- ^ parser
               -> OrgParser [a]
enclosedByPair s e p = char s *> many1Till p (char e)

emph      :: OrgParser (F Inlines)
emph      = fmap B.emph         <$> emphasisBetween '/'

strong    :: OrgParser (F Inlines)
strong    = fmap B.strong       <$> emphasisBetween '*'

strikeout :: OrgParser (F Inlines)
strikeout = fmap B.strikeout    <$> emphasisBetween '+'

-- There is no underline, so we use strong instead.
underline :: OrgParser (F Inlines)
underline = fmap B.strong       <$> emphasisBetween '_'

verbatim  :: OrgParser (F Inlines)
verbatim  = return . B.code     <$> verbatimBetween '='

code      :: OrgParser (F Inlines)
code      = return . B.code     <$> verbatimBetween '~'

subscript   :: OrgParser (F Inlines)
subscript   = fmap B.subscript   <$> try (char '_' *> subOrSuperExpr)

superscript :: OrgParser (F Inlines)
superscript = fmap B.superscript <$> try (char '^' *> subOrSuperExpr)

math      :: OrgParser (F Inlines)
math      = return . B.math      <$> choice [ math1CharBetween '$'
                                            , mathStringBetween '$'
                                            , rawMathBetween "\\(" "\\)"
                                            ]

displayMath :: OrgParser (F Inlines)
displayMath = return . B.displayMath <$> choice [ rawMathBetween "\\[" "\\]"
                                                , rawMathBetween "$$"  "$$"
                                                ]

updatePositions :: Char
                -> OrgParser (Char)
updatePositions c = do
  when (c `elem` emphasisPreChars) updateLastPreCharPos
  when (c `elem` emphasisForbiddenBorderChars) updateLastForbiddenCharPos
  return c

symbol :: OrgParser (F Inlines)
symbol = return . B.str . (: "") <$> (oneOf specialChars >>= updatePositions)

emphasisBetween :: Char
                -> OrgParser (F Inlines)
emphasisBetween c = try $ do
  startEmphasisNewlinesCounting emphasisAllowedNewlines
  res <- enclosedInlines (emphasisStart c) (emphasisEnd c)
  isTopLevelEmphasis <- null . orgStateEmphasisCharStack <$> getState
  when isTopLevelEmphasis
       resetEmphasisNewlines
  return res

verbatimBetween :: Char
                -> OrgParser String
verbatimBetween c = try $
  emphasisStart c *>
  many1TillNOrLessNewlines 1 (noneOf "\n\r") (emphasisEnd c)

-- | Parses a raw string delimited by @c@ using Org's math rules
mathStringBetween :: Char
                  -> OrgParser String
mathStringBetween c = try $ do
  mathStart c
  body <- many1TillNOrLessNewlines mathAllowedNewlines
                                   (noneOf (c:"\n\r"))
                                   (lookAhead $ mathEnd c)
  final <- mathEnd c
  return $ body ++ [final]

-- | Parse a single character between @c@ using math rules
math1CharBetween :: Char
                -> OrgParser String
math1CharBetween c = try $ do
  char c
  res <- noneOf $ c:mathForbiddenBorderChars
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return [res]

rawMathBetween :: String
               -> String
               -> OrgParser String
rawMathBetween s e = try $ string s *> manyTill anyChar (try $ string e)

-- | Parses the start (opening character) of emphasis
emphasisStart :: Char -> OrgParser Char
emphasisStart c = try $ do
  guard =<< afterEmphasisPreChar
  guard =<< notAfterString
  char c
  lookAhead (noneOf emphasisForbiddenBorderChars)
  pushToInlineCharStack c
  return c

-- | Parses the closing character of emphasis
emphasisEnd :: Char -> OrgParser Char
emphasisEnd c = try $ do
  guard =<< notAfterForbiddenBorderChar
  char c
  eof <|> () <$ lookAhead acceptablePostChars
  updateLastStrPos
  popInlineCharStack
  return c
 where acceptablePostChars =
           surroundingEmphasisChar >>= \x -> oneOf (x ++ emphasisPostChars)

mathStart :: Char -> OrgParser Char
mathStart c = try $
  char c <* notFollowedBy' (oneOf (c:mathForbiddenBorderChars))

mathEnd :: Char -> OrgParser Char
mathEnd c = try $ do
  res <- noneOf (c:mathForbiddenBorderChars)
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return res


enclosedInlines :: OrgParser a
                -> OrgParser b
                -> OrgParser (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> enclosed start end inline

enclosedRaw :: OrgParser a
            -> OrgParser b
            -> OrgParser String
enclosedRaw start end = try $
  start *> (onSingleLine <|> spanningTwoLines)
 where onSingleLine = try $ many1Till (noneOf "\n\r") end
       spanningTwoLines = try $
         anyLine >>= \f -> mappend (f <> " ") <$> onSingleLine

-- | Like many1Till, but parses at most @n+1@ lines.  @p@ must not consume
--   newlines.
many1TillNOrLessNewlines :: Int
                         -> OrgParser Char
                         -> OrgParser a
                         -> OrgParser String
many1TillNOrLessNewlines n p end = try $
  nMoreLines (Just n) mempty >>= oneOrMore
 where
   nMoreLines Nothing  cs = return cs
   nMoreLines (Just 0) cs = try $ (cs ++) <$> finalLine
   nMoreLines k        cs = try $ (final k cs <|> rest k cs)
                                  >>= uncurry nMoreLines
   final _ cs = (\x -> (Nothing,      cs ++ x)) <$> try finalLine
   rest  m cs = (\x -> (minus1 <$> m, cs ++ x ++ "\n")) <$> try (manyTill p P.newline)
   finalLine = try $ manyTill p end
   minus1 k = k - 1
   oneOrMore cs = guard (not $ null cs) *> return cs

-- Org allows customization of the way it reads emphasis.  We use the defaults
-- here (see, e.g., the Emacs Lisp variable `org-emphasis-regexp-components`
-- for details).

-- | Chars allowed to occur before emphasis (spaces and newlines are ok, too)
emphasisPreChars :: [Char]
emphasisPreChars = "\t \"'({"

-- | Chars allowed at after emphasis
emphasisPostChars :: [Char]
emphasisPostChars = "\t\n !\"'),-.:;?\\}"

-- | Chars not allowed at the (inner) border of emphasis
emphasisForbiddenBorderChars :: [Char]
emphasisForbiddenBorderChars = "\t\n\r \"',"

-- | The maximum number of newlines within
emphasisAllowedNewlines :: Int
emphasisAllowedNewlines = 1

-- LaTeX-style math: see `org-latex-regexps` for details

-- | Chars allowed after an inline ($...$) math statement
mathPostChars :: [Char]
mathPostChars = "\t\n \"'),-.:;?"

-- | Chars not allowed at the (inner) border of math
mathForbiddenBorderChars :: [Char]
mathForbiddenBorderChars = "\t\n\r ,;.$"

-- | Maximum number of newlines in an inline math statement
mathAllowedNewlines :: Int
mathAllowedNewlines = 2

-- | Whether we are right behind a char allowed before emphasis
afterEmphasisPreChar :: OrgParser Bool
afterEmphasisPreChar = do
  pos <- getPosition
  lastPrePos <- orgStateLastPreCharPos <$> getState
  return . fromMaybe True $ (== pos) <$> lastPrePos

-- | Whether the parser is right after a forbidden border char
notAfterForbiddenBorderChar :: OrgParser Bool
notAfterForbiddenBorderChar = do
  pos <- getPosition
  lastFBCPos <- orgStateLastForbiddenCharPos <$> getState
  return $ lastFBCPos /= Just pos

-- | Read a sub- or superscript expression
subOrSuperExpr :: OrgParser (F Inlines)
subOrSuperExpr = try $
  choice [ id                   <$> charsInBalanced '{' '}' (noneOf "\n\r")
         , enclosing ('(', ')') <$> charsInBalanced '(' ')' (noneOf "\n\r")
         , simpleSubOrSuperString
         ] >>= parseFromString (mconcat <$> many inline)
 where enclosing (left, right) s = left : s ++ [right]

simpleSubOrSuperString :: OrgParser String
simpleSubOrSuperString = try $ do
  state <- getState
  guard . exportSubSuperscripts . orgStateExportSettings $ state
  choice [ string "*"
         , mappend <$> option [] ((:[]) <$> oneOf "+-")
                   <*> many1 alphaNum
         ]

inlineLaTeX :: OrgParser (F Inlines)
inlineLaTeX = try $ do
  cmd <- inlineLaTeXCommand
  maybe mzero returnF $
     parseAsMath cmd `mplus` parseAsMathMLSym cmd `mplus` parseAsInlineLaTeX cmd
 where
   parseAsMath :: String -> Maybe Inlines
   parseAsMath cs = B.fromList <$> texMathToPandoc cs

   parseAsInlineLaTeX :: String -> Maybe Inlines
   parseAsInlineLaTeX cs = maybeRight $ runParser inlineCommand state "" cs

   parseAsMathMLSym :: String -> Maybe Inlines
   parseAsMathMLSym cs = B.str <$> MathMLEntityMap.getUnicode (clean cs)
    -- drop initial backslash and any trailing "{}"
    where clean = dropWhileEnd (`elem` ("{}" :: String)) . drop 1

   state :: ParserState
   state = def{ stateOptions = def{ readerParseRaw = True }}

   texMathToPandoc :: String -> Maybe [Inline]
   texMathToPandoc cs = (maybeRight $ readTeX cs) >>= writePandoc DisplayInline

maybeRight :: Either a b -> Maybe b
maybeRight = either (const Nothing) Just

inlineLaTeXCommand :: OrgParser String
inlineLaTeXCommand = try $ do
  rest <- getInput
  case runParser rawLaTeXInline def "source" rest of
    Right (RawInline _ cs) -> do
      -- drop any trailing whitespace, those are not be part of the command as
      -- far as org mode is concerned.
      let cmdNoSpc = dropWhileEnd isSpace cs
      let len = length cmdNoSpc
      count len anyChar
      return cmdNoSpc
    _ -> mzero

-- Taken from Data.OldList.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

smart :: OrgParser (F Inlines)
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice (map (return <$>) [orgApostrophe, orgDash, orgEllipses])
  where
    orgDash = dash <* updatePositions '-'
    orgEllipses = ellipses <* updatePositions '.'
    orgApostrophe =
          (char '\'' <|> char '\8217') <* updateLastPreCharPos
                                       <* updateLastForbiddenCharPos
                                       *> return (B.str "\x2019")

singleQuoted :: OrgParser (F Inlines)
singleQuoted = try $ do
  singleQuoteStart
  updatePositions '\''
  withQuoteContext InSingleQuote $
    fmap B.singleQuoted . trimInlinesF . mconcat <$>
      many1Till inline (singleQuoteEnd <* updatePositions '\'')

-- doubleQuoted will handle regular double-quoted sections, as well
-- as dialogues with an open double-quote without a close double-quote
-- in the same paragraph.
doubleQuoted :: OrgParser (F Inlines)
doubleQuoted = try $ do
  doubleQuoteStart
  updatePositions '"'
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  (withQuoteContext InDoubleQuote $ (doubleQuoteEnd <* updateLastForbiddenCharPos) >> return
       (fmap B.doubleQuoted . trimInlinesF $ contents))
   <|> (return $ return (B.str "\8220") <> contents)

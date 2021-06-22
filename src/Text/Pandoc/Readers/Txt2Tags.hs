{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- |
   Module      : Text.Pandoc.Readers.Txt2Tags
   Copyright   : Copyright (C) 2014 Matthew Pickering
   License     : GNU GPL, version 2 or above

   Maintainer  : Matthew Pickering <matthewtpickering@gmail.com>

Conversion of txt2tags formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Txt2Tags ( readTxt2Tags
                                    , getT2TMeta
                                    , T2TMeta (..)
                                    )
                                    where

import Control.Monad (guard, void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Default
import Data.List (intercalate, transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (formatTime)
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Data.Time (defaultTimeLocale)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (space, spaces, uri)
import Text.Pandoc.Shared (compactify, compactifyDL, escapeURI)

type T2T = ParserT Sources ParserState (Reader T2TMeta)

-- | An object for the T2T macros meta information
-- the contents of each field is simply substituted verbatim into the file
data  T2TMeta = T2TMeta {
                 date    :: Text -- ^ Current date
               , mtime   :: Text -- ^ Last modification time of infile
               , infile  :: FilePath -- ^ Input file
               , outfile :: FilePath -- ^ Output file
               } deriving Show

instance Default T2TMeta where
    def = T2TMeta "" "" "" ""

-- | Get the meta information required by Txt2Tags macros
getT2TMeta :: PandocMonad m => m T2TMeta
getT2TMeta = do
    inps <- P.getInputFiles
    outp <- fromMaybe "" <$> P.getOutputFile
    curDate <- formatTime defaultTimeLocale "%F" <$> P.getZonedTime
    curMtime <- catchError
                 (mapM P.getModificationTime inps >>=
                   (\case
                       Nothing ->
                         formatTime defaultTimeLocale "%T" <$> P.getZonedTime
                       Just ts -> return $
                         formatTime defaultTimeLocale "%T" $ maximum ts)
                    . nonEmpty)
                (const (return ""))
    return $ T2TMeta (T.pack curDate) (T.pack curMtime)
                     (intercalate ", " inps) outp

-- | Read Txt2Tags from an input string returning a Pandoc document
readTxt2Tags :: (PandocMonad m, ToSources a)
             => ReaderOptions
             -> a
             -> m Pandoc
readTxt2Tags opts s = do
  let sources = ensureFinalNewlines 2 (toSources s)
  meta <- getT2TMeta
  let parsed = flip runReader meta $
        readWithM parseT2T (def {stateOptions = opts}) sources
  case parsed of
    Right result -> return result
    Left e       -> throwError e

-- | Read Txt2Tags (ignoring all macros) from an input string returning
-- a Pandoc document
-- readTxt2TagsNoMacros :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
-- readTxt2TagsNoMacros = readTxt2Tags

parseT2T :: T2T Pandoc
parseT2T = do
  -- Parse header if standalone flag is set
  standalone <- getOption readerStandalone
  when standalone parseHeader
  body <- mconcat <$>  manyTill block eof
  meta' <- stateMeta <$> getState
  return $ Pandoc meta' (B.toList body)

parseHeader :: T2T ()
parseHeader = do
  () <$ try blankline <|> header
  meta <- stateMeta <$> getState
  optional blanklines
  config <- manyTill setting (notFollowedBy setting)
  -- TODO: Handle settings better
  let settings = foldr (\(k,v) -> B.setMeta k (MetaString v)) meta config
  updateState (\s -> s {stateMeta = settings}) <* optional blanklines

header :: T2T ()
header = titleline >> authorline >> dateline

headerline :: B.ToMetaValue a => Text -> T2T a -> T2T ()
headerline field p = (() <$ try blankline)
                        <|> (p >>= updateState . B.setMeta field)

titleline :: T2T ()
titleline =
  headerline "title" (trimInlines . mconcat <$> manyTill inline newline)

authorline :: T2T ()
authorline =
  headerline "author" (sepBy author (char ';') <* newline)
  where
    author = trimInlines . mconcat <$> many (notFollowedBy (char ';' <|> newline) >> inline)

dateline :: T2T ()
dateline = headerline "date" (trimInlines . mconcat <$> manyTill inline newline)

type Keyword = Text
type Value = Text

setting :: T2T (Keyword, Value)
setting = do
  string "%!"
  keyword <- ignoreSpacesCap (many1Char alphaNum)
  char ':'
  value <- ignoreSpacesCap (manyTillChar anyChar newline)
  return (keyword, value)

-- Blocks

parseBlocks :: T2T Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: T2T Blocks
block =
  choice
    [ mempty <$ blanklines
    , quote
    , hrule -- hrule must go above title
    , title
    , commentBlock
    , verbatim
    , rawBlock
    , taggedBlock
    , list
    , table
    , para
    ]

title :: T2T Blocks
title = try $ balancedTitle '+' <|> balancedTitle '='

balancedTitle :: Char -> T2T Blocks
balancedTitle c = try $ do
  spaces
  level <- length <$> many1 (char c)
  guard (level <= 5) -- Max header level 5
  heading <- manyTillChar (noneOf "\n\r") (count level (char c))
  label <- optionMaybe (enclosed (char '[') (char ']') (alphaNum <|> oneOf "_-"))
  many spaceChar *> newline
  let attr = maybe nullAttr (\x -> (T.pack x, [], [])) label
  return $ B.headerWith attr level (trimInlines $ B.text heading)

para :: T2T Blocks
para = try $ do
  ils <- parseInlines
  nl <- option False (True <$ newline)
  option (B.plain ils) (guard nl >> notFollowedBy listStart >> return (B.para ils))
  where
    listStart = try bulletListStart <|> orderedListStart

commentBlock :: T2T Blocks
commentBlock = try (blockMarkupArea anyLine (const mempty) "%%%") <|> comment

-- Separator and Strong line treated the same
hrule :: T2T Blocks
hrule = try $ do
  spaces
  line <- many1 (oneOf "=-_")
  guard (length line >= 20)
  B.horizontalRule <$ blankline

quote :: T2T Blocks
quote = try $ do
  lookAhead tab
  rawQuote <-  many1 (tab *> optional spaces *> anyLine)
  contents <- parseFromString' parseBlocks (T.intercalate "\n" rawQuote <> "\n\n")
  return $ B.blockQuote contents

commentLine :: T2T Inlines
commentLine = comment

-- List Parsing code from Org Reader

list :: T2T Blocks
list = choice [bulletList, orderedList, definitionList]

bulletList :: T2T Blocks
bulletList = B.bulletList . compactify
             <$> many1 (listItem bulletListStart parseBlocks)

orderedList :: T2T Blocks
orderedList = B.orderedList . compactify
              <$> many1 (listItem orderedListStart parseBlocks)

definitionList :: T2T Blocks
definitionList = try $
  B.definitionList . compactifyDL <$>
    many1 (listItem definitionListStart definitionListEnd)

definitionListEnd :: T2T (Inlines, [Blocks])
definitionListEnd = (,) <$> (mconcat <$> manyTill inline newline) <*> ((:[]) <$> parseBlocks)

genericListStart :: T2T Char
                 -> T2T Int
genericListStart listMarker = try $
  (2+) <$> (length <$> many spaceChar
            <* listMarker <* space <* notFollowedBy space)

-- parses bullet list \start and returns its length (excl. following whitespace)
bulletListStart :: T2T  Int
bulletListStart = genericListStart (char '-')

orderedListStart :: T2T Int
orderedListStart = genericListStart (char '+' )

definitionListStart :: T2T Int
definitionListStart = genericListStart (char ':')

-- parse raw text for one list item, excluding start marker and continuations
listItem :: T2T Int
         -> T2T a
         -> T2T a
listItem start end = try $ do
  markerLength <- try start
  firstLine <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  rest <- T.concat <$> many (listContinuation markerLength)
  parseFromString' end $ firstLine <> blank <> rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> T2T Text
listContinuation markerLength = try $
  notFollowedBy' (blankline >> blankline)
  *> (mappend <$> (T.concat <$> many1 listLine)
              <*> manyChar blankline)
 where listLine = try $ indentWith markerLength *> anyLineNewline

-- Table

table :: T2T Blocks
table = try $ do
  tableHeader <- fmap snd <$> option mempty (try headerRow)
  rows <- many1 (many commentLine *> tableRow)
  let columns = transpose rows
  let ncolumns = length columns
  let aligns = map (fromMaybe AlignDefault . foldr findAlign Nothing) columns
  let rows' = map (map snd) rows
  let size = maybe 0 maximum $ nonEmpty $ map length rows'
  let rowsPadded = map (pad size) rows'
  let headerPadded = if null tableHeader then mempty else pad size tableHeader
  let toRow = Row nullAttr . map B.simpleCell
      toHeaderRow l = [toRow l | not (null l)]
  return $ B.table B.emptyCaption
                    (zip aligns (replicate ncolumns ColWidthDefault))
                      (TableHead nullAttr $ toHeaderRow headerPadded)
                      [TableBody nullAttr 0 [] $ map toRow rowsPadded]
                      (TableFoot nullAttr [])

pad :: (Monoid a) => Int -> [a] -> [a]
pad n xs = xs ++ replicate (n - length xs) mempty


findAlign :: (Alignment, a) -> Maybe Alignment -> Maybe Alignment
findAlign (x,_) (Just y)
  | x == y = Just x
  | otherwise = Just AlignDefault
findAlign (x,_) Nothing = Just x

headerRow :: T2T [(Alignment, Blocks)]
headerRow = genericRow (string "||")

tableRow :: T2T [(Alignment, Blocks)]
tableRow = genericRow (char '|')

genericRow :: T2T a -> T2T [(Alignment, Blocks)]
genericRow start = try $ do
  spaces *> start
  manyTill tableCell newline <?> "genericRow"


tableCell :: T2T (Alignment, Blocks)
tableCell = try $ do
  leftSpaces <- length <$> lookAhead (many1 space) -- Case of empty cell means we must lookAhead
  content <- manyTill inline (try $ lookAhead cellEnd)
  rightSpaces <- length <$> many space
  let align =
        case compare leftSpaces rightSpaces of
              LT -> AlignLeft
              EQ -> AlignCenter
              GT -> AlignRight
  endOfCell
  return (align, B.plain (B.trimInlines $ mconcat content))
  where
    cellEnd = void newline <|> (many1 space *> endOfCell)

endOfCell :: T2T ()
endOfCell = try (skipMany1 $ char '|') <|> ( () <$ lookAhead newline)

-- Raw area

verbatim :: T2T Blocks
verbatim = genericBlock anyLineNewline B.codeBlock "```"

rawBlock :: T2T Blocks
rawBlock = genericBlock anyLineNewline (B.para . B.str) "\"\"\""

taggedBlock :: T2T Blocks
taggedBlock = do
  target <- getTarget
  genericBlock anyLineNewline (B.rawBlock target) "'''"

-- Generic

genericBlock :: Monoid a => T2T a -> (a -> Blocks) -> Text -> T2T Blocks
genericBlock p f s = blockMarkupArea p f s <|> blockMarkupLine p f s

blockMarkupArea :: Monoid a => T2T a -> (a -> Blocks) -> Text -> T2T Blocks
blockMarkupArea p f s = try (do
  textStr s *> blankline
  f . mconcat <$> manyTill p (eof <|> void (textStr s *> blankline)))

blockMarkupLine :: T2T a -> (a -> Blocks) -> Text -> T2T Blocks
blockMarkupLine p f s = try (f <$> (textStr s *> space *> p))

-- Can be in either block or inline position
comment :: Monoid a => T2T a
comment = try $ do
  atStart
  notFollowedBy macro
  mempty <$ (char '%' *> anyLine)

-- Inline

parseInlines :: T2T Inlines
parseInlines = trimInlines . mconcat <$> many1 inline

inline :: T2T Inlines
inline =
  choice
    [ endline
    , macro
    , commentLine
    , whitespace
    , url
    , link
    , image
    , bold
    , underline
    , code
    , raw
    , tagged
    , strike
    , italic
    , code
    , str
    , symbol
    ]

bold :: T2T Inlines
bold = inlineMarkup inline B.strong '*' B.str

underline :: T2T Inlines
underline = inlineMarkup inline B.underline '_' B.str

strike :: T2T Inlines
strike = inlineMarkup inline B.strikeout '-' B.str

italic :: T2T Inlines
italic = inlineMarkup inline B.emph '/' B.str

code :: T2T Inlines
code = inlineMarkup (T.singleton <$> anyChar) B.code '`' id

raw :: T2T Inlines
raw = inlineMarkup (T.singleton <$> anyChar) B.text '"' id

tagged :: T2T Inlines
tagged = do
  target <- getTarget
  inlineMarkup (T.singleton <$> anyChar) (B.rawInline target) '\'' id

-- Parser for markup indicated by a double character.
-- Inline markup is greedy and glued
-- Greedy meaning ***a*** = Bold [Str "*a*"]
-- Glued meaning that markup must be tight to content
-- Markup can't pass newlines
inlineMarkup :: Monoid a
             => T2T a -- Content parser
             -> (a -> Inlines) -- Constructor
             -> Char -- Fence
             -> (Text -> a) -- Special Case to handle ******
             -> T2T Inlines
inlineMarkup p f c special = try $ do
  start <- many1Char (char c)
  let l = T.length start
  guard (l >= 2)
  when (l == 2) (void $ notFollowedBy space)
  -- We must make sure that there is no space before the start of the
  -- closing tags
  body <-  optionMaybe (try $ manyTillChar (noneOf "\n\r")
                (try $ lookAhead (noneOf " " >> string [c,c] )))
  case body of
    Just middle -> do
      lastChar <- anyChar
      end <- many1Char (char c)
      let parser inp = parseFromString' (mconcat <$> many p) inp
      let start' = case T.drop 2 start of
                          "" -> mempty
                          xs -> special xs
      body' <- parser (middle <> T.singleton lastChar)
      let end' = case T.drop 2 end of
                          "" -> mempty
                          xs -> special xs
      return $ f (start' `mappend` body' `mappend` end')
    Nothing -> do -- Either bad or case such as *****
      guard (l >= 5)
      let body' = T.replicate (l - 4) $ T.singleton c
      return $ f (special body')

link :: T2T Inlines
link = try imageLink <|> titleLink

-- Link with title
titleLink :: T2T Inlines
titleLink = try $ do
  char '['
  notFollowedBy space
  tokens <- sepBy1 (manyChar $ noneOf " ]") space
  guard (length tokens >= 2)
  char ']'
  let link' = last tokens
  guard $ not $ T.null link'
  let tit = T.unwords (init tokens)
  return $ B.link link' "" (B.text tit)

-- Link with image
imageLink :: T2T Inlines
imageLink = try $ do
  char '['
  body <- image
  many1 space
  l <- manyTillChar (noneOf "\n\r ") (char ']')
  return (B.link l "" body)

macro :: T2T Inlines
macro = try $ do
  name <- string "%%" *> oneOfStringsCI (map fst commands)
  optional (try $ enclosed (char '(') (char ')') anyChar)
  lookAhead (spaceChar <|> oneOf specialChars <|> newline)
  maybe (return mempty) (\f -> asks (B.str . f)) (lookup name commands)
  where
    commands = [ ("date", date), ("mtime", mtime)
               , ("infile", T.pack . infile), ("outfile", T.pack . outfile)]

-- raw URLs in text are automatically linked
url :: T2T Inlines
url = try $ do
  (rawUrl, escapedUrl) <- try uri <|> emailAddress'
  return $ B.link rawUrl "" (B.str escapedUrl)

emailAddress' :: T2T (Text, Text)
emailAddress' = do
  (base, mailURI) <- emailAddress
  query <- option "" emailQuery
  return (base <> query, mailURI <> query)

emailQuery :: T2T Text
emailQuery = do
  char '?'
  parts <- kv `sepBy1` (char '&')
  return $ "?" <> T.intercalate "&" parts

kv :: T2T Text
kv = do
  k <- T.pack <$> many1 alphaNum
  char '='
  let vchar = alphaNum <|> try (oneOf "%._/~:,=$@&+-;*" <* lookAhead alphaNum)
  v <- T.pack <$> many1 vchar
  return (k <> "=" <> v)

uri :: T2T (Text, Text)
uri = try $ do
  address <- t2tURI
  return (address, escapeURI address)

-- The definition of a URI in the T2T source differs from the
-- actual definition. This is a transcription of the definition in
-- the source of v2.6
--isT2TURI :: String -> Bool
--isT2TURI (parse t2tURI "" -> Right _) = True
--isT2TURI _ = False

t2tURI :: T2T Text
t2tURI = do
  start <- try ((<>) <$> proto <*> urlLogin) <|> guess
  domain <- many1Char chars
  sep <- manyChar (char '/')
  form' <- option mempty (T.cons <$> char '?' <*> many1Char form)
  anchor' <- option mempty (T.cons <$> char '#' <*> manyChar anchor)
  return (start <> domain <> sep <> form' <> anchor')
  where
    protos = ["http", "https", "ftp", "telnet", "gopher", "wais"]
    proto = (<>) <$> oneOfStrings protos <*> textStr "://"
    guess = (<>) <$> (((<>) <$> stringAnyCase "www" <*> option mempty (T.singleton <$> oneOf "23"))
              <|> stringAnyCase "ftp") <*> (T.singleton <$> char '.')
    login = alphaNum <|> oneOf "_.-"
    pass = manyChar (noneOf " @")
    chars = alphaNum <|> oneOf "%._/~:,=$@&+-"
    anchor = alphaNum <|> oneOf "%._0"
    form = chars <|> oneOf ";*"
    urlLogin = option mempty $ try ((\x y z -> x <> y <> T.singleton z) <$> many1Char login <*> option mempty (T.cons <$> char ':' <*> pass) <*> char '@')


image :: T2T Inlines
image =  try $ do
  -- List taken from txt2tags source
  let extensions = [".jpg", ".jpeg", ".gif", ".png", ".eps", ".bmp"]
  char '['
  (path, ext) <- manyUntilChar (noneOf "\n\t\r ") (oneOfStrings extensions)
  char ']'
  return $ B.image (path <> ext) "" mempty

-- Characters used in markup
specialChars :: [Char]
specialChars = "%*-_/|:+;"

tab :: T2T Char
tab = char '\t'

space :: T2T Char
space = char ' '

spaces :: T2T Text
spaces = manyChar space

endline :: T2T Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy hrule
  notFollowedBy title
  notFollowedBy verbatim
  notFollowedBy rawBlock
  notFollowedBy taggedBlock
  notFollowedBy quote
  notFollowedBy list
  notFollowedBy table
  return B.softbreak

str :: T2T Inlines
str = try $ B.str <$> many1Char (noneOf $ specialChars ++ "\n\r ")

whitespace :: T2T Inlines
whitespace = try $ B.space <$ spaceChar

symbol :: T2T Inlines
symbol = B.str . T.singleton <$> oneOf specialChars

-- Utility

getTarget :: T2T Text
getTarget = do
  mv <- lookupMeta "target" . stateMeta <$> getState
  return $ case mv of
              Just (MetaString target)        -> target
              Just (MetaInlines [Str target]) -> target
              _                               -> "html"

atStart :: T2T ()
atStart = getPosition >>= guard . (== 1) . sourceColumn

ignoreSpacesCap :: T2T Text -> T2T Text
ignoreSpacesCap p = T.toLower <$> (spaces *> p <* spaces)

{-# LANGUAGE ViewPatterns #-}
{-
Copyright (C) 2014 Matthew Pickering <matthewtpickering@gmail.com>

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
   Module      : Text.Pandoc.Readers.Txt2Tags
   Copyright   : Copyright (C) 2014 Matthew Pickering
   License     : GNU GPL, version 2 or above

   Maintainer  : Matthew Pickering <matthewtpickering@gmail.com>

Conversion of txt2tags formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Txt2Tags ( readTxt2Tags
                                    , getT2TMeta
                                    , T2TMeta (..)
                                    , readTxt2TagsNoMacros)
                                    where

import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder ( Inlines, Blocks, trimInlines )
import Text.Pandoc.Compat.Monoid ((<>))
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (escapeURI,compactify', compactify'DL)
import Text.Pandoc.Parsing hiding (space, spaces, uri, macro)
import Data.Char (toLower)
import Data.List (transpose, intersperse, intercalate)
import Data.Maybe (fromMaybe)
--import Network.URI (isURI) -- Not sure whether to use this function
import Control.Monad (void, guard, when)
import Data.Default
import Control.Monad.Reader (Reader, runReader, asks)
import Text.Pandoc.Error

import Data.Time.LocalTime (getZonedTime)
import Text.Pandoc.Compat.Directory(getModificationTime)
import Data.Time.Format (formatTime)
import Text.Pandoc.Compat.Time (defaultTimeLocale)
import System.IO.Error (catchIOError)

type T2T = ParserT String ParserState (Reader T2TMeta)

-- | An object for the T2T macros meta information
-- the contents of each field is simply substituted verbatim into the file
data  T2TMeta = T2TMeta {
                 date :: String -- ^ Current date
               , mtime :: String -- ^ Last modification time of infile
               , infile :: FilePath -- ^ Input file
               , outfile :: FilePath -- ^ Output file
               } deriving Show

instance Default T2TMeta where
    def = T2TMeta "" "" "" ""

-- | Get the meta information required by Txt2Tags macros
getT2TMeta :: [FilePath] -> FilePath -> IO T2TMeta
getT2TMeta inps out = do
    curDate <- formatTime defaultTimeLocale "%F" <$> getZonedTime
    let getModTime = fmap (formatTime defaultTimeLocale "%T") .
                       getModificationTime
    curMtime <- case inps of
                  [] -> formatTime defaultTimeLocale "%T" <$> getZonedTime
                  _ -> catchIOError
                        (maximum <$> mapM getModTime inps)
                        (const (return ""))
    return $ T2TMeta curDate curMtime (intercalate ", " inps) out

-- | Read Txt2Tags from an input string returning a Pandoc document
readTxt2Tags :: T2TMeta -> ReaderOptions -> String -> Either PandocError Pandoc
readTxt2Tags t opts s = flip runReader t $ readWithM parseT2T (def {stateOptions = opts}) (s ++ "\n\n")

-- | Read Txt2Tags (ignoring all macros) from an input string returning
-- a Pandoc document
readTxt2TagsNoMacros :: ReaderOptions -> String -> Either PandocError Pandoc
readTxt2TagsNoMacros = readTxt2Tags def

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

headerline :: B.ToMetaValue a => String -> T2T a -> T2T ()
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

type Keyword = String
type Value = String

setting :: T2T (Keyword, Value)
setting = do
  string "%!"
  keyword <- ignoreSpacesCap (many1 alphaNum)
  char ':'
  value <- ignoreSpacesCap (manyTill anyChar (newline))
  return (keyword, value)

-- Blocks

parseBlocks :: T2T Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: T2T Blocks
block = do
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
  heading <- manyTill (noneOf "\n\r") (count level (char c))
  label <- optionMaybe (enclosed (char '[') (char ']') (alphaNum <|> oneOf "_-"))
  many spaceChar *> newline
  let attr = maybe nullAttr (\x -> (x, [], [])) label
  return $ B.headerWith attr level (trimInlines $ B.text heading)

para :: T2T Blocks
para = try $ do
  ils <- parseInlines
  nl <- option False (True <$ newline)
  option (B.plain ils) (guard nl >> notFollowedBy listStart >> return (B.para ils))
  where
    listStart = try bulletListStart <|> orderedListStart

commentBlock :: T2T Blocks
commentBlock = try (blockMarkupArea (anyLine) (const mempty) "%%%") <|> comment

-- Seperator and Strong line treated the same
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
  contents <- parseFromString parseBlocks (intercalate "\n" rawQuote ++ "\n\n")
  return $ B.blockQuote contents

commentLine :: T2T Inlines
commentLine = comment

-- List Parsing code from Org Reader

list :: T2T Blocks
list = choice [bulletList, orderedList, definitionList]

bulletList :: T2T Blocks
bulletList = B.bulletList . compactify'
             <$> many1 (listItem bulletListStart parseBlocks)

orderedList :: T2T Blocks
orderedList = B.orderedList . compactify'
              <$> many1 (listItem orderedListStart parseBlocks)

definitionList :: T2T Blocks
definitionList = try $ do
  B.definitionList . compactify'DL <$>
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
  rest <- concat <$> many (listContinuation markerLength)
  parseFromString end $ firstLine ++ blank ++ rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> T2T String
listContinuation markerLength = try $
  notFollowedBy' (blankline >> blankline)
  *> (mappend <$> (concat <$> many1 listLine)
              <*> many blankline)
 where listLine = try $ indentWith markerLength *> anyLineNewline

anyLineNewline :: T2T String
anyLineNewline = (++ "\n") <$> anyLine

indentWith :: Int -> T2T String
indentWith n = count n space

-- Table

table :: T2T Blocks
table = try $ do
  tableHeader <- fmap snd <$> option mempty (try headerRow)
  rows <- many1 (many commentLine *> tableRow)
  let columns = transpose rows
  let ncolumns = length columns
  let aligns = map (foldr1 findAlign) (map (map fst) columns)
  let rows' = map (map snd) rows
  let size = maximum (map length rows')
  let rowsPadded = map (pad size) rows'
  let headerPadded = if (not (null tableHeader)) then pad size tableHeader else mempty
  return $ B.table mempty
                    (zip aligns (replicate ncolumns 0.0))
                      headerPadded rowsPadded

pad :: (Show a, Monoid a) => Int -> [a] -> [a]
pad n xs = xs ++ (replicate (n - length xs) mempty)


findAlign :: Alignment -> Alignment -> Alignment
findAlign x y
  | x == y = x
  | otherwise = AlignDefault

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
  content <- (manyTill inline (try $ lookAhead (cellEnd)))
  rightSpaces <- length <$> many space
  let align =
        case compare leftSpaces rightSpaces of
              LT -> AlignLeft
              EQ -> AlignCenter
              GT -> AlignRight
  endOfCell
  return $ (align, B.plain (B.trimInlines $ mconcat content))
  where
    cellEnd = (void newline <|> (many1 space *> endOfCell))

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

genericBlock :: Monoid a => T2T a -> (a -> Blocks) -> String -> T2T Blocks
genericBlock p f s = blockMarkupArea p f s <|> blockMarkupLine p f s

blockMarkupArea :: Monoid a => (T2T a) -> (a -> Blocks) -> String -> T2T Blocks
blockMarkupArea p f s = try $ (do
  string s *> blankline
  f . mconcat <$> (manyTill p (eof <|> void (string s *> blankline))))

blockMarkupLine :: T2T a -> (a -> Blocks) -> String -> T2T Blocks
blockMarkupLine p f s = try (f <$> (string s *> space *> p))

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
inline = do
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
bold = inlineMarkup inline B.strong '*' (B.str)

underline :: T2T Inlines
underline = inlineMarkup inline B.emph '_' (B.str)

strike :: T2T Inlines
strike = inlineMarkup inline B.strikeout '-' (B.str)

italic :: T2T Inlines
italic = inlineMarkup inline B.emph '/' (B.str)

code :: T2T Inlines
code = inlineMarkup ((:[]) <$> anyChar) B.code '`' id

raw :: T2T Inlines
raw = inlineMarkup ((:[]) <$> anyChar) B.text '"' id

tagged :: T2T Inlines
tagged = do
  target <- getTarget
  inlineMarkup ((:[]) <$> anyChar) (B.rawInline target) '\'' id

-- Parser for markup indicated by a double character.
-- Inline markup is greedy and glued
-- Greedy meaning ***a*** = Bold [Str "*a*"]
-- Glued meaning that markup must be tight to content
-- Markup can't pass newlines
inlineMarkup :: Monoid a
             => (T2T a) -- Content parser
             -> (a -> Inlines) -- Constructor
             -> Char -- Fence
             -> (String -> a) -- Special Case to handle ******
             -> T2T Inlines
inlineMarkup p f c special = try $ do
  start <- many1 (char c)
  let l = length start
  guard (l >= 2)
  when (l == 2) (void $ notFollowedBy space)
  -- We must make sure that there is no space before the start of the
  -- closing tags
  body <-  optionMaybe (try $ manyTill (noneOf "\n\r") $
                (try $ lookAhead (noneOf " " >> string [c,c] )))
  case body of
    Just middle -> do
      lastChar <- anyChar
      end <- many1 (char c)
      let parser inp = parseFromString (mconcat <$> many p) inp
      let start' = special (drop 2 start)
      body' <- parser (middle ++ [lastChar])
      let end' = special (drop 2 end)
      return $ f (start' <> body' <> end')
    Nothing -> do -- Either bad or case such as *****
      guard (l >= 5)
      let body' = (replicate (l - 4) c)
      return $ f (special body')

link :: T2T Inlines
link = try imageLink <|> titleLink

-- Link with title
titleLink :: T2T Inlines
titleLink = try $ do
  char '['
  notFollowedBy space
  tokens <- sepBy1 (many $ noneOf " ]") space
  guard (length tokens >= 2)
  char ']'
  let link' = last tokens
  guard (length link' > 0)
  let tit = concat (intersperse " " (init tokens))
  return $ B.link link' "" (B.text tit)

-- Link with image
imageLink :: T2T Inlines
imageLink = try $ do
  char '['
  body <- image
  many1 space
  l <- manyTill (noneOf "\n\r ") (char ']')
  return (B.link l "" body)

macro :: T2T Inlines
macro = try $ do
  name <- string "%%" *> oneOfStringsCI (map fst commands)
  optional (try $ enclosed (char '(') (char ')') anyChar)
  lookAhead (spaceChar <|> oneOf specialChars <|> newline)
  maybe (return mempty) (\f -> B.str <$> asks f) (lookup name commands)
  where
    commands = [ ("date", date), ("mtime", mtime)
               , ("infile", infile), ("outfile", outfile)]

-- raw URLs in text are automatically linked
url :: T2T Inlines
url = try $ do
  (rawUrl, escapedUrl) <- (try uri <|> emailAddress)
  return $ B.link rawUrl "" (B.str escapedUrl)

uri :: T2T (String, String)
uri = try $ do
  address <- t2tURI
  return (address, escapeURI address)

-- The definition of a URI in the T2T source differs from the
-- actual definition. This is a transcription of the definition in
-- the source of v2.6
--isT2TURI :: String -> Bool
--isT2TURI (parse t2tURI "" -> Right _) = True
--isT2TURI _ = False

t2tURI :: T2T String
t2tURI = do
  start <- try ((++) <$> proto <*> urlLogin) <|> guess
  domain <- many1 chars
  sep <- many (char '/')
  form' <- option mempty ((:) <$> char '?' <*> many1 form)
  anchor' <- option mempty ((:) <$> char '#' <*> many anchor)
  return (start ++ domain ++ sep ++ form' ++ anchor')
  where
    protos = ["http", "https", "ftp", "telnet", "gopher", "wais"]
    proto = (++) <$> oneOfStrings protos <*> string "://"
    guess = (++) <$> (((++) <$> stringAnyCase "www" <*> option mempty ((:[]) <$> oneOf "23"))
              <|> stringAnyCase "ftp") <*> ((:[]) <$> char '.')
    login = alphaNum <|> oneOf "_.-"
    pass = many (noneOf " @")
    chars = alphaNum <|> oneOf "%._/~:,=$@&+-"
    anchor = alphaNum <|> oneOf "%._0"
    form = chars <|> oneOf ";*"
    urlLogin = option mempty $ try ((\x y z -> x ++ y ++ [z]) <$> many1 login <*> option mempty ((:) <$> char ':' <*> pass) <*> char '@')


image :: T2T Inlines
image =  try $ do
  -- List taken from txt2tags source
  let extensions = [".jpg", ".jpeg", ".gif", ".png", ".eps", ".bmp"]
  char '['
  path <- manyTill (noneOf "\n\t\r ") (try $ lookAhead (oneOfStrings extensions))
  ext <- oneOfStrings extensions
  char ']'
  return $ B.image (path ++ ext) "" mempty

-- Characters used in markup
specialChars :: String
specialChars = "%*-_/|:+;"

tab :: T2T Char
tab = char '\t'

space :: T2T Char
space = char ' '

spaces :: T2T String
spaces = many space

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
  return $ B.softbreak

str :: T2T Inlines
str = try $ do
  B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")

whitespace :: T2T Inlines
whitespace = try $ B.space <$ spaceChar

symbol :: T2T Inlines
symbol = B.str . (:[]) <$> oneOf specialChars

-- Utility

getTarget :: T2T String
getTarget = do
  mv <- lookupMeta "target" . stateMeta <$> getState
  let MetaString target = fromMaybe (MetaString "html") mv
  return target

atStart :: T2T ()
atStart = (sourceColumn <$> getPosition) >>= guard . (== 1)

ignoreSpacesCap :: T2T String -> T2T String
ignoreSpacesCap p = map toLower <$> (spaces *> p <* spaces)

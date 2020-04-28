{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Readers.DokuWiki
   Copyright   : Copyright (C) 2018-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of DokuWiki text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.DokuWiki (readDokuWiki) where

import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isAlphaNum, isDigit)
import qualified Data.Foldable as F
import Data.List (transpose)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, nested)
import Text.Pandoc.Shared (crFilter, trim, stringify, tshow)

-- | Read DokuWiki from an input string and return a Pandoc document.
readDokuWiki :: PandocMonad m
             => ReaderOptions
             -> Text
             -> m Pandoc
readDokuWiki opts s = do
  let input = crFilter s
  res <- runParserT parseDokuWiki def {stateOptions = opts } "source" input
  case res of
       Left e  -> throwError $ PandocParsecError input e
       Right d -> return d

type DWParser = ParserT Text ParserState

-- * Utility functions

-- | Parse end-of-line, which can be either a newline or end-of-file.
eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

nested :: PandocMonad m => DWParser m a -> DWParser m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

guardColumnOne :: PandocMonad m => DWParser m ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

-- | Parse DokuWiki document.
parseDokuWiki :: PandocMonad m => DWParser m Pandoc
parseDokuWiki =
  B.doc . mconcat <$> many block <* spaces <* eof

-- | Parse <code> and <file> attributes
codeLanguage :: PandocMonad m => DWParser m (Text, [Text], [(Text, Text)])
codeLanguage = try $ do
  rawLang <- option "-" (spaceChar *> manyTillChar anyChar (lookAhead (spaceChar <|> char '>')))
  let attr = case rawLang of
               "-" -> []
               l -> [l]
  return ("", attr, [])

-- | Generic parser for <code> and <file> tags
codeTag :: PandocMonad m
        => ((Text, [Text], [(Text, Text)]) -> Text -> a)
        -> Text
        -> DWParser m a
codeTag f tag = try $ f
  <$  char '<'
  <*  textStr tag
  <*> codeLanguage
  <*  manyTill anyChar (char '>')
  <*  optional (manyTill spaceChar eol)
  <*> manyTillChar anyChar (try $ string "</" <* textStr tag <* char '>')

-- * Inline parsers

-- | Parse any inline element but softbreak.
inline' :: PandocMonad m => DWParser m B.Inlines
inline' = whitespace
      <|> br
      <|> bold
      <|> italic
      <|> underlined
      <|> nowiki
      <|> percent
      <|> link
      <|> image
      <|> monospaced
      <|> subscript
      <|> superscript
      <|> deleted
      <|> footnote
      <|> inlineCode
      <|> inlineFile
      <|> inlineHtml
      <|> inlinePhp
      <|> autoLink
      <|> autoEmail
      <|> notoc
      <|> nocache
      <|> str
      <|> symbol
      <?> "inline"

-- | Parse any inline element, including soft break.
inline :: PandocMonad m => DWParser m B.Inlines
inline = endline <|> inline'

endline :: PandocMonad m => DWParser m B.Inlines
endline = try $ B.softbreak <$ skipMany spaceChar <* linebreak

whitespace :: PandocMonad m => DWParser m B.Inlines
whitespace = try $ B.space <$ skipMany1 spaceChar

br :: PandocMonad m => DWParser m B.Inlines
br = try $ B.linebreak <$ string "\\\\" <* space

linebreak :: PandocMonad m => DWParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = mempty <$ eof
        innerNewline = pure B.space

between :: (Monoid c, PandocMonad m, Show b)
        => DWParser m a -> DWParser m b -> (DWParser m b -> DWParser m c)
        -> DWParser m c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

enclosed :: (Monoid b, PandocMonad m, Show a)
         => DWParser m a -> (DWParser m a -> DWParser m b) -> DWParser m b
enclosed sep p = between sep (try sep) p

nestedInlines :: (Show a, PandocMonad m)
              => DWParser m a -> DWParser m B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* notFollowedBy end
    nestedInline = notFollowedBy whitespace >> nested inline

bold :: PandocMonad m => DWParser m B.Inlines
bold = try $ B.strong <$> enclosed (string "**") nestedInlines

italic :: PandocMonad m => DWParser m B.Inlines
italic = try $ B.emph <$> enclosed (string "//") nestedInlines

underlined :: PandocMonad m => DWParser m B.Inlines
underlined = try $ B.underline <$> enclosed (string "__") nestedInlines

nowiki :: PandocMonad m => DWParser m B.Inlines
nowiki = try $ B.text <$ string "<nowiki>" <*> manyTillChar anyChar (try $ string "</nowiki>")

percent :: PandocMonad m => DWParser m B.Inlines
percent = try $ B.text <$> enclosed (string "%%") nestedText

nestedText :: (Show a, PandocMonad m)
             => DWParser m a -> DWParser m Text
nestedText end = innerSpace <|> countChar 1 nonspaceChar
  where
    innerSpace = try $ many1Char spaceChar <* notFollowedBy end

monospaced :: PandocMonad m => DWParser m B.Inlines
monospaced = try $ B.code . (T.concat . map stringify . B.toList) <$> enclosed (string "''") nestedInlines

subscript :: PandocMonad m => DWParser m B.Inlines
subscript = try $ B.subscript <$> between (string "<sub>") (try $ string "</sub>") nestedInlines

superscript :: PandocMonad m => DWParser m B.Inlines
superscript = try $ B.superscript <$> between (string "<sup>") (try $ string "</sup>") nestedInlines

deleted :: PandocMonad m => DWParser m B.Inlines
deleted = try $ B.strikeout <$> between (string "<del>") (try $ string "</del>") nestedInlines

-- | Parse a footnote.
footnote :: PandocMonad m => DWParser m B.Inlines
footnote = try $ B.note . B.para <$> between (string "((") (try $ string "))") nestedInlines

inlineCode :: PandocMonad m => DWParser m B.Inlines
inlineCode = codeTag B.codeWith "code"

inlineFile :: PandocMonad m => DWParser m B.Inlines
inlineFile = codeTag B.codeWith "file"

inlineHtml :: PandocMonad m => DWParser m B.Inlines
inlineHtml = try $ B.rawInline "html" <$ string "<html>" <*> manyTillChar anyChar (try $ string "</html>")

inlinePhp :: PandocMonad m => DWParser m B.Inlines
inlinePhp = try $ B.codeWith ("", ["php"], []) <$ string "<php>" <*> manyTillChar anyChar (try $ string "</php>")

makeLink :: (Text, Text) -> B.Inlines
makeLink (text, url) = B.link url "" $ B.str text

autoEmail :: PandocMonad m => DWParser m B.Inlines
autoEmail = try $ do
  state <- getState
  guard $ stateAllowLinks state
  makeLink <$ char '<' <*> emailAddress <* char '>'

autoLink :: PandocMonad m => DWParser m B.Inlines
autoLink = try $ do
  state <- getState
  guard $ stateAllowLinks state
  (text, url) <- uri
  guard $ checkLink (T.last url)
  return $ makeLink (text, url)
  where
    checkLink c
      | c == '/' = True
      | otherwise = isAlphaNum c

notoc :: PandocMonad m => DWParser m B.Inlines
notoc = try $ mempty <$ string "~~NOTOC~~"

nocache :: PandocMonad m => DWParser m B.Inlines
nocache = try $ mempty <$ string "~~NOCACHE~~"

str :: PandocMonad m => DWParser m B.Inlines
str = B.str <$> (many1Char alphaNum <|> countChar 1 characterReference)

symbol :: PandocMonad m => DWParser m B.Inlines
symbol = B.str <$> countChar 1 nonspaceChar

link :: PandocMonad m => DWParser m B.Inlines
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  l <- linkText
  setState $ st{ stateAllowLinks = True }
  return l

isExternalLink :: Text -> Bool
isExternalLink s = "://" `T.isPrefixOf` sSuff
  where
    sSuff = T.dropWhile (\c -> isAlphaNum c || (c `elem` ['-', '.', '+'])) s

isAbsolutePath :: Text -> Bool
isAbsolutePath (T.uncons -> Just ('.', _)) = False
isAbsolutePath s = T.any (== ':') s

normalizeDots :: Text -> Text
normalizeDots path
  | not (T.null pref) = case T.uncons suff of
      Just (':', _) -> path
      _             -> pref <> ":" <> suff
  | otherwise = path
  where
    (pref, suff) = T.span (== '.') path

normalizeInternalPath :: Text -> Text
normalizeInternalPath path =
  if isAbsolutePath path
    then ensureAbsolute normalizedPath
    else normalizedPath
  where
    normalizedPath = T.intercalate "/" $ dropWhile (== ".") $ T.splitOn ":" $ normalizeDots path
    ensureAbsolute s@(T.uncons -> Just ('/', _)) = s
    ensureAbsolute s = "/" <> s

normalizePath :: Text -> Text
normalizePath path =
  if isExternalLink path
    then path
    else normalizeInternalPath path

urlToText :: Text -> Text
urlToText url =
  if isExternalLink url
    then url
    else T.takeWhileEnd (/= ':') url

-- Parse link or image
parseLink :: PandocMonad m
          => (Text -> Maybe B.Inlines -> B.Inlines)
          -> Text
          -> Text
          -> DWParser m B.Inlines
parseLink f l r = f
  <$  textStr l
  <*> many1TillChar anyChar (lookAhead (void (char '|') <|> try (void $ textStr r)))
  <*> optionMaybe (B.trimInlines . mconcat <$> (char '|' *> manyTill inline (try $ lookAhead $ textStr r)))
  <*  textStr r

-- | Split Interwiki link into left and right part
-- | Return Nothing if it is not Interwiki link
splitInterwiki :: Text -> Maybe (Text, Text)
splitInterwiki path =
  case T.span (\c -> isAlphaNum c || c == '.') path of
    (l, T.uncons -> Just ('>', r)) -> Just (l, r)
    _ -> Nothing

interwikiToUrl :: Text -> Text -> Text
interwikiToUrl "callto" page = "callto://" <> page
interwikiToUrl "doku" page = "https://www.dokuwiki.org/" <> page
interwikiToUrl "phpfn" page = "https://secure.php.net/" <> page
interwikiToUrl "tel" page = "tel:" <> page
interwikiToUrl "wp" page = "https://en.wikipedia.org/wiki/" <> page
interwikiToUrl "wpde" page = "https://de.wikipedia.org/wiki/" <> page
interwikiToUrl "wpes" page = "https://es.wikipedia.org/wiki/" <> page
interwikiToUrl "wpfr" page = "https://fr.wikipedia.org/wiki/" <> page
interwikiToUrl "wpjp" page = "https://jp.wikipedia.org/wiki/" <> page
interwikiToUrl "wppl" page = "https://pl.wikipedia.org/wiki/" <> page
interwikiToUrl _ page = "https://www.google.com/search?q=" <> page <> "&btnI=lucky"

linkText :: PandocMonad m => DWParser m B.Inlines
linkText = parseLink fromRaw "[[" "]]"
  where
    fromRaw path description =
      B.link normalizedPath "" (fromMaybe (B.str defaultDescription) description)
      where
        path' = trim path
        interwiki = splitInterwiki path'
        normalizedPath =
          case interwiki of
            Nothing -> normalizePath path'
            Just (l, r) -> interwikiToUrl l r
        defaultDescription =
          case interwiki of
            Nothing -> urlToText path'
            Just (_, r) -> r

-- Matches strings like "100x100" (width x height) and "50" (width)
isWidthHeightParameter :: Text -> Bool
isWidthHeightParameter s =
  case T.uncons s of
    Just (x, xs) ->
      isDigit x && case T.uncons $ T.dropWhile isDigit xs of
                     Just ('x', ys) | not (T.null ys) -> T.all isDigit ys
                     Nothing -> True
                     _ -> False
    _ -> False

parseWidthHeight :: Text -> (Maybe Text, Maybe Text)
parseWidthHeight s = (width, height)
  where
    width = Just $ T.takeWhile isDigit s
    height =
      case T.uncons $ T.dropWhile isDigit s of
        Just ('x', xs) -> Just xs
        _ -> Nothing

image :: PandocMonad m => DWParser m B.Inlines
image = try $ parseLink fromRaw "{{" "}}"
  where
    fromRaw path description =
      if linkOnly
        then B.link normalizedPath "" (fromMaybe defaultDescription description)
        else B.imageWith ("", classes, attributes) normalizedPath "" (fromMaybe defaultDescription description)
      where
        (path', parameters) = T.span (/= '?') $ trim path
        normalizedPath = normalizePath path'
        leftPadding = " " `T.isPrefixOf` path
        rightPadding = " " `T.isSuffixOf` path
        classes =
          case (leftPadding, rightPadding) of
            (False, False) -> []
            (False, True) -> ["align-left"]
            (True, False) -> ["align-right"]
            (True, True) -> ["align-center"]
        parameterList = T.splitOn "&" $ T.drop 1 parameters
        linkOnly = "linkonly" `elem` parameterList
        (width, height) = maybe (Nothing, Nothing) parseWidthHeight (F.find isWidthHeightParameter parameterList)
        attributes = catMaybes [fmap ("width",) width, fmap ("height",) height]
        defaultDescription = B.str $ urlToText path'

-- * Block parsers

block :: PandocMonad m => DWParser m B.Blocks
block = do
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  trace (T.take 60 $ tshow $ B.toList res)
  return res

blockElements :: PandocMonad m => DWParser m B.Blocks
blockElements = horizontalLine
            <|> header
            <|> list "  "
            <|> indentedCode
            <|> quote
            <|> blockCode
            <|> blockFile
            <|> blockHtml
            <|> blockPhp
            <|> table

horizontalLine :: PandocMonad m => DWParser m B.Blocks
horizontalLine = try $ B.horizontalRule <$ string "---" <* many1 (char '-') <* eol

header :: PandocMonad m => DWParser m B.Blocks
header = try $ do
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev < 7
  contents <- B.trimInlines . mconcat <$> manyTill inline (try $ char '=' *> many1 (char '='))
  attr <- registerHeader nullAttr contents
  return $ B.headerWith attr (7 - lev) contents

list :: PandocMonad m => Text -> DWParser m B.Blocks
list prefix = bulletList prefix <|> orderedList prefix

bulletList :: PandocMonad m => Text -> DWParser m B.Blocks
bulletList prefix = try $ B.bulletList <$> parseList prefix '*'

orderedList :: PandocMonad m => Text -> DWParser m B.Blocks
orderedList prefix = try $ B.orderedList <$> parseList prefix '-'

parseList :: PandocMonad m
          => Text
          -> Char
          -> DWParser m [B.Blocks]
parseList prefix marker =
  many1 ((<>) <$> item <*> fmap mconcat (many continuation))
  where
    continuation = try $ list ("  " <> prefix)
    item = try $ textStr prefix *> char marker *> char ' ' *> itemContents
    itemContents = B.plain . mconcat <$> many1Till inline' eol

indentedCode :: PandocMonad m => DWParser m B.Blocks
indentedCode = try $ B.codeBlock . T.unlines <$> many1 indentedLine
 where
   indentedLine = try $ string "  " *> manyTillChar anyChar eol

quote :: PandocMonad m => DWParser m B.Blocks
quote = try $ nestedQuote 0
  where
    prefix level = count level (char '>')
    contents level = nestedQuote level <|> quoteLine
    quoteLine = try $ B.plain . B.trimInlines . mconcat <$> many1Till inline' eol
    quoteContents level = (<>) <$> contents level <*> quoteContinuation level
    quoteContinuation level = mconcat <$> many (try $ prefix level *> contents level)
    nestedQuote level = B.blockQuote <$ char '>' <*> quoteContents (level + 1 :: Int)

blockHtml :: PandocMonad m => DWParser m B.Blocks
blockHtml = try $ B.rawBlock "html"
  <$  string "<HTML>"
  <*  optional (manyTill spaceChar eol)
  <*> manyTillChar anyChar (try $ string "</HTML>")

blockPhp :: PandocMonad m => DWParser m B.Blocks
blockPhp = try $ B.codeBlockWith ("", ["php"], [])
  <$  string "<PHP>"
  <*  optional (manyTill spaceChar eol)
  <*> manyTillChar anyChar (try $ string "</PHP>")

table :: PandocMonad m => DWParser m B.Blocks
table = do
  firstSeparator <- lookAhead tableCellSeparator
  rows <- tableRows
  let (headerRow, body) = if firstSeparator == '^'
                            then (head rows, tail rows)
                            else ([], rows)
  let attrs = (AlignDefault, ColWidthDefault) <$ transpose rows
  let toRow = Row nullAttr . map B.simpleCell
      toHeaderRow l = if null l then [] else [toRow l]
  pure $ B.table B.emptyCaption
                 attrs
                 (TableHead nullAttr $ toHeaderRow headerRow)
                 [TableBody nullAttr 0 [] $ map toRow body]
                 (TableFoot nullAttr [])

tableRows :: PandocMonad m => DWParser m [[B.Blocks]]
tableRows = many1 tableRow

tableRow :: PandocMonad m => DWParser m [B.Blocks]
tableRow = many1Till tableCell tableRowEnd

tableRowEnd :: PandocMonad m => DWParser m Char
tableRowEnd = try $ tableCellSeparator <* manyTill spaceChar eol

tableCellSeparator :: PandocMonad m => DWParser m Char
tableCellSeparator = char '|' <|> char '^'

tableCell :: PandocMonad m => DWParser m B.Blocks
tableCell = try $ B.plain . B.trimInlines . mconcat <$> (normalCell <|> headerCell)
  where
    normalCell = char '|' *> manyTill inline' (lookAhead tableCellSeparator)
    headerCell = char '^' *> manyTill inline' (lookAhead tableCellSeparator)

blockCode :: PandocMonad m => DWParser m B.Blocks
blockCode = codeTag B.codeBlockWith "code"

blockFile :: PandocMonad m => DWParser m B.Blocks
blockFile = codeTag B.codeBlockWith "file"

para :: PandocMonad m => DWParser m B.Blocks
para = result . mconcat <$> many1Till inline endOfParaElement
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> void blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

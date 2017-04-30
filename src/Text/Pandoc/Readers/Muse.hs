{-
  Copyright (C) 2017 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Readers.Muse
   Copyright   : Copyright (C) 2017 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of Muse text to 'Pandoc' document.
-}
{-
TODO:
- {{{ }}} syntax for <example>
- Page breaks (five "*")
- Headings with anchors (make it round trip with Muse writer)
- <verse> and ">"
- Lists
- Tables
- Images with attributes (floating and width)
- Anchors
- Citations and <biblio>
- <play> environment
- <verbatim> tag
-}
module Text.Pandoc.Readers.Muse (readMuse) where

import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.Map as M
import Data.Text (Text, unpack)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (macro, nested)
import Text.Pandoc.Readers.HTML (htmlTag)
import Text.Pandoc.XML (fromEntities)
import System.FilePath (takeExtension)

-- | Read Muse from an input string and return a Pandoc document.
readMuse :: PandocMonad m
         => ReaderOptions
         -> Text
         -> m Pandoc
readMuse opts s = do
  res <- readWithM parseMuse def{ stateOptions = opts } (unpack s)
  case res of
       Left e  -> throwError e
       Right d -> return d

type MuseParser = ParserT String ParserState

--
-- main parser
--

parseMuse :: PandocMonad m => MuseParser m Pandoc
parseMuse = do
  many directive
  blocks <- parseBlocks
  st <- getState
  let doc = runF (do Pandoc _ bs <- B.doc <$> blocks
                     meta <- stateMeta' st
                     return $ Pandoc meta bs) st
  reportLogMessages
  return doc

parseBlocks :: PandocMonad m => MuseParser m (F Blocks)
parseBlocks = do
  res <- mconcat <$> many block
  spaces
  eof
  return res

--
-- utility functions
--

nested :: PandocMonad m => MuseParser m a -> MuseParser m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

htmlElement :: PandocMonad m => String -> MuseParser m (Attr, String)
htmlElement tag = try $ do
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen tag [])
  content <- manyTill anyChar (endtag <|> endofinput)
  return (htmlAttrToPandoc attr, trim content)
  where
    endtag     = void $ htmlTag (~== TagClose tag)
    endofinput = lookAhead $ try $ skipMany blankline >> skipSpaces >> eof
    trim       = dropWhile (=='\n') . reverse . dropWhile (=='\n') . reverse

htmlAttrToPandoc :: [Attribute String] -> Attr
htmlAttrToPandoc attrs = (ident, classes, keyvals)
  where
    ident   = fromMaybe "" $ lookup "id" attrs
    classes = maybe [] words $ lookup "class" attrs
    keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]

parseHtmlContentWithAttrs :: PandocMonad m
                          => String -> MuseParser m a -> MuseParser m (Attr, [a])
parseHtmlContentWithAttrs tag parser = do
  (attr, content) <- htmlElement tag
  parsedContent <- try $ parseContent content
  return (attr, parsedContent)
  where
    parseContent = parseFromString $ nested $ manyTill parser endOfContent
    endOfContent = try $ skipMany blankline >> skipSpaces >> eof

parseHtmlContent :: PandocMonad m => String -> MuseParser m a -> MuseParser m [a]
parseHtmlContent tag p = liftM snd (parseHtmlContentWithAttrs tag p)

--
-- directive parsers
--

parseDirective :: PandocMonad m => MuseParser m (String, F Inlines)
parseDirective = do
  char '#'
  key <- many letter
  space
  spaces
  raw <- many $ noneOf "\n"
  newline
  value <- parseFromString (trimInlinesF . mconcat <$> many inline) raw
  return (key, value)

directive :: PandocMonad m => MuseParser m ()
directive = do
  (key, value) <- parseDirective
  updateState $ \st -> st { stateMeta' = B.setMeta key <$> value <*> stateMeta' st }

--
-- block parsers
--

block :: PandocMonad m => MuseParser m (F Blocks)
block = do
  pos <- getPosition
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  report $ ParsingTrace (take 60 $ show $ B.toList $ runF res defaultParserState) pos
  return res

blockElements :: PandocMonad m => MuseParser m (F Blocks)
blockElements = choice [ comment
                       , separator
                       , header
                       , exampleTag
                       , literal
                       , centerTag
                       , rightTag
                       , quoteTag
                       , commentTag
                       , noteBlock
                       ]

comment :: PandocMonad m => MuseParser m (F Blocks)
comment = try $ do
  char ';'
  space
  many $ noneOf "\n"
  void newline <|> eof
  return mempty

separator :: PandocMonad m => MuseParser m (F Blocks)
separator = try $ do
  string "---"
  newline
  return $ return B.horizontalRule

header :: PandocMonad m => MuseParser m (F Blocks)
header = try $ do
  level <- liftM length $ many1 $ char '*'
  guard $ level <= 5
  skipSpaces
  content <- trimInlinesF . mconcat <$> manyTill inline newline
  attr <- registerHeader ("", [], []) (runF content defaultParserState)
  return $ B.headerWith attr level <$> content

exampleTag :: PandocMonad m => MuseParser m (F Blocks)
exampleTag = liftM (return . uncurry B.codeBlockWith) $ htmlElement "example"

literal :: PandocMonad m => MuseParser m (F Blocks)
literal = liftM (return . rawBlock) $ htmlElement "literal"
  where
    format (_, _, kvs)        = fromMaybe "html" $ lookup "format" kvs
    rawBlock (attrs, content) = B.rawBlock (format attrs) content

blockTag :: PandocMonad m
          => (Blocks -> Blocks)
          -> String
          -> MuseParser m (F Blocks)
blockTag f s = do
  res <- parseHtmlContent s block
  return $ f <$> mconcat res

-- <center> tag is ignored
centerTag :: PandocMonad m => MuseParser m (F Blocks)
centerTag = blockTag id "center"

-- <right> tag is ignored
rightTag :: PandocMonad m => MuseParser m (F Blocks)
rightTag = blockTag id "right"

quoteTag :: PandocMonad m => MuseParser m (F Blocks)
quoteTag = blockTag B.blockQuote "quote"

commentTag :: PandocMonad m => MuseParser m (F Blocks)
commentTag = parseHtmlContent "comment" block >> return mempty

para :: PandocMonad m => MuseParser m (F Blocks)
para = do
 res <- trimInlinesF . mconcat <$> many1Till inline endOfParaElement
 return $ B.para <$> res
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> void blockElements

noteMarker :: PandocMonad m => MuseParser m String
noteMarker = try $ do
  char '['
  many1Till digit $ char ']'

noteBlock :: PandocMonad m => MuseParser m (F Blocks)
noteBlock = try $ do
  pos <- getPosition
  ref <- noteMarker <* skipSpaces
  content <- mconcat <$> blocksTillNote
  oldnotes <- stateNotes' <$> getState
  case M.lookup ref oldnotes of
    Just _ -> logMessage $ DuplicateNoteReference ref pos
    Nothing -> return ()
  updateState $ \s -> s{ stateNotes' = M.insert ref (pos, content) oldnotes }
  return mempty
  where
    blocksTillNote =
      many1Till block (eof <|> () <$ lookAhead noteMarker)
--
-- inline parsers
--

inline :: PandocMonad m => MuseParser m (F Inlines)
inline = choice [ whitespace
                , br
                , footnote
                , strong
                , strongTag
                , emph
                , emphTag
                , superscriptTag
                , subscriptTag
                , strikeoutTag
                , link
                , code
                , codeTag
                , str
                , symbol
                ] <?> "inline"

footnote :: PandocMonad m => MuseParser m (F Inlines)
footnote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF stateNotes'
    case M.lookup ref notes of
      Nothing -> return $ B.str $ "[" ++ ref ++ "]"
      Just (_pos, contents) -> do
        st <- askF
        let contents' = runF contents st { stateNotes' = M.empty }
        return $ B.note contents'

whitespace :: PandocMonad m => MuseParser m (F Inlines)
whitespace = liftM return (lb <|> regsp)
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

br :: PandocMonad m => MuseParser m (F Inlines)
br = try $ do
  string "<br>"
  return $ return B.linebreak

linebreak :: PandocMonad m => MuseParser m (F Inlines)
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = do
                         eof
                         return $ return mempty
        innerNewline = return $ return B.space

emphasisBetween :: (PandocMonad m, Show a) => MuseParser m a -> MuseParser m (F Inlines)
emphasisBetween c = try $ enclosedInlines c c

enclosedInlines :: (PandocMonad m, Show a, Show b)
                => MuseParser m a
                -> MuseParser m b
                -> MuseParser m (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> enclosed start end inline

verbatimBetween :: PandocMonad m
                => Char
                -> MuseParser m String
verbatimBetween c = try $ do
  char c
  many1Till anyChar $ char c

inlineTag :: PandocMonad m
          => (Inlines -> Inlines)
          -> String
          -> MuseParser m (F Inlines)
inlineTag f s = do
  res <- parseHtmlContent s inline
  return $ f <$> mconcat res

strongTag :: PandocMonad m => MuseParser m (F Inlines)
strongTag = inlineTag B.strong "strong"

strong :: PandocMonad m => MuseParser m (F Inlines)
strong = fmap B.strong <$> emphasisBetween (string "**")

emph :: PandocMonad m => MuseParser m (F Inlines)
emph = fmap B.emph <$> emphasisBetween (char '*')

emphTag :: PandocMonad m => MuseParser m (F Inlines)
emphTag = inlineTag B.emph "em"

superscriptTag :: PandocMonad m => MuseParser m (F Inlines)
superscriptTag = inlineTag B.superscript "sup"

subscriptTag :: PandocMonad m => MuseParser m (F Inlines)
subscriptTag = inlineTag B.subscript "sub"

strikeoutTag :: PandocMonad m => MuseParser m (F Inlines)
strikeoutTag = inlineTag B.strikeout "del"

code :: PandocMonad m => MuseParser m (F Inlines)
code = return . B.code <$> verbatimBetween '='

codeTag :: PandocMonad m => MuseParser m (F Inlines)
codeTag = do
  (attrs, content) <- parseHtmlContentWithAttrs "code" anyChar
  return $ return $ B.codeWith attrs $ fromEntities content

str :: PandocMonad m => MuseParser m (F Inlines)
str = liftM (return . B.str) (many1 alphaNum <|> count 1 characterReference)

symbol :: PandocMonad m => MuseParser m (F Inlines)
symbol = liftM (return . B.str) $ count 1 nonspaceChar

link :: PandocMonad m => MuseParser m (F Inlines)
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, content) <- linkText
  setState $ st{ stateAllowLinks = True }
  return $ case stripPrefix "URL:" url of
             Nothing -> if isImageUrl url
                          then B.image url title <$> fromMaybe (return mempty) content
                          else B.link url title <$> fromMaybe (return $ B.str url) content
             Just url' -> B.link url' title <$> fromMaybe (return $ B.str url') content
    where -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
          imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
          isImageUrl = (`elem` imageExtensions) . takeExtension

linkContent :: PandocMonad m => MuseParser m (F Inlines)
linkContent = do
  char '['
  res <- many1Till anyChar $ char ']'
  parseFromString (mconcat <$> many1 inline) res

linkText :: PandocMonad m => MuseParser m (String, String, Maybe (F Inlines))
linkText = do
  string "[["
  url <- many1Till anyChar $ char ']'
  content <- optionMaybe linkContent
  char ']'
  return (url, "", content)

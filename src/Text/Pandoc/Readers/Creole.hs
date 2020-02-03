{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Creole
   Copyright   : Copyright (C) 2017 Sascha Wilde
   License     : GNU GPL, version 2 or above

   Maintainer  : Sascha Wilde <wilde@sha-bang.de>
   Stability   : alpha
   Portability : portable

Conversion of creole text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Creole ( readCreole
                                  ) where

import Control.Monad.Except (guard, liftM2, throwError)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed)
import Text.Pandoc.Shared (crFilter)


-- | Read creole from an input string and return a Pandoc document.
readCreole :: PandocMonad m
          => ReaderOptions
          -> Text
          -> m Pandoc
readCreole opts s = do
  res <- readWithM parseCreole def{ stateOptions = opts } $ crFilter s <> "\n\n"
  case res of
       Left e  -> throwError e
       Right d -> return d

type CRLParser = ParserT Text ParserState

--
-- Utility functions
--

(<+>) :: (Monad m, Semigroup a) => m a -> m a -> m a
(<+>) = liftM2 (<>)

-- we have to redefine `enclosed' from Text.Pandoc.Parsing, because it
-- assumes, that there can't be a space after the start parser, but
-- with creole this is possible.
enclosed :: (Show end, PandocMonad m) => CRLParser m start   -- ^ start parser
         -> CRLParser m end  -- ^ end parser
         -> CRLParser m a    -- ^ content parser (to be used repeatedly)
         -> CRLParser m [a]
enclosed start end parser = try $ start >> many1Till parser end

--
-- main parser
--

specialChars :: [Char]
specialChars = "*/~{}\\|[]()<>\"'"

parseCreole :: PandocMonad m => CRLParser m Pandoc
parseCreole = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

--
-- block parsers
--

block :: PandocMonad m => CRLParser m B.Blocks
block = do
  res <- mempty <$ skipMany1 blankline
         <|> nowiki
         <|> header
         <|> horizontalRule
         <|> anyList 1
         <|> table
         <|> para
  skipMany blankline
  return res

nowiki :: PandocMonad m => CRLParser m B.Blocks
nowiki = try $ fmap (B.codeBlock . mconcat) (nowikiStart
                                             >> manyTill content nowikiEnd)
  where
    content = brackets <|> line
    brackets = try $ option "" (T.singleton <$> newline)
               <+> (char ' ' >> (manyChar (char ' ') <+> textStr "}}}") <* eol)
    line = option "" (T.singleton <$> newline) <+> manyTillChar anyChar eol
    eol = lookAhead $ try $ nowikiEnd <|> newline
    nowikiStart = optional newline >> string "{{{" >> skipMany spaceChar >> newline
    nowikiEnd = try $ linebreak >> string "}}}" >> skipMany spaceChar >> newline

header :: PandocMonad m => CRLParser m B.Blocks
header = try $ do
  skipSpaces
  level <-
    fmap length (many1 (char '='))
  guard $ level <= 6
  skipSpaces
  content <- B.str <$> manyTillChar (noneOf "\n") headerEnd
  return $ B.header level content
  where
    headerEnd = try $ skipSpaces >> many (char '=') >> skipSpaces >> newline

unorderedList :: PandocMonad m => Int -> CRLParser m B.Blocks
unorderedList = list '*' B.bulletList

orderedList :: PandocMonad m => Int -> CRLParser m B.Blocks
orderedList = list '#' B.orderedList

anyList :: PandocMonad m => Int -> CRLParser m B.Blocks
anyList n = unorderedList n <|> orderedList n

anyListItem :: PandocMonad m => Int -> CRLParser m B.Blocks
anyListItem n = listItem '*' n <|> listItem '#' n

list :: PandocMonad m => Char -> ([B.Blocks] -> B.Blocks) -> Int -> CRLParser m B.Blocks
list c f n =
  fmap f (many1 (itemPlusSublist <|> listItem c n))
  where itemPlusSublist = try $ listItem c n <+> anyList (n+1)

listItem :: PandocMonad m => Char -> Int -> CRLParser m B.Blocks
listItem c n =
  fmap (B.plain . B.trimInlines .mconcat) (listStart >> many1Till inline itemEnd)
  where
    listStart = try $ skipSpaces >> optional newline >> skipSpaces
                >> count n (char c)
                >> lookAhead (noneOf [c]) >> skipSpaces
    itemEnd = endOfParaElement <|> nextItem n
              <|> if n < 3 then nextItem (n+1)
                  else nextItem (n+1) <|> nextItem (n-1)
    nextItem x = lookAhead $ try $ blankline >> anyListItem x >> return mempty

table :: PandocMonad m => CRLParser m B.Blocks
table = try $ do
  headers <- optionMaybe headerRow
  rows <- many1 row
  return $ B.simpleTable (fromMaybe [mempty] headers) rows
  where
    headerRow = try $ skipSpaces >> many1Till headerCell rowEnd
    headerCell = B.plain . B.trimInlines . mconcat
                 <$> (string "|=" >> many1Till inline cellEnd)
    row = try $ skipSpaces >> many1Till cell rowEnd
    cell = B.plain . B.trimInlines . mconcat
           <$> (char '|' >> many1Till inline cellEnd)
    rowEnd = try $ optional (char '|') >> skipSpaces >> newline
    cellEnd = lookAhead $ try $ char '|' <|> rowEnd

para :: PandocMonad m => CRLParser m B.Blocks
para = fmap (result . mconcat) (many1Till inline endOfParaElement)
 where
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

endOfParaElement :: PandocMonad m => CRLParser m ()
endOfParaElement = lookAhead $ endOfInput <|> endOfPara
                   <|> startOfList <|> startOfTable
                   <|> startOfHeader <|> hr <|> startOfNowiki
  where
   endOfInput    = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara     = try $ blankline >> skipMany1 blankline
   startOf      :: PandocMonad m => CRLParser m a -> CRLParser m ()
   startOf p     = try $ blankline >> p >> return mempty
   startOfList   = startOf $ anyListItem 1
   startOfTable  = startOf table
   startOfHeader = startOf header
   startOfNowiki = startOf nowiki
   hr            = startOf horizontalRule

horizontalRule :: PandocMonad m => CRLParser m B.Blocks
horizontalRule = try $ skipSpaces >> string "----" >> skipSpaces >> newline
                 >> return B.horizontalRule

--
-- inline parsers
--

inline :: PandocMonad m => CRLParser m B.Inlines
inline = choice [ whitespace
                , escapedLink
                , escapedChar
                , link
                , inlineNowiki
                , placeholder
                , image
                , forcedLinebreak
                , bold
                , finalBold
                , italics
                , finalItalics
                , str
                , symbol
                ] <?> "inline"

escapedChar :: PandocMonad m => CRLParser m B.Inlines
escapedChar =
  fmap (B.str . T.singleton) (try $ char '~' >> noneOf "\t\n ")

escapedLink :: PandocMonad m => CRLParser m B.Inlines
escapedLink = try $ do
  char '~'
  (orig, _) <- uri
  return $ B.str orig

image :: PandocMonad m => CRLParser m B.Inlines
image = try $ do
  (orig, src) <- wikiImg
  return $ B.image src "" (B.str orig)
  where
    linkSrc = manyChar $ noneOf "|}\n\r\t"
    linkDsc = char '|' >> manyChar (noneOf "}\n\r\t")
    wikiImg = try $ do
      string "{{"
      src <- linkSrc
      dsc <- option "" linkDsc
      string "}}"
      return (dsc, src)

link :: PandocMonad m => CRLParser m B.Inlines
link = try $ do
  (orig, src) <- uriLink <|> wikiLink
  return $ B.link src "" orig
  where
    linkSrc = manyChar $ noneOf "|]\n\r\t"
    linkDsc :: PandocMonad m => Text -> CRLParser m B.Inlines
    linkDsc otxt = B.str
                   <$> try (option otxt
                         (char '|' >> manyChar (noneOf "]\n\r\t")))
    linkImg = try $ char '|' >> image
    wikiLink = try $ do
      string "[["
      src <- linkSrc
      dsc <- linkImg <|> linkDsc src
      string "]]"
      return (dsc, src)
    uriLink = try $ do
      (orig, src) <- uri
      return (B.str orig, src)

inlineNowiki :: PandocMonad m => CRLParser m B.Inlines
inlineNowiki = B.code <$> (start >> manyTillChar (noneOf "\n\r") end)
  where
    start = try $ string "{{{"
    end = try $ string "}}}" >> lookAhead (noneOf "}")

placeholder :: PandocMonad m => CRLParser m B.Inlines
-- The semantics of the placeholder is basicallly implementation
-- dependent, so there is no way to DTRT for all cases.
-- So for now we just drop them.
placeholder = B.text <$> try (string "<<<" >> manyTill anyChar (string ">>>")
              >> return "")

whitespace :: PandocMonad m => CRLParser m B.Inlines
whitespace = lb <|> regsp
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

linebreak :: PandocMonad m => CRLParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = eof >> return mempty
        innerNewline = return B.space

symbol :: PandocMonad m => CRLParser m B.Inlines
symbol = fmap (B.str . T.singleton) (oneOf specialChars)

str :: PandocMonad m => CRLParser m B.Inlines
str = let strChar = noneOf ("\t\n " ++ specialChars) in
        fmap B.str (many1Char strChar)

bold :: PandocMonad m => CRLParser m B.Inlines
bold = B.strong . mconcat <$>
       enclosed (string "**") (try $ string "**") inline

italics :: PandocMonad m => CRLParser m B.Inlines
italics = B.emph . mconcat <$>
          enclosed (string "//") (try $ string "//") inline

finalBold :: PandocMonad m => CRLParser m B.Inlines
finalBold = B.strong . mconcat <$>
            try (string "**" >> many1Till inline endOfParaElement)

finalItalics :: PandocMonad m => CRLParser m B.Inlines
finalItalics = B.emph . mconcat <$>
               try (string "//" >> many1Till inline endOfParaElement)

forcedLinebreak :: PandocMonad m => CRLParser m B.Inlines
forcedLinebreak = try $ string "\\\\" >> return B.linebreak

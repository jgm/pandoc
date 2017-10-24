{-
  Copyright (C) 2017 Sascha Wilde <wilde@sha-bang.de>
  heavily based on all the other readers, especialy the work by
  John MacFarlane <jgm@berkeley.edu> and
  Alexander Sulfrian <alexander.sulfrian@fu-berlin.de>
  all bugs are solely created by me.

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
   Module      : Text.Pandoc.Readers.Creole
   Copyright   : Copyright (C) 2017 Sascha Wilde
   License     : GNU GPL, version 2 or above

   Maintainer  : Sascha Wilde <wilde@sha-bang.de>
   Stability   : WIP
   Portability : portable

Conversion of creole text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Creole ( readCreole
                                  ) where

import Control.Monad.Except (liftM2, throwError, guard)
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed)
import Text.Pandoc.Shared (crFilter)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T


-- | Read creole from an input string and return a Pandoc document.
readCreole :: PandocMonad m
          => ReaderOptions
          -> Text
          -> m Pandoc
readCreole opts s = do
  res <- readWithM parseCreole def{ stateOptions = opts }
             (T.unpack (crFilter s) ++ "\n\n")
  case res of
       Left e  -> throwError e
       Right d -> return d

type CRLParser = ParserT [Char] ParserState

--
-- Utility funcitons
--

(<+>) :: (Monad m, Monoid a) => m a -> m a -> m a
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
specialChars = "*/~{}\\"

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
         <|> para
  skipMany blankline
  return res

nowiki :: PandocMonad m => CRLParser m B.Blocks
nowiki = try $ nowikiStart >> manyTill content nowikiEnd >>= return . B.codeBlock . mconcat
  where
    content = brackets <|> line
    brackets = try $ option "" ((:[]) <$> newline)
               <+> (char ' ' >> (many (char ' ') <+> string "}}}") <* eol)
    line = option "" ((:[]) <$> newline) <+> manyTill anyChar eol
    eol = lookAhead $ try $ nowikiEnd <|> newline
    nowikiStart = optional newline >> string "{{{" >> skipMany spaceChar >> newline
    nowikiEnd = try $ linebreak >> string "}}}" >> skipMany spaceChar >> newline

header :: PandocMonad m => CRLParser m B.Blocks
header = try $ do
  skipSpaces
  level <- many1 (char '=') >>= return . length
  guard $ level <= 6
  skipSpaces
  content <- B.str <$> manyTill (noneOf "\n") headerEnd
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
list c f n = many1 (itemPlusSublist <|> listItem c n)
             >>= return . f
  where itemPlusSublist = try $ listItem c n <+> anyList (n+1)

listItem :: PandocMonad m => Char -> Int -> CRLParser m B.Blocks
listItem c n = (listStart >> many1Till inline itemEnd)
               >>= return . B.plain . B.trimInlines .mconcat
  where
    listStart = try $ optional newline >> skipSpaces >> count n (char c)
                >> (lookAhead $ noneOf [c]) >> skipSpaces
    itemEnd = endOfParaElement <|> nextItem n
              <|> if n < 3 then nextItem (n+1)
                  else nextItem (n+1) <|> nextItem (n-1)
    nextItem x = lookAhead $ try $ blankline >> anyListItem x >> return mempty

para :: PandocMonad m => CRLParser m B.Blocks
para = many1Till inline endOfParaElement >>= return . result . mconcat
 where
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

endOfParaElement :: PandocMonad m => CRLParser m ()
endOfParaElement = lookAhead $ endOfInput <|> endOfPara
                   <|> startOfList <|> startOfHeader <|> hr <|> startOfNowiki
  where
   endOfInput    = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara     = try $ blankline >> skipMany1 blankline
   startOf      :: PandocMonad m => CRLParser m a -> CRLParser m ()
   startOf p     = try $ blankline >> p >> return mempty
   startOfList   = startOf $ anyList 1
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
escapedChar = (try $ char '~' >> noneOf "\t\n ") >>= return . B.str . (:[])

image :: PandocMonad m => CRLParser m B.Inlines
image = try $ do
  (orig, src) <- wikiImg
  return $ B.image src "" (B.str $ orig)
  where
    linkSrc = many $ noneOf "|}\n\r\t"
    linkDsc = char '|' >> many (noneOf "}\n\r\t")
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
    linkSrc = many $ noneOf "|]\n\r\t"
    linkDsc = B.str <$> (try $ option "" (char '|' >> many (noneOf "]\n\r\t")))
    linkImg = try $ char '|' >> image
    wikiLink = try $ do
      string "[["
      src <- linkSrc
      dsc <- linkImg <|> linkDsc
      string "]]"
      return (dsc, src)
    uriLink = try $ do
      (orig, src) <- uri
      return (B.str orig, src)

inlineNowiki :: PandocMonad m => CRLParser m B.Inlines
inlineNowiki = B.code <$> (start >> manyTill (noneOf "\n\r") end)
  where
    start = try $ string "{{{"
    end = try $ string "}}}" >> (lookAhead $ noneOf "}")

placeholder :: PandocMonad m => CRLParser m B.Inlines
-- The semantics of the placeholder is basicallly implementation
-- dependent, so there is no way to DTRT for all cases.
-- So for now we just drop them.
placeholder = B.text <$> (try $ string "<<<" >> manyTill anyChar (string ">>>")
              >> return "")

whitespace :: PandocMonad m => CRLParser m B.Inlines
whitespace = (lb <|> regsp) >>= return
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

linebreak :: PandocMonad m => CRLParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = eof >> return mempty
        innerNewline = return B.space

symbol :: PandocMonad m => CRLParser m B.Inlines
symbol = oneOf specialChars >>= return . B.str . (:[])

str :: PandocMonad m => CRLParser m B.Inlines
str = let strChar = noneOf ("\t\n " ++ specialChars) in
        many1 strChar >>= return . B.str

bold :: PandocMonad m => CRLParser m B.Inlines
bold = B.strong . B.trimInlines . mconcat <$>
       enclosed (string "**") (try $ string "**") inline

italics :: PandocMonad m => CRLParser m B.Inlines
italics = B.emph . B.trimInlines . mconcat <$>
          enclosed (string "//") (try $ string "//") inline

finalBold :: PandocMonad m => CRLParser m B.Inlines
finalBold = B.strong . B.trimInlines . mconcat <$>
            try (string "**" >> many1Till inline endOfParaElement)

finalItalics :: PandocMonad m => CRLParser m B.Inlines
finalItalics = B.emph . B.trimInlines . mconcat <$>
               try (string "//" >> many1Till inline endOfParaElement)

forcedLinebreak :: PandocMonad m => CRLParser m B.Inlines
forcedLinebreak = try $ string "\\\\" >> return B.linebreak

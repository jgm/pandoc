
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

import Control.Monad.Except (throwError)
import qualified Data.Foldable as F
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter)
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
-- main parser
--

specialChars :: [Char]
specialChars = "*/"

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
         <|> para
  skipMany blankline
  return res

para :: PandocMonad m => CRLParser m B.Blocks
para = many1Till inline endOfParaElement >>= return . result . mconcat
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content

--
-- inline parsers
--

inline :: PandocMonad m => CRLParser m B.Inlines
inline = choice [ whitespace
                , bold
                , italics
                , str
                , symbol
                ] <?> "inline"

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

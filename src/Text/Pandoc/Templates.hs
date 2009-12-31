{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.
Example:

> > renderTemplate [("name","Sam"),("salary","50,000")] $
>   "Hi, $name$.  $if(salary)$You make $$$salary$.$else$No salary data.$endif$" 
> > "Hi, John.  You make $50,000."

A slot for an interpolated variable is a variable name surrounded
by dollar signs.  To include a literal @$@ in your template, use
@$$@.  Variable names must begin with a letter and can contain letters,
numbers, @_@, and @-@.

A conditional begins with @$if(variable_name)$@ and ends with @$endif$@.
It may optionally contain an @$else$@ section.  The if section is
used if @variable_name@ has a non-null value, otherwise the else section
is used.
-}

module Text.Pandoc.Templates (renderTemplate, getDefaultTemplate) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import qualified Control.Exception as E (try, IOException)
import System.FilePath
import System.Directory
import Prelude hiding (readFile)
import System.IO.UTF8 (readFile)
import Paths_pandoc

-- | Get the default template, either from the application's user data
-- directory (~/.pandoc on unix) or from the cabal data directory.
getDefaultTemplate :: String -> IO (Either E.IOException String)
getDefaultTemplate format = do
  ut <- getTemplateFromUserDataDirectory format
  case ut of
       Right t -> return $ Right t
       Left _  -> getTemplateFromCabalDataDirectory format
 
getTemplateFromUserDataDirectory :: String -> IO (Either E.IOException String)
getTemplateFromUserDataDirectory format = E.try $ do
  userDir <- getAppUserDataDirectory "pandoc"
  let templatePath = userDir </> "templates" </> format <.> "template"
  readFile templatePath

getTemplateFromCabalDataDirectory :: String -> IO (Either E.IOException String)
getTemplateFromCabalDataDirectory format = E.try $ do 
  templatePath <- getDataFileName $ "templates" </> format <.> "template"
  readFile templatePath

-- | Renders a template 
renderTemplate :: [(String,String)]  -- ^ Assoc. list of values for variables
               -> String             -- ^ Template
               -> String
renderTemplate vals templ =
  case runParser (do x <- parseTemplate; eof; return x) vals "template" templ of
       Left e        -> show e
       Right r       -> concat r

reservedWords :: [String]
reservedWords = ["else","endif"]

parseTemplate :: GenParser Char [(String,String)] [String]
parseTemplate =
  many $ plaintext <|> escapedDollar <|> conditional <|> variable

plaintext :: GenParser Char [(String,String)] String
plaintext = many1 $ satisfy (/='$')

escapedDollar :: GenParser Char [(String,String)] String
escapedDollar = try $ string "$$" >> return "$"

conditional :: GenParser Char [(String,String)] String
conditional = try $ do
  string "$if("
  id' <- ident
  string ")$"
  skipMany (oneOf " \t")
  optional newline
  ifContents <- liftM concat parseTemplate
  elseContents <- option "" $ do try (string "$else$")
                                 skipMany (oneOf " \t")
                                 optional newline
                                 liftM concat parseTemplate
  string "$endif$"
  skipMany (oneOf " \t")
  optional newline
  st <- getState
  return $ case lookup id' st of
             Just ""  -> elseContents
             Just _   -> ifContents
             Nothing  -> elseContents

ident :: GenParser Char [(String,String)] String
ident = do
  first <- letter
  rest <- many (alphaNum <|> oneOf "_-")
  let id' = first : rest
  if id' `elem` reservedWords
     then pzero
     else return id'

variable :: GenParser Char [(String,String)] String
variable = try $ do
  char '$'
  id' <- ident
  char '$'
  st <- getState
  return $ case lookup id' st of
           Just val  -> val
           Nothing   -> ""

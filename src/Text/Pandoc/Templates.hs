{-# LANGUAGE TypeSynonymInstances #-}
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

The value of a variable will be indented to the same level as the
variable.

A conditional begins with @$if(variable_name)$@ and ends with @$endif$@.
It may optionally contain an @$else$@ section.  The if section is
used if @variable_name@ has a non-null value, otherwise the else section
is used.

Conditional keywords should not be indented, or unexpected spacing
problems may occur.
-}

module Text.Pandoc.Templates ( renderTemplate
                             , TemplateTarget
                             , getDefaultTemplate) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM, when)
import qualified Control.Exception as E (try, IOException)
import System.FilePath
import Text.Pandoc.Shared (readDataFile)
import Data.List (intercalate)
import Text.PrettyPrint (text, Doc)
import Text.XHtml (primHtml, Html)
import Data.ByteString.Lazy.UTF8 (ByteString, fromString)

-- | Get the default template, either from the application's user data
-- directory (~/.pandoc on unix) or from the cabal data directory.
getDefaultTemplate :: String -> IO (Either E.IOException String)
getDefaultTemplate "native" = return $ Right ""
getDefaultTemplate "s5" = getDefaultTemplate "html"
getDefaultTemplate "odt" = getDefaultTemplate "opendocument"
getDefaultTemplate format = do
  let format' = takeWhile (/='+') format  -- strip off "+lhs" if present
  E.try $ readDataFile $ "templates" </> format' <.> "template"

data TemplateState = TemplateState Int [(String,String)]

adjustPosition :: String -> GenParser Char TemplateState String
adjustPosition str = do
  let lastline = takeWhile (/= '\n') $ reverse str
  updateState $ \(TemplateState pos x) ->
    if str == lastline
       then TemplateState (pos + length lastline) x
       else TemplateState (length lastline) x
  return str

class TemplateTarget a where
  toTarget :: String -> a 

instance TemplateTarget String where
  toTarget = id

instance TemplateTarget ByteString where 
  toTarget = fromString

instance TemplateTarget Html where
  toTarget = primHtml

instance TemplateTarget Doc where
  toTarget = text

-- | Renders a template 
renderTemplate :: TemplateTarget a
               => [(String,String)]  -- ^ Assoc. list of values for variables
               -> String             -- ^ Template
               -> a
renderTemplate vals templ =
  case runParser (do x <- parseTemplate; eof; return x) (TemplateState 0 vals) "template" templ of
       Left e        -> error $ show e
       Right r       -> toTarget $ concat r

reservedWords :: [String]
reservedWords = ["else","endif"]

parseTemplate :: GenParser Char TemplateState [String]
parseTemplate =
  many $ (plaintext <|> escapedDollar <|> conditional <|> variable)
           >>= adjustPosition

plaintext :: GenParser Char TemplateState String
plaintext = many1 $ noneOf "$"

escapedDollar :: GenParser Char TemplateState String
escapedDollar = try $ string "$$" >> return "$"

conditional :: GenParser Char TemplateState String
conditional = try $ do
  let skipEndline = try $ skipMany (oneOf " \t") >> newline
  TemplateState pos vars <- getState
  string "$if("
  id' <- ident
  string ")$"
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- option False $ try $ skipEndline >> return True
  let conditionSatisfied = case lookup id' vars of
                                Nothing -> False
                                Just "" -> False
                                Just _  -> True
  contents <- if conditionSatisfied
                 then liftM concat parseTemplate
                 else do
                   parseTemplate  -- skip if part, then reset position
                   setState $ TemplateState pos vars
                   option "" $ do try (string "$else$")
                                  when multiline $ optional skipEndline
                                  liftM concat parseTemplate
  string "$endif$"
  when multiline $ optional skipEndline
  return contents

ident :: GenParser Char TemplateState String
ident = do
  first <- letter
  rest <- many (alphaNum <|> oneOf "_-")
  let id' = first : rest
  if id' `elem` reservedWords
     then pzero
     else return id'

variable :: GenParser Char TemplateState String
variable = try $ do
  char '$'
  id' <- ident
  char '$'
  TemplateState pos vars <- getState
  let indent = replicate pos ' '
  return $ case lookup id' vars of
             Just val  -> intercalate ('\n' : indent) $ lines val
             Nothing   -> ""

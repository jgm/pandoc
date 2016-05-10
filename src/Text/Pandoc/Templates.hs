{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, CPP,
    OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-
Copyright (C) 2009-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2009-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.
The following program illustrates its use:

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Text
> import Data.Aeson
> import Text.Pandoc.Templates
>
> data Employee = Employee { firstName :: String
>                          , lastName  :: String
>                          , salary    :: Maybe Int }
> instance ToJSON Employee where
>   toJSON e = object [ "name" .= object [ "first" .= firstName e
>                                        , "last"  .= lastName e ]
>                     , "salary" .= salary e ]
>
> employees :: [Employee]
> employees = [ Employee "John" "Doe" Nothing
>             , Employee "Omar" "Smith" (Just 30000)
>             , Employee "Sara" "Chen" (Just 60000) ]
>
> template :: Template
> template = either error id $ compileTemplate
>   "$for(employee)$Hi, $employee.name.first$. $if(employee.salary)$You make $employee.salary$.$else$No salary data.$endif$$sep$\n$endfor$"
>
> main = putStrLn $ renderTemplate template $ object ["employee" .= employees ]

A slot for an interpolated variable is a variable name surrounded
by dollar signs.  To include a literal @$@ in your template, use
@$$@.  Variable names must begin with a letter and can contain letters,
numbers, @_@, @-@, and @.@.

The values of variables are determined by a JSON object that is
passed as a parameter to @renderTemplate@.  So, for example,
@title@ will return the value of the @title@ field, and
@employee.salary@ will return the value of the @salary@ field
of the object that is the value of the @employee@ field.

The value of a variable will be indented to the same level as the
variable.

A conditional begins with @$if(variable_name)$@ and ends with @$endif$@.
It may optionally contain an @$else$@ section.  The if section is
used if @variable_name@ has a non-null value, otherwise the else section
is used.

Conditional keywords should not be indented, or unexpected spacing
problems may occur.

The @$for$@ keyword can be used to iterate over an array.  If
the value of the associated variable is not an array, a single
iteration will be performed on its value.

You may optionally specify separators using @$sep$@, as in the
example above.

-}

module Text.Pandoc.Templates ( renderTemplate
                             , renderTemplate'
                             , TemplateTarget(..)
                             , varListToJSON
                             , compileTemplate
                             , Template
                             , getDefaultTemplate ) where

import Data.Char (isAlphaNum)
import Control.Monad (guard, when)
import Data.Aeson (ToJSON(..), Value(..))
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Text.Pandoc.Compat.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.List (intersperse)
import System.FilePath ((</>), (<.>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Foldable (toList)
import qualified Control.Exception.Extensible as E (try, IOException)
#if MIN_VERSION_blaze_html(0,5,0)
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (preEscapedText)
#else
import Text.Blaze (preEscapedText, Html)
#endif
import Data.ByteString.Lazy (ByteString, fromChunks)
import Text.Pandoc.Shared (readDataFileUTF8, ordNub)
import Data.Vector ((!?))
import Control.Applicative (many, (<|>))

-- | Get default template for the specified writer.
getDefaultTemplate :: (Maybe FilePath) -- ^ User data directory to search first
                   -> String           -- ^ Name of writer
                   -> IO (Either E.IOException String)
getDefaultTemplate user writer = do
  let format = takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
  case format of
       "native" -> return $ Right ""
       "json"   -> return $ Right ""
       "docx"   -> return $ Right ""
       "fb2"    -> return $ Right ""
       "odt"    -> getDefaultTemplate user "opendocument"
       "markdown_strict"   -> getDefaultTemplate user "markdown"
       "multimarkdown"     -> getDefaultTemplate user "markdown"
       "markdown_github"   -> getDefaultTemplate user "markdown"
       "markdown_mmd"      -> getDefaultTemplate user "markdown"
       "markdown_phpextra" -> getDefaultTemplate user "markdown"
       _        -> let fname = "templates" </> "default" <.> format
                   in  E.try $ readDataFileUTF8 user fname

newtype Template = Template { unTemplate :: Value -> Text }
                 deriving Monoid

type Variable = [Text]

class TemplateTarget a where
  toTarget :: Text -> a

instance TemplateTarget Text where
  toTarget = id

instance TemplateTarget String where
  toTarget = T.unpack

instance TemplateTarget ByteString where
  toTarget = fromChunks . (:[]) . encodeUtf8

instance TemplateTarget Html where
  toTarget = preEscapedText

varListToJSON :: [(String, String)] -> Value
varListToJSON assoc = toJSON $ M.fromList assoc'
  where assoc' = [(T.pack k, toVal [T.pack z | (y,z) <- assoc,
                                                not (null z),
                                                y == k])
                        | k <- ordNub $ map fst assoc ]
        toVal [x] = toJSON x
        toVal []  = Null
        toVal xs  = toJSON xs

renderTemplate :: (ToJSON a, TemplateTarget b) => Template -> a -> b
renderTemplate (Template f) context = toTarget $ f $ toJSON context

compileTemplate :: Text -> Either String Template
compileTemplate template =
  case P.parse (pTemplate <* P.eof) "template" template of
       Left e   -> Left (show e)
       Right x  -> Right x

-- | Like 'renderTemplate', but compiles the template first,
-- raising an error if compilation fails.
renderTemplate' :: (ToJSON a, TemplateTarget b) => String -> a -> b
renderTemplate' template =
  renderTemplate (either error id $ compileTemplate $ T.pack template)

var :: Variable -> Template
var = Template . resolveVar

resolveVar :: Variable -> Value -> Text
resolveVar var' val =
  case multiLookup var' val of
       Just (Array vec) -> maybe mempty (resolveVar []) $ vec !? 0
       Just (String t)  -> T.stripEnd t
       Just (Number n)  -> T.pack $ show n
       Just (Bool True) -> "true"
       Just (Object _)  -> "true"
       Just _           -> mempty
       Nothing          -> mempty

multiLookup :: [Text] -> Value -> Maybe Value
multiLookup [] x = Just x
multiLookup (v:vs) (Object o) = H.lookup v o >>= multiLookup vs
multiLookup _ _ = Nothing

lit :: Text -> Template
lit = Template . const

cond :: Variable -> Template -> Template -> Template
cond var' (Template ifyes) (Template ifno) = Template $ \val ->
  case resolveVar var' val of
       "" -> ifno val
       _  -> ifyes val

iter :: Variable -> Template -> Template -> Template
iter var' template sep = Template $ \val -> unTemplate
  (case multiLookup var' val of
           Just (Array vec) -> mconcat $ intersperse sep
                                       $ map (setVar template var')
                                       $ toList vec
           Just x           -> cond var' (setVar template var' x) mempty
           Nothing          -> mempty) val

setVar :: Template -> Variable -> Value -> Template
setVar (Template f) var' val = Template $ f . replaceVar var' val

replaceVar :: Variable -> Value -> Value -> Value
replaceVar []     new _          = new
replaceVar (v:vs) new (Object o) =
  Object $ H.adjust (\x -> replaceVar vs new x) v o
replaceVar _ _ old = old

--- parsing

pTemplate :: Parser Template
pTemplate = do
  sp <- P.option mempty pInitialSpace
  rest <- mconcat <$> many (pConditional <|>
                            pFor <|>
                            pNewline <|>
                            pVar <|>
                            pLit <|>
                            pEscapedDollar)
  return $ sp <> rest

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 f = T.pack <$> P.many1 (P.satisfy f)

pLit :: Parser Template
pLit = lit <$> takeWhile1 (\x -> x /='$' && x /= '\n')

pNewline :: Parser Template
pNewline = do
  P.char '\n'
  sp <- P.option mempty pInitialSpace
  return $ lit "\n" <> sp

pInitialSpace :: Parser Template
pInitialSpace = do
  sps <- takeWhile1 (==' ')
  let indentVar = if T.null sps
                     then id
                     else indent (T.length sps)
  v <- P.option mempty $ indentVar <$> pVar
  return $ lit sps <> v

pEscapedDollar :: Parser Template
pEscapedDollar = lit "$" <$ P.try (P.string "$$")

pVar :: Parser Template
pVar = var <$> (P.try $ P.char '$' *> pIdent <* P.char '$')

pIdent :: Parser [Text]
pIdent = do
  first <- pIdentPart
  rest <- many (P.char '.' *> pIdentPart)
  return (first:rest)

pIdentPart :: Parser Text
pIdentPart = P.try $ do
  first <- P.letter
  rest <- T.pack <$> P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  let id' = T.singleton first <> rest
  guard $ id' `notElem` reservedWords
  return id'

reservedWords :: [Text]
reservedWords = ["else","endif","for","endfor","sep"]

skipEndline :: Parser ()
skipEndline = P.try $ P.skipMany (P.satisfy (`elem` (" \t" :: String))) >> P.char '\n' >> return ()

pConditional :: Parser Template
pConditional = do
  P.try $ P.string "$if("
  id' <- pIdent
  P.string ")$"
  -- if newline after the "if", then a newline after "endif" will be swallowed
  multiline <- P.option False (True <$ skipEndline)
  ifContents <- pTemplate
  elseContents <- P.option mempty $ P.try $
                      do P.string "$else$"
                         when multiline $ P.option () skipEndline
                         pTemplate
  P.string "$endif$"
  when multiline $ P.option () skipEndline
  return $ cond id' ifContents elseContents

pFor :: Parser Template
pFor = do
  P.try $ P.string "$for("
  id' <- pIdent
  P.string ")$"
  -- if newline after the "for", then a newline after "endfor" will be swallowed
  multiline <- P.option False $ skipEndline >> return True
  contents <- pTemplate
  sep <- P.option mempty $
           do P.try $ P.string "$sep$"
              when multiline $ P.option () skipEndline
              pTemplate
  P.string "$endfor$"
  when multiline $ P.option () skipEndline
  return $ iter id' contents sep

indent :: Int -> Template -> Template
indent 0   x            = x
indent ind (Template f) = Template $ \val -> indent' (f val)
  where indent' t = T.concat
                    $ intersperse ("\n" <> T.replicate ind " ") $ T.lines t

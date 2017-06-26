{-
Copyright (C) 2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.BCP47
   Copyright   : Copyright (C) 2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for parsing and rendering BCP47 language identifiers.
-}
module Text.Pandoc.BCP47 (
                       getLang
                     , toLang
                     , parseBCP47
                     , Lang(..)
                     , renderLang
                     )
where
import Control.Monad (guard)
import Data.Char (isAscii, isLetter, isUpper, isLower, toUpper, toLower,
                  isAlphaNum)
import Data.List (intercalate)
import Text.Pandoc.Definition
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import qualified Text.Parsec as P

-- | Represents BCP 47 language/country code.
data Lang = Lang{ langLanguage   :: String
                , langScript     :: String
                , langRegion     :: String
                , langVariants   :: [String] }
                deriving (Eq, Ord, Show)

-- | Render a Lang as BCP 47.
renderLang :: Lang -> String
renderLang lang = intercalate "-" (langLanguage lang : filter (not . null)
                    ([langScript lang, langRegion lang] ++ langVariants lang))

-- | Get the contents of the `lang` metadata field or variable.
getLang :: WriterOptions -> Meta -> Maybe String
getLang opts meta =
  case lookup "lang" (writerVariables opts) of
        Just s -> Just s
        _      ->
          case lookupMeta "lang" meta of
               Just (MetaInlines [Str s]) -> Just s
               Just (MetaString s)        -> Just s
               _                          -> Nothing

-- | Convert BCP47 string to a Lang, issuing warning
-- if there are problems.
toLang :: PandocMonad m => Maybe String -> m (Maybe Lang)
toLang Nothing = return Nothing
toLang (Just s) =
  case parseBCP47 s of
       Left _ -> do
         report $ InvalidLang s
         return Nothing
       Right l -> return (Just l)

-- | Parse a BCP 47 string as a Lang.  Currently we parse
-- extensions and private-use fields as "variants," even
-- though officially they aren't.
parseBCP47 :: String -> Either String Lang
parseBCP47 lang =
  case P.parse bcp47 "lang" lang of
       Right r -> Right r
       Left e  -> Left $ show e
  where bcp47 = do
          language <- pLanguage
          script <- P.option "" pScript
          region <- P.option "" pRegion
          variants <- P.many (pVariant P.<|> pExtension P.<|> pPrivateUse)
          P.eof
          return $ Lang{ langLanguage = language
                       , langScript = script
                       , langRegion = region
                       , langVariants = variants }
        asciiLetter = P.satisfy (\c -> isAscii c && isLetter c)
        pLanguage = do
          cs <- P.many1 asciiLetter
          let lcs = length cs
          guard $ lcs == 2 || lcs == 3
          return $ map toLower cs
        pScript = P.try $ do
          P.char '-'
          x <- P.satisfy (\c -> isAscii c && isLetter c && isUpper c)
          xs <- P.count 3
                 (P.satisfy (\c -> isAscii c && isLetter c && isLower c))
          return $ map toLower (x:xs)
        pRegion = P.try $ do
          P.char '-'
          cs <- P.many1 asciiLetter
          let lcs = length cs
          guard $ lcs == 2 || lcs == 3
          return $ map toUpper cs
        pVariant = P.try $ do
          P.char '-'
          ds <- P.option "" (P.count 1 P.digit)
          cs <- P.many1 asciiLetter
          let var = ds ++ cs
          guard $ if null ds
                     then length var >= 5 && length var <= 8
                     else length var == 4
          return $ map toLower var
        pExtension = P.try $ do
          P.char '-'
          cs <- P.many1 $ P.satisfy (\c -> isAscii c && isAlphaNum c)
          guard $ length cs >= 2 && length cs <= 8
          return $ map toLower cs
        pPrivateUse = P.try $ do
          P.char '-'
          P.char 'x'
          P.char '-'
          cs <- P.many1 $ P.satisfy (\c -> isAscii c && isAlphaNum c)
          guard $ length cs >= 1 && length cs <= 8
          let var = "x-" ++ cs
          return $ map toLower var

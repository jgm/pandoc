{- |
Module      : Text.Pandoc.CSS
Copyright   : © 2006-2021 John MacFarlane <jgm@berkeley.edu>,
                2015-2016 Mauro Bieg,
                2015      Ophir Lifshitz <hangfromthefloor@gmail.com>
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Tools for working with CSS.
-}
module Text.Pandoc.CSS
  ( cssAttributes
  , pickStyleAttrProps
  , pickStylesToKVs
  )
where

import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text, pack)
import Text.Pandoc.Shared (trim)
import Text.Parsec
import Text.Parsec.Text

ruleParser :: Parser (Text, Text)
ruleParser = do
    p <- many1 (noneOf ":")  <* char ':'
    v <- many1 (noneOf ":;") <* optional (char ';') <* spaces
    return (trim $ pack p, trim $ pack v)

styleAttrParser :: Parser [(Text, Text)]
styleAttrParser = many1 ruleParser

-- | Parses a style string, returning the CSS attributes.
-- Returns an empty list on failure.
cssAttributes :: Text -> [(Text, Text)]
cssAttributes styleString =
  -- Use Data.Either.fromRight once GHC 8.0 is no longer supported
  case parse styleAttrParser "" styleString of
    Left _  -> []
    Right x -> x

-- | takes a list of keys/properties and a CSS string and
-- returns the corresponding key-value-pairs.
pickStylesToKVs :: [Text] -> Text -> [(Text, Text)]
pickStylesToKVs props styleAttr =
  filter (\s -> fst s `elem` props) $ cssAttributes styleAttr

-- | takes a list of key/property synonyms and a CSS string and maybe
-- returns the value of the first match (in order of the supplied list)
pickStyleAttrProps :: [Text] -> Text -> Maybe Text
pickStyleAttrProps lookupProps styleAttr = do
    styles <- either (const Nothing) Just $ parse styleAttrParser "" styleAttr
    listToMaybe $ mapMaybe (`lookup` styles) lookupProps

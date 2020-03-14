{- |
Module      : Text.Pandoc.CSS
Copyright   : © 2006-2020 John MacFarlane <jgm@berkeley.edu>,
                2015-2016 Mauro Bieg,
                2015      Ophir Lifshitz <hangfromthefloor@gmail.com>
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Tools for working with CSS.
-}
module Text.Pandoc.CSS ( foldOrElse
                       , pickStyleAttrProps
                       , pickStylesToKVs
                       )
where

import qualified Data.Text as T
import Text.Pandoc.Shared (trim)
import Text.Parsec
import Text.Parsec.Text

ruleParser :: Parser (T.Text, T.Text)
ruleParser = do
    p <- many1 (noneOf ":")  <* char ':'
    v <- many1 (noneOf ":;") <* optional (char ';') <* spaces
    return (trim $ T.pack p, trim $ T.pack v)

styleAttrParser :: Parser [(T.Text, T.Text)]
styleAttrParser = many1 ruleParser

orElse :: Eq a => a -> a -> a -> a
orElse v x y = if v == x then y else x

foldOrElse :: Eq a => a -> [a] -> a
foldOrElse v xs = foldr (orElse v) v xs

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe _         = Nothing

-- | takes a list of keys/properties and a CSS string and
-- returns the corresponding key-value-pairs.
pickStylesToKVs :: [T.Text] -> T.Text -> [(T.Text, T.Text)]
pickStylesToKVs props styleAttr =
  case parse styleAttrParser "" styleAttr of
    Left _       -> []
    Right styles -> filter (\s -> fst s `elem` props) styles

-- | takes a list of key/property synonyms and a CSS string and maybe
-- returns the value of the first match (in order of the supplied list)
pickStyleAttrProps :: [T.Text] -> T.Text -> Maybe T.Text
pickStyleAttrProps lookupProps styleAttr = do
    styles <- eitherToMaybe $ parse styleAttrParser "" styleAttr
    foldOrElse Nothing $ map (`lookup` styles) lookupProps

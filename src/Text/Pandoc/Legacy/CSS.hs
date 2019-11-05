module Text.Pandoc.Legacy.CSS ( TP.foldOrElse
                              , pickStyleAttrProps
                              , pickStylesToKVs
                              )
where

import qualified Data.Text as T
import qualified Text.Pandoc.CSS as TP

pickStylesToKVs :: [String] -> String -> [(String, String)]
pickStylesToKVs x = fmap go . TP.pickStylesToKVs (map T.pack x) . T.pack
  where
    go (a, b) = (T.unpack a, T.unpack b)

pickStyleAttrProps :: [String] -> String -> Maybe String
pickStyleAttrProps x = fmap T.unpack . TP.pickStyleAttrProps (map T.pack x) . T.pack


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Text.Pandoc.Legacy.BCP47 (
                       getLang
                     , parseBCP47
                     , TP.Lang
                     , pattern Lang
                     , langLanguage
                     , langScript
                     , langRegion
                     , langVariants    
                     , renderLang
                     )
where

import qualified Text.Pandoc.BCP47 as TP
import qualified Data.Text as T
import qualified Text.Pandoc.Options as TP
import qualified Text.Pandoc.Definition as TP

pattern Lang :: String -> String -> String -> [String] -> TP.Lang
pattern Lang
  { langLanguage
  , langScript
  , langRegion
  , langVariants
  } <- TP.Lang (T.unpack -> langLanguage)
               (T.unpack -> langScript)
               (T.unpack -> langRegion)
               (map T.unpack -> langVariants)
  where
    Lang x y z = TP.Lang (T.pack x) (T.pack y) (T.pack z) . map T.pack
  
renderLang :: TP.Lang -> String
renderLang = T.unpack . TP.renderLang

getLang :: TP.WriterOptions -> TP.Meta -> Maybe String
getLang opts = fmap T.unpack . TP.getLang opts

parseBCP47 :: String -> Either String TP.Lang
parseBCP47 = either (Left . T.unpack) Right . TP.parseBCP47 . T.pack

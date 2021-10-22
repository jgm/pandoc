{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc.Util
 ( splitStrWhen
 , toIETF )
where
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Definition

-- Split Str elements so that characters satisfying the
-- predicate each have their own Str.
splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen p = foldr go []
 where
  go (Str t) = (map Str (T.groupBy goesTogether t) ++)
  go x = (x :)
  goesTogether c d   = not (p c || p d)

toIETF :: Text -> Text
toIETF "english"         = "en-US" -- "en-EN" unavailable in CSL
toIETF "usenglish"       = "en-US"
toIETF "american"        = "en-US"
toIETF "british"         = "en-GB"
toIETF "ukenglish"       = "en-GB"
toIETF "canadian"        = "en-US" -- "en-CA" unavailable in CSL
toIETF "australian"      = "en-GB" -- "en-AU" unavailable in CSL
toIETF "newzealand"      = "en-GB" -- "en-NZ" unavailable in CSL
toIETF "afrikaans"       = "af-ZA"
toIETF "arabic"          = "ar"
toIETF "basque"          = "eu"
toIETF "bulgarian"       = "bg-BG"
toIETF "catalan"         = "ca-AD"
toIETF "croatian"        = "hr-HR"
toIETF "czech"           = "cs-CZ"
toIETF "danish"          = "da-DK"
toIETF "dutch"           = "nl-NL"
toIETF "estonian"        = "et-EE"
toIETF "finnish"         = "fi-FI"
toIETF "canadien"        = "fr-CA"
toIETF "acadian"         = "fr-CA"
toIETF "french"          = "fr-FR"
toIETF "francais"        = "fr-FR"
toIETF "austrian"        = "de-AT"
toIETF "naustrian"       = "de-AT"
toIETF "german"          = "de-DE"
toIETF "germanb"         = "de-DE"
toIETF "ngerman"         = "de-DE"
toIETF "greek"           = "el-GR"
toIETF "polutonikogreek" = "el-GR"
toIETF "hebrew"          = "he-IL"
toIETF "hungarian"       = "hu-HU"
toIETF "icelandic"       = "is-IS"
toIETF "italian"         = "it-IT"
toIETF "japanese"        = "ja-JP"
toIETF "latvian"         = "lv-LV"
toIETF "lithuanian"      = "lt-LT"
toIETF "magyar"          = "hu-HU"
toIETF "mongolian"       = "mn-MN"
toIETF "norsk"           = "nb-NO"
toIETF "nynorsk"         = "nn-NO"
toIETF "farsi"           = "fa-IR"
toIETF "polish"          = "pl-PL"
toIETF "brazil"          = "pt-BR"
toIETF "brazilian"       = "pt-BR"
toIETF "portugues"       = "pt-PT"
toIETF "portuguese"      = "pt-PT"
toIETF "romanian"        = "ro-RO"
toIETF "russian"         = "ru-RU"
toIETF "serbian"         = "sr-RS"
toIETF "serbianc"        = "sr-RS"
toIETF "slovak"          = "sk-SK"
toIETF "slovene"         = "sl-SL"
toIETF "spanish"         = "es-ES"
toIETF "swedish"         = "sv-SE"
toIETF "thai"            = "th-TH"
toIETF "turkish"         = "tr-TR"
toIETF "ukrainian"       = "uk-UA"
toIETF "vietnamese"      = "vi-VN"
toIETF "latin"           = "la"
toIETF x                 = x


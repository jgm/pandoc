{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Lang
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.LaTeX.Lang
  ( toPolyglossiaEnv,
    toPolyglossia,
    toBabel
  ) where
import Data.Text (Text)
import Text.Pandoc.BCP47 (Lang (..))


-- In environments \Arabic instead of \arabic is used
toPolyglossiaEnv :: Lang -> (Text, Text)
toPolyglossiaEnv l =
  case toPolyglossia l of
    ("arabic", o) -> ("Arabic", o)
    x             -> x

-- Takes a list of the constituents of a BCP 47 language code and
-- converts it to a Polyglossia (language, options) tuple
-- http://mirrors.ctan.org/macros/latex/contrib/polyglossia/polyglossia.pdf
toPolyglossia :: Lang -> (Text, Text)
toPolyglossia (Lang "ar" _ "DZ" _)        = ("arabic", "locale=algeria")
toPolyglossia (Lang "ar" _ "IQ" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "JO" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "LB" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "LY" _)        = ("arabic", "locale=libya")
toPolyglossia (Lang "ar" _ "MA" _)        = ("arabic", "locale=morocco")
toPolyglossia (Lang "ar" _ "MR" _)        = ("arabic", "locale=mauritania")
toPolyglossia (Lang "ar" _ "PS" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "SY" _)        = ("arabic", "locale=mashriq")
toPolyglossia (Lang "ar" _ "TN" _)        = ("arabic", "locale=tunisia")
toPolyglossia (Lang "de" _ _ vars)
  | "1901" `elem` vars                    = ("german", "spelling=old")
toPolyglossia (Lang "de" _ "AT" vars)
  | "1901" `elem` vars                    = ("german", "variant=austrian, spelling=old")
toPolyglossia (Lang "de" _ "AT" _)        = ("german", "variant=austrian")
toPolyglossia (Lang "de" _ "CH" vars)
  | "1901" `elem` vars                    = ("german", "variant=swiss, spelling=old")
toPolyglossia (Lang "de" _ "CH" _)        = ("german", "variant=swiss")
toPolyglossia (Lang "de" _ _ _)           = ("german", "")
toPolyglossia (Lang "dsb" _ _ _)          = ("lsorbian", "")
toPolyglossia (Lang "el" _ "polyton" _)   = ("greek",   "variant=poly")
toPolyglossia (Lang "en" _ "AU" _)        = ("english", "variant=australian")
toPolyglossia (Lang "en" _ "CA" _)        = ("english", "variant=canadian")
toPolyglossia (Lang "en" _ "GB" _)        = ("english", "variant=british")
toPolyglossia (Lang "en" _ "NZ" _)        = ("english", "variant=newzealand")
toPolyglossia (Lang "en" _ "UK" _)        = ("english", "variant=british")
toPolyglossia (Lang "en" _ "US" _)        = ("english", "variant=american")
toPolyglossia (Lang "grc" _ _ _)          = ("greek",   "variant=ancient")
toPolyglossia (Lang "hsb" _ _  _)         = ("usorbian", "")
toPolyglossia (Lang "la" _ _ vars)
  | "x-classic" `elem` vars               = ("latin", "variant=classic")
toPolyglossia (Lang "pt" _ "BR" _)        = ("portuguese", "variant=brazilian")
toPolyglossia (Lang "sl" _ _ _)           = ("slovenian", "")
toPolyglossia x                           = (commonFromBcp47 x, "")

-- Takes a list of the constituents of a BCP 47 language code and
-- converts it to a Babel language string.
-- http://mirrors.ctan.org/macros/latex/required/babel/base/babel.pdf
-- List of supported languages (slightly outdated):
-- http://tug.ctan.org/language/hyph-utf8/doc/generic/hyph-utf8/hyphenation.pdf
toBabel :: Lang -> Text
toBabel (Lang "de" _ "AT" vars)
  | "1901" `elem` vars                  = "austrian"
  | otherwise                           = "naustrian"
toBabel (Lang "de" _ "CH" vars)
  | "1901" `elem` vars                  = "swissgerman"
  | otherwise                           = "nswissgerman"
toBabel (Lang "de" _ _ vars)
  | "1901" `elem` vars                  = "german"
  | otherwise                           = "ngerman"
toBabel (Lang "dsb" _ _ _)              = "lowersorbian"
toBabel (Lang "el" _ _ vars)
  | "polyton" `elem` vars               = "polutonikogreek"
toBabel (Lang "en" _ "AU" _)            = "australian"
toBabel (Lang "en" _ "CA" _)            = "canadian"
toBabel (Lang "en" _ "GB" _)            = "british"
toBabel (Lang "en" _ "NZ" _)            = "newzealand"
toBabel (Lang "en" _ "UK" _)            = "british"
toBabel (Lang "en" _ "US" _)            = "american"
toBabel (Lang "fr" _ "CA" _)            = "canadien"
toBabel (Lang "fra" _ _ vars)
  | "aca" `elem` vars                   = "acadian"
toBabel (Lang "grc" _ _ _)              = "polutonikogreek"
toBabel (Lang "hsb" _ _ _)              = "uppersorbian"
toBabel (Lang "la" _ _ vars)
  | "x-classic" `elem` vars             = "classiclatin"
toBabel (Lang "pt" _ "BR" _)            = "brazilian"
toBabel (Lang "sl" _ _ _)               = "slovene"
toBabel x                               = commonFromBcp47 x

-- Takes a list of the constituents of a BCP 47 language code
-- and converts it to a string shared by Babel and Polyglossia.
-- https://tools.ietf.org/html/bcp47#section-2.1
commonFromBcp47 :: Lang -> Text
commonFromBcp47 (Lang "sr" "Cyrl" _ _)          = "serbianc"
commonFromBcp47 (Lang "zh" "Latn" _ vars)
  | "pinyin" `elem` vars                        = "pinyin"
commonFromBcp47 (Lang l _ _ _) = fromIso l
  where
    fromIso "af"  = "afrikaans"
    fromIso "am"  = "amharic"
    fromIso "ar"  = "arabic"
    fromIso "as"  = "assamese"
    fromIso "ast" = "asturian"
    fromIso "bg"  = "bulgarian"
    fromIso "bn"  = "bengali"
    fromIso "bo"  = "tibetan"
    fromIso "br"  = "breton"
    fromIso "ca"  = "catalan"
    fromIso "cy"  = "welsh"
    fromIso "cs"  = "czech"
    fromIso "cop" = "coptic"
    fromIso "da"  = "danish"
    fromIso "dv"  = "divehi"
    fromIso "el"  = "greek"
    fromIso "en"  = "english"
    fromIso "eo"  = "esperanto"
    fromIso "es"  = "spanish"
    fromIso "et"  = "estonian"
    fromIso "eu"  = "basque"
    fromIso "fa"  = "farsi"
    fromIso "fi"  = "finnish"
    fromIso "fr"  = "french"
    fromIso "fur" = "friulan"
    fromIso "ga"  = "irish"
    fromIso "gd"  = "scottish"
    fromIso "gez" = "ethiopic"
    fromIso "gl"  = "galician"
    fromIso "he"  = "hebrew"
    fromIso "hi"  = "hindi"
    fromIso "hr"  = "croatian"
    fromIso "hu"  = "magyar"
    fromIso "hy"  = "armenian"
    fromIso "ia"  = "interlingua"
    fromIso "id"  = "indonesian"
    fromIso "ie"  = "interlingua"
    fromIso "is"  = "icelandic"
    fromIso "it"  = "italian"
    fromIso "ja"  = "japanese"
    fromIso "km"  = "khmer"
    fromIso "kmr" = "kurmanji"
    fromIso "kn"  = "kannada"
    fromIso "ko"  = "korean"
    fromIso "la"  = "latin"
    fromIso "lo"  = "lao"
    fromIso "lt"  = "lithuanian"
    fromIso "lv"  = "latvian"
    fromIso "ml"  = "malayalam"
    fromIso "mn"  = "mongolian"
    fromIso "mr"  = "marathi"
    fromIso "nb"  = "norsk"
    fromIso "nl"  = "dutch"
    fromIso "nn"  = "nynorsk"
    fromIso "no"  = "norsk"
    fromIso "nqo" = "nko"
    fromIso "oc"  = "occitan"
    fromIso "pa"  = "panjabi"
    fromIso "pl"  = "polish"
    fromIso "pms" = "piedmontese"
    fromIso "pt"  = "portuguese"
    fromIso "rm"  = "romansh"
    fromIso "ro"  = "romanian"
    fromIso "ru"  = "russian"
    fromIso "sa"  = "sanskrit"
    fromIso "se"  = "samin"
    fromIso "sk"  = "slovak"
    fromIso "sq"  = "albanian"
    fromIso "sr"  = "serbian"
    fromIso "sv"  = "swedish"
    fromIso "syr" = "syriac"
    fromIso "ta"  = "tamil"
    fromIso "te"  = "telugu"
    fromIso "th"  = "thai"
    fromIso "ti"  = "ethiopic"
    fromIso "tk"  = "turkmen"
    fromIso "tr"  = "turkish"
    fromIso "uk"  = "ukrainian"
    fromIso "ur"  = "urdu"
    fromIso "vi"  = "vietnamese"
    fromIso _     = ""

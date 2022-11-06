{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Lang
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Writers.LaTeX.Lang
  ( toBabel
  ) where
import Data.Text (Text)
import Text.Collate.Lang (Lang(..))


-- Takes a list of the constituents of a BCP47 language code and
-- converts it to a Babel language string.
-- http://mirrors.ctan.org/macros/latex/required/babel/base/babel.pdf
-- List of supported languages (slightly outdated):
-- http://tug.ctan.org/language/hyph-utf8/doc/generic/hyph-utf8/hyphenation.pdf
toBabel :: Lang -> Maybe Text
toBabel (Lang "de" _ (Just "AT") vars _ _)
  | "1901" `elem` vars                  = Just "austrian"
  | otherwise                           = Just "naustrian"
toBabel (Lang "de" _ (Just "CH") vars _ _)
  | "1901" `elem` vars                  = Just "swissgerman"
  | otherwise                           = Just "nswissgerman"
toBabel (Lang "de" _ _ vars _ _)
  | "1901" `elem` vars                  = Just "german"
  | otherwise                           = Just "ngerman"
toBabel (Lang "dsb" _ _ _ _ _)          = Just "lowersorbian"
toBabel (Lang "el" _ _ vars _ _)
  | "polyton" `elem` vars               = Just "polutonikogreek"
toBabel (Lang "en" _ (Just "AU") _ _ _) = Just "australian"
toBabel (Lang "en" _ (Just "CA") _ _ _) = Just "canadian"
toBabel (Lang "en" _ (Just "GB") _ _ _) = Just "british"
toBabel (Lang "en" _ (Just "NZ") _ _ _) = Just "newzealand"
toBabel (Lang "en" _ (Just "UK") _ _ _) = Just "british"
toBabel (Lang "en" _ (Just "US") _ _ _) = Just "american"
toBabel (Lang "fr" _ (Just "CA") _ _ _) = Just "canadien"
toBabel (Lang "fra" _ _ vars _ _)
  | "aca" `elem` vars                   = Just "acadian"
toBabel (Lang "grc" _ _ _ _ _)          = Just "ancientgreek"
toBabel (Lang "hsb" _ _ _ _ _)          = Just "uppersorbian"
toBabel (Lang "la" _ _ vars _ _)
  | "x-classic" `elem` vars             = Just "classiclatin"
toBabel (Lang "pt" _ (Just "BR") _ _ _) = Just "brazilian"
toBabel (Lang "sl" _ _ _ _ _)           = Just "slovene"
toBabel x                               = commonFromBcp47 x

-- Takes a list of the constituents of a BCP47 language code
-- and converts it to a string shared by Babel and Polyglossia.
-- https://tools.ietf.org/html/bcp47#section-2.1
commonFromBcp47 :: Lang -> Maybe Text
commonFromBcp47 (Lang "sr" (Just "Cyrl") _ _ _ _)      = Just "serbianc"
commonFromBcp47 (Lang "zh" (Just "Latn") _ vars _ _)
  | "pinyin" `elem` vars                               = Just "pinyin"
commonFromBcp47 (Lang l _ _ _ _ _) = fromIso l
  where
    fromIso "af"  = Just "afrikaans"
    fromIso "am"  = Just "amharic"
    fromIso "ar"  = Just "arabic"
    fromIso "as"  = Just "assamese"
    fromIso "ast" = Just "asturian"
    fromIso "bg"  = Just "bulgarian"
    fromIso "bn"  = Just "bengali"
    fromIso "bo"  = Just "tibetan"
    fromIso "br"  = Just "breton"
    fromIso "ca"  = Just "catalan"
    fromIso "cy"  = Just "welsh"
    fromIso "cs"  = Just "czech"
    fromIso "cop" = Just "coptic"
    fromIso "da"  = Just "danish"
    fromIso "dv"  = Just "divehi"
    fromIso "el"  = Just "greek"
    fromIso "en"  = Just "english"
    fromIso "eo"  = Just "esperanto"
    fromIso "es"  = Just "spanish"
    fromIso "et"  = Just "estonian"
    fromIso "eu"  = Just "basque"
    fromIso "fa"  = Just "farsi"
    fromIso "fi"  = Just "finnish"
    fromIso "fr"  = Just "french"
    fromIso "fur" = Just "friulan"
    fromIso "ga"  = Just "irish"
    fromIso "gd"  = Just "scottish"
    fromIso "gez" = Just "ethiopic"
    fromIso "gl"  = Just "galician"
    fromIso "gu"  = Just "gujarati"
    fromIso "he"  = Just "hebrew"
    fromIso "hi"  = Just "hindi"
    fromIso "hr"  = Just "croatian"
    fromIso "hu"  = Just "magyar"
    fromIso "hy"  = Just "armenian"
    fromIso "ia"  = Just "interlingua"
    fromIso "id"  = Just "indonesian"
    fromIso "ie"  = Just "interlingua"
    fromIso "is"  = Just "icelandic"
    fromIso "it"  = Just "italian"
    fromIso "ja"  = Just "japanese"
    fromIso "km"  = Just "khmer"
    fromIso "kmr" = Just "kurmanji"
    fromIso "kn"  = Just "kannada"
    fromIso "ko"  = Just "korean"
    fromIso "la"  = Just "latin"
    fromIso "lo"  = Just "lao"
    fromIso "lt"  = Just "lithuanian"
    fromIso "lv"  = Just "latvian"
    fromIso "ml"  = Just "malayalam"
    fromIso "mn"  = Just "mongolian"
    fromIso "mr"  = Just "marathi"
    fromIso "nb"  = Just "norsk"
    fromIso "nl"  = Just "dutch"
    fromIso "nn"  = Just "nynorsk"
    fromIso "no"  = Just "norsk"
    fromIso "nqo" = Just "nko"
    fromIso "oc"  = Just "occitan"
    fromIso "or"  = Just "oriya"
    fromIso "pa"  = Just "punjabi"
    fromIso "pl"  = Just "polish"
    fromIso "pms" = Just "piedmontese"
    fromIso "pt"  = Just "portuguese"
    fromIso "rm"  = Just "romansh"
    fromIso "ro"  = Just "romanian"
    fromIso "ru"  = Just "russian"
    fromIso "sa"  = Just "sanskrit"
    fromIso "se"  = Just "samin"
    fromIso "sk"  = Just "slovak"
    fromIso "sq"  = Just "albanian"
    fromIso "sr"  = Just "serbian"
    fromIso "sv"  = Just "swedish"
    fromIso "syr" = Just "syriac"
    fromIso "ta"  = Just "tamil"
    fromIso "te"  = Just "telugu"
    fromIso "th"  = Just "thai"
    fromIso "ti"  = Just "ethiopic"
    fromIso "tk"  = Just "turkmen"
    fromIso "tr"  = Just "turkish"
    fromIso "uk"  = Just "ukrainian"
    fromIso "ur"  = Just "urdu"
    fromIso "vi"  = Just "vietnamese"
    fromIso _     = Nothing

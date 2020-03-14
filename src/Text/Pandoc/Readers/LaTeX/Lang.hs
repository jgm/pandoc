{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Lang
   Copyright   : Copyright (C) 2018-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for parsing polyglossia and babel language specifiers to
BCP47 'Lang'.
-}
module Text.Pandoc.Readers.LaTeX.Lang
  ( polyglossiaLangToBCP47
  , babelLangToBCP47
  )
where
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.BCP47 (Lang(..))

polyglossiaLangToBCP47 :: M.Map T.Text (T.Text -> Lang)
polyglossiaLangToBCP47 = M.fromList
  [ ("arabic", \o -> case T.filter (/=' ') o of
       "locale=algeria"    -> Lang "ar" "" "DZ" []
       "locale=mashriq"    -> Lang "ar" "" "SY" []
       "locale=libya"      -> Lang "ar" "" "LY" []
       "locale=morocco"    -> Lang "ar" "" "MA" []
       "locale=mauritania" -> Lang "ar" "" "MR" []
       "locale=tunisia"    -> Lang "ar" "" "TN" []
       _                   -> Lang "ar" "" "" [])
  , ("german", \o -> case T.filter (/=' ') o of
       "spelling=old" -> Lang "de" "" "DE" ["1901"]
       "variant=austrian,spelling=old"
                       -> Lang "de" "" "AT" ["1901"]
       "variant=austrian" -> Lang "de" "" "AT" []
       "variant=swiss,spelling=old"
                       -> Lang "de" "" "CH" ["1901"]
       "variant=swiss" -> Lang "de" "" "CH" []
       _ -> Lang "de" "" "" [])
  , ("lsorbian", \_ -> Lang "dsb" "" "" [])
  , ("greek", \o -> case T.filter (/=' ') o of
       "variant=poly"    -> Lang "el" "" "polyton" []
       "variant=ancient" -> Lang "grc" "" "" []
       _                 -> Lang "el" "" "" [])
  , ("english", \o -> case T.filter (/=' ') o of
       "variant=australian" -> Lang "en" "" "AU" []
       "variant=canadian"   -> Lang "en" "" "CA" []
       "variant=british"    -> Lang "en" "" "GB" []
       "variant=newzealand" -> Lang "en" "" "NZ" []
       "variant=american"   -> Lang "en" "" "US" []
       _                    -> Lang "en" "" "" [])
  , ("usorbian", \_ -> Lang "hsb" "" "" [])
  , ("latin", \o -> case T.filter (/=' ') o of
       "variant=classic" -> Lang "la" "" "" ["x-classic"]
       _                 -> Lang "la" "" "" [])
  , ("slovenian", \_ -> Lang "sl" "" "" [])
  , ("serbianc", \_ -> Lang "sr" "cyrl" "" [])
  , ("pinyin", \_ -> Lang "zh" "Latn" "" ["pinyin"])
  , ("afrikaans", \_ -> Lang "af" "" "" [])
  , ("amharic", \_ -> Lang "am" "" "" [])
  , ("assamese", \_ -> Lang "as" "" "" [])
  , ("asturian", \_ -> Lang "ast" "" "" [])
  , ("bulgarian", \_ -> Lang "bg" "" "" [])
  , ("bengali", \_ -> Lang "bn" "" "" [])
  , ("tibetan", \_ -> Lang "bo" "" "" [])
  , ("breton", \_ -> Lang "br" "" "" [])
  , ("catalan", \_ -> Lang "ca" "" "" [])
  , ("welsh", \_ -> Lang "cy" "" "" [])
  , ("czech", \_ -> Lang "cs" "" "" [])
  , ("coptic", \_ -> Lang "cop" "" "" [])
  , ("danish", \_ -> Lang "da" "" "" [])
  , ("divehi", \_ -> Lang "dv" "" "" [])
  , ("esperanto", \_ -> Lang "eo" "" "" [])
  , ("spanish", \_ -> Lang "es" "" "" [])
  , ("estonian", \_ -> Lang "et" "" "" [])
  , ("basque", \_ -> Lang "eu" "" "" [])
  , ("farsi", \_ -> Lang "fa" "" "" [])
  , ("finnish", \_ -> Lang "fi" "" "" [])
  , ("french", \_ -> Lang "fr" "" "" [])
  , ("friulan", \_ -> Lang "fur" "" "" [])
  , ("irish", \_ -> Lang "ga" "" "" [])
  , ("scottish", \_ -> Lang "gd" "" "" [])
  , ("ethiopic", \_ -> Lang "gez" "" "" [])
  , ("galician", \_ -> Lang "gl" "" "" [])
  , ("hebrew", \_ -> Lang "he" "" "" [])
  , ("hindi", \_ -> Lang "hi" "" "" [])
  , ("croatian", \_ -> Lang "hr" "" "" [])
  , ("magyar", \_ -> Lang "hu" "" "" [])
  , ("armenian", \_ -> Lang "hy" "" "" [])
  , ("interlingua", \_ -> Lang "ia" "" "" [])
  , ("indonesian", \_ -> Lang "id" "" "" [])
  , ("icelandic", \_ -> Lang "is" "" "" [])
  , ("italian", \_ -> Lang "it" "" "" [])
  , ("japanese", \_ -> Lang "jp" "" "" [])
  , ("khmer", \_ -> Lang "km" "" "" [])
  , ("kurmanji", \_ -> Lang "kmr" "" "" [])
  , ("kannada", \_ -> Lang "kn" "" "" [])
  , ("korean", \_ -> Lang "ko" "" "" [])
  , ("lao", \_ -> Lang "lo" "" "" [])
  , ("lithuanian", \_ -> Lang "lt" "" "" [])
  , ("latvian", \_ -> Lang "lv" "" "" [])
  , ("malayalam", \_ -> Lang "ml" "" "" [])
  , ("mongolian", \_ -> Lang "mn" "" "" [])
  , ("marathi", \_ -> Lang "mr" "" "" [])
  , ("dutch", \_ -> Lang "nl" "" "" [])
  , ("nynorsk", \_ -> Lang "nn" "" "" [])
  , ("norsk", \_ -> Lang "no" "" "" [])
  , ("nko", \_ -> Lang "nqo" "" "" [])
  , ("occitan", \_ -> Lang "oc" "" "" [])
  , ("panjabi", \_ -> Lang "pa" "" "" [])
  , ("polish", \_ -> Lang "pl" "" "" [])
  , ("piedmontese", \_ -> Lang "pms" "" "" [])
  , ("portuguese", \_ -> Lang "pt" "" "" [])
  , ("romansh", \_ -> Lang "rm" "" "" [])
  , ("romanian", \_ -> Lang "ro" "" "" [])
  , ("russian", \_ -> Lang "ru" "" "" [])
  , ("sanskrit", \_ -> Lang "sa" "" "" [])
  , ("samin", \_ -> Lang "se" "" "" [])
  , ("slovak", \_ -> Lang "sk" "" "" [])
  , ("albanian", \_ -> Lang "sq" "" "" [])
  , ("serbian", \_ -> Lang "sr" "" "" [])
  , ("swedish", \_ -> Lang "sv" "" "" [])
  , ("syriac", \_ -> Lang "syr" "" "" [])
  , ("tamil", \_ -> Lang "ta" "" "" [])
  , ("telugu", \_ -> Lang "te" "" "" [])
  , ("thai", \_ -> Lang "th" "" "" [])
  , ("turkmen", \_ -> Lang "tk" "" "" [])
  , ("turkish", \_ -> Lang "tr" "" "" [])
  , ("ukrainian", \_ -> Lang "uk" "" "" [])
  , ("urdu", \_ -> Lang "ur" "" "" [])
  , ("vietnamese", \_ -> Lang "vi" "" "" [])
  ]

babelLangToBCP47 :: T.Text -> Maybe Lang
babelLangToBCP47 s =
  case s of
       "austrian" -> Just $ Lang "de" "" "AT" ["1901"]
       "naustrian" -> Just $ Lang "de" "" "AT" []
       "swissgerman" -> Just $ Lang "de" "" "CH" ["1901"]
       "nswissgerman" -> Just $ Lang "de" "" "CH" []
       "german" -> Just $ Lang "de" "" "DE" ["1901"]
       "ngerman" -> Just $ Lang "de" "" "DE" []
       "lowersorbian" -> Just $ Lang "dsb" "" "" []
       "uppersorbian" -> Just $ Lang "hsb" "" "" []
       "polutonikogreek" -> Just $ Lang "el" "" "" ["polyton"]
       "slovene" -> Just $ Lang "sl" "" "" []
       "australian" -> Just $ Lang "en" "" "AU" []
       "canadian" -> Just $ Lang "en" "" "CA" []
       "british" -> Just $ Lang "en" "" "GB" []
       "newzealand" -> Just $ Lang "en" "" "NZ" []
       "american" -> Just $ Lang "en" "" "US" []
       "classiclatin" -> Just $ Lang "la" "" "" ["x-classic"]
       _ -> fmap ($ "") $ M.lookup s polyglossiaLangToBCP47

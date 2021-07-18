{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Lang
   Copyright   : Copyright (C) 2018-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for parsing polyglossia and babel language specifiers to
BCP47 'Lang'.
-}
module Text.Pandoc.Readers.LaTeX.Lang
  ( setDefaultLanguage
  , polyglossiaLangToBCP47
  , babelLangToBCP47
  , enquoteCommands
  , inlineLanguageCommands
  )
where
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Shared (extractSpaces)
import Text.Collate.Lang (Lang(..), renderLang)
import Text.Pandoc.Class (PandocMonad(..), setTranslations)
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Parsing (updateState, option, getState, QuoteContext(..),
                            withQuoteContext)
import Text.Pandoc.Builder (Blocks, Inlines, setMeta, str, spanWith,
                            singleQuoted, doubleQuoted)

enquote :: PandocMonad m
        => LP m Inlines
        -> Bool -> Maybe Text -> LP m Inlines
enquote tok starred mblang = do
  skipopts
  let lang = mblang >>= babelLangToBCP47
  let langspan = case lang of
                      Nothing -> id
                      Just l  -> spanWith ("",[],[("lang", renderLang l)])
  quoteContext <- sQuoteContext <$> getState
  if starred || quoteContext == InDoubleQuote
     then singleQuoted . langspan <$> withQuoteContext InSingleQuote tok
     else doubleQuoted . langspan <$> withQuoteContext InDoubleQuote tok

enquoteCommands :: PandocMonad m
                => LP m Inlines -> M.Map Text (LP m Inlines)
enquoteCommands tok = M.fromList
  [ ("enquote*", enquote tok True Nothing)
  , ("enquote", enquote tok False Nothing)
  -- foreignquote is supposed to use native quote marks
  , ("foreignquote*", braced >>= enquote tok True . Just . untokenize)
  , ("foreignquote", braced >>= enquote tok False . Just . untokenize)
  -- hypehnquote uses regular quotes
  , ("hyphenquote*", braced >>= enquote tok True . Just . untokenize)
  , ("hyphenquote", braced >>= enquote tok False . Just . untokenize)
  ]

foreignlanguage :: PandocMonad m => LP m Inlines -> LP m Inlines
foreignlanguage tok = do
  babelLang <- untokenize <$> braced
  case babelLangToBCP47 babelLang of
       Just lang -> spanWith ("", [], [("lang",  renderLang lang)]) <$> tok
       _ -> tok

inlineLanguageCommands :: PandocMonad m
                       => LP m Inlines -> M.Map Text (LP m Inlines)
inlineLanguageCommands tok =
  M.fromList $
    ("foreignlanguage", foreignlanguage tok) :
    (mk <$> M.toList polyglossiaLangToBCP47)
  where
    mk (polyglossia, bcp47Func) =
      ("text" <> polyglossia, inlineLanguage tok bcp47Func)

inlineLanguage :: PandocMonad m
               => LP m Inlines -> (Text -> Lang) -> LP m Inlines
inlineLanguage tok bcp47Func = do
  o <- option "" $ T.filter (\c -> c /= '[' && c /= ']')
                <$> rawopt
  let lang = renderLang $ bcp47Func o
  extractSpaces (spanWith ("", [], [("lang", lang)])) <$> tok

setDefaultLanguage :: PandocMonad m => LP m Blocks
setDefaultLanguage = do
  o <- option "" $ T.filter (\c -> c /= '[' && c /= ']')
                <$> rawopt
  polylang <- untokenize <$> braced
  case M.lookup polylang polyglossiaLangToBCP47 of
       Nothing -> return mempty -- TODO mzero? warning?
       Just langFunc -> do
         let l = langFunc o
         setTranslations l
         updateState $ setMeta "lang" $ str (renderLang l)
         return mempty

polyglossiaLangToBCP47 :: M.Map T.Text (T.Text -> Lang)
polyglossiaLangToBCP47 = M.fromList
  [ ("arabic", \o -> case T.filter (/=' ') o of
       "locale=algeria"    -> Lang "ar" Nothing (Just "DZ") [] [] []
       "locale=mashriq"    -> Lang "ar" Nothing (Just "SY") [] [] []
       "locale=libya"      -> Lang "ar" Nothing (Just "LY") [] [] []
       "locale=morocco"    -> Lang "ar" Nothing (Just "MA") [] [] []
       "locale=mauritania" -> Lang "ar" Nothing (Just "MR") [] [] []
       "locale=tunisia"    -> Lang "ar" Nothing (Just "TN") [] [] []
       _                   -> Lang "ar" Nothing Nothing     [] [] [])
  , ("german", \o -> case T.filter (/=' ') o of
       "spelling=old" -> Lang "de" Nothing (Just "DE") ["1901"] [] []
       "variant=austrian,spelling=old"
                       -> Lang "de" Nothing (Just "AT") ["1901"] [] []
       "variant=austrian" -> Lang "de" Nothing (Just "AT") [] [] []
       "variant=swiss,spelling=old"
                       -> Lang "de" Nothing (Just "CH") ["1901"] [] []
       "variant=swiss" -> Lang "de" Nothing (Just "CH") [] [] []
       _ -> Lang "de" Nothing Nothing [] [] [])
  , ("lsorbian", \_ -> Lang "dsb" Nothing Nothing [] [] [])
  , ("greek", \o -> case T.filter (/=' ') o of
       "variant=poly"    -> Lang "el" Nothing (Just "polyton") [] [] []
       "variant=ancient" -> Lang "grc" Nothing Nothing [] [] []
       _                 -> Lang "el" Nothing Nothing [] [] [])
  , ("english", \o -> case T.filter (/=' ') o of
       "variant=australian" -> Lang "en" Nothing (Just "AU") [] [] []
       "variant=canadian"   -> Lang "en" Nothing (Just "CA") [] [] []
       "variant=british"    -> Lang "en" Nothing (Just "GB") [] [] []
       "variant=newzealand" -> Lang "en" Nothing (Just "NZ") [] [] []
       "variant=american"   -> Lang "en" Nothing (Just "US") [] [] []
       _                    -> Lang "en" Nothing Nothing     [] [] [])
  , ("usorbian", \_ -> Lang "hsb" Nothing Nothing [] [] [])
  , ("latin", \o -> case T.filter (/=' ') o of
       "variant=classic" -> Lang "la" Nothing Nothing ["x-classic"] [] []
       _                 -> Lang "la" Nothing Nothing [] [] [])
  , ("slovenian", \_ -> Lang "sl" Nothing Nothing [] [] [])
  , ("serbianc", \_ -> Lang "sr" (Just "Cyrl") Nothing [] [] [])
  , ("pinyin", \_ -> Lang "zh" (Just "Latn") Nothing ["pinyin"] [] [])
  , ("afrikaans", \_ -> simpleLang "af")
  , ("amharic", \_ -> simpleLang "am")
  , ("assamese", \_ -> simpleLang "as")
  , ("asturian", \_ -> simpleLang "ast")
  , ("bulgarian", \_ -> simpleLang "bg")
  , ("bengali", \_ -> simpleLang "bn")
  , ("tibetan", \_ -> simpleLang "bo")
  , ("breton", \_ -> simpleLang "br")
  , ("catalan", \_ -> simpleLang "ca")
  , ("welsh", \_ -> simpleLang "cy")
  , ("czech", \_ -> simpleLang "cs")
  , ("coptic", \_ -> simpleLang "cop")
  , ("danish", \_ -> simpleLang "da")
  , ("divehi", \_ -> simpleLang "dv")
  , ("esperanto", \_ -> simpleLang "eo")
  , ("spanish", \_ -> simpleLang "es")
  , ("estonian", \_ -> simpleLang "et")
  , ("basque", \_ -> simpleLang "eu")
  , ("farsi", \_ -> simpleLang "fa")
  , ("finnish", \_ -> simpleLang "fi")
  , ("french", \_ -> simpleLang "fr")
  , ("friulan", \_ -> simpleLang "fur")
  , ("irish", \_ -> simpleLang "ga")
  , ("scottish", \_ -> simpleLang "gd")
  , ("ethiopic", \_ -> simpleLang "gez")
  , ("galician", \_ -> simpleLang "gl")
  , ("hebrew", \_ -> simpleLang "he")
  , ("hindi", \_ -> simpleLang "hi")
  , ("croatian", \_ -> simpleLang "hr")
  , ("magyar", \_ -> simpleLang "hu")
  , ("armenian", \_ -> simpleLang "hy")
  , ("interlingua", \_ -> simpleLang "ia")
  , ("indonesian", \_ -> simpleLang "id")
  , ("icelandic", \_ -> simpleLang "is")
  , ("italian", \_ -> simpleLang "it")
  , ("japanese", \_ -> simpleLang "jp")
  , ("khmer", \_ -> simpleLang "km")
  , ("kurmanji", \_ -> simpleLang "kmr")
  , ("kannada", \_ -> simpleLang "kn")
  , ("korean", \_ -> simpleLang "ko")
  , ("lao", \_ -> simpleLang "lo")
  , ("lithuanian", \_ -> simpleLang "lt")
  , ("latvian", \_ -> simpleLang "lv")
  , ("malayalam", \_ -> simpleLang "ml")
  , ("mongolian", \_ -> simpleLang "mn")
  , ("marathi", \_ -> simpleLang "mr")
  , ("dutch", \_ -> simpleLang "nl")
  , ("nynorsk", \_ -> simpleLang "nn")
  , ("norsk", \_ -> simpleLang "no")
  , ("nko", \_ -> simpleLang "nqo")
  , ("occitan", \_ -> simpleLang "oc")
  , ("panjabi", \_ -> simpleLang "pa")
  , ("polish", \_ -> simpleLang "pl")
  , ("piedmontese", \_ -> simpleLang "pms")
  , ("portuguese", \_ -> simpleLang "pt")
  , ("romansh", \_ -> simpleLang "rm")
  , ("romanian", \_ -> simpleLang "ro")
  , ("russian", \_ -> simpleLang "ru")
  , ("sanskrit", \_ -> simpleLang "sa")
  , ("samin", \_ -> simpleLang "se")
  , ("slovak", \_ -> simpleLang "sk")
  , ("albanian", \_ -> simpleLang "sq")
  , ("serbian", \_ -> simpleLang "sr")
  , ("swedish", \_ -> simpleLang "sv")
  , ("syriac", \_ -> simpleLang "syr")
  , ("tamil", \_ -> simpleLang "ta")
  , ("telugu", \_ -> simpleLang "te")
  , ("thai", \_ -> simpleLang "th")
  , ("turkmen", \_ -> simpleLang "tk")
  , ("turkish", \_ -> simpleLang "tr")
  , ("ukrainian", \_ -> simpleLang "uk")
  , ("urdu", \_ -> simpleLang "ur")
  , ("vietnamese", \_ -> simpleLang "vi")
  ]

simpleLang :: Text -> Lang
simpleLang l = Lang l Nothing Nothing [] [] []

babelLangToBCP47 :: T.Text -> Maybe Lang
babelLangToBCP47 s =
  case s of
       "austrian" -> Just $ Lang "de" Nothing (Just "AT") ["1901"] [] []
       "naustrian" -> Just $ Lang "de" Nothing (Just "AT") [] [] []
       "swissgerman" -> Just $ Lang "de" Nothing (Just "CH") ["1901"] [] []
       "nswissgerman" -> Just $ Lang "de" Nothing (Just "CH") [] [] []
       "german" -> Just $ Lang "de" Nothing (Just "DE") ["1901"] [] []
       "ngerman" -> Just $ Lang "de" Nothing (Just "DE") [] [] []
       "lowersorbian" -> Just $ Lang "dsb" Nothing Nothing [] [] []
       "uppersorbian" -> Just $ Lang "hsb" Nothing Nothing [] [] []
       "polutonikogreek" -> Just $ Lang "el" Nothing Nothing ["polyton"] [] []
       "slovene" -> Just $ simpleLang "sl"
       "australian" -> Just $ Lang "en" Nothing (Just "AU") [] [] []
       "canadian" -> Just $ Lang "en" Nothing (Just "CA") [] [] []
       "british" -> Just $ Lang "en" Nothing (Just "GB") [] [] []
       "newzealand" -> Just $ Lang "en" Nothing (Just "NZ") [] [] []
       "american" -> Just $ Lang "en" Nothing (Just "US") [] [] []
       "classiclatin" -> Just $ Lang "la" Nothing Nothing ["x-classic"] [] []
       _ -> ($ "") <$> M.lookup s polyglossiaLangToBCP47

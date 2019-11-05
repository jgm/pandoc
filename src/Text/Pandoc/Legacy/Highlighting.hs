module Text.Pandoc.Legacy.Highlighting ( highlightingStyles
                                , languages
                                , languagesByExtension
                                , highlight
                                , TP.formatLaTeXInline
                                , TP.formatLaTeXBlock
                                , TP.styleToLaTeX
                                , TP.formatHtmlInline
                                , TP.formatHtmlBlock
                                , TP.styleToCss
                                , TP.pygments
                                , TP.espresso
                                , TP.zenburn
                                , TP.tango
                                , TP.kate
                                , TP.monochrome
                                , TP.breezeDark
                                , TP.haddock
                                , TP.Style
                                , fromListingsLanguage
                                , toListingsLanguage
                                ) where

import qualified Text.Pandoc.Highlighting as TP
import qualified Text.Pandoc.Legacy.Definition as TP
import qualified Skylighting as S
import qualified Data.Text as T

highlightingStyles :: [(String, TP.Style)]
highlightingStyles = map (\(x, y) -> (T.unpack x, y)) TP.highlightingStyles

languages :: [String]
languages = map T.unpack TP.languages

languagesByExtension :: String -> [String]
languagesByExtension = map T.unpack . TP.languagesByExtension . T.pack

highlight :: S.SyntaxMap
          -> (S.FormatOptions -> [S.SourceLine] -> a)
          -> TP.Attr
          -> String
          -> Either String a
highlight x y z = either (Left . T.unpack) Right . TP.highlight x y (go z) . T.pack
  where
    go (a, b, c) = (T.pack a, map T.pack b, map (\(r, s) -> (T.pack r, T.pack s)) c)

toListingsLanguage :: String -> Maybe String
toListingsLanguage = fmap T.unpack . TP.toListingsLanguage . T.pack

fromListingsLanguage :: String -> Maybe String
fromListingsLanguage = fmap T.unpack . TP.fromListingsLanguage . T.pack

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.LaTeX
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the LaTeX reader.
-}
module Tests.Readers.LaTeX (tests) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Readers.LaTeX (tokenize, untokenize)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

latex :: Text -> Pandoc
latex = purely $ readLaTeX def{
                   readerExtensions = getDefaultExtensions "latex" }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test latex

table' :: [Alignment] -> [Row] -> Blocks
table' aligns rows
  = table emptyCaption
          (zip aligns (repeat ColWidthDefault))
          (TableHead nullAttr [])
          [TableBody nullAttr 0 [] rows]
          (TableFoot nullAttr [])

simpleTable' :: [Alignment] -> [[Blocks]] -> Blocks
simpleTable' aligns rows
  = table' aligns (map toRow rows)
  where
    toRow = Row nullAttr . map simpleCell

tokUntokRt :: String -> Bool
tokUntokRt s = untokenize (tokenize "random" t) == t
  where t = T.pack s

tests :: [TestTree]
tests = [ testGroup "tokenization"
          [ testCase "tokenizer round trip on test case" $ do
                 orig <- T.pack <$> UTF8.readFile "../test/latex-reader.latex"
                 let new = untokenize $ tokenize "../test/latex-reader.latex"
                             orig
                 assertEqual "untokenize . tokenize is identity" orig new
          , testProperty "untokenize . tokenize is identity" tokUntokRt
          ]

        , testGroup "basic"
          [ "simple" =:
            "word" =?> para "word"
          , "space" =:
            "some text" =?> para "some text"
          , "emphasized" =:
            "\\emph{emphasized}" =?> para (emph "emphasized")
          ]

        , testGroup "headers"
          [ "level 1" =:
            "\\section{header}" =?> headerWith ("header",[],[]) 1 "header"
          , "level 2" =:
            "\\subsection{header}" =?> headerWith ("header",[],[]) 2 "header"
          , "level 3" =:
            "\\subsubsection{header}" =?> headerWith ("header",[],[]) 3 "header"
          , "emph" =:
            "\\section{text \\emph{emph}}" =?>
             headerWith ("text-emph",[],[]) 1 ("text" <> space <> emph "emph")
          , "link" =:
            "\\section{text \\href{/url}{link}}" =?>
              headerWith ("text-link",[],[]) 1 ("text" <> space <> link "/url" "" "link")
          ]

        , testGroup "math"
          [ "escaped $" =:
            "$x=\\$4$" =?> para (math "x=\\$4")
          ]

        , testGroup "space and comments"
          [ "blank lines + space at beginning" =:
            "\n  \n  hi" =?> para "hi"
          , "blank lines + space + comments" =:
            "% my comment\n\n  \n  % another\n\nhi" =?> para "hi"
          , "comment in paragraph" =:
            "hi % this is a comment\nthere\n" =?>
                para ("hi" <> softbreak <> "there")
          ]

        , testGroup "code blocks"
          [ "identifier" =:
            "\\begin{lstlisting}[label=test]\\end{lstlisting}" =?> codeBlockWith ("test", [], [("label","test")]) ""
          , "no identifier" =:
            "\\begin{lstlisting}\\end{lstlisting}" =?> codeBlock ""
          ]

        , testGroup "tables"
          [ "Single cell table" =:
            "\\begin{tabular}{|l|}Test\\\\\\end{tabular}" =?>
            simpleTable' [AlignLeft] [[plain "Test"]]
          , "Multi cell table" =:
            "\\begin{tabular}{|rl|}One & Two\\\\ \\end{tabular}" =?>
            simpleTable' [AlignRight,AlignLeft] [[plain "One", plain "Two"]]
          , "Multi line table" =:
            T.unlines [ "\\begin{tabular}{|c|}"
                    , "One\\\\"
                    , "Two\\\\"
                    , "Three\\\\"
                    , "\\end{tabular}" ] =?>
            simpleTable' [AlignCenter]
                         [[plain "One"], [plain "Two"], [plain "Three"]]
          , "Empty table" =:
            "\\begin{tabular}{}\\end{tabular}" =?>
            simpleTable' [] []
          , "Table with fixed column width" =:
            "\\begin{tabular}{|p{5cm}r|}One & Two\\\\ \\end{tabular}" =?>
            simpleTable' [AlignLeft,AlignRight] [[plain "One", plain "Two"]]
          , "Table with empty column separators" =:
            "\\begin{tabular}{@{}r@{}l}One & Two\\\\ \\end{tabular}" =?>
            simpleTable' [AlignRight,AlignLeft] [[plain "One", plain "Two"]]
          , "Table with custom column separators" =:
            T.unlines [ "\\begin{tabular}{@{($\\to$)}r@{\\hspace{2cm}}l}"
                    , "One&Two\\\\"
                    , "\\end{tabular}" ] =?>
            simpleTable' [AlignRight,AlignLeft] [[plain "One", plain "Two"]]
          , "Table with vertical alignment argument" =:
            "\\begin{tabular}[t]{r|r}One & Two\\\\ \\end{tabular}" =?>
            simpleTable' [AlignRight,AlignRight] [[plain "One", plain "Two"]]
          , "Table with multicolumn item" =:
            "\\begin{tabular}{l c r}\\multicolumn{2}{c}{One} & Two\\\\ \\end{tabular}" =?>
            table' [AlignLeft, AlignCenter, AlignRight]
                   [ Row nullAttr [ cell AlignCenter (RowSpan 1) (ColSpan 2) (plain "One")
                                  , simpleCell (plain "Two")
                                  ]
                   ]
          , "Table with multirow item" =:
            T.unlines ["\\begin{tabular}{c}"
                      ,"\\multirow{2}{c}{One}\\\\Two\\\\"
                      ,"\\end{tabular}"
                      ] =?>
            table' [AlignCenter]
                  [ Row nullAttr [ cell AlignCenter (RowSpan 2) (ColSpan 1) (plain "One") ]
                  , Row nullAttr [ simpleCell (plain "Two") ]
                  ]
          , "Table with nested multirow/multicolumn item" =:
            T.unlines [ "\\begin{tabular}{c c c}"
                      , "\\multirow{2}{c}{\\multicolumn{2}{c}{One}}&Two\\\\"
                      , "Three\\\\"
                      , "Four&Five&Six\\\\"
                      , "\\end{tabular}"
                      ] =?>
            table' [AlignCenter, AlignCenter, AlignCenter]
                   [ Row nullAttr [ cell AlignCenter (RowSpan 2) (ColSpan 2) (plain "One")
                                  , simpleCell (plain "Two")
                                  ]
                   , Row nullAttr [ simpleCell (plain "Three") ]
                   , Row nullAttr [ simpleCell (plain "Four") 
                                  , simpleCell (plain "Five")
                                  , simpleCell (plain "Six")
                                  ]
                   ]
          , "Table with multicolumn header" =:
            T.unlines [ "\\begin{tabular}{ |l|l| }"
                      , "\\hline\\multicolumn{2}{|c|}{Header}\\\\" 
                      , "\\hline key & val\\\\" 
                      , "\\hline\\end{tabular}"
                      ] =?>
            table emptyCaption
                  (zip [AlignLeft, AlignLeft] (repeat ColWidthDefault))
                  (TableHead nullAttr [ Row nullAttr [cell AlignCenter (RowSpan 1) (ColSpan 2) (plain "Header")]])
                  [TableBody nullAttr 0 [] [Row nullAttr [ simpleCell (plain "key")
                                                         , simpleCell (plain "val")
                                                         ]
                                           ]
                  ]
                  (TableFoot nullAttr [])
          ]

        , testGroup "citations"
          [ natbibCitations
          , biblatexCitations
          ]

        , testGroup "images"
          [ "Basic image" =:
            "\\includegraphics{foo.png}" =?>
            para (image "foo.png" "" (text "image"))
          , "Basic image with blank options" =:
            "\\includegraphics[]{foo.png}" =?>
            para (image "foo.png" "" (text "image"))
          , "Image with both width and height" =:
            "\\includegraphics[width=17cm,height=5cm]{foo.png}" =?>
            para (imageWith ("", [], [("width", "17cm"), ("height", "5cm")]) "foo.png" "" "image")
          , "Image with width and height and a bunch of other options" =:
            "\\includegraphics[width=17cm,height=5cm,clip,keepaspectratio]{foo.png}" =?>
            para (imageWith ("", [], [("width", "17cm"), ("height", "5cm")]) "foo.png" "" "image")
          , "Image with just width" =:
            "\\includegraphics[width=17cm]{foo.png}" =?>
            para (imageWith ("", [], [("width", "17cm")]) "foo.png" "" "image")
          , "Image with just height" =:
            "\\includegraphics[height=17cm]{foo.png}" =?>
            para (imageWith ("", [], [("height", "17cm")]) "foo.png" "" "image")
          , "Image width relative to textsize" =:
            "\\includegraphics[width=0.6\\textwidth]{foo.png}" =?>
            para (imageWith ("", [], [("width", "60%")]) "foo.png" "" "image")
          , "Image with options with spaces" =:
            "\\includegraphics[width=12cm, height = 5cm]{foo.png}" =?>
            para (imageWith ("", [], [("width", "12cm"), ("height", "5cm")]) "foo.png" "" "image")
          ]

        , let hex = ['0'..'9']++['a'..'f'] in
          testGroup "Character Escapes"
          [ "Two-character escapes" =:
            mconcat ["^^" <> T.pack [i,j] | i <- hex, j <- hex] =?>
            para (str $ T.pack ['\0'..'\255'])
          , "One-character escapes" =:
            mconcat ["^^" <> T.pack [i] | i <- hex] =?>
            para (str $ T.pack $ ['p'..'y']++['!'..'&'])
          ]
        , testGroup "memoir scene breaks"
          [ "plainbreak" =:
            "hello\\plainbreak{2}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "plainbreak*" =:
            "hello\\plainbreak*{2}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "fancybreak" =:
            "hello\\fancybreak{b r e a k}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "fancybreak*" =:
            "hello\\fancybreak*{b r e a k}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "plainfancybreak" =:
            "hello\\plainfancybreak{4}{2}{b r e a k}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "plainfancybreak*" =:
            "hello\\plainfancybreak*{4}{2}{b r e a k}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "pfbreak" =:
            "hello\\pfbreak{}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          , "pfbreak*" =:
            "hello\\pfbreak*{}goodbye" =?>
            para (str "hello") <> horizontalRule <> para (str "goodbye")
          ]
        , testGroup "biblatex roman numerals"
          [ "upper" =:
            "number \\RN{12}" =?>
            para (str "number" <> space <> str "XII")
          , "lower" =:
            "number \\Rn{29}" =?>
            para (str "number" <> space <> str "xxix")
          , "leading zero" =:
            "\\Rn{014}" =?>
            para (str "xiv")
          , "surrounding spaces" =:
            "number \\Rn{ 41 }" =?>
            para (str "number" <> space <> str "xli")
          , "zero" =:
            "\\RN{0}" =?>
            para (str "")
          , "space then unbraced argument" =:
            "\\RN 7 ok" =?>
            para (str "VII" <> space <> str "ok")
          , "space before braced argument" =:
            "\\Rn {13}ok" =?>
            para (str "xiiiok")
          ]
        , testGroup "polyglossia language spans"
          [ "french" =:
            "hello \\textfrench{bonjour}" =?>
            para (str "hello" <> space <> spanWith ("", [], [("lang", "fr")]) (str "bonjour"))
          , "nested" =:
            "\\textfrench{quelle c'est \\textlatin{primus}?}" =?>
            para (spanWith ("", [], [("lang", "fr")]) $
                    str "quelle" <> space <> str "c\8217est" <> space <>
                    spanWith ("", [], [("lang", "la")]) (str "primus") <> str "?")
          , "with formatting" =:
            "\\textgerman{wie \\emph{spaet} ist es?}" =?>
            para (spanWith ("", [], [("lang", "de")]) $
                    str "wie" <> space <> emph (str "spaet") <> space <> str "ist" <> space <> str "es?")
          , "language options" =:
            "\\textgerman[variant=swiss]{hoechdeutsche}" =?>
            para (spanWith ("", [], [("lang", "de-CH")]) $ str "hoechdeutsche")
          , "unknown option fallback" =:
            "\\textgerman[variant=moon]{ueberhoechdeutsche}" =?>
            para (spanWith ("", [], [("lang", "de")]) $ str "ueberhoechdeutsche")
          ]
        ]

baseCitation :: Citation
baseCitation = Citation{ citationId      = "item1"
                       , citationPrefix  = []
                       , citationSuffix  = []
                       , citationMode    = AuthorInText
                       , citationNoteNum = 0
                       , citationHash    = 0
                       }

rt :: String -> Inlines
rt = rawInline "latex" . T.pack

natbibCitations :: TestTree
natbibCitations = testGroup "natbib"
  [ "citet" =: "\\citet{item1}"
    =?> para (cite [baseCitation] (rt "\\citet{item1}"))
  , "suffix" =: "\\citet[p.~30]{item1}"
    =?> para
        (cite [baseCitation{ citationSuffix = toList $ text "p.\160\&30" }] (rt "\\citet[p.~30]{item1}"))
  , "suffix long" =: "\\citet[p.~30, with suffix]{item1}"
    =?> para (cite [baseCitation{ citationSuffix =
                       toList $ text "p.\160\&30, with suffix" }] (rt "\\citet[p.~30, with suffix]{item1}"))
  , "multiple" =: "\\citeauthor{item1} \\citetext{\\citeyear{item1}; \\citeyear[p.~30]{item2}; \\citealp[see also][]{item3}}"
    =?> para (cite [baseCitation{ citationMode = AuthorInText }
                   ,baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&30"]
                                , citationId = "item2" }
                   ,baseCitation{ citationId = "item3"
                                , citationPrefix = [Str "see",Space,Str "also"]
                                , citationMode = NormalCitation }
                   ] (rt "\\citetext{\\citeyear{item1}; \\citeyear[p.~30]{item2}; \\citealp[see also][]{item3}}"))
  , "group" =: "\\citetext{\\citealp[see][p.~34--35]{item1}; \\citealp[also][chap. 3]{item3}}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Str "see"]
                                , citationSuffix = [Str "p.\160\&34\8211\&35"] }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationId = "item3"
                                , citationPrefix = [Str "also"]
                                , citationSuffix = [Str "chap.",Space,Str "3"] }
                   ] (rt "\\citetext{\\citealp[see][p.~34--35]{item1}; \\citealp[also][chap. 3]{item3}}"))
  , "suffix and locator" =: "\\citep[pp.~33, 35--37, and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "pp.\160\&33,",Space,Str "35\8211\&37,",Space,Str "and",Space,Str "nowhere",Space, Str "else"] }] (rt "\\citep[pp.~33, 35--37, and nowhere else]{item1}"))
  , "suffix only" =: "\\citep[and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = toList $ text "and nowhere else" }] (rt "\\citep[and nowhere else]{item1}"))
  , "no author" =: "\\citeyearpar{item1}, and now Doe with a locator \\citeyearpar[p.~44]{item2}"
    =?> para (cite [baseCitation{ citationMode = SuppressAuthor }] (rt "\\citeyearpar{item1}") <>
              text ", and now Doe with a locator " <>
              cite [baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&44"]
                                , citationId = "item2" }] (rt "\\citeyearpar[p.~44]{item2}"))
  , "markup" =: "\\citep[\\emph{see}][p. \\textbf{32}]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Emph [Str "see"]]
                                , citationSuffix = [Str "p.",Space,
                                    Strong [Str "32"]] }] (rt "\\citep[\\emph{see}][p. \\textbf{32}]{item1}"))
  ]

biblatexCitations :: TestTree
biblatexCitations = testGroup "biblatex"
  [ "textcite" =: "\\textcite{item1}"
    =?> para (cite [baseCitation] (rt "\\textcite{item1}"))
  , "suffix" =: "\\textcite[p.~30]{item1}"
    =?> para
        (cite [baseCitation{ citationSuffix = toList $ text "p.\160\&30" }] (rt "\\textcite[p.~30]{item1}"))
  , "suffix long" =: "\\textcite[p.~30, with suffix]{item1}"
    =?> para (cite [baseCitation{ citationSuffix =
                       toList $ text "p.\160\&30, with suffix" }] (rt "\\textcite[p.~30, with suffix]{item1}"))
  , "multiple" =: "\\textcites{item1}[p.~30]{item2}[see also][]{item3}"
    =?> para (cite [baseCitation{ citationMode = AuthorInText }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "p.\160\&30"]
                                , citationId = "item2" }
                   ,baseCitation{ citationId = "item3"
                                , citationPrefix = [Str "see",Space,Str "also"]
                                , citationMode = NormalCitation }
                   ] (rt "\\textcites{item1}[p.~30]{item2}[see also][]{item3}"))
  , "group" =: "\\autocites[see][p.~34--35]{item1}[also][chap. 3]{item3}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Str "see"]
                                , citationSuffix = [Str "p.\160\&34\8211\&35"] }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationId = "item3"
                                , citationPrefix = [Str "also"]
                                , citationSuffix = [Str "chap.",Space,Str "3"] }
                   ] (rt "\\autocites[see][p.~34--35]{item1}[also][chap. 3]{item3}"))
  , "suffix and locator" =: "\\autocite[pp.~33, 35--37, and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "pp.\160\&33,",Space,Str "35\8211\&37,",Space,Str "and",Space,Str "nowhere",Space, Str "else"] }] (rt "\\autocite[pp.~33, 35--37, and nowhere else]{item1}"))
  , "suffix only" =: "\\autocite[and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = toList $ text "and nowhere else" }] (rt "\\autocite[and nowhere else]{item1}"))
  , "no author" =: "\\autocite*{item1}, and now Doe with a locator \\autocite*[p.~44]{item2}"
    =?> para (cite [baseCitation{ citationMode = SuppressAuthor }] (rt "\\autocite*{item1}") <>
              text ", and now Doe with a locator " <>
              cite [baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&44"]
                                , citationId = "item2" }] (rt "\\autocite*[p.~44]{item2}"))
  , "markup" =: "\\autocite[\\emph{see}][p. \\textbf{32}]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Emph [Str "see"]]
                                , citationSuffix = [Str "p.",Space,
                                    Strong [Str "32"]] }] (rt "\\autocite[\\emph{see}][p. \\textbf{32}]{item1}"))
  , "parencite" =: "\\parencite{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation }] (rt "\\parencite{item1}"))
  ]

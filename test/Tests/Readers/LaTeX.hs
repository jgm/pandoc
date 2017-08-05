{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.LaTeX (tests) where

import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Data.Text (Text)
import qualified Data.Text as T

latex :: Text -> Pandoc
latex = purely $ readLaTeX def{
                   readerExtensions = getDefaultExtensions "latex" }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test latex

simpleTable' :: [Alignment] -> [[Blocks]] -> Blocks
simpleTable' aligns = table "" (zip aligns (repeat 0.0))
                      (map (const mempty) aligns)

tests :: [TestTree]
tests = [ testGroup "basic"
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
          ]

        , testGroup "citations"
          [ natbibCitations
          , biblatexCitations
          ]

        , let hex = ['0'..'9']++['a'..'f'] in
          testGroup "Character Escapes"
          [ "Two-character escapes" =:
            mconcat ["^^" <> T.pack [i,j] | i <- hex, j <- hex] =?>
            para (str ['\0'..'\255'])
          , "One-character escapes" =:
            mconcat ["^^" <> T.pack [i] | i <- hex] =?>
            para (str $ ['p'..'y']++['!'..'&'])
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
rt = rawInline "latex"

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

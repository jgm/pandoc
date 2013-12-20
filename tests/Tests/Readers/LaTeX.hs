{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.LaTeX (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc

latex :: String -> Pandoc
latex = readLaTeX def

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test latex

tests :: [Test]
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
            "hi % this is a comment\nthere\n" =?> para "hi there"
          ]

        , testGroup "code blocks"
          [ "identifier" =:
            "\\begin{lstlisting}[label=test]\\end{lstlisting}" =?> codeBlockWith ("test", [], [("label","test")]) ""
          , "no identifier" =:
            "\\begin{lstlisting}\\end{lstlisting}" =?> codeBlock ""
          ]

        , testGroup "citations"
          [ natbibCitations
          , biblatexCitations
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

natbibCitations :: Test
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

biblatexCitations :: Test
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

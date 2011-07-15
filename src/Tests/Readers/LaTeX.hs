{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.LaTeX (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc

latex :: String -> Pandoc
latex = readLaTeX defaultParserState

infix 5 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test latex

tests :: [Test]
tests = [ testGroup "basic"
          [ "simple" =:
            "word" =?> para "word"
          , "space" =:
            "some text" =?> para ("some text")
          , "emphasized" =:
            "\\emph{emphasized}" =?> para (emph "emphasized")
          ]

        , testGroup "headers"
          [ "level 1" =:
            "\\section{header}" =?> header 1 "header"
          , "level 2" =:
            "\\subsection{header}" =?> header 2 "header"
          , "level 3" =:
            "\\subsubsection{header}" =?> header 3 "header"
          , "emph" =:
            "\\section{text \\emph{emph}}" =?>
             header 1 ("text" +++ space +++ emph "emph")
          , "link" =:
            "\\section{text \\href{/url}{link}}" =?>
              header 1 ("text" +++ space +++ link "/url" "" "link")
          ]

        , testGroup "space and comments"
          [ "blank lines + space at beginning" =:
            "\n  \n  hi" =?> para "hi"
          , "blank lines + space + comments" =:
            "% my comment\n\n  \n  % another\n\nhi" =?> para "hi"
          , "comment in paragraph" =:
            "hi % this is a comment\nthere\n" =?> para "hi there"
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
                       , citationHash    = 0 }

natbibCitations :: Test
natbibCitations = testGroup "natbib"
  [ "citet" =: "\\citet{item1}"
    =?> para (cite [baseCitation] empty)
  , "suffix" =: "\\citet[p.~30]{item1}"
    =?> para
        (cite [baseCitation{ citationSuffix = toList $ text "p.\160\&30" }] empty)
  , "suffix long" =: "\\citet[p.~30, with suffix]{item1}"
    =?> para (cite [baseCitation{ citationSuffix =
                       toList $ text "p.\160\&30, with suffix" }] empty)
  , "multiple" =: "\\citeauthor{item1} \\citetext{\\citeyear{item1}; \\citeyear[p.~30]{item2}; \\citealp[see also][]{item3}}"
    =?> para (cite [baseCitation{ citationMode = AuthorInText }
                   ,baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&30"]
                                , citationId = "item2" }
                   ,baseCitation{ citationId = "item3"
                                , citationPrefix = [Str "see",Space,Str "also"]
                                , citationMode = NormalCitation }
                   ] empty)
  , "group" =: "\\citetext{\\citealp[see][p.~34--35]{item1}; \\citealp[also][chap. 3]{item3}}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Str "see"]
                                , citationSuffix = [Str "p.\160\&34",EnDash,Str "35"] }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationId = "item3"
                                , citationPrefix = [Str "also"]
                                , citationSuffix = [Str "chap.",Space,Str "3"] }
                   ] empty)
  , "suffix and locator" =: "\\citep[pp.~33, 35--37, and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "pp.\160\&33,",Space,Str "35",EnDash,Str "37,",Space,Str "and",Space,Str "nowhere",Space, Str "else"] }] empty)
  , "suffix only" =: "\\citep[and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = toList $ text "and nowhere else" }] empty)
  , "no author" =: "\\citeyearpar{item1}, and now Doe with a locator \\citeyearpar[p.~44]{item2}"
    =?> para (cite [baseCitation{ citationMode = SuppressAuthor }] empty +++
              text ", and now Doe with a locator " +++
              cite [baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&44"]
                                , citationId = "item2" }] empty)
  , "markup" =: "\\citep[\\emph{see}][p. \\textbf{32}]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Emph [Str "see"]]
                                , citationSuffix = [Str "p.",Space,
                                    Strong [Str "32"]] }] empty)
  ]

biblatexCitations :: Test
biblatexCitations = testGroup "biblatex"
  [ "textcite" =: "\\textcite{item1}"
    =?> para (cite [baseCitation] empty)
  , "suffix" =: "\\textcite[p.~30]{item1}"
    =?> para
        (cite [baseCitation{ citationSuffix = toList $ text "p.\160\&30" }] empty)
  , "suffix long" =: "\\textcite[p.~30, with suffix]{item1}"
    =?> para (cite [baseCitation{ citationSuffix =
                       toList $ text "p.\160\&30, with suffix" }] empty)
  , "multiple" =: "\\textcites{item1}[p.~30]{item2}[see also][]{item3}"
    =?> para (cite [baseCitation{ citationMode = AuthorInText }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "p.\160\&30"]
                                , citationId = "item2" }
                   ,baseCitation{ citationId = "item3"
                                , citationPrefix = [Str "see",Space,Str "also"]
                                , citationMode = NormalCitation }
                   ] empty)
  , "group" =: "\\autocites[see][p.~34--35]{item1}[also][chap. 3]{item3}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Str "see"]
                                , citationSuffix = [Str "p.\160\&34",EnDash,Str "35"] }
                   ,baseCitation{ citationMode = NormalCitation
                                , citationId = "item3"
                                , citationPrefix = [Str "also"]
                                , citationSuffix = [Str "chap.",Space,Str "3"] }
                   ] empty)
  , "suffix and locator" =: "\\autocite[pp.~33, 35--37, and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = [Str "pp.\160\&33,",Space,Str "35",EnDash,Str "37,",Space,Str "and",Space,Str "nowhere",Space, Str "else"] }] empty)
  , "suffix only" =: "\\autocite[and nowhere else]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationSuffix = toList $ text "and nowhere else" }] empty)
  , "no author" =: "\\autocite*{item1}, and now Doe with a locator \\autocite*[p.~44]{item2}"
    =?> para (cite [baseCitation{ citationMode = SuppressAuthor }] empty +++
              text ", and now Doe with a locator " +++
              cite [baseCitation{ citationMode = SuppressAuthor
                                , citationSuffix = [Str "p.\160\&44"]
                                , citationId = "item2" }] empty)
  , "markup" =: "\\autocite[\\emph{see}][p. \\textbf{32}]{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation
                                , citationPrefix = [Emph [Str "see"]]
                                , citationSuffix = [Str "p.",Space,
                                    Strong [Str "32"]] }] empty)
  , "parencite" =: "\\parencite{item1}"
    =?> para (cite [baseCitation{ citationMode = NormalCitation }] empty)
  ]

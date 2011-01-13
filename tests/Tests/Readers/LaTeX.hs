module Tests.Readers.LaTeX (tests) where

import Text.Pandoc.Definition

import Test.Framework
import Tests.Helpers

tests :: [Test]
tests = [ testGroup "basic" [ latexTest "simplest" "word"               
                              (Inline  $ Str "word") 
                              
                            , latexTest "space"    "some text"          
                              (Inlines $ [Str "some", Space, Str "text"])
                              
                            , latexTest "emphasis" "\\emph{emphasized}" 
                              (Inline  $ Emph [Str "emphasized"])
                            ]

        , testGroup "headers" [ latexTest "1. level"      "\\section{header}"       
                                $ Block $ Header 1 [Str "header"]

                              , latexTest "2. level"      "\\subsection{header}"    
                                $ Block $ Header 2 [Str "header"]

                              , latexTest "3. level"      "\\subsubsection{header}" 
                                $ Block $ Header 3 [Str "header"]

                              , latexTest "with emphasis" "\\section{text \\emph{emph}}"
                                $ Block $ Header 1 [Str "text", Space, Emph [Str "emph"]]

                              , latexTest "with link"     "\\section{text \\href{/url}{link}}"
                                $ Block $ Header 1 [Str "text", Space, Link [Str "link"] ("/url", "")]
                              ]
        ]


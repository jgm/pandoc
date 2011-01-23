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
        ]


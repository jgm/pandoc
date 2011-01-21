module Tests.Readers.LaTeX (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Shared (normalize)

latex :: String -> (String, Pandoc)
latex s = (s, normalize . readLaTeX defaultParserState{stateSmart = True} $ s)

tests :: [Test]
tests = [ testGroup "basic"
          [ "simple" =:
            latex "word" =?> str "word"
          , "space" =:
            latex "some text" =?> text "some text"
          , "emphasized" =:
            latex "\\emph{emphasized}" =?> (emph $ str "emphasized")
          ]

        , testGroup "headers"
          [ "level 1" =:
            latex "\\section{header}" =?> header 1 (str "header")
          , "level 2" =:
            latex "\\subsection{header}" =?> header 2 (str "header")
          , "level 3" =:
            latex "\\subsubsection{header}" =?> header 3 (str "header")
          , "emph" =:
            latex "\\section{text \\emph{emph}}" =?>
              header 1 (str "text" +++ space +++ emph (str "emph"))
          , "link" =:
            latex "\\section{text \\href{/url}{link}}" =?>
              header 1 (str "text" +++ space +++ link "/url" "" (str "link"))
          ]
        ]


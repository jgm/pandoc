{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.LaTeX (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

latex :: (ToString a, ToPandoc a) => a -> String
latex = writeLaTeX def . toPandoc

latexListing :: (ToString a, ToPandoc a) => a -> String
latexListing = writeLaTeX def{ writerListings = True } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test latex "my test" $ X =?> Y

which is in turn shorthand for

  test latex "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test latex

tests :: [Test]
tests = [ testGroup "code blocks"
          [ "in footnotes" =: note (para "hi" <> codeBlock "hi") =?>
            "\\footnote{hi\n\n\\begin{Verbatim}\nhi\n\\end{Verbatim}\n}"
          , test latexListing "identifier" $ codeBlockWith ("id",[],[]) "hi" =?>
            ("\\begin{lstlisting}[label=id]\nhi\n\\end{lstlisting}" :: String)
          , test latexListing "no identifier" $ codeBlock "hi" =?>
            ("\\begin{lstlisting}\nhi\n\\end{lstlisting}" :: String)
          ]
        , testGroup "math"
          [ "escape |" =: para (math "\\sigma|_{\\{x\\}}") =?>
            "$\\sigma|_{\\{x\\}}$"
          ]
        , testGroup "headers"
          [ "unnumbered header" =:
            headerWith ("foo",["unnumbered"],[]) 1
              (text "Header 1" <> note (plain $ text "note")) =?>
            "\\section*{Header 1\\footnote{note}}\\label{foo}\n\\addcontentsline{toc}{section}{Header 1}\n"
          ]
        ]

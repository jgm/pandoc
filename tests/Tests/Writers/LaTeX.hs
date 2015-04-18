{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.LaTeX (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

latex :: (ToString a, ToPandoc a) => a -> String
latex = writeLaTeX def{ writerHighlight = True } . toPandoc

latexListing :: (ToString a, ToPandoc a) => a -> String
latexListing = writeLaTeX def{ writerListings = True } . toPandoc

latexBiblatex :: (ToString a, ToPandoc a) => a -> String
latexBiblatex = writeLaTeX def{ writerCiteMethod = Biblatex } . toPandoc

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
        , testGroup "definition lists"
          [ "with internal link" =: definitionList [(link "#go" "" (str "testing"),
             [plain (text "hi there")])] =?>
            "\\begin{description}\n\\tightlist\n\\item[{\\hyperref[go]{testing}}]\nhi there\n\\end{description}"
          ]
        , testGroup "math"
          [ "escape |" =: para (math "\\sigma|_{\\{x\\}}") =?>
            "\\(\\sigma|_{\\{x\\}}\\)"
          ]
        , testGroup "headers"
          [ "unnumbered header" =:
            headerWith ("foo",["unnumbered"],[]) 1
              (text "Header 1" <> note (plain $ text "note")) =?>
            "\\section*{\\texorpdfstring{Header 1\\footnote{note}}{Header 1}}\\label{foo}\n\\addcontentsline{toc}{section}{Header 1}\n"
          , "in list item" =:
            bulletList [header 2 (text "foo")] =?>
            "\\begin{itemize}\n\\item ~\n  \\subsection{foo}\n\\end{itemize}"
          , "in definition list item" =:
            definitionList [(text "foo", [header 2 (text "bar"),
                                          para $ text "baz"])] =?>
            "\\begin{description}\n\\item[foo] ~ \n\\subsection{bar}\n\nbaz\n\\end{description}"
          , "containing image" =:
            header 1 (image "imgs/foo.jpg" "" (text "Alt text")) =?>
            "\\section{\\texorpdfstring{\\protect\\includegraphics{imgs/foo.jpg}}{Alt text}}"
          ]
        , testGroup "inline code"
          [ "struck out and highlighted" =:
            strikeout (codeWith ("",["haskell"],[]) "foo" <> space
              <> str "bar") =?>
            "\\sout{\\mbox{\\VERB|\\NormalTok{foo}|} bar}"
          , "struck out and not highlighted" =:
            strikeout (code "foo" <> space
              <> str "bar") =?>
            "\\sout{\\texttt{foo} bar}"
          , "single quotes" =:
              code "dog's" =?> "\\texttt{dog\\textquotesingle{}s}"
          ]
        , let c p s i = Citation i p s NormalCitation 0 0
              c' = c [] []
              idlst = map $ ("c"++) . show
              clist p s = map (c p s) . idlst
          in testGroup "BibLaTeX citations"
          [ test latexBiblatex "Regular citation" $
              cite [c' "cite"] (str "@cite") =?>
              ("\\autocite{cite}" :: String)
          , test latexBiblatex "Citation list" $
              cite (clist [] [] [1..5::Int]) (str "@cite") =?>
              ("\\autocites{c1,c2,c3,c4,c5}" :: String)
          , test latexBiblatex "Group by prefix" $
              cite (clist [Str "prefix1"] [] [1..3::Int] ++
                    clist [] [] [6..7::Int] ++
                    clist [Str "prefix1"] [] [4..5::Int]
                   ) (str "@cite") =?>
              ("\\autocites[prefix1][]{c1,c2,c3,c4,c5}{c6,c7}" :: String)
          , test latexBiblatex "Group by suffix" $
              cite (clist [Str "p1"] [Str "s1"] [1..3::Int] ++
                    clist [Str "p1"] [] [4..5::Int] ++
                    clist [] [] [6..7::Int]
                   ) (str "@cite") =?>
              ("\\autocites[p1][s1]{c1,c2,c3}[p1][]{c4,c5}{c6,c7}" :: String)
          ]
        ]

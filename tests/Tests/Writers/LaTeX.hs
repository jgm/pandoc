{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.LaTeX (tests) where

import Test.Framework
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

latex :: (ToPandoc a) => a -> String
latex = latexWithOpts def{ writerHighlight = True }

latexListing :: (ToPandoc a) => a -> String
latexListing = latexWithOpts def{ writerListings = True }

latexWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
latexWithOpts opts = writeLaTeX opts . toPandoc

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
            "\\begin{description}\n\\tightlist\n\\item[{\\protect\\hyperlink{go}{testing}}]\nhi there\n\\end{description}"
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
          , "backtick" =:
              code "`nu?`" =?> "\\texttt{\\textasciigrave{}nu?\\textasciigrave{}}"
          ]
        , testGroup "writer options"
          [ testGroup "top-level division" $
            let
              headers =  header 1 (text "header1")
                      <> header 2 (text "header2")
                      <> header 3 (text "header3")

              latexTopLevelDiv :: (ToPandoc a) => TopLevelDivision -> a -> String
              latexTopLevelDiv division =
                latexWithOpts def{ writerTopLevelDivision = division }

              beamerTopLevelDiv :: (ToPandoc a)
                                => TopLevelDivision -> a -> String
              beamerTopLevelDiv division =
                latexWithOpts def { writerTopLevelDivision = division
                                  , writerBeamer = True }
            in
            [ test (latexTopLevelDiv TopLevelSection)
                   "sections as top-level" $ headers =?>
              unlines [ "\\section{header1}\n"
                      , "\\subsection{header2}\n"
                      , "\\subsubsection{header3}"
                      ]
            , test (latexTopLevelDiv TopLevelChapter)
                   "chapters as top-level" $ headers =?>
              unlines [ "\\chapter{header1}\n"
                      , "\\section{header2}\n"
                      , "\\subsection{header3}"
                      ]
            , test (latexTopLevelDiv TopLevelPart)
                   "parts as top-level" $ headers =?>
              unlines [ "\\part{header1}\n"
                      , "\\chapter{header2}\n"
                      , "\\section{header3}"
                      ]
            , test (latexTopLevelDiv TopLevelDefault)
                   "default top-level" $ headers =?>
              unlines [ "\\section{header1}\n"
                      , "\\subsection{header2}\n"
                      , "\\subsubsection{header3}"
                      ]
            , test (beamerTopLevelDiv TopLevelSection)
                   "sections as top-level in beamer" $ headers =?>
              unlines [ "\\section{header1}\n"
                      , "\\subsection{header2}\n"
                      , "\\subsubsection{header3}"
                      ]
            , test (beamerTopLevelDiv TopLevelChapter)
                   "chapters are as part in beamer" $ headers =?>
              unlines [ "\\part{header1}\n"
                      , "\\section{header2}\n"
                      , "\\subsection{header3}"
                      ]
            , test (beamerTopLevelDiv TopLevelPart)
                   "parts as top-level in beamer" $ headers =?>
              unlines [ "\\part{header1}\n"
                      , "\\section{header2}\n"
                      , "\\subsection{header3}"
                      ]
            , test (beamerTopLevelDiv TopLevelDefault)
                   "default top-level in beamer" $ headers =?>
              unlines [ "\\section{header1}\n"
                      , "\\subsection{header2}\n"
                      , "\\subsubsection{header3}"
                      ]
            ]
          ]
        ]

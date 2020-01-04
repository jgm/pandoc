{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.LaTeX (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

latex :: (ToPandoc a) => a -> String
latex = latexWithOpts def

latexListing :: (ToPandoc a) => a -> String
latexListing = latexWithOpts def{ writerListings = True }

latexWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
latexWithOpts opts = unpack . purely (writeLaTeX opts) . toPandoc

beamerWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
beamerWithOpts opts = unpack . purely (writeBeamer opts) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test latex "my test" $ X =?> Y

which is in turn shorthand for

  test latex "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test latex

tests :: [TestTree]
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
            "\\hypertarget{foo}{%\n\\section*{\\texorpdfstring{Header 1\\footnote{note}}{Header 1}}\\label{foo}}\n\\addcontentsline{toc}{section}{Header 1}\n"
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
            "\\sout{\\mbox{\\texttt{foo}} bar}"
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
                beamerWithOpts def { writerTopLevelDivision = division }
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
            , test (latexTopLevelDiv TopLevelPart)
                   "part top-level, section not in toc" $
                   (   headerWith ("", ["unnumbered"], []) 1 (text "header1")
                    <> headerWith ("", ["unnumbered"], []) 2 (text "header2")
                    <> headerWith ("", ["unnumbered"], []) 3 (text "header3")
                    <> headerWith ("", ["unnumbered"], []) 4 (text "header4")
                    <> headerWith ("", ["unnumbered"], []) 5 (text "header5")
                    <> headerWith ("", ["unnumbered"], []) 6 (text "header6"))
                   =?>
              unlines [ "\\part*{header1}"
                      , "\\addcontentsline{toc}{part}{header1}\n"
                      , "\\chapter*{header2}"
                      , "\\addcontentsline{toc}{chapter}{header2}\n"
                      , "\\section*{header3}"
                      , "\\addcontentsline{toc}{section}{header3}\n"
                      , "\\subsection*{header4}"
                      , "\\addcontentsline{toc}{subsection}{header4}\n"
                      , "\\subsubsection*{header5}"
                      , "\\addcontentsline{toc}{subsubsection}{header5}\n"
                      , "\\paragraph*{header6}"
                      , "\\addcontentsline{toc}{paragraph}{header6}"
                      ]
            ]
          ]
        ]

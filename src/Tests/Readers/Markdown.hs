{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Readers.Markdown (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
-- import Text.Pandoc.Shared ( normalize )
import Text.Pandoc
import Data.Sequence (singleton)

markdown :: String -> Pandoc
markdown = readMarkdown defaultParserState{ stateStandalone = True }

markdownSmart :: String -> Pandoc
markdownSmart = readMarkdown defaultParserState{ stateSmart = True }

infix 5 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test markdown

{-
p_markdown_round_trip :: Block -> Bool
p_markdown_round_trip b = matches d' d''
  where d'  = normalize $ Pandoc (Meta [] [] []) [b]
        d'' = normalize
              $ readMarkdown defaultParserState{ stateSmart = True }
              $ writeMarkdown defaultWriterOptions d'
        matches (Pandoc _ [Plain []]) (Pandoc _ []) = True
        matches (Pandoc _ [Para []]) (Pandoc _ []) = True
        matches (Pandoc _ [Plain xs]) (Pandoc _ [Para xs']) = xs == xs'
        matches x y = x == y
-}

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with attribute" =:
            "`document.write(\"Hello\");`{.javascript}"
            =?> para
                (codeWith ("",["javascript"],[]) "document.write(\"Hello\");")
          , "with attribute space" =:
            "`*` {.haskell .special x=\"7\"}"
            =?> para (codeWith ("",["haskell","special"],[("x","7")]) "*")
          ]
        , testGroup "smart punctuation"
          [ test markdownSmart "quote before ellipses"
            ("'...hi'"
            =?> para (singleQuoted (singleton Ellipses +++ "hi")))
          ]
        , testGroup "mixed emphasis and strong"
          [ "emph and strong emph alternating" =:
            "*xxx* ***xxx*** xxx\n*xxx* ***xxx*** xxx"
            =?> para (emph "xxx" +++ space +++ strong (emph "xxx") +++
                      space +++ "xxx" +++ space +++
                      emph "xxx" +++ space +++ strong (emph "xxx") +++
                      space +++ "xxx")
          , "emph with spaced strong" =:
            "*x **xx** x*"
            =?> para (emph ("x" +++ space +++ strong "xx" +++ space +++ "x"))
          ]
        , testGroup "footnotes"
          [ "indent followed by newline and flush-left text" =:
            "[^1]\n\n[^1]: my note\n\n     \nnot in note\n"
            =?> para (note (para "my note")) +++ para "not in note"
          , "indent followed by newline and indented text" =:
            "[^1]\n\n[^1]: my note\n     \n    in note\n"
            =?> para (note (para "my note" +++ para "in note"))
          , "recursive note" =:
            "[^1]\n\n[^1]: See [^1]\n"
            =?> para (note (para "See [^1]"))
          ]
        , testGroup "lhs"
          [ test (readMarkdown defaultParserState{stateLiterateHaskell = True})
              "inverse bird tracks and html" $
              "> a\n\n< b\n\n<div>\n"
              =?> codeBlockWith ("",["sourceCode","literate","haskell"],[]) "a"
                  +++
                  codeBlockWith ("",["sourceCode","haskell"],[]) "b"
                  +++
                  rawBlock "html" "<div>\n\n"
          ]
-- the round-trip properties frequently fail
--        , testGroup "round trip"
--          [ property "p_markdown_round_trip" p_markdown_round_trip
--          ]
        ]

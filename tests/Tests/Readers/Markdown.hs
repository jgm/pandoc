{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Readers.Markdown (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc

markdown :: String -> Pandoc
markdown = readMarkdown defaultParserState{ stateStandalone = True }

infix 5 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test markdown

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
        ]

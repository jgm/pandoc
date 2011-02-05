{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Writers.HTML (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Highlighting (languages) -- null if no hl support

html :: (ToString a, ToPandoc a) => a -> String
html = writeHtmlString defaultWriterOptions{ writerWrapText = False } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 5 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test html

tests :: [Test]
tests = [ testGroup "inline code"
          [ "basic" =: code "@&" =?> "<code>@&amp;</code>"
          , "haskell" =: codeWith ("",["haskell"],[]) ">>="
            =?> if null languages
                   then "<code class=\"haskell\">&gt;&gt;=</code>"
                   else "<code class=\"sourceCode haskell\"><span class=\"fu\">&gt;&gt;=</span></code>"
          , "nolanguage" =: codeWith ("",["nolanguage"],[]) ">>="
            =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
          ]
        , testGroup "images"
          [ "alt with formatting" =:
            image "/url" "title" ("my " +++ emph "image")
            =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
          ]
        ]

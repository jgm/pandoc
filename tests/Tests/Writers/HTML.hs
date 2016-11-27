{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.HTML (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Text.Pandoc.Arbitrary()

html :: (ToPandoc a) => a -> String
html = purely (writeHtmlString def{ writerWrapText = WrapNone }) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test html

tests :: [Test]
tests = [ testGroup "inline code"
          [ "basic" =: code "@&" =?> "<code>@&amp;</code>"
          , "haskell" =: codeWith ("",["haskell"],[]) ">>="
            =?> "<code class=\"haskell\">&gt;&gt;=</code>"
          , "nolanguage" =: codeWith ("",["nolanguage"],[]) ">>="
            =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
          ]
        , testGroup "images"
          [ "alt with formatting" =:
            image "/url" "title" ("my " <> emph "image")
            =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
          ]
        ]

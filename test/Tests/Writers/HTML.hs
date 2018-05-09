{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.HTML (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

html :: (ToPandoc a) => a -> String
html = unpack . purely (writeHtml4String def{ writerWrapText = WrapNone }) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test html

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ "basic" =: code "@&" =?> "<code>@&amp;</code>"
          , "haskell" =: codeWith ("",["haskell"],[]) ">>="
            =?> "<code class=\"sourceCode haskell\"><span class=\"fu\">&gt;&gt;=</span></code>"
          , "nolanguage" =: codeWith ("",["nolanguage"],[]) ">>="
            =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
          ]
        , testGroup "images"
          [ "alt with formatting" =:
            image "/url" "title" ("my " <> emph "image")
            =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
          ]
        ]

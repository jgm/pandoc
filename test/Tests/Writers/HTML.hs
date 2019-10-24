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

htmlQTags :: (ToPandoc a) => a -> String
htmlQTags = unpack
  . purely (writeHtml4String def{ writerWrapText = WrapNone, writerHtmlQTags = True })
  . toPandoc

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
            =?> "<code class=\"sourceCode haskell\"><span class=\"op\">&gt;&gt;=</span></code>"
          , "nolanguage" =: codeWith ("",["nolanguage"],[]) ">>="
            =?> "<code class=\"nolanguage\">&gt;&gt;=</code>"
          ]
        , testGroup "images"
          [ "alt with formatting" =:
            image "/url" "title" ("my " <> emph "image")
            =?> "<img src=\"/url\" title=\"title\" alt=\"my image\" />"
          ]
        , testGroup "blocks"
          [ "definition list with empty <dt>" =:
            definitionList [(mempty, [para $ text "foo bar"])]
            =?> "<dl><dt></dt><dd><p>foo bar</p></dd></dl>"
          ]
        , testGroup "quotes"
          [ "quote with cite attribute (without q-tags)" =:
            doubleQuoted (spanWith ("", [], [("cite", "http://example.org")]) (str "examples"))
            =?> "“<span cite=\"http://example.org\">examples</span>”"
          , tQ "quote with cite attribute (with q-tags)" $
            doubleQuoted (spanWith ("", [], [("cite", "http://example.org")]) (str "examples"))
            =?> "<q cite=\"http://example.org\">examples</q>"
          ]
        ]
        where
          tQ :: (ToString a, ToPandoc a)
               => String -> (a, String) -> TestTree
          tQ = test htmlQTags

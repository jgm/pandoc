{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.JATS (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jats :: (ToPandoc a) => a -> String
jats = unpack . purely (writeJATS def{ writerWrapText = WrapNone }) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test jats "my test" $ X =?> Y

which is in turn shorthand for

  test jats "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test jats

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ "basic" =: code "@&" =?> "<p>\n  <monospace>@&amp;</monospace>\n</p>"
          ]
        , testGroup "images"
          [ "basic" =:
            image "/url" "title" mempty
            =?> "<graphic mimetype=\"image\" mime-subtype=\"\" xlink:href=\"/url\" xlink:title=\"title\" />"
          ]
        ]



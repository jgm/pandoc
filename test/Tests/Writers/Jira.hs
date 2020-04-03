{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Jira (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jira :: (ToPandoc a) => a -> String
jira = unpack . purely (writeJira def) . toPandoc

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test jira

tests :: [TestTree]
tests =
  [ testGroup "inlines"
    [ "underlined text" =:
      spanWith ("ignored", ["ignored", "underline"], [("foo", "bar")])
               "underlined text" =?>
      "+underlined text+"

    , "image with attributes" =:
      imageWith ("", [], [("align", "right"), ("height", "50")])
                "image.png" "" mempty =?>
      "!image.png|align=right, height=50!"
    ]
  ]

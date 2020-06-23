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
      underline "underlined text" =?>
      "+underlined text+"

    , "image with attributes" =:
      imageWith ("", [], [("align", "right"), ("height", "50")])
                "image.png" "" mempty =?>
      "!image.png|align=right, height=50!"

    , testGroup "links"
      [ "external link" =:
        link "https://example.com/test.php" "" "test" =?>
        "[test|https://example.com/test.php]"

      , "external link without description" =:
        link "https://example.com/tmp.js" "" "https://example.com/tmp.js" =?>
        "[https://example.com/tmp.js]"

      , "email link" =:
        link "mailto:me@example.com" "" "Jane" =?>
        "[Jane|mailto:me@example.com]"

      , "email link without description" =:
        link "mailto:me@example.com" "" "me@example.com" =?>
        "[mailto:me@example.com]"

      , "attachment link" =:
        linkWith ("", ["attachment"], []) "foo.txt" "" "My file" =?>
        "[My file^foo.txt]"

      , "attachment link without description" =:
        linkWith ("", ["attachment"], []) "foo.txt" "" "foo.txt" =?>
        "[^foo.txt]"

      , "user link" =:
        linkWith ("", ["user-account"], []) "~johndoe" "" "John Doe" =?>
        "[John Doe|~johndoe]"

      , "user link with user as description" =:
        linkWith ("", ["user-account"], []) "~johndoe" "" "~johndoe" =?>
        "[~johndoe]"
      ]
    ]
  ]

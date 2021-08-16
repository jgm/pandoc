{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Jira (tests) where

import Data.Text (unpack)
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jira :: (ToPandoc a) => a -> String
jira = unpack . purely (writeJira def) . toPandoc

infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
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

      , "'smart' link" =:
        para (linkWith ("", ["smart-link"], []) "http://example.com" "" "x") =?>
        "[x|http://example.com|smart-link]"

      , "'smart' card" =:
        para (linkWith ("", ["smart-card"], []) "http://example.org" "" "x") =?>
        "[x|http://example.org|smart-card]"
      ]

    , testGroup "spans"
      [ "id is used as anchor" =:
        spanWith ("unicorn", [], []) (str "Unicorn") =?>
        "{anchor:unicorn}Unicorn"

      , "use `color` attribute" =:
        spanWith ("",[],[("color","red")]) "ruby" =?>
        "{color:red}ruby{color}"
      ]

    , testGroup "code"
      [ "code block with known language" =:
        codeBlockWith ("", ["java"], []) "Book book = new Book(\"Algebra\")" =?>
        "{code:java}\nBook book = new Book(\"Algebra\")\n{code}"

      , "code block without language" =:
        codeBlockWith ("", [], []) "preformatted\n  text.\n" =?>
        "{noformat}\npreformatted\n  text.\n{noformat}"
      ]
    ]

  , testGroup "blocks"
    [ testGroup "div"
      [ "empty attributes" =:
        divWith nullAttr (para "interesting text") =?>
        "interesting text"

      , "just identifier" =:
        divWith ("a", [], []) (para "interesting text") =?>
        "{anchor:a}interesting text"

      , "with class 'panel'" =:
        divWith ("", ["panel"], []) (para "Contents!") =?>
        "{panel}\nContents\\!\n{panel}\n"

      , "panel with id" =:
        divWith ("b", ["panel"], []) (para "text") =?>
        "{panel}\n{anchor:b}text\n{panel}\n"

      , "title attribute" =:
        divWith ("", [], [("title", "Gimme!")]) (para "Contents!") =?>
        "{panel:title=Gimme!}\nContents\\!\n{panel}\n"

      , "nested panels" =:
        let panelAttr = ("", ["panel"], [])
        in divWith panelAttr (para "hi" <>
                              divWith panelAttr (para "wassup?")) =?>
        "{panel}\nhi\n\nwassup?\n{panel}\n"
      ]
    ]
  ]

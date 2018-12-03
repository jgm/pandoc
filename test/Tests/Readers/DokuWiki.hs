{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Readers.DokuWiki (tests) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared (underlineSpan)

dokuwiki :: Text -> Pandoc
dokuwiki = purely $ readDokuWiki def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test dokuwiki

tests :: [TestTree]
tests = [ testGroup "inlines"
          [ "Bold" =:
            "**bold**" =?>
            para (strong "bold")
          , "Italic" =:
            "//italic//" =?>
            para (emph "italic")
          , "Underlined" =:
            "__underlined__" =?>
            para (underlineSpan "underlined")
          , "Monospaced" =:
            "''monospaced''" =?>
            para (code "monospaced")
          , "Combined" =:
            "**__//''combine''//__**" =?>
            para (strong $ underlineSpan $ emph $ code "combine")
          , "Nowiki" =:
            T.unlines [ "<nowiki>"
                      , "This is some text which contains addresses like this: http://www.splitbrain.org and **formatting**, but nothing is done with it."
                      , "</nowiki>"
                      ] =?>
            para "This is some text which contains addresses like this: http://www.splitbrain.org and **formatting**, but nothing is done with it."
          , "Percent" =:
            "The same is true for %%//__this__ text// with a smiley ;-)%%." =?>
            para "The same is true for //__this__ text// with a smiley ;-)."
          , "Subscript" =:
            "<sub>subscript</sub>" =?>
            para (subscript "subscript")
          , "Superscript" =:
            "<sup>superscript</sup>" =?>
            para (superscript "superscript")
          , "Deleted" =:
            "<del>deleted</del>" =?>
            para (strikeout "deleted")
          , "Inline HTML" =:
            "<html>\nThis is some <span style=\"color:red;font-size:150%;\">inline HTML</span>\n</html>" =?>
            para (rawInline "html" "\nThis is some <span style=\"color:red;font-size:150%;\">inline HTML</span>\n")
          , "Linebreak" =:
            T.unlines [ "This is some text with some linebreaks\\\\ Note that the"
                      , "two backslashes are only recognized at the end of a line\\\\"
                      , "or followed by\\\\ a whitespace \\\\this happens without it."
                      ] =?>
            para ("This is some text with some linebreaks" <> linebreak <> "Note that the\n" <>
                  "two backslashes are only recognized at the end of a line" <> linebreak <>
                  "or followed by" <> linebreak <> "a whitespace \\\\this happens without it.")
          , testGroup "External links"
            [ "Autolink" =:
              "http://www.google.com" =?>
              para (link "http://www.google.com" "" (str "http://www.google.com"))
            , "Link" =:
              "[[http://www.google.com|This Link points to google]]" =?>
              para (link "http://www.google.com" "" (text "This Link points to google"))
            , "Email address" =:
              "<andi@splitbrain.org>" =?>
              para (link "mailto:andi@splitbrain.org" "" (str "andi@splitbrain.org"))
            ]
          , "Footnote" =:
            "((This is a footnote))" =?>
            para (note (para "This is a footnote"))
          ]
        , testGroup "Sectioning"
          [ "Headline level 1" =:
            "====== Headline Level 1 ======" =?>
            header 1 "Headline Level 1"
          , "Headline level 2" =:
            "===== Headline Level 2 =====" =?>
            header 2 "Headline Level 2"
          , "Headline level 3" =:
            "==== Headline Level 3 ====" =?>
            header 3 "Headline Level 3"
          , "Headline level 4" =:
            "=== Headline Level 4 ===" =?>
            header 4 "Headline Level 4"
          , "Headline level 5" =:
            "== Headline Level 5 ==" =?>
            header 5 "Headline Level 5"
          ]
        , "Horizontal line" =:
          "----" =?>
          horizontalRule
        , testGroup "Lists"
          [ "Unordered list" =:
            T.unlines [ "  * This is a list"
                      , "  * The second item"
                      , "    * You may have different levels"
                      , "  * Another item"
                      ] =?>
            bulletList [ plain "This is a list"
                       , plain "The second item" <>
                         bulletList [ plain "You may have different levels" ]
                       , plain "Another item"
                       ]
          , "Ordered list" =:
            T.unlines [ "  - The same list but ordered"
                      , "  - Another item"
                      , "    - Just use indention for deeper levels"
                      , "  - That's it"
                      ] =?>
            orderedList [ plain "The same list but ordered"
                        , plain "Another item" <>
                          orderedList [ plain "Just use indention for deeper levels" ]
                        , plain "That's it"
                        ]
          , "Multiline list items" =: -- https://www.dokuwiki.org/faq:lists
            T.unlines [ "  - first item"
                      , "  - second item with linebreak\\\\ second line"
                      , "  - third item with code: <code>"
                      , "some code"
                      , "comes here"
                      , "</code>"
                      , "  - fourth item"
                      ] =?>
            orderedList [ plain "first item"
                        , plain ("second item with linebreak" <> linebreak <> " second line")
                        , plain ("third item with code: " <> code "\nsome code\ncomes here\n")
                        , plain "fourth item"
                        ]
          ]
        , "Block HTML" =:
          T.unlines [ "<HTML>"
                    , "<p style=\"border:2px dashed red;\">And this is some block HTML</p>"
                    , "</HTML>"
                    ] =?>
          rawBlock "html" "\n<p style=\"border:2px dashed red;\">And this is some block HTML</p>\n"
        , "Quote" =:
          T.unlines [ "> foo"
                    , "> bar"
                    , ">> baz"
                    , "> bat"
                    ] =?>
          blockQuote (plain ("foo" <> linebreak <> "bar") <>
                      blockQuote (plain "bat") <>
                      plain ("baz"))
        ]

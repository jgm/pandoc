{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Tests.Readers.DokuWiki
   Copyright   : © 2018-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov
   Stability   : alpha
   Portability : portable

Tests for DokuWiki reader.
-}
module Tests.Readers.DokuWiki (tests) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

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
            para (underline "underlined")
          , "Monospaced" =:
            "''monospaced''" =?>
            para (code "monospaced")
          , "Monospaced with nowiki" =:
            "''%%monospaced%%''" =?>
            para (code "monospaced")
          , "Combined" =:
            "**__//''combine''//__**" =?>
            para (strong $ underline $ emph $ code "combine")
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
          , "Inline code" =:
            "foo <code java>public static void main</code> bar" =?>
            para (text "foo " <> codeWith ("", ["java"], []) "public static void main" <> text " bar")
          , "Inline file" =:
            "foo <file></code></file> bar" =?>
            para (text "foo " <> code "</code>" <> text " bar")
          , "Inline HTML" =:
            "<html>\nThis is some <span style=\"color:red;font-size:150%;\">inline HTML</span>\n</html>" =?>
            para (rawInline "html" "\nThis is some <span style=\"color:red;font-size:150%;\">inline HTML</span>\n")
          , "Inline PHP" =:
            "<php>echo '<p>Hello World</p>';</php>" =?>
            para (codeWith ("", ["php"], []) "echo '<p>Hello World</p>';")
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
            , "Link without description" =:
              "[[https://example.com]]" =?>
              para (link "https://example.com" "" (str "https://example.com"))
            , "Link with description" =:
              "[[http://www.google.com|This Link points to google]]" =?>
              para (link "http://www.google.com" "" (text "This Link points to google"))
            , "Trim whitespace around link and description" =:
              "[[   http://www.google.com    |   This Link points to google   ]]" =?>
              para (link "http://www.google.com" "" (text "This Link points to google"))
            , "Email address" =:
              "<andi@splitbrain.org>" =?>
              para (link "mailto:andi@splitbrain.org" "" (str "andi@splitbrain.org"))
            ]
          , testGroup "Internal links"
            [ "Current namespace" =:
              "[[example]]" =?>
              para (link "example" "" (str "example"))
            , "Current namespace starting with dot" =:
              "[[.example]]" =?>
              para (link "example" "" (str ".example"))
            , "Current namespace starting with dot and colon" =:
              "[[.:example]]" =?>
              para (link "example" "" (str "example"))
            , "Root namespace" =:
              "[[:example]]" =?>
              para (link "/example" "" (str "example"))
            , "Parent namespace" =:
              "[[..example]]" =?>
              para (link "../example" "" (str "..example"))
            , "Parent namespace with colon" =:
              "[[..:example]]" =?>
              para (link "../example" "" (str "example"))
            , "Beneath the root namespace" =:
              "[[wiki:example]]" =?>
              para (link "/wiki/example" "" (str "example"))
            , "Explicitly beneath the root namespace" =:
              "[[:wiki:example]]" =?>
              para (link "/wiki/example" "" (str "example"))
            ]
          , testGroup "Interwiki links"
            [ "Interwiki without description" =:
              "[[doku>DokuWiki]]" =?>
              para (link "https://www.dokuwiki.org/DokuWiki" "" (str "DokuWiki"))
            , "Interwiki link with description" =:
              "[[doku>toolbar|quickbuttons]]" =?>
              para (link "https://www.dokuwiki.org/toolbar" "" (str "quickbuttons"))
            ]
          , "Footnote" =:
            "((This is a footnote))" =?>
            para (note (para "This is a footnote"))
          , testGroup "Images"
            [ "Image" =:
              "{{image.jpg}}" =?>
              para (image "image.jpg" "" (str "image.jpg"))
            , "Image with caption" =:
              "{{image.png|This is the caption}}" =?>
              para (image "image.png" "" "This is the caption")
            , "Image with } in caption" =:
              "{{image.png|There is an } in the caption}}" =?>
              para (image "image.png" "" "There is an } in the caption")
            , "Wiki namespace starting with dot" =:
              "{{.wiki:image.jpg}}" =?>
              para (image "wiki/image.jpg" "" (str "image.jpg"))
            , "Left aligned image" =:
              "{{wiki:dokuwiki-128.png }}" =?>
              para (imageWith ("", ["align-left"], []) "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            , "Right aligned image" =:
              "{{ wiki:dokuwiki-128.png}}" =?>
              para (imageWith ("", ["align-right"], []) "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            , "Centered image" =:
              "{{ wiki:dokuwiki-128.png }}" =?>
              para (imageWith ("", ["align-center"], []) "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            , "Image with width" =:
              "{{wiki:dokuwiki-128.png?50}}" =?>
              para (imageWith ("", [], [("width", "50")]) "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            , "Image with width and height" =:
              "{{wiki:dokuwiki-128.png?nocache&50x100}}" =?>
              para (imageWith ("", [], [("width", "50"), ("height", "100")]) "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            , "Linkonly" =:
              "{{wiki:dokuwiki-128.png?linkonly}}" =?>
              para (link "/wiki/dokuwiki-128.png" "" (str "dokuwiki-128.png"))
            ]
          , "Ignore ~~NOTOC~~" =:
            "Here is a ~~NOTOC~~ macro" =?>
            para "Here is a macro"
          , "Ignore ~~NOCACHE~~" =:
            "Here is a ~~NOCACHE~~ macro" =?>
            para "Here is a macro"
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
          , "Only two closing = are required" =:
            "====== Headline Level 1 ==" =?>
            header 1 "Headline Level 1"
          , "One closing = is not enough" =:
            "====== Headline Level 1 =" =?>
            para "====== Headline Level 1 ="
          , "One closing = is not enough" =:
            "== Headline with = sign ==" =?>
            header 5 "Headline with = sign"
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
                        , plain ("third item with code: " <> code "some code\ncomes here\n")
                        , plain "fourth item"
                        ]
          ]
        , "Block HTML" =:
          T.unlines [ "<HTML>"
                    , "<p style=\"border:2px dashed red;\">And this is some block HTML</p>"
                    , "</HTML>"
                    ] =?>
          rawBlock "html" "<p style=\"border:2px dashed red;\">And this is some block HTML</p>\n"
        , "Block PHP" =:
          T.unlines [ "<PHP>"
                    , "echo '<p>Hello World</p>';"
                    , "</PHP>"
                    ] =?>
          codeBlockWith ("", ["php"], []) "echo '<p>Hello World</p>';\n"
        , "Quote" =:
          T.unlines [ "> foo"
                    , ">no space is required after >"
                    , "> bar"
                    , ">> baz"
                    , "> bat"
                    ] =?>
          blockQuote (plain "foo" <>
                      plain "no space is required after >" <>
                      plain "bar" <>
                      blockQuote (plain "baz") <>
                      plain "bat")
        , "Code block" =:
          T.unlines [ "<code>"
                    , "foo bar baz"
                    , "</code>"
                    ] =?>
          codeBlock "foo bar baz\n"
        , "Java code block" =:
          T.unlines [ "<code java>"
                    , "public static void main"
                    , "</code>"
                    ] =?>
          codeBlockWith ("", ["java"], []) "public static void main\n"
        , "File with filename and no language" =:
          T.unlines [ "<file - foo.bar>"
                    , "file contents"
                    , "</file>"
                    ] =?>
          codeBlock "file contents\n"
        , "Table" =:
          T.unlines [ "| foo | bar |"
                    , "| bat | baz |"
                    ] =?>
          simpleTable [] [[plain "foo", plain "bar"]
                         ,[plain "bat", plain "baz"]]
        , "Table with header" =:
          T.unlines [ "^ foo ^ bar ^"
                    , "| bat | baz |"
                    ] =?>
          simpleTable [plain "foo", plain "bar"] [[plain "bat", plain "baz"]]
        , "Table with colspan" =:
          T.unlines [ "^ 0,0 ^ 0,1 ^ 0,2 ^"
                    , "| 1,0 | 1,1 ||"
                    , "| 2,0 | 2,1 | 2,2 |"
                    ] =?>
          simpleTable [plain "0,0", plain "0,1", plain "0,2"]
                      [[plain "1,0", plain "1,1", mempty]
                      ,[plain "2,0", plain "2,1", plain "2,2"]
                      ]
        , "Indented code block" =:
          T.unlines [ "foo"
                    , "  bar"
                    , "    bat"
                    , "baz"
                    ] =?>
          para "foo" <>
          codeBlock "bar\n  bat\n" <>
          para "baz"
        ]

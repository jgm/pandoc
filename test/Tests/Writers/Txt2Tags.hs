{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Txt2Tags (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

txt2tags :: (ToPandoc a) => a -> Text
txt2tags = purely (writeTxt2Tags def) . toPandoc

infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, Text) -> TestTree
(=:) = test txt2tags

tests :: [TestTree]
tests =
  [ testGroup "inline formatting"
    [ "bold"      =: strong "text"    =?> "**text**"
    , "italic"    =: emph "text"      =?> "//text//"
    , "underline" =: underline "text" =?> "__text__"
    , "strikeout" =: strikeout "text" =?> "--text--"
    , "inline code" =: code "foo"     =?> "``foo``"
    ]
  , testGroup "escape special characters"
    [ "escape bold marker"      =: str "a**b" =?> "a%%**%%b"
    , "escape italic marker"    =: str "a//b" =?> "a%%//%%b"
    , "escape underline marker" =: str "a__b" =?> "a%%__%%b"
    , "escape strikeout marker" =: str "a--b" =?> "a%%--%%b"
    , "escape code marker"      =: str "a``b" =?> "a%%``%%b"
    ]
  , testGroup "headers"
    [ "h1" =: header 1 "Heading" =?> "= Heading =\n"
    , "h2" =: header 2 "Heading" =?> "== Heading ==\n"
    , "h3" =: header 3 "Heading" =?> "=== Heading ===\n"
    , "h4" =: header 4 "Heading" =?> "==== Heading ====\n"
    ]
  , testGroup "code blocks"
    [ "no trailing newline in source"
        =: codeBlock "foo"
        =?> "```\nfoo\n```\n"
    , "trailing newline preserved"
        =: codeBlock "foo\n"
        =?> "```\nfoo\n```\n"
    ]
  , testGroup "lists"
    [ "bullet list"
        =: bulletList [para "foo", para "bar"]
        =?> "- foo\n- bar\n"
    , "ordered list"
        =: orderedList [para "foo", para "bar"]
        =?> "+ foo\n+ bar\n"
    , "nested bullet list"
        =: bulletList [para "a" <> bulletList [para "b"]]
        =?> "- a\n  - b\n"
    , "nested ordered list"
        =: orderedList [para "a" <> orderedList [para "b"]]
        =?> "+ a\n  + b\n"
    ]
  , testGroup "definition lists"
    [ "single term and definition"
        =: definitionList [("term", [para "definition"])]
        =?> ": **term**\n  definition\n"
    , "multiple terms"
        =: definitionList
            [ ("foo", [para "def foo"])
            , ("bar", [para "def bar"])
            ]
        =?> ": **foo**\n  def foo\n: **bar**\n  def bar\n"
    , "multiple definitions for one term"
        =: definitionList [("term", [para "def1", para "def2"])]
        =?> ": **term**\n  def1\n  def2\n"
    ]
  , testGroup "blockquote"
    -- Txt2Tags has no blockquote syntax; content is rendered without special markup.
    [ "blockquote rendered as plain content"
        =: blockQuote (para "quoted")
        =?> "quoted\n"
    ]
  , testGroup "horizontal rule"
    [ "hr" =: horizontalRule =?> "\n---\n"
    ]
  , testGroup "tables"
    [ "table with headers"
        =: simpleTable [plain "A", plain "B"]
                       [[plain "1", plain "2"]]
        =?> "|A|B|\n|1|2|\n"
    , "table without headers"
        =: simpleTable [] [[plain "1", plain "2"]]
        =?> "|1|2|\n"
    ]
  , testGroup "links"
    [ "link with label"
        =: link "http://example.com" "" "example"
        =?> "[example http://example.com]"
    , "autolink"
        =: link "http://example.com" "" "http://example.com"
        =?> "http://example.com"
    , "email"
        =: link "mailto:user@example.com" "" "user@example.com"
        =?> "<user@example.com>"
    ]
  , testGroup "images"
    [ "simple image (no title, no alt)"
        -- Use imageWith + mempty to get truly empty alt ([]) vs str "" which gives [Str ""]
        =: imageWith ("", [], []) "image.png" "" mempty
        =?> "[image.png]"
    , "image with title"
        =: image "image.png" "My title" "ignored alt"
        =?> "[image.png|My title]"
    , "image with alt text (no title)"
        =: image "image.png" "" "alt text"
        =?> "[image.png|alt text]"
    ]
  , testGroup "raw blocks"
    [ "txt2tags passthrough"
        =: rawBlock "txt2tags" "raw content"
        =?> "raw content"
    ]
  ]

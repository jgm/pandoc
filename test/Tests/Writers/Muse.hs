module Tests.Writers.Muse (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary()
import Text.Pandoc.Builder

muse :: (ToPandoc a) => a -> String
muse = museWithOpts def{ writerWrapText = WrapNone }

museWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
museWithOpts opts = unpack . purely (writeMuse opts) . toPandoc

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test muse

tests :: [TestTree]
tests = [ testGroup "block elements"
          [ "plain" =: plain (text "Foo bar.") =?> "Foo bar."
          , testGroup "paragraphs"
            [ "single paragraph" =: para (text "Sample paragraph.")
                                 =?> "Sample paragraph."
            , "two paragraphs" =: para (text "First paragraph.") <>
                                  para (text "Second paragraph.")
                               =?> unlines [ "First paragraph."
                                           , ""
                                           , "Second paragraph."
                                           ]
            ]
          , "line block" =: lineBlock [text "Foo", text "bar", text "baz"]
                         =?> unlines [ "<verse>"
                                     , "Foo"
                                     , "bar"
                                     , "baz"
                                     , "</verse>"
                                     ]
          , "code block" =: codeBlock "int main(void) {\n\treturn 0;\n}"
                         =?> unlines [ "<example>"
                                     , "int main(void) {"
                                     , "\treturn 0;"
                                     , "}"
                                     , "</example>"
                                     ]
          , "html raw block" =: rawBlock "html" "<hr>"
                             =?> unlines [ "<literal style=\"html\">"
                                         , "<hr>"
                                         , "</literal>"
                                         ]
          , "block quote" =: blockQuote (para (text "Foo"))
                          =?> unlines [ "<quote>"
                                      , "Foo"
                                      , "</quote>"
                                      ]
          , testGroup "lists"
            [ testGroup "simple lists"
              [
                "ordered list" =: orderedList [ plain $ text "first"
                                              , plain $ text "second"
                                              , plain $ text "third"
                                              ]
                               =?> unlines [ " 1. first"
                                           , " 2. second"
                                           , " 3. third"
                                           ]
              , "ordered list with Roman numerals"
                =: orderedListWith (1, UpperRoman, DefaultDelim)
                   [ plain $ text "first"
                   , plain $ text "second"
                   , plain $ text "third"
                   ]
                =?> unlines [ " I.   first"
                            , " II.  second"
                            , " III. third"
                            ]
              , "bullet list" =: bulletList [ plain $ text "first"
                                            , plain $ text "second"
                                            , plain $ text "third"
                                            ]
                              =?> unlines [ " - first"
                                          , " - second"
                                          , " - third"
                                          ]
              , "definition list" =: definitionList [ (text "first definition", [plain $ text "first description"])
                                                    , (text "second definition", [plain $ text "second description"])
                                                    , (text "third definition", [plain $ text "third description"])
                                                    ]
                                  =?> unlines [ " first definition :: first description"
                                              , " second definition :: second description"
                                              , " third definition :: third description"
                                              ]
              ]
            , testGroup "nested lists"
              [ "nested ordered list" =: orderedList [ plain $ text "First outer"
                                                     , plain (text "Second outer:") <>
                                                       orderedList [ plain $ text "first"
                                                                   , plain $ text "second"
                                                                   ]
                                                     , plain $ text "Third outer"
                                                     ]
                                      =?> unlines [ " 1. First outer"
                                                  , " 2. Second outer:"
                                                  , "    1. first"
                                                  , "    2. second"
                                                  , " 3. Third outer"
                                                  ]
              , "nested bullet lists" =: bulletList [ plain $ text "First outer"
                                                    , plain (text "Second outer:") <>
                                                      bulletList [ plain $ text "first"
                                                                 , plain $ text "second"
                                                                 ]
                                                    , plain $ text "Third outer"
                                                    ]
                                      =?> unlines [ " - First outer"
                                                  , " - Second outer:"
                                                  , "   - first"
                                                  , "   - second"
                                                  , " - Third outer"
                                                  ]
              , "nested definition lists" =: definitionList [ (text "first definition", [plain $ text "first description"])
                                                            , (text "second definition",
                                                               [ plain (text "second description")
                                                               , definitionList [ ( text "first inner definition"
                                                                                  , [plain $ text "first inner description"])
                                                                                , ( text "second inner definition"
                                                                                  , [plain $ text "second inner description"])
                                                                                ]
                                                               ]
                                                              )
                                                            ]
                                          =?> unlines [ " first definition :: first description"
                                                      , " second definition :: second description"
                                                      , "                       first inner definition :: first inner description"
                                                      , "                       second inner definition :: second inner description"
                                                      ]
              ]
            -- Check that list is intended with one space even inside a quote
            , "List inside block quote" =: blockQuote (orderedList [ plain $ text "first"
                                                                   , plain $ text "second"
                                                                   , plain $ text "third"
                                                                   ])
                                        =?> unlines [ "<quote>"
                                                    , " 1. first"
                                                    , " 2. second"
                                                    , " 3. third"
                                                    , "</quote>"
                                                    ]
            ]
          , testGroup "headings"
            [ "normal heading" =:
              header 1 (text "foo") =?> "* foo"
            , "heading levels" =:
              header 1 (text "First level") <>
              header 3 (text "Third level") =?>
              unlines [ "* First level"
                      , ""
                      , "*** Third level"
                      ]
            ]
          , "horizontal rule" =: horizontalRule =?> "----"
          , testGroup "tables"
            [ "table without header" =:
              let rows = [[para $ text "Para 1.1", para $ text "Para 1.2"]
                         ,[para $ text "Para 2.1", para $ text "Para 2.2"]]
              in simpleTable [] rows
              =?>
              unlines [ " Para 1.1 | Para 1.2"
                      , " Para 2.1 | Para 2.2"
                      ]
            , "table with header" =:
              let headers = [plain $ text "header 1", plain $ text "header 2"]
                  rows = [[para $ text "Para 1.1", para $ text "Para 1.2"]
                         ,[para $ text "Para 2.1", para $ text "Para 2.2"]]
              in simpleTable headers rows
              =?>
              unlines [ " header 1 || header 2"
                      , " Para 1.1 |  Para 1.2"
                      , " Para 2.1 |  Para 2.2"
                      ]
            , "table with header and caption" =:
              let caption = text "Table 1"
                  headers = [plain $ text "header 1", plain $ text "header 2"]
                  rows = [[para $ text "Para 1.1", para $ text "Para 1.2"]
                         ,[para $ text "Para 2.1", para $ text "Para 2.2"]]
              in table caption mempty headers rows
              =?> unlines [ " header 1 || header 2"
                          , " Para 1.1 |  Para 1.2"
                          , " Para 2.1 |  Para 2.2"
                          , " |+ Table 1 +|"
                          ]
            ]
          -- Div is trivial
          -- Null is trivial
          ]
        , testGroup "inline elements"
          [ testGroup "string"
            [ "string" =: str "foo" =?> "foo"
            , "escape footnote" =: str "[1]" =?> "<verbatim>[1]</verbatim>"
            , "escape verbatim close tag" =: str "foo</verbatim>bar"
               =?> "<verbatim>foo<</verbatim><verbatim>/verbatim>bar</verbatim>"
            , "escape pipe to avoid accidental tables" =: str "foo | bar"
               =?> "<verbatim>foo | bar</verbatim>"
            , "escape definition list markers" =: str "::" =?> "<verbatim>::</verbatim>"
            -- We don't want colons to be escaped if they can't be confused
            -- with definition list item markers.
            , "do not escape colon" =: str ":" =?> ":"
            ]
          , testGroup "emphasis"
            [ "emph" =: emph (text "foo") =?> "<em>foo</em>"
            , "strong" =: strong (text "foo") =?> "<strong>foo</strong>"
            , "strikeout" =: strikeout (text "foo") =?> "<del>foo</del>"
            ]
          , "superscript" =: superscript (text "foo") =?> "<sup>foo</sup>"
          , "subscript" =: subscript (text "foo") =?> "<sub>foo</sub>"
          , "smallcaps" =: smallcaps (text "foo") =?> "foo"
          , "single quoted" =: singleQuoted (text "foo") =?> "'foo'"
          , "double quoted" =: doubleQuoted (text "foo") =?> "\"foo\""
          -- Cite is trivial
          , testGroup "code"
            [ "simple" =: code "foo" =?> "<code>foo</code>"
            , "escape lightweight markup" =: code "foo = bar" =?> "<code><verbatim>foo = bar</verbatim></code>"
            , "escape tag" =: code "<code>foo = bar</code> baz" =?> "<code><verbatim><code>foo = bar</code> baz</verbatim></code>"
            ]
          , testGroup "spaces"
            [ "space" =: text "a" <> space <> text "b" =?> "a b"
            , "soft break" =: text "a" <> softbreak <> text "b" =?> "a b"
            , test (museWithOpts def{ writerWrapText = WrapPreserve })
                   "preserve soft break" $ text "a" <> softbreak <> text "b"
                   =?> "a\nb"
            , "line break" =: text "a" <> linebreak <> text "b" =?> "a<br>\nb"
            ]
          , testGroup "math"
            [ "inline math" =: math "2^3" =?> "2<sup>3</sup>"
            , "display math" =: displayMath "2^3" =?> "<verse>2<sup>3</sup></verse>"
            ]
          , "raw inline"
            =: rawInline "html" "<mark>marked text</mark>"
            =?> "<literal style=\"html\"><mark>marked text</mark></literal>"
          , testGroup "links"
            [ "link with description" =: link "https://example.com" "" (str "Link 1")
                                      =?> "[[https://example.com][Link 1]]"
            , "link without description" =: link "https://example.com" "" (str "https://example.com")
                                         =?> "[[https://example.com]]"
            -- Internal links in Muse include '#'
            , "link to anchor" =: link "#intro" "" (str "Introduction")
                               =?> "[[#intro][Introduction]]"
            -- According to Emacs Muse manual, links to images should be prefixed with "URL:"
            , "link to image with description" =: link "1.png" "" (str "Link to image")
                                               =?> "[[URL:1.png][Link to image]]"
            , "link to image without description" =: link "1.png" "" (str "1.png")
                                                  =?> "[[URL:1.png]]"
            ]
          , "image" =: image "image.png" "Image 1" (str "") =?> "[[image.png][Image 1]]"
          , "note" =: note (plain (text "Foo"))
                   =?> unlines [ "[1]"
                               , ""
                               , "[1] Foo"
                               ]
          , "span" =: spanWith ("",["foobar"],[]) (str "Some text")
                   =?> "<class name=\"foobar\">Some text</class>"
          , testGroup "combined"
            [ "emph word before" =:
                para (text "foo" <> emph (text "bar")) =?>
                    "foo<em>bar</em>"
            , "emph word after" =:
                para (emph (text "foo") <> text "bar") =?>
                    "<em>foo</em>bar"
            , "emph quoted" =:
                para (doubleQuoted (emph (text "foo"))) =?>
                    "\"<em>foo</em>\""
            , "strong word before" =:
                para (text "foo" <> strong (text "bar")) =?>
                    "foo<strong>bar</strong>"
            , "strong word after" =:
                para (strong (text "foo") <> text "bar") =?>
                    "<strong>foo</strong>bar"
            , "strong quoted" =:
                para (singleQuoted (strong (text "foo"))) =?>
                    "'<strong>foo</strong>'"
            ]
         ]
       ]

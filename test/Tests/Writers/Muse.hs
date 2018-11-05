{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Writers.Muse (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

defopts :: WriterOptions
defopts = def{ writerWrapText = WrapPreserve,
               writerExtensions = extensionsFromList [Ext_amuse,
                                                      Ext_auto_identifiers] }

muse :: (ToPandoc a) => a -> String
muse = museWithOpts defopts

museWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
museWithOpts opts = unpack . purely (writeMuse opts) . toPandoc

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test muse

noteLocationTestDoc :: Blocks
noteLocationTestDoc =
  header 1 (text "First Header") <>
  para (text "This is a footnote." <>
        note (para (text "First note."))) <>
  blockQuote (para (text "A note inside a block quote." <>
                    note (para (text "The second note."))) <>
              para (text "A second paragraph.")) <>
  header 1 (text "Second Header") <>
  para (text "Some more text.")

noteLocationTests :: TestTree
noteLocationTests = testGroup "note location"
  [ test (museWithOpts defopts {writerReferenceLocation=EndOfDocument})
    "footnotes at the end of document" $
    noteLocationTestDoc =?>
    (unlines [ "* First Header"
             , ""
             , "This is a footnote.[1]"
             , ""
             , "<quote>"
             , "A note inside a block quote.[2]"
             , ""
             , "A second paragraph."
             , "</quote>"
             , ""
             , "* Second Header"
             , ""
             , "Some more text."
             , ""
             , "[1] First note."
             , ""
             , "[2] The second note."
             ])
  , test (museWithOpts defopts {writerReferenceLocation=EndOfBlock})
    "footnotes at the end of block" $
    noteLocationTestDoc =?>
    (unlines [ "* First Header"
             , ""
             , "This is a footnote.[1]"
             , ""
             , "[1] First note."
             , ""
             , "<quote>"
             , "A note inside a block quote.[2]"
             , ""
             , "[2] The second note."
             , ""
             , "A second paragraph."
             , "</quote>"
             , ""
             , "* Second Header"
             , ""
             , "Some more text."
             ])
  , test (museWithOpts defopts {writerReferenceLocation=EndOfSection})
    "footnotes at the end of section" $
    noteLocationTestDoc =?>
    (unlines [ "* First Header"
             , ""
             , "This is a footnote.[1]"
             , ""
             , "<quote>"
             , "A note inside a block quote.[2]"
             , ""
             , "A second paragraph."
             , "</quote>"
             , ""
             , "[1] First note."
             , ""
             , "[2] The second note."
             , ""
             , "* Second Header"
             , ""
             , "Some more text."
             ])
  ]

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
                         =?> unlines [ "> Foo"
                                     , "> bar"
                                     , "> baz"
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
                =?> unlines [ " I. first"
                            , " II. second"
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
              , "definition list with multiple descriptions" =:
                definitionList [ (text "first definition", [plain $ text "first description"
                                                           ,plain $ text "second description"])
                               , (text "second definition", [plain $ text "third description"])
                               ]
                =?> unlines [ " first definition :: first description"
                            , "                  :: second description"
                            , " second definition :: third description"
                            ]
              , "definition list with empty term" =:
                definitionList [ (text "first definition", [plain $ text "first description"])
                               , (mempty, [plain $ text "second description"])
                               , (str "", [plain $ text "third description"])
                               ]
                =?> unlines [ " first definition :: first description"
                            , " <verbatim></verbatim> :: second description"
                            , " <verbatim></verbatim> :: third description"
                            ]
              , "definition list terms starting with space" =:
                definitionList [ (text "first definition", [plain $ text "first description"])
                               , (space <> str "foo", [plain $ text "second description"])
                               , (str " > bar", [plain $ text "third description"])
                               ]
                =?> unlines [ " first definition :: first description"
                            , " <verbatim></verbatim> foo :: second description"
                            , " <verbatim></verbatim> > bar :: third description"
                            ]
              , "definition list terms starting with list markers" =:
                definitionList [ (text "first definition", [plain $ text "first description"])
                               , (str "-", [plain $ text "second description"])
                               , (str "1.", [plain $ text "third description"])
                               ]
                =?> unlines [ " first definition :: first description"
                            , " <verbatim></verbatim>- :: second description"
                            , " <verbatim></verbatim>1. :: third description"
                            ]
              ]
            -- Test that lists of the same type and style are separated with two blanklines
            , testGroup "sequential lists"
              [ "bullet lists" =:
                bulletList [ para $ text "First"
                           , para $ text "Second"
                           , para $ text "Third"
                           ] <>
                bulletList [ para $ text "Fourth"
                           , para $ text "Fifth"
                           ] =?>
                unlines [ " - First"
                        , " - Second"
                        , " - Third"
                        , ""
                        , ""
                        , " - Fourth"
                        , " - Fifth"
                        ]
              , "ordered lists of the same style" =:
                orderedListWith (1, UpperRoman, DefaultDelim) [ para $ text "First"
                                                              , para $ text "Second"
                                                              ] <>
                orderedListWith (1, UpperRoman, DefaultDelim) [ para $ text "Third"
                                                              , para $ text "Fourth"
                                                              ] =?>
                unlines [ " I. First"
                        , " II. Second"
                        , ""
                        , ""
                        , " I. Third"
                        , " II. Fourth"
                        ]
              , "ordered lists with equal styles" =:
                orderedList [ para $ text "First"
                            , para $ text "Second"
                            ] <>
                orderedListWith (1, Decimal, DefaultDelim) [ para $ text "Third"
                                                           , para $ text "Fourth"
                                                           ] =?>
                unlines [ " 1. First"
                        , " 2. Second"
                        , ""
                        , ""
                        , " 1. Third"
                        , " 2. Fourth"
                        ]
              , "bullet and ordered lists" =:
                bulletList [ para $ text "First"
                           , para $ text "Second"
                           ] <>
                orderedListWith (1, UpperRoman, DefaultDelim) [ para $ text "Third"
                                                              , para $ text "Fourth"
                                                              ] =?>
                unlines [ " - First"
                        , " - Second"
                        , ""
                        , " I. Third"
                        , " II. Fourth"
                        ]
              , "different style ordered lists" =:
                orderedListWith (1, UpperRoman, DefaultDelim) [ para $ text "First"
                                                              , para $ text "Second"
                                                              ] <>
                orderedListWith (1, Decimal, DefaultDelim) [ para $ text "Third"
                                                           , para $ text "Fourth"
                                                           ] =?>
                unlines [ " I. First"
                        , " II. Second"
                        , ""
                        , " 1. Third"
                        , " 2. Fourth"
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
                                                               [ plain (text "second description") <>
                                                                 definitionList [ ( text "first inner definition"
                                                                                  , [plain $ text "first inner description"])
                                                                                , ( text "second inner definition"
                                                                                  , [plain $ text "second inner description"])
                                                                                ]
                                                               ]
                                                              )
                                                            ]
                                          =?> unlines [ " first definition :: first description"
                                                      , " second definition :: second description"
                                                      , "                      first inner definition :: first inner description"
                                                      , "                      second inner definition :: second inner description"
                                                      ]
              , "list item starting with list" =: bulletList [ bulletList [ plain $ text "foo"] ] =?> " - - foo"
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
            , "heading with ID" =:
               headerWith ("bar", [], []) 2 (text "Foo") =?>
               unlines [ "#bar"
                       , "** Foo"
                      ]
            , "empty heading" =: header 4 mempty =?> "**** <verbatim></verbatim>"
            ]
          , "horizontal rule" =: horizontalRule =?> "----"
          , "escape horizontal rule" =: para (text "----") =?> "<verbatim></verbatim>----"
          , "escape long horizontal rule" =: para (text "----------") =?> "<verbatim></verbatim>----------"
          , "don't escape horizontal inside paragraph" =: para (text "foo ---- bar") =?> "foo ---- bar"
          , "escape nonbreaking space" =: para (text "~~") =?> "<verbatim>~~</verbatim>"
          , "escape > in the beginning of line" =: para (text "> foo bar") =?> "<verbatim></verbatim>> foo bar"
          , "escape string with > and space in the beginning of line" =: para (str "> foo bar") =?> "<verbatim></verbatim>> foo bar"
          , testGroup "tables"
            [ "table without header" =:
              let rows = [[para $ text "Para 1.1", para $ text "Para 1.2"]
                         ,[para $ text "Para 2.1", para $ text "Para 2.2"]]
              in table mempty [(AlignDefault,0.0),(AlignDefault,0.0)]
                       [mempty, mempty] rows
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
              in table caption [(AlignDefault,0.0),(AlignDefault,0.0)]
                        headers rows
              =?> unlines [ " header 1 || header 2"
                          , " Para 1.1 |  Para 1.2"
                          , " Para 2.1 |  Para 2.2"
                          , " |+ Table 1 +|"
                          ]
            ]
          , "div with bullet list" =:
            divWith nullAttr (bulletList [para $ text "foo"]) =?>
            unlines [ " - foo" ] -- Making sure bullets are indented
          -- Null is trivial
          ]
        , testGroup "inline elements"
          [ testGroup "string"
            [ "string" =: str "foo" =?> "foo"
            , "escape footnote" =: str "[1]" =?> "<verbatim>[1]</verbatim>"
            , "do not escape brackets" =: str "[12ab]" =?> "[12ab]"
            , "escape verbatim close tag" =: str "foo</verbatim>bar"
               =?> "<verbatim>foo<</verbatim><verbatim>/verbatim>bar</verbatim>"
            , "escape link-like text" =: str "[[https://www.example.org]]"
               =?> "<verbatim>[[https://www.example.org]]</verbatim>"
            , "escape pipe to avoid accidental tables" =: str "foo | bar"
               =?> "<verbatim>foo | bar</verbatim>"
            , "escape hash to avoid accidental anchors" =: text "#foo bar"
              =?> "<verbatim>#foo</verbatim> bar"
            , "escape definition list markers" =: str "::" =?> "<verbatim>::</verbatim>"
            , "normalize strings before escaping" =: fromList [Str ":", Str ":"] =?> "<verbatim>::</verbatim>"
            -- We don't want colons to be escaped if they can't be confused
            -- with definition list item markers.
            , "do not escape colon" =: str ":" =?> ":"
            , "escape - to avoid accidental unordered lists" =: text " - foo" =?> "<verbatim></verbatim> - foo"
            , "escape - inside a list to avoid accidental nested unordered lists" =:
              bulletList [ para (text "foo") <>
                           para (text "- bar")
                         ] =?>
              unlines [ " - foo"
                      , ""
                      , "   <verbatim></verbatim>- bar"
                      ]
            , "escape strings starting with - inside a list" =:
              bulletList [ para (str "foo") <>
                           para (str "- bar")
                         ] =?>
              unlines [ " - foo"
                      , ""
                      , "   <verbatim></verbatim>- bar"
                      ]
            , "escape - inside a note" =:
              note (para (text "- foo")) =?>
              unlines [ "[1]"
                      , ""
                      , "[1] <verbatim></verbatim>- foo"
                      ]
            , "escape - after softbreak in note" =:
              note (para (str "foo" <> softbreak <> str "- bar")) =?>
              unlines [ "[1]"
                      , ""
                      , "[1] foo"
                      , "    <verbatim></verbatim>- bar"
                      ]
            , "escape ; to avoid accidental comments" =: text "; foo" =?> "<verbatim></verbatim>; foo"
            , "escape strings starting with ; and space" =: str "; foo" =?> "<verbatim></verbatim>; foo"
            , "escape ; after softbreak" =: text "foo" <> softbreak <> text "; bar" =?> "foo\n<verbatim></verbatim>; bar"
            , "escape ; after linebreak" =: text "foo" <> linebreak <> text "; bar" =?> "foo<br>\n<verbatim></verbatim>; bar"
            , "do not escape ; inside paragraph" =: text "foo ; bar" =?> "foo ; bar"
            , "escape newlines" =: str "foo\nbar" =?> "foo bar"
            ]
          , testGroup "emphasis"
            [ "emphasis" =: emph (text "foo") =?> "*foo*"
            , "emphasis inside word" =: text "foo" <> emph (text "bar") <> text "baz" =?> "foo<em>bar</em>baz"
            , "emphasis before comma" =: emph (text "foo") <> text ", bar" =?> "*foo*, bar"
            , "emphasis before period" =: emph (text "foobar") <> text "." =?> "*foobar*."
            , "empty emphasis" =: emph mempty =?> "<em></em>"
            , "empty strong" =: strong mempty =?> "<strong></strong>"
            , "empty strong emphasis" =: strong (emph mempty) =?> "**<em></em>**"
            , "empty emphasized strong" =: emph (strong mempty) =?> "*<strong></strong>*"
            , "emphasized empty string" =: emph (str "") =?> "<em></em>"
            , "strong empty string" =: strong (str "") =?> "<strong></strong>"
            , "strong emphasized empty string" =: strong (emph (str "")) =?> "**<em></em>**"
            , "emphasized strong empty string" =: emph (strong (str "")) =?> "*<strong></strong>*"
            , "emphasized string with space" =: emph (str " ") =?> "<em> </em>"
            , "emphasized string ending with space" =: emph (str "foo ") =?> "<em>foo </em>"
            , "emphasized string with tab" =: emph (str "\t") =?> "<em>\t</em>"
            , "emphasized space between empty strings" =: emph (str "" <> space <> str "") =?> "<em> </em>"
            , "strong" =: strong (text "foo") =?> "**foo**"
            , "strong inside word" =: text "foo" <> strong (text "bar") <> text "baz" =?> "foo<strong>bar</strong>baz"
            , "strong emphasis" =: strong (emph (text "foo")) =?> "***foo***"
            , "strong after emphasis" =: emph (text "foo") <> strong (text "bar") =?> "*foo*<strong>bar</strong>"
            , "strong emphasis after emphasis" =: emph (text "foo") <> strong (emph (text "bar")) =?> "*foo*<strong>*bar*</strong>"
            , "strong in the end of emphasis" =: emph (text "foo" <> strong (text "bar")) =?> "*foo<strong>bar</strong>*"
            , "switch to lightweight markup after <em> tag" =:
              strong (str "foo") <> emph (str "bar") <> strong (str "baz") =?>
              "**foo**<em>bar</em>**baz**"
            , "strikeout" =: strikeout (text "foo") =?> "<del>foo</del>"
            , "space at the beginning of emphasis" =: emph (text " foo") =?> "<em> foo</em>"
            , "space at the end of emphasis" =: emph (text "foo ") =?> "<em>foo </em>"
            , "space at the beginning of strong" =: strong (text " foo") =?> "<strong> foo</strong>"
            , "space at the end of strong" =: strong (text "foo ") =?> "<strong>foo </strong>"
            , "space at the beginning of strong emphasis" =: strong (emph (text " foo")) =?> "**<em> foo</em>**"
            , "space at the end of strong emphasis" =: strong (emph (text "foo ")) =?> "**<em>foo </em>**"
            , "space at the beginning of emphasiszed strong" =: emph (strong (text " foo")) =?> "*<strong> foo</strong>*"
            , "space at the end of emphasized strong" =: emph (strong (text "foo ")) =?> "*<strong>foo </strong>*"
            ]
          , "superscript" =: superscript (text "foo") =?> "<sup>foo</sup>"
          , "subscript" =: subscript (text "foo") =?> "<sub>foo</sub>"
          , "smallcaps" =: smallcaps (text "foo") =?> "*foo*"
          , "smallcaps near emphasis" =: emph (str "foo") <> smallcaps (str "bar") =?> "*foobar*"
          , "single quoted" =: singleQuoted (text "foo") =?> "‘foo’"
          , "double quoted" =: doubleQuoted (text "foo") =?> "“foo”"
          -- Cite is trivial
          , testGroup "code"
            [ "simple" =: code "foo" =?> "=foo="
            , "empty" =: code "" =?> "<code></code>"
            , "space" =: code " " =?> "<code> </code>"
            , "space at the beginning" =: code " foo" =?> "<code> foo</code>"
            , "space at the end" =: code "foo " =?> "<code>foo </code>"
            , "use tags for =" =: code "foo = bar" =?> "<code>foo = bar</code>"
            , "escape tag" =: code "<code>foo = bar</code> baz" =?> "<code><code>foo = bar<</code><code>/code> baz</code>"
            , "normalization with attributes" =: codeWith ("",["haskell"],[]) "foo" <> code "bar" =?> "=foobar="
            , "code tag" =: code "<code>foo</code>" =?> "=<code>foo</code>="
            , "normalization" =: code "</co" <> code "de>" <> code "=" =?> "<code><</code><code>/code>=</code>"
            , "normalization with empty string" =: code "</co" <> str "" <> code "de>" <> code "=" =?> "<code><</code><code>/code>=</code>"
            , "emphasized code" =: emph (code "foo") =?> "*=foo=*"
            , "strong code" =: strong (code "foo") =?> "**=foo=**"
            ]
          , testGroup "spaces"
            [ "space" =: text "a" <> space <> text "b" =?> "a b"
            , "soft break" =: text "a" <> softbreak <> text "b" =?> "a\nb"
            , test (museWithOpts def{ writerWrapText = WrapNone })
                   "remove soft break" $ text "a" <> softbreak <> text "b"
                   =?> "a b"
            , "line break" =: text "a" <> linebreak <> text "b" =?> "a<br>\nb"
            , "no newline after line break in header" =: header 1 (text "a" <> linebreak <> text "b") =?> "* a<br>b"
            , "no softbreak in header" =: header 1 (text "a" <> softbreak <> text "b") =?> "* a b"
            ]
          , testGroup "math"
            [ "inline math" =: math "2^3" =?> "2<sup>3</sup>"
            , "display math" =: displayMath "2^3" =?> "2<sup>3</sup>"
            , "multiple letters in inline math" =: math "abc" =?> "*abc*"
            , "expand math before normalization" =: math "[" <> str "2]" =?> "<verbatim>[2]</verbatim>"
            , "multiple math expressions inside one inline list" =: math "5_4" <> text ", " <> displayMath "3^2" =?> "5<sub>4</sub>, 3<sup>2</sup>"
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

            , testGroup "escape brackets in links"
              [ "link with description"
                =: link "https://example.com/foo].txt" "" (str "Description")
                =?> "[[https://example.com/foo%5D.txt][Description]]"
              , "link without description"
                =: link "https://example.com/foo].txt" "" (str "https://example.com/foo].txt")
                =?> "[[https://example.com/foo%5D.txt][<verbatim>https://example.com/foo].txt</verbatim>]]"
              , "image link with description"
                =: link "foo]bar.png" "" (str "Image link")
                =?> "[[URL:foo%5Dbar.png][Image link]]"
              , "image link without description"
                =: link "foo]bar.png" "" (str "foo]bar.png")
                =?> "[[URL:foo%5Dbar.png][<verbatim>foo]bar.png</verbatim>]]"
              ]
            ]
          , "image" =: image "image.png" "Image 1" (str "") =?> "[[image.png][Image 1]]"
          , "image with width" =:
            imageWith ("", [], [("width", "60%")]) "image.png" "Image" (str "") =?>
            "[[image.png 60][Image]]"
          , "left-aligned image with width" =:
            imageWith ("", ["align-left"], [("width", "60%")]) "image.png" "Image" (str "") =?>
            "[[image.png 60 l][Image]]"
          , "right-aligned image with width" =:
            imageWith ("", ["align-right"], [("width", "60%")]) "image.png" "Image" (str "") =?>
            "[[image.png 60 r][Image]]"
          , "escape brackets in image title" =: image "image.png" "Foo]bar" (str "") =?> "[[image.png][<verbatim>Foo]bar</verbatim>]]"
          , "note" =: note (plain (text "Foo"))
                   =?> unlines [ "[1]"
                               , ""
                               , "[1] Foo"
                               ]
          , noteLocationTests
          , "span with class" =: spanWith ("",["foobar"],[]) (text "Some text")
                              =?> "<class name=\"foobar\">Some text</class>"
          , "span without class" =: spanWith ("",[],[]) (text "Some text")
                                 =?> "<class>Some text</class>"
          , "span with anchor" =: spanWith ("anchor", [], []) mempty <> text "Foo bar"
                               =?> "#anchor Foo bar"
          , "empty span with anchor" =: spanWith ("anchor", [], []) mempty
                                     =?> "#anchor"
          , "empty span without class and anchor" =: spanWith ("", [], []) mempty
                                                  =?> "<class></class>"
          , "span with class and anchor" =: spanWith ("anchor", ["foo"], []) (text "bar")
                                         =?> "#anchor <class name=\"foo\">bar</class>"
          , "adjacent spans" =: spanWith ("", ["syllable"], []) (str "wa") <>
                                spanWith ("", ["syllable"], []) (str "ter")
                             =?> "<class name=\"syllable\">wa</class><class name=\"syllable\">ter</class>"
          , testGroup "combined"
            [ "emph word before" =:
                para (text "foo" <> emph (text "bar")) =?>
                    "foo<em>bar</em>"
            , "emph word after" =:
                para (emph (text "foo") <> text "bar") =?>
                    "<em>foo</em>bar"
            , "emph quoted" =:
                para (doubleQuoted (emph (text "foo"))) =?>
                    "“*foo*”"
            , "strong word before" =:
                para (text "foo" <> strong (text "bar")) =?>
                    "foo<strong>bar</strong>"
            , "strong word after" =:
                para (strong (text "foo") <> text "bar") =?>
                    "<strong>foo</strong>bar"
            , "strong quoted" =:
                para (singleQuoted (strong (text "foo"))) =?>
                    "‘**foo**’"
            ]
         ]
       ]

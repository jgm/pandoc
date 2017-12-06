{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Muse (tests) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
-- import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared (underlineSpan)
-- import Text.Pandoc.Walk (walk)

amuse :: Text -> Pandoc
amuse = purely $ readMuse def { readerExtensions = extensionsFromList [Ext_amuse]}

emacsMuse :: Text -> Pandoc
emacsMuse = purely $ readMuse def { readerExtensions = emptyExtensions }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test amuse

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

{-
-- Tables and code blocks don't round-trip yet

removeTables :: Block -> Block
removeTables (Table{}) = Para [Str "table was here"]
removeTables x = x

-- Demand that any AST produced by Muse reader and written by Muse writer can be read back exactly the same way.
-- Currently we remove code blocks and tables and compare third rewrite to the second.
-- First and second rewrites are not equal yet.
roundTrip :: Block -> Bool
roundTrip b = d'' == d'''
  where d = walk removeTables $ Pandoc nullMeta [b]
        d' = rewrite d
        d'' = rewrite d'
        d''' = rewrite d''
        rewrite = amuse . T.pack . (++ "\n") . T.unpack .
                  (purely $ writeMuse def { writerExtensions = extensionsFromList [Ext_amuse]
                                          , writerWrapText = WrapPreserve
                                          })
-}

tests :: [TestTree]
tests =
  [ testGroup "Inlines"
      [ "Plain String" =:
          "Hello, World" =?>
          para "Hello, World"

      , "Muse is not XML" =: "&lt;" =?> para "&lt;"

      , "Emphasis" =:
        "*Foo bar*" =?>
        para (emph . spcSep $ ["Foo", "bar"])

      , "Comma after closing *" =:
        "Foo *bar*, baz" =?>
        para ("Foo " <> emph "bar" <> ", baz")

      , "Letter after closing *" =:
        "Foo *bar*x baz" =?>
        para "Foo *bar*x baz"

      , "Letter before opening *" =:
        "Foo x*bar* baz" =?>
        para "Foo x*bar* baz"

      , "Emphasis tag" =:
        "<em>Foo bar</em>" =?>
        para (emph . spcSep $ ["Foo", "bar"])

      , "Strong" =:
          "**Cider**" =?>
          para (strong "Cider")

      , "Strong tag" =: "<strong>Strong</strong>" =?> para (strong "Strong")

      , "Strong Emphasis" =:
          "***strength***" =?>
          para (strong . emph $ "strength")

      , test emacsMuse "Underline"
        ("_Underline_" =?> para (underlineSpan "Underline"))

      , "Superscript tag" =: "<sup>Superscript</sup>" =?> para (superscript "Superscript")

      , "Subscript tag" =: "<sub>Subscript</sub>" =?> para (subscript "Subscript")

      , "Strikeout tag" =: "<del>Strikeout</del>" =?> para (strikeout "Strikeout")

      , "Opening inline tags" =: "foo <em> bar <strong>baz" =?> para "foo <em> bar <strong>baz"

      , "Closing inline tags" =: "foo </em> bar </strong>baz" =?> para "foo </em> bar </strong>baz"

      , "Tag soup" =: "foo <em> bar </strong>baz" =?> para "foo <em> bar </strong>baz"

      -- Both inline tags must be within the same paragraph
      , "No multiparagraph inline tags" =:
        T.unlines [ "First line"
                  , "<em>Second line"
                  , ""
                  , "Fourth line</em>"
                  ] =?>
        para "First line\n<em>Second line" <>
        para "Fourth line</em>"

      , "Linebreak" =: "Line <br>  break" =?> para ("Line" <> linebreak <> "break")

      , test emacsMuse "Non-breaking space"
        ("Foo~~bar" =?> para "Foo\160bar")

      , testGroup "Code markup"
        [ "Code" =: "=foo(bar)=" =?> para (code "foo(bar)")

        , "Not code" =: "a=b= =c=d" =?> para (text "a=b= =c=d")

        -- Emacs Muse 3.20 parses this as code, we follow Amusewiki
        , "Not code if closing = is detached" =: "=this is not a code =" =?> para "=this is not a code ="

        , "Not code if opening = is detached" =: "= this is not a code=" =?> para "= this is not a code="

        , "Code if followed by comma" =:
          "Foo =bar=, baz" =?>
          para (text "Foo " <> code "bar" <> text ", baz")

        , "One character code" =: "=c=" =?> para (code "c")

        , "Three = characters is not a code" =: "===" =?> para "==="

        , "Multiline code markup" =:
          "foo =bar\nbaz= end of code" =?>
          para (text "foo " <> code "bar\nbaz" <> text " end of code")

{- Emacs Muse 3.20 has a bug: it publishes
 - <p>foo <code>bar
 -
 - baz</code> foo</p>
 - which is displayed as one paragraph by browsers.
 - We follow Amusewiki here and avoid joining paragraphs.
 -}
        , "No multiparagraph code" =:
          T.unlines [ "foo =bar"
                    , ""
                    , "baz= foo"
                    ] =?>
          para "foo =bar" <>
          para "baz= foo"
        ]

      , "Code tag" =: "<code>foo(bar)</code>" =?> para (code "foo(bar)")

      , "Verbatim tag" =: "*<verbatim>*</verbatim>*" =?> para (emph "*")

      , "Verbatim inside code" =: "<code><verbatim>foo</verbatim></code>" =?> para (code "<verbatim>foo</verbatim>")

      , testGroup "Links"
        [ "Link without description" =:
          "[[https://amusewiki.org/]]" =?>
          para (link "https://amusewiki.org/" "" (str "https://amusewiki.org/"))
        , "Link with description" =:
          "[[https://amusewiki.org/][A Muse Wiki]]" =?>
          para (link "https://amusewiki.org/" "" (text "A Muse Wiki"))
        , "Image" =:
          "[[image.jpg]]" =?>
          para (image "image.jpg" "" mempty)
        , "Image with description" =:
          "[[image.jpg][Image]]" =?>
          para (image "image.jpg" "" (text "Image"))
        , "Image link" =:
          "[[URL:image.jpg]]" =?>
          para (link "image.jpg" "" (str "image.jpg"))
        , "Image link with description" =:
          "[[URL:image.jpg][Image]]" =?>
          para (link "image.jpg" "" (text "Image"))
        -- Implicit links are supported in Emacs Muse, but not in Amusewiki:
        -- https://github.com/melmothx/text-amuse/issues/18
        --
        -- This test also makes sure '=' without whitespace is not treated as code markup
        , "No implicit links" =: "http://example.org/index.php?action=view&id=1"
               =?> para "http://example.org/index.php?action=view&id=1"
        ]

      , testGroup "Literal"
        [ test emacsMuse "Inline literal"
          ("Foo<literal style=\"html\">lit</literal>bar" =?>
          para (text "Foo" <> rawInline "html" "lit" <> text "bar"))
        , "No literal in Text::Amuse" =:
          "Foo<literal style=\"html\">lit</literal>bar" =?>
          para "Foo<literal style=\"html\">lit</literal>bar"
        ]
      ]

  , testGroup "Blocks"
      [ -- testProperty "Round trip" roundTrip,
        "Block elements end paragraphs" =:
        T.unlines [ "First paragraph"
                  , "----"
                  , "Second paragraph"
                  ] =?> para (text "First paragraph") <> horizontalRule <> para (text "Second paragraph")
      , testGroup "Horizontal rule"
        [ "Less than 4 dashes is not a horizontal rule" =: "---" =?> para (text "---")
        , "4 dashes is a horizontal rule" =: "----" =?> horizontalRule
        , "5 dashes is a horizontal rule" =: "-----" =?> horizontalRule
        , "4 dashes with spaces is a horizontal rule" =: "----  " =?> horizontalRule
        ]
      , testGroup "Paragraphs"
        [ "Simple paragraph" =:
          T.unlines [ "First line"
                    , "second line."
                    ] =?>
          para "First line\nsecond line."
        , "Indented paragraph" =:
          T.unlines [ " First line"
                    , "second line."
                    ] =?>
          para "First line\nsecond line."
        -- Emacs Muse starts a blockquote on the second line.
        -- We copy Amusewiki behavior and require a blank line to start a blockquote.
        , "Indentation in the middle of paragraph" =:
           T.unlines [ "First line"
                     , "  second line"
                     , "third line"
                     ] =?>
           para "First line\nsecond line\nthird line"
        , "Quote" =:
          "  This is a quotation\n" =?>
          blockQuote (para "This is a quotation")
        , "Multiline quote" =:
          T.unlines [ "  This is a quotation"
                    , "  with a continuation"
                    ] =?>
          blockQuote (para "This is a quotation\nwith a continuation")
        , testGroup "Div"
          [ "Div without id" =:
            "<div>Foo bar</div>" =?>
            divWith nullAttr (para "Foo bar")
          , "Div with id" =:
            "<div id=\"foo\">Foo bar</div>" =?>
            divWith ("foo", [], []) (para "Foo bar")
          ]
        , "Verse" =:
          T.unlines [ "> This is"
                    , "> First stanza"
                    , ">" -- Emacs produces verbatim ">" here, we follow Amusewiki
                    , "> And this is"
                    , ">   Second stanza"
                    , ">"
                    , ""
                    , ">"
                    , ""
                    , "> Another verse"
                    , ">    is here"
                    ] =?>
          lineBlock [ "This is"
                    , "First stanza"
                    , ""
                    , "And this is"
                    , "\160\160Second stanza"
                    , ""
                    ] <>
          lineBlock [ "" ] <>
          lineBlock [ "Another verse"
                    , "\160\160\160is here"
                    ]
        ]
      , "Quote tag" =: "<quote>Hello, world</quote>" =?> blockQuote (para $ text "Hello, world")
      , "Verse tag" =:
        T.unlines [ "<verse>"
                  , ""
                  , "Foo bar baz"
                  , "  One two three"
                  , ""
                  , "</verse>"
                  , "<verse>Foo bar</verse>"
                  , "<verse>"
                  , "Foo bar</verse>"
                  , "<verse>"
                  , "   Foo</verse>"
                  ] =?>
        lineBlock [ ""
                  , text "Foo bar baz"
                  , text "\160\160One two three"
                  , ""
                  ] <>
        lineBlock [ "Foo bar" ] <>
        lineBlock [ "Foo bar" ] <>
        lineBlock [ "\160\160\160Foo" ]
      , testGroup "Example"
        [ "Braces on separate lines" =:
          T.unlines [ "{{{"
                    , "Example line"
                    , "}}}"
                    ] =?>
          codeBlock "Example line"
        , "Spaces after opening braces" =:
          T.unlines [ "{{{  "
                    , "Example line"
                    , "}}}"
                    ] =?>
          codeBlock "Example line"
        , "One blank line in the beginning" =:
          T.unlines [ "{{{"
                    , ""
                    , "Example line"
                    , "}}}"
                    ] =?>
          codeBlock "\nExample line"
        , "One blank line in the end" =:
          T.unlines [ "{{{"
                    , "Example line"
                    , ""
                    , "}}}"
                    ] =?>
          codeBlock "Example line\n"
        -- Amusewiki requires braces to be on separate line,
        -- this is an extension.
        , "One line" =:
          "{{{Example line}}}" =?>
          codeBlock "Example line"
        ]
      , testGroup "Example tag"
        [ "Tags on separate lines" =:
          T.unlines [ "<example>"
                    , "Example line"
                    , "</example>"
                    ] =?>
          codeBlock "Example line"
        , "One line" =:
          "<example>Example line</example>" =?>
          codeBlock "Example line"
        , "One blank line in the beginning" =:
          T.unlines [ "<example>"
                    , ""
                    , "Example line"
                    , "</example>"
                    ] =?>
          codeBlock "\nExample line"
        , "One blank line in the end" =:
          T.unlines [ "<example>"
                    , "Example line"
                    , ""
                    , "</example>"
                    ] =?>
          codeBlock "Example line\n"
        , "Example inside list" =:
          T.unlines [ " - <example>"
                    , "   foo"
                    , "   </example>"
                    ] =?>
          bulletList [ codeBlock "foo" ]
        , "Example inside list with empty lines" =:
          T.unlines [ " - <example>"
                    , "   foo"
                    , "   </example>"
                    , ""
                    , "   bar"
                    , ""
                    , "   <example>"
                    , "   baz"
                    , "   </example>"
                    ] =?>
          bulletList [ codeBlock "foo" <> para "bar" <> codeBlock "baz" ]
        , "Indented example inside list" =:
          T.unlines [ " -  <example>"
                    , "    foo"
                    , "    </example>"
                    ] =?>
          bulletList [ codeBlock "foo" ]
        , "Example inside definition list" =:
          T.unlines [ " foo :: <example>"
                    , "        bar"
                    , "        </example>"
                    ] =?>
          definitionList [ ("foo", [codeBlock "bar"]) ]
        , "Example inside list definition with empty lines" =:
          T.unlines [ " term :: <example>"
                    , "         foo"
                    , "         </example>"
                    , ""
                    , "         bar"
                    , ""
                    , "         <example>"
                    , "         baz"
                    , "         </example>"
                    ] =?>
          definitionList [ ("term", [codeBlock "foo" <> para "bar" <> codeBlock "baz"]) ]
        , "Example inside note" =:
          T.unlines [ "Foo[1]"
                    , ""
                    , "[1] <example>"
                    , "    bar"
                    , "    </example>"
                    ] =?>
          para ("Foo" <> note (codeBlock "bar"))
        ]
      , testGroup "Literal blocks"
        [ test emacsMuse "Literal block"
          (T.unlines [ "<literal style=\"latex\">"
                    , "\\newpage"
                    , "</literal>"
                    ] =?>
          rawBlock "latex" "\\newpage")
        , "No literal blocks in Text::Amuse" =:
          T.unlines [ "<literal style=\"latex\">"
                    , "\\newpage"
                    , "</literal>"
                    ] =?>
          para "<literal style=\"latex\">\n\\newpage\n</literal>"
        ]
      , "Center" =: "<center>Hello, world</center>" =?> para (text "Hello, world")
      , "Right" =: "<right>Hello, world</right>" =?> para (text "Hello, world")
      , testGroup "Comments"
        [ "Comment tag" =: "<comment>\nThis is a comment\n</comment>" =?> (mempty::Blocks)
        , "Line comment" =: "; Comment" =?> (mempty::Blocks)
        , "Not a comment (does not start with a semicolon)" =: " ; Not a comment" =?> para (text "; Not a comment")
        , "Not a comment (has no space after semicolon)" =: ";Not a comment" =?> para (text ";Not a comment")
        ]
      , testGroup "Headers"
        [ "Part" =:
          "* First level" =?>
          header 1 "First level"
        , "Chapter" =:
          "** Second level" =?>
          header 2 "Second level"
        , "Section" =:
          "*** Third level" =?>
          header 3 "Third level"
        , "Subsection" =:
          "**** Fourth level" =?>
          header 4 "Fourth level"
        , "Subsubsection" =:
          "***** Fifth level" =?>
          header 5 "Fifth level"
        , "Whitespace is required after *" =: "**Not a header" =?> para "**Not a header"
        , "No headers in footnotes" =:
          T.unlines [ "Foo[1]"
                    , "[1] * Bar"
                    ] =?>
          para (text "Foo" <>
                note (para "* Bar"))
        , "No headers in quotes" =:
          T.unlines [ "<quote>"
                    , "* Hi"
                    , "</quote>"
                    ] =?>
          blockQuote (para "* Hi")
        ]
      , testGroup "Directives"
        [ "Title" =:
          "#title Document title" =?>
          let titleInline = toList "Document title"
              meta = setMeta "title" (MetaInlines titleInline) nullMeta
          in Pandoc meta mempty
        -- Emacs Muse documentation says that "You can use any combination
        -- of uppercase and lowercase letters for directives",
        -- but also allows '-', which is not documented, but used for disable-tables.
        , test emacsMuse "Disable tables"
          ("#disable-tables t" =?>
          Pandoc (setMeta "disable-tables" (MetaInlines $ toList "t") nullMeta) mempty)
        ]
      , testGroup "Anchors"
        [ "Anchor" =:
          T.unlines [ "; A comment to make sure anchor is not parsed as a directive"
                    , "#anchor Target"
                    ] =?>
          para (spanWith ("anchor", [], []) mempty <> "Target")
        , "Anchor cannot start with a number" =:
          T.unlines [ "; A comment to make sure anchor is not parsed as a directive"
                    , "#0notanchor Target"
                    ] =?>
          para "#0notanchor Target"
        , "Not anchor if starts with a space" =:
          " #notanchor Target" =?>
          para "#notanchor Target"
        , "Anchor inside a paragraph" =:
          T.unlines [ "Paragraph starts here"
                    , "#anchor and ends here."
                    ] =?>
          para ("Paragraph starts here\n" <> spanWith ("anchor", [], []) mempty <> "and ends here.")
        ]
      , testGroup "Footnotes"
        [ "Simple footnote" =:
          T.unlines [ "Here is a footnote[1]."
                    , ""
                    , "[1] Footnote contents"
                    ] =?>
          para (text "Here is a footnote" <>
                note (para "Footnote contents") <>
                str ".")
        , "Recursive footnote" =:
          T.unlines [ "Start recursion here[1]"
                    , ""
                    , "[1] Recursion continues here[1]"
                    ] =?>
          para (text "Start recursion here" <>
                note (para "Recursion continues here[1]"))
        , testGroup "Multiparagraph footnotes"
          [ "Amusewiki multiparagraph footnotes" =:
            T.unlines [ "Multiparagraph[1] footnotes[2]"
                      , ""
                      , "[1] First footnote paragraph"
                      , ""
                      , "    Second footnote paragraph"
                      , "Not a note"
                      , "[2] Second footnote"
                      ] =?>
            para (text "Multiparagraph" <>
                  note (para "First footnote paragraph" <>
                        para "Second footnote paragraph") <>
                  text " footnotes" <>
                  note (para "Second footnote")) <>
            para (text "Not a note")
          , test emacsMuse "Emacs multiparagraph footnotes"
            (T.unlines
              [ "First footnote reference[1] and second footnote reference[2]."
              , ""
              , "[1] First footnote paragraph"
              , ""
              , "Second footnote"
              , "paragraph"
              , ""
              , "[2] Third footnote paragraph"
              , ""
              , "Fourth footnote paragraph"
              ] =?>
            para (text "First footnote reference" <>
                  note (para "First footnote paragraph" <>
                        para "Second footnote\nparagraph") <>
                  text " and second footnote reference" <>
                  note (para "Third footnote paragraph" <>
                        para "Fourth footnote paragraph") <>
                  text "."))
          ]
        ]
      ]
    , testGroup "Tables"
        [ "Two cell table" =:
          "One | Two" =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
                       []
                       [[plain "One", plain "Two"]]
        , "Table with multiple words" =:
          "One two | three four" =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
                       []
                       [[plain "One two", plain "three four"]]
        , "Not a table" =:
          "One| Two" =?>
          para (text "One| Two")
        , "Not a table again" =:
          "One |Two" =?>
          para (text "One |Two")
        , "Two line table" =:
          T.unlines
            [ "One |  Two"
            , "Three  | Four"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
                       []
                       [[plain "One", plain "Two"],
                       [plain "Three", plain "Four"]]
        , "Table with one header" =:
          T.unlines
            [ "First || Second"
            , "Third | Fourth"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
            [plain "First", plain "Second"]
            [[plain "Third", plain "Fourth"]]
        , "Table with two headers" =:
          T.unlines
            [ "First || header"
            , "Second || header"
            , "Foo | bar"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
            [plain "First", plain "header"]
            [[plain "Second", plain "header"],
             [plain "Foo", plain "bar"]]
        , "Header and footer reordering" =:
          T.unlines
            [ "Foo ||| bar"
            , "Baz || foo"
            , "Bar | baz"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
            [plain "Baz", plain "foo"]
            [[plain "Bar", plain "baz"],
             [plain "Foo", plain "bar"]]
        , "Table with caption" =:
          T.unlines
            [ "Foo || bar || baz"
            , "First | row | here"
            , "Second | row | there"
            , "|+ Table caption +|"
            ] =?>
          table (text "Table caption") (replicate 3 (AlignDefault, 0.0))
            [plain "Foo", plain "bar", plain "baz"]
            [[plain "First", plain "row", plain "here"],
             [plain "Second", plain "row", plain "there"]]
        , "Caption without table" =:
          "|+ Foo bar baz +|" =?>
          table (text "Foo bar baz") [] [] []
        , "Table indented with space" =:
          T.unlines
            [ " Foo | bar"
            , " Baz | foo"
            , " Bar | baz"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
            []
            [[plain "Foo", plain "bar"],
             [plain "Baz", plain "foo"],
             [plain "Bar", plain "baz"]]
        , "Empty cells" =:
          T.unlines
            [ " | Foo"
            , " |"
            , " bar |"
            , " || baz"
            ] =?>
          table mempty [(AlignDefault, 0.0), (AlignDefault, 0.0)]
            [plain "", plain "baz"]
            [[plain "", plain "Foo"],
             [plain "", plain ""],
             [plain "bar", plain ""]]
        ]
    , testGroup "Lists"
      [ "Bullet list" =:
        T.unlines
           [ " - Item1"
           , ""
           , " - Item2"
           ] =?>
        bulletList [ para "Item1"
                   , para "Item2"
                   ]
      , "Ordered list" =:
        T.unlines
          [ " 1. Item1"
          , ""
          , " 2. Item2"
          ] =?>
        orderedListWith (1, Decimal, Period) [ para "Item1"
                                             , para "Item2"
                                             ]
      , "Ordered list with implicit numbers" =:
        T.unlines
          [ " 1. Item1"
          , ""
          , " 1. Item2"
          , ""
          , " 1. Item3"
          ] =?>
        orderedListWith (1, Decimal, Period) [ para "Item1"
                                             , para "Item2"
                                             , para "Item3"
                                             ]
      , "Bullet list with empty items" =:
        T.unlines
          [ " -"
          , ""
          , " - Item2"
          ] =?>
        bulletList [ mempty
                   , para "Item2"
                   ]
      , "Ordered list with empty items" =:
        T.unlines
          [ " 1."
          , ""
          , " 2."
          , ""
          , " 3. Item3"
          ] =?>
        orderedListWith (1, Decimal, Period) [ mempty
                                             , mempty
                                             , para "Item3"
                                             ]
      , testGroup "Nested lists"
        [ "Nested list" =:
          T.unlines
            [ " - Item1"
            , "   - Item2"
            , "   - Item3"
            , " - Item4"
            , "   1. Nested"
            , "   2. Ordered"
            , "   3. List"
            ] =?>
          bulletList [ mconcat [ para "Item1"
                               , bulletList [ para "Item2"
                                            , para "Item3"
                                            ]
                               ]
                     , mconcat [ para "Item4"
                               , orderedListWith (1, Decimal, Period) [ para "Nested"
                                                                      , para "Ordered"
                                                                      , para "List"
                                                                      ]
                               ]
                     ]
        , "Incorrectly indented Text::Amuse nested list" =:
          T.unlines
            [ " - First item"
            , "  - Not nested item"
            ] =?>
          bulletList [ para "First item", para "Not nested item"]
        , "Text::Amuse includes only one space in list marker" =:
          T.unlines
            [ " -    First item"
            , "   - Nested item"
            ] =?>
          bulletList [ para "First item" <> bulletList [ para "Nested item"]]
        ]
      , "List continuation" =:
         T.unlines
           [ " - a"
           , ""
           , "   b"
           , ""
           , "   c"
           ] =?>
         bulletList [ mconcat [ para "a"
                              , para "b"
                              , para "c"
                              ]
                    ]
      -- Emacs Muse allows to separate lists with two or more blank lines.
      -- Text::Amuse (Amusewiki engine) always creates a single list as of version 0.82.
      -- pandoc follows Emacs Muse behavior
      , testGroup "Blank lines"
        [ "Blank lines between list items are not required" =:
          T.unlines
            [ " - Foo"
            , " - Bar"
            ] =?>
          bulletList [ para "Foo"
                     , para "Bar"
                     ]
        , "One blank line between list items is allowed" =:
          T.unlines
            [ " - Foo"
            , ""
            , " - Bar"
            ] =?>
          bulletList [ para "Foo"
                     , para "Bar"
                     ]
        , "Two blank lines separate lists" =:
          T.unlines
            [ " - Foo"
            , ""
            , ""
            , " - Bar"
            ] =?>
          bulletList [ para "Foo" ] <> bulletList [ para "Bar" ]
        , "No blank line after multiline first item" =:
          T.unlines
            [ " - Foo"
            , "   bar"
            , " - Baz"
            ] =?>
          bulletList [ para "Foo\nbar"
                     , para "Baz"
                     ]
        , "One blank line after multiline first item" =:
          T.unlines
            [ " - Foo"
            , "   bar"
            , ""
            , " - Baz"
            ] =?>
          bulletList [ para "Foo\nbar"
                     , para "Baz"
                     ]
        , "Two blank lines after multiline first item" =:
          T.unlines
            [ " - Foo"
            , "   bar"
            , ""
            , ""
            , " - Baz"
            ] =?>
          bulletList [ para "Foo\nbar" ] <> bulletList [ para "Baz" ]
        , "No blank line after list continuation" =:
          T.unlines
            [ " - Foo"
            , ""
            , "   bar"
            , " - Baz"
            ] =?>
          bulletList [ para "Foo" <> para "bar"
                     , para "Baz"
                     ]
        , "One blank line after list continuation" =:
          T.unlines
            [ " - Foo"
            , ""
            , "   bar"
            , ""
            , " - Baz"
            ] =?>
          bulletList [ para "Foo" <> para "bar"
                     , para "Baz"
                     ]
        , "Two blank lines after list continuation" =:
          T.unlines
            [ " - Foo"
            , ""
            , "   bar"
            , ""
            , ""
            , " - Baz"
            ] =?>
          bulletList [ para "Foo" <> para "bar" ] <> bulletList [ para "Baz" ]
        ]
      -- Test that definition list requires a leading space.
      -- Emacs Muse does not require a space, we follow Amusewiki here.
      , "Not a definition list" =:
        T.unlines
          [ "First :: second"
          , "Foo :: bar"
          ] =?>
        para "First :: second\nFoo :: bar"
      , test emacsMuse "Emacs Muse definition list"
        (T.unlines
          [ "First :: second"
          , "Foo :: bar"
          ] =?>
        definitionList [ ("First", [ para "second" ])
                       , ("Foo", [ para "bar" ])
                       ])
      , "Definition list" =:
        T.unlines
          [ " First :: second"
          , " Foo :: bar"
          ] =?>
        definitionList [ ("First", [ para "second" ])
                       , ("Foo", [ para "bar" ])
                       ]
      , "Definition list term cannot include newline" =:
        T.unlines
          [ " Foo" -- "Foo" is not a part of the definition list term
          , " Bar :: baz"
          ] =?>
        para "Foo" <>
        definitionList [ ("Bar", [ para "baz" ]) ]
      , "One-line definition list" =: " foo :: bar" =?>
        definitionList [ ("foo", [ para "bar" ]) ]
      , "Definition list term with emphasis" =: " *Foo* :: bar\n" =?>
        definitionList [ (emph "Foo", [ para "bar" ]) ]
      , "Multi-line definition lists" =:
        T.unlines
          [ " First term :: Definition of first term"
          , "and its continuation."
          , " Second term :: Definition of second term."
          ] =?>
        definitionList [ ("First term", [ para "Definition of first term\nand its continuation." ])
                       , ("Second term", [ para "Definition of second term." ])
                       ]
      , test emacsMuse "Multi-line definition lists from Emacs Muse manual"
        (T.unlines
          [ "Term1 ::"
          , "  This is a first definition"
          , "  And it has two lines;"
          , "no, make that three."
          , ""
          , "Term2 :: This is a second definition"
          ] =?>
         definitionList [ ("Term1", [ para "This is a first definition\nAnd it has two lines;\nno, make that three."])
                        , ("Term2", [ para "This is a second definition"])
                        ])
      -- Text::Amuse requires indentation with one space
      , "Multi-line definition lists from Emacs Muse manual with initial space" =:
        (T.unlines
          [ " Term1 ::"
          , "  This is a first definition"
          , "  And it has two lines;"
          , "no, make that three."
          , ""
          , " Term2 :: This is a second definition"
          ] =?>
         definitionList [ ("Term1", [ para "This is a first definition\nAnd it has two lines;\nno, make that three."])
                        , ("Term2", [ para "This is a second definition"])
                        ])
      -- Emacs Muse creates two separate lists when indentation of items is different.
      -- We follow Amusewiki and allow different indentation within one list.
      , "Changing indentation" =:
        T.unlines
          [ " First term :: Definition of first term"
          , "and its continuation."
          , "   Second term :: Definition of second term."
          ] =?>
        definitionList [ ("First term", [ para "Definition of first term\nand its continuation." ])
                       , ("Second term", [ para "Definition of second term." ])
                       ]
      , "Two blank lines separate definition lists" =:
        T.unlines
          [ " First :: list"
          , ""
          , ""
          , " Second :: list"
          ] =?>
        definitionList [ ("First", [ para "list" ]) ] <>
        definitionList [ ("Second", [ para "list" ]) ]
      -- Headers in first column of list continuation are not allowed
      , "No headers in list continuation" =:
        T.unlines
          [ " - Foo"
          , ""
          , "   * Bar"
          ] =?>
        bulletList [ mconcat [ para "Foo"
                             , para "* Bar"
                             ]
                   ]
      , "List inside a tag" =:
        T.unlines
          [ "<quote>"
          , " 1. First"
          , ""
          , " 2. Second"
          , ""
          , " 3. Third"
          , "</quote>"
          ] =?>
        blockQuote (orderedListWith (1, Decimal, Period) [ para "First"
                                                         , para "Second"
                                                         , para "Third"
                                                         ])
      -- Amusewiki requires block tags to be on separate lines,
      -- but Emacs Muse allows them to be on the same line as contents.
      , "List inside an inline tag" =:
        T.unlines
          [ "<quote> 1. First"
          , ""
          , " 2. Second"
          , ""
          , " 3. Third</quote>"
          ] =?>
        blockQuote (orderedListWith (1, Decimal, Period) [ para "First"
                                                         , para "Second"
                                                         , para "Third"
                                                         ])
      ]
  ]

{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Muse (tests) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Class

muse :: Text -> Pandoc
muse = purely $ \s -> do
  putCommonState
      def { stInputFiles = Just ["in"]
          , stOutputFile = Just "out"
          }
  readMuse def s

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test muse

spcSep :: [Inlines] -> Inlines
spcSep = mconcat . intersperse space

tests :: [TestTree]
tests =
  [ testGroup "Inlines"
      [ "Plain String" =:
          "Hello, World" =?>
          para (spcSep [ "Hello,", "World" ])

      , "Emphasis" =: "*Foo bar*" =?> para (emph . spcSep $ ["Foo", "bar"])

      , "Emphasis tag" =: "<em>Foo bar</em>" =?> para (emph . spcSep $ ["Foo", "bar"])

      , "Strong" =:
          "**Cider**" =?>
          para (strong "Cider")

      , "Strong tag" =: "<strong>Strong</strong>" =?> para (strong "Strong")

      , "Strong Emphasis" =:
          "***strength***" =?>
          para (strong . emph $ "strength")

      , "Superscript tag" =: "<sup>Superscript</sup>" =?> para (superscript "Superscript")

      , "Subscript tag" =: "<sub>Subscript</sub>" =?> para (subscript "Subscript")

      , "Strikeout tag" =: "<del>Strikeout</del>" =?> para (strikeout "Strikeout")

      , "Linebreak" =: "Line <br>  break" =?> para ("Line" <> linebreak <> "break")

      , "Code" =: "=foo(bar)=" =?> para (code "foo(bar)")

      , "Not code" =: "a=b= =c=d" =?> para (text "a=b= =c=d")

      , "Code tag" =: "<code>foo(bar)</code>" =?> para (code "foo(bar)")

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
      ]

  , testGroup "Blocks"
      [ "Block elements end paragraphs" =:
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
      , "Quote tag" =: "<quote>Hello, world</quote>" =?> blockQuote (para $ text "Hello, world")
      , "Quote" =: "  This is a quotation\n" =?> blockQuote (para $ text "This is a quotation")
      , "Multiline quote" =: T.unlines [ "  This is a quotation"
                                       , "  with a continuation"
                                       ]
        =?> blockQuote (para $ text "This is a quotation with a continuation")
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
          "* First level\n" =?>
          header 1 "First level"
        , "Chapter" =:
          "** Second level\n" =?>
          header 2 "Second level"
        , "Section" =:
          "*** Third level\n" =?>
          header 3 "Third level"
        , "Subsection" =:
          "**** Fourth level\n" =?>
          header 4 "Fourth level"
        , "Subsubsection" =:
          "***** Fifth level\n" =?>
          header 5 "Fifth level"
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
      , "Nested list" =:
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

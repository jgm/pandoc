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
        ]
      ]

  , testGroup "Blocks"
      [ "Quote" =: "<quote>Hello, world</quote>" =?> blockQuote (para $ text "Hello, world")
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
  ]

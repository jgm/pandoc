{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Muse
   Copyright   : © 2017-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Tests for the Muse reader.
-}
module Tests.Readers.Muse (tests) where

import Prelude
import Data.List (intersperse)
import Data.Monoid (Any (..))
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Options (IsOption(defaultValue))
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Writers.Shared (toLegacyTable)
import Text.Pandoc.Walk

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

simpleTable' :: Int -> Caption -> [Blocks] -> [[Blocks]] -> Blocks
simpleTable' n capt headers rows
  = table capt
          (replicate n (AlignDefault, ColWidthDefault))
          (TableHead nullAttr $ toHeaderRow headers)
          [TableBody nullAttr 0 [] $ map toRow rows]
          (TableFoot nullAttr [])
  where
    toRow = Row nullAttr . map simpleCell
    toHeaderRow l = if null l then [] else [toRow l]

-- Tables don't round-trip yet
--
makeRoundTrip :: Block -> Block
makeRoundTrip t@(Table tattr blkCapt specs thead tbody tfoot) =
  if isSimple && numcols > 1
    then t
    else Para [Str "table was here"]
  where (_, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
        numcols = maximum (length aligns : length widths : map length (headers:rows))
        isLineBreak LineBreak = Any True
        isLineBreak _         = Any False
        hasLineBreak = getAny . query isLineBreak
        isSimple = and [ isSimpleHead thead
                       , isSimpleBodies tbody
                       , isSimpleFoot tfoot
                       , all (== 0) widths
                       , isNullAttr tattr
                       , simpleCapt ]
        isNullAttr ("", [], []) = True
        isNullAttr _            = False
        isAlignDefault AlignDefault = True
        isAlignDefault _            = False
        isSimpleRow (Row attr body) = isNullAttr attr && all isSimpleCell body
        isSimpleCell (Cell attr ali h w body)
          = and [ h == 1
                , w == 1
                , isNullAttr attr
                , isAlignDefault ali
                , isSimpleCellBody body ]
        isSimpleCellBody [Plain ils] = not (hasLineBreak ils)
        isSimpleCellBody [Para ils ] = not (hasLineBreak ils)
        isSimpleCellBody []          = True
        isSimpleCellBody _           = False
        simpleCapt = case blkCapt of
          Caption Nothing [Para _]  -> True
          Caption Nothing [Plain _] -> True
          _                         -> False
        isSimpleHead (TableHead attr [r])
          = isNullAttr attr && isSimpleRow r
        isSimpleHead _ = False
        isSimpleBody (TableBody attr rhc hd bd) = and [ isNullAttr attr
                                                      , rhc == 0
                                                      , null hd
                                                      , all isSimpleRow bd ]
        isSimpleBodies [b] = isSimpleBody b
        isSimpleBodies _   = False
        isSimpleFoot (TableFoot attr rs) = isNullAttr attr && null rs

makeRoundTrip (OrderedList (start, LowerAlpha, _) items) = OrderedList (start, Decimal, Period) items
makeRoundTrip (OrderedList (start, UpperAlpha, _) items) = OrderedList (start, Decimal, Period) items
makeRoundTrip x = x

-- Demand that any AST produced by Muse reader and written by Muse writer can be read back exactly the same way.
-- Currently we remove tables and compare first rewrite to the second.
roundTrip :: Blocks -> Bool
roundTrip b = d' == d''
  where d = walk makeRoundTrip $ Pandoc nullMeta $ toList b
        d' = rewrite d
        d'' = rewrite d'
        rewrite = amuse . T.pack . (++ "\n") . T.unpack .
                  purely (writeMuse def { writerExtensions = extensionsFromList [Ext_amuse]
                                          , writerWrapText = WrapPreserve
                                          })

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

      , "Newline in the beginning of emphasis" =:
        "*\nFoo bar*" =?>
        para (text "*\nFoo bar*")
      , "Newline in the end of emphasis" =:
        "*Foo bar\n*" =?>
        para (text "*Foo bar\n*")

      , "Comma after closing *" =:
        "Foo *bar*, baz" =?>
        para ("Foo " <> emph "bar" <> ", baz")

      , "Letter after closing *" =:
        "Foo *bar*x baz" =?>
        para "Foo *bar*x baz"

      , "Letter before opening *" =:
        "Foo x*bar* baz" =?>
        para "Foo x*bar* baz"

      , "Digit after closing *" =:
        "Foo *bar*0 baz" =?>
        para "Foo *bar*0 baz"

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

      , "Strong inside emphasis" =:
        "*foo **bar** baz*" =?>
        para (emph (text "foo " <> strong (text "bar") <> text " baz"))

      , "Emphasis inside strong" =:
        "**foo *bar* baz**" =?>
        para (strong (text "foo " <> emph (text "bar") <> text " baz"))

      , "Opening asterisk can't be preceded by another one" =:
        "**foo*" =?>
        para "**foo*"

      , "Asterisk between words does not terminate emphasis" =:
        "*foo*bar*" =?>
        para (emph "foo*bar")

      , "Two asterisks between words do not terminate emphasis" =:
        "*foo**bar*" =?>
        para (emph "foo**bar")

      , "Three asterisks between words do not terminate emphasis" =:
        "*foo***bar*" =?>
        para (emph "foo***bar")

      , "Two asterisks between words do not terminate strong" =:
        "**foo**bar**" =?>
        para (strong "foo**bar")

      , "Three asterisks between words do not terminate strong" =:
        "**foo***bar**" =?>
        para (strong "foo***bar")

      , "Three asterisks between words do not terminate strong emphasis" =:
        "***foo***bar***" =?>
        para (strong . emph $ "foo***bar")

      , "Six asterisks between words do not terminate strong emphasis" =:
        "***foo******bar***" =?>
        para (strong . emph $ "foo******bar")

      , test emacsMuse "Underline"
        ("_Underline_" =?> para (underline "Underline"))

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

      , "Trailing whitespace inside paragraph" =:
        T.unlines [ "First line " -- trailing whitespace here
                  , "second line"
                  ]
        =?> para "First line\nsecond line"

      , "Non-breaking space" =: "Foo~~bar" =?> para "Foo\160bar"
      , "Single ~" =: "Foo~bar" =?> para "Foo~bar"

      , testGroup "Code markup"
        [ "Code" =: "=foo(bar)=" =?> para (code "foo(bar)")

        , "Not code" =: "a=b= =c=d" =?> para (text "a=b= =c=d")

        -- Emacs Muse 3.20 parses this as code, we follow Amusewiki
        , "Not code if closing = is detached" =: "=this is not a code =" =?> para "=this is not a code ="

        , "Not code if opening = is detached" =: "= this is not a code=" =?> para "= this is not a code="

        , "Code if followed by comma" =:
          "Foo =bar=, baz" =?>
          para (text "Foo " <> code "bar" <> text ", baz")

        , "Not code if followed by digit" =:
          "Foo =bar=0 baz" =?>
          para (text "Foo =bar=0 baz")

        , "One character code" =: "=c=" =?> para (code "c")

        , "Code with equal sign" =: "=foo = bar=" =?> para (code "foo = bar")

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

        , "Code at the beginning of paragraph but not first column" =:
          " - =foo=" =?> bulletList [ para $ code "foo" ]
        ]

      , "Code tag" =: "<code>foo(bar)</code>" =?> para (code "foo(bar)")

      , "Math tag" =: "<math>\\sum_{i=0}^n i^2</math>" =?> para (math "\\sum_{i=0}^n i^2")

      , "Verbatim tag" =: "*<verbatim>*</verbatim>*" =?> para (emph "*")

      , "Verbatim inside code" =: "<code><verbatim>foo</verbatim></code>" =?> para (code "<verbatim>foo</verbatim>")

      , "Verbatim tag after text" =: "Foo <verbatim>bar</verbatim>" =?> para "Foo bar"

      , "Verbatim tag escapes block level markup" =:
        T.unlines [ "Foo <verbatim>bar"
                  , "* Not a heading"
                  , "</verbatim>baz"
                  ] =?>
        para "Foo bar\n* Not a heading\nbaz"

      , "Class tag" =: "<class name=\"foo\">bar</class>" =?> para (spanWith ("", ["foo"], []) "bar")
      , "Class tag without name" =: "<class>foobar</class>" =?> para (spanWith ("", [], []) "foobar")

      , "RTL" =: "<<<foo bar>>>" =?> para (spanWith ("", [], [("dir", "rtl")]) "foo bar")
      , "LTR" =: ">>>foo bar<<<" =?> para (spanWith ("", [], [("dir", "ltr")]) "foo bar")

      -- <em> tag should match with the last </em> tag, not verbatim one
      , "Nested \"</em>\" inside em tag" =: "<em>foo<verbatim></em></verbatim>bar</em>" =?> para (emph "foo</em>bar")

      , testGroup "Links"
        [ "Link without description" =:
          "[[https://amusewiki.org/]]" =?>
          para (link "https://amusewiki.org/" "" (str "https://amusewiki.org/"))
        , "Link with description" =:
          "[[https://amusewiki.org/][A Muse Wiki]]" =?>
          para (link "https://amusewiki.org/" "" (text "A Muse Wiki"))
        , "Link with empty description" =:
          "[[https://amusewiki.org/][]]" =?>
          para (link "https://amusewiki.org/" "" (text ""))
        , "Image" =:
          "[[image.jpg]]" =?>
          para (image "image.jpg" "" mempty)
        , "Closing bracket is not allowed in image filename" =:
          "[[foo]bar.jpg]]" =?>
          para (text "[[foo]bar.jpg]]")
        , "Image with description" =:
          "[[image.jpg][Image]]" =?>
          para (image "image.jpg" "" (text "Image"))
        , "Image with space in filename" =:
          "[[image name.jpg]]" =?>
          para (image "image name.jpg" "" mempty)
        , "Image with width" =:
          "[[image.jpg 60]]" =?>
          para (imageWith ("", [], [("width", "60%")]) "image.jpg" mempty mempty)
        , "At least one space is required between image filename and width" =:
          "[[image.jpg60]]" =?>
          para (link "image.jpg60" mempty (str "image.jpg60"))
        , "Left-aligned image with width" =:
          "[[image.png 60 l][Image]]" =?>
          para (imageWith ("", ["align-left"], [("width", "60%")]) "image.png" "" (str "Image"))
        , "Right-aligned image with width" =:
          "[[image.png 60 r][Image]]" =?>
          para (imageWith ("", ["align-right"], [("width", "60%")]) "image.png" "" (str "Image"))
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
        , "Link with empty URL" =: "[[][empty URL]]" =?> para (link "" "" (text "empty URL"))
        , "No footnotes inside links" =:
          "[[https://amusewiki.org/][foo[1]]" =?>
          para (link "https://amusewiki.org/" "" (text "foo[1"))
        , "Image inside link" =:
          "[[https://amusewiki.org/][Image [[image.png][with it's own description]] inside link description]]" =?>
          para (link "https://amusewiki.org/" "" (text "Image " <> image "image.png" "" (text "with it's own description") <> text " inside link description"))
        , "Link inside image description" =:
          "[[image.jpg][Image from [[https://amusewiki.org/]]]]" =?>
          para (image "image.jpg" "" (text "Image from " <> link "https://amusewiki.org/" "" (str "https://amusewiki.org/")))
        ]

      , testGroup "Literal"
        [ test emacsMuse "Inline literal"
          ("Foo<literal style=\"html\">lit</literal>bar" =?>
          para (text "Foo" <> rawInline "html" "lit" <> text "bar"))
        , test emacsMuse "Single inline literal in paragraph"
          ("<literal style=\"html\">lit</literal>" =?>
          para (rawInline "html" "lit"))
        ]
      ]

  , testGroup "Blocks"
      [ askOption $ \(QuickCheckTests numtests) ->
          testProperty "Round trip" $
            withMaxSuccess (if QuickCheckTests numtests == defaultValue
                               then 25
                               else numtests) roundTrip
      , "Block elements end paragraphs" =:
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
      , testGroup "Page breaks"
        [ "Page break" =:
          "      * * * * *" =?>
          divWith ("", [], [("style", "page-break-before: always;")]) mempty
        , "Page break with trailing space" =:
          "      * * * * * " =?>
          divWith ("", [], [("style", "page-break-before: always;")]) mempty
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
        , "Indentation does not indicate quote inside quote tag" =:
          T.unlines [ "<quote>"
                    , "  Not a nested quote"
                    , "</quote>"
                    ] =?>
          blockQuote (para "Not a nested quote")
        , "Multiline quote" =:
          T.unlines [ "  This is a quotation"
                    , "  with a continuation"
                    ] =?>
          blockQuote (para "This is a quotation\nwith a continuation")
        , testGroup "Div"
          [ "Div without id" =:
            T.unlines [ "<div>"
                      , "Foo bar"
                      , "</div>"
                      ] =?>
            divWith nullAttr (para "Foo bar")
          , "Div with id" =:
            T.unlines [ "<div id=\"foo\">"
                      , "Foo bar"
                      , "</div>"
                      ] =?>
            divWith ("foo", [], []) (para "Foo bar")
          ]
        , "Biblio" =:
          T.unlines [ "<biblio>"
                    , ""
                    , "Author, *Title*, description"
                    , ""
                    , "Another author, *Another title*, another description"
                    , ""
                    , "</biblio>"
                    ] =?>
          divWith ("", ["biblio"], []) (para (text "Author, " <> emph "Title" <> ", description") <>
                                        para (text "Another author, " <> emph "Another title" <> text ", another description"))
        , "Play" =:
          T.unlines [ "<play>"
                    , "Foo bar"
                    , "</play>"
                    ] =?>
          divWith ("", ["play"], []) (para "Foo bar")
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
      , "Verse in list" =: " - > foo" =?> bulletList [ lineBlock [ "foo" ] ]
      , "Verse line starting with emphasis" =: "> *foo* bar" =?> lineBlock [ emph "foo" <> text " bar" ]
      , "Multiline verse in list" =:
        T.unlines [ " - > foo"
                  , "   > bar"
                  ] =?>
        bulletList [ lineBlock [ "foo", "bar" ] ]
      , "Paragraph after verse in list" =:
        T.unlines [ " - > foo"
                  , "   bar"
                  ] =?>
        bulletList [ lineBlock [ "foo" ] <> para "bar" ]
      , "Empty quote tag" =:
        T.unlines [ "<quote>"
                  , "</quote>"
                  ]
        =?> blockQuote mempty
      , "Quote tag" =:
        T.unlines [ "<quote>"
                  , "Hello, world"
                  , "</quote>"
                  ]
        =?> blockQuote (para $ text "Hello, world")
      , "Nested quote tag" =:
        T.unlines [ "<quote>"
                  , "foo"
                  , "<quote>"
                  , "bar"
                  , "</quote>"
                  , "baz"
                  , "</quote>"
                  ] =?>
        blockQuote (para "foo" <> blockQuote (para "bar") <> para "baz")
      , "Indented quote inside list" =:
        T.unlines [ " -  <quote>"
                  , "    foo"
                  , "    </quote>"
                  ] =?>
        bulletList [ blockQuote (para "foo") ]
      , "Verse tag" =:
        T.unlines [ "<verse>"
                  , ""
                  , "Foo bar baz"
                  , "  One two three"
                  , ""
                  , "</verse>"
                  ] =?>
        lineBlock [ ""
                  , text "Foo bar baz"
                  , text "\160\160One two three"
                  , ""
                  ]
      , "Verse tag with empty line inside" =:
        T.unlines [ "<verse>"
                  , ""
                  , "</verse>"
                  ] =?>
        lineBlock [ "" ]
      , "Verse tag with verbatim close tag inside" =:
        T.unlines [ "<verse>"
                  , "<verbatim></verse></verbatim>"
                  , "</verse>"
                  ] =?>
        lineBlock [ "</verse>" ]
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
        , "Indented braces" =:
          T.unlines [ " - {{{"
                    , "   Example line"
                    , "   }}}"
                    ] =?>
          bulletList [ codeBlock "Example line" ]
        , "Tabs" =:
          T.unlines [ "{{{"
                    , "\t  foo"
                    , "\t\t"
                    , "\t  bar"
                    , "}}}"
                    ] =?>
          codeBlock "  foo\n\t\n  bar"
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
        , "Empty example inside list" =:
          T.unlines [ " - <example>"
                    , "   </example>"
                    ] =?>
          bulletList [ codeBlock "" ]
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
        ]
      , "Center" =:
        T.unlines [ "<center>"
                  , "Hello, world"
                  , "</center>"
                  ] =?>
        para (text "Hello, world")
      , "Right" =:
        T.unlines [ "<right>"
                  , "Hello, world"
                  , "</right>"
                  ] =?>
        para (text "Hello, world")
      , testGroup "Comments"
        [ "Comment tag" =: "<comment>\nThis is a comment\n</comment>" =?> (mempty::Blocks)
        , "Line comment" =: "; Comment" =?> (mempty::Blocks)
        , "Empty comment" =: ";" =?> (mempty::Blocks)
        , "Text after empty comment" =: ";\nfoo" =?> para "foo" -- Make sure we don't consume newline while looking for whitespace
        , "Not a comment (does not start with a semicolon)" =: " ; Not a comment" =?> para (text "; Not a comment")
        , "Not a comment (has no space after semicolon)" =: ";Not a comment" =?> para (text ";Not a comment")
        , "Not a comment (semicolon not in the first column)" =: " - ; foo" =?> bulletList [para "; foo"]
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
        , "Headers consume anchors" =:
          T.unlines [ "; A comment to make sure anchor is not parsed as a directive"
                    , "#bar"
                    , "** Foo"
                    ] =?>
          headerWith ("bar",[],[]) 2 "Foo"
        , "Headers don't consume anchors separated with a blankline" =:
          T.unlines [ "; A comment to make sure anchor is not parsed as a directive"
                    , "#bar"
                    , ""
                    , "** Foo"
                    ] =?>
          para (spanWith ("bar", [], []) mempty) <>
          header 2 "Foo"
        , "Headers terminate paragraph" =:
          T.unlines [ "foo"
                    , "* bar"
                    ] =?>
          para "foo" <> header 1 "bar"
        , "Headers terminate lists" =:
          T.unlines [ " - foo"
                    , "* bar"
                    ] =?>
          bulletList [ para "foo" ] <>
          header 1 "bar"
        , test emacsMuse "Paragraphs terminate Emacs Muse headers"
          (T.unlines [ "* Foo"
                    , "bar"
                    ] =?> header 1 "Foo" <> para "bar")
        , "Paragraphs don't terminate Text::Amuse headers" =:
          T.unlines [ "* Foo"
                    , "bar"
                    ] =?> header 1 "Foo\nbar"
        , "Empty header" =:
          T.unlines [ "Foo"
                    , ""
                    , "* "
                    , ""
                    , "bar"
                    ] =?> para (text "Foo") <> header 1 "" <> para (text "bar")
        , test (purely $ readMuse def { readerExtensions = extensionsFromList [Ext_amuse, Ext_auto_identifiers]})
               "Auto identifiers"
          (T.unlines [ "* foo"
                     , "** Foo"
                     , "* bar"
                     , "** foo"
                     , "* foo"
                     ] =?> headerWith ("foo",[],[]) 1 "foo" <>
                           headerWith ("foo-1",[],[]) 2 "Foo" <>
                           headerWith ("bar",[],[]) 1 "bar" <>
                           headerWith ("foo-2",[],[]) 2 "foo" <>
                           headerWith ("foo-3",[],[]) 1 "foo")
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
        , "Multiple directives" =:
          T.unlines [ "#title Document title"
                    , "#subtitle Document subtitle"
                    ] =?>
          Pandoc (setMeta "title" (MetaInlines $ toList "Document title") $
                  setMeta "subtitle" (MetaInlines $ toList "Document subtitle") nullMeta) mempty
        , "Multiline directive" =:
          T.unlines [ "#title Document title"
                    , "#notes First line"
                    , "and second line"
                    , "#author Name"
                    ] =?>
          Pandoc (setMeta "title" (MetaInlines $ toList "Document title") $
                  setMeta "notes" (MetaInlines $ toList "First line\nand second line") $
                  setMeta "author" (MetaInlines $ toList "Name") nullMeta) mempty
        , "Amusewiki's #cover is translated to pandoc's #cover-image" =:
          "#cover cover.png" =?>
          let titleInline = toList "cover.png"
              meta = setMeta "cover-image" (MetaInlines titleInline) nullMeta
          in Pandoc meta mempty
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
        , "Anchor with \"-\"" =:
          T.unlines [ "; A comment to make sure anchor is not parsed as a directive"
                    , "#anchor-id Target"
                    ] =?>
          para (spanWith ("anchor-id", [], []) mempty <> "Target")
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
        , "Simple secondary footnote" =:
          T.unlines [ "Here is a secondary note{1}."
                    , ""
                    , "{1} Secondary note contents"
                    ] =?>
          para (text "Here is a secondary note" <>
                note (para "Secondary note contents") <>
                str ".")
        , "Missing footnote" =: "Foo[1]" =?> para "Foo[1]"
        , "Missing secondary note" =: "Foo{1}" =?> para "Foo{1}"
        , "Wrong note type" =:
          T.unlines [ "Here is a secondary note{1}"
                    , ""
                    , "Footnote contents[1]"
                    ] =?>
          para "Here is a secondary note{1}" <>
          para "Footnote contents[1]"
        , "Recursive footnote" =:
          T.unlines [ "Start recursion here[1]"
                    , ""
                    , "[1] Recursion continues here[1]"
                    ] =?>
          para (text "Start recursion here" <>
                note (para "Recursion continues here[1]"))
        , "Nested footnotes" =:
          T.unlines [ "Footnote: [1]"
                    , ""
                    , "[1] Nested: [2]"
                    , ""
                    , "[2] No recursion: [1]"
                    ] =?>
          para (text "Footnote: " <>
                note (para (text "Nested: " <> note (para $ text "No recursion: [1]"))))
        , "No zero footnotes" =:
          T.unlines [ "Here is a footnote[0]."
                    , ""
                    , "[0] Footnote contents"
                    ] =?>
          para "Here is a footnote[0]." <>
          para "[0] Footnote contents"
        , "Footnotes can't start with zero" =:
          T.unlines [ "Here is a footnote[01]."
                    , ""
                    , "[01] Footnote contents"
                    ] =?>
          para "Here is a footnote[01]." <>
          para "[01] Footnote contents"
        , testGroup "Multiparagraph footnotes"
          [ "Amusewiki multiparagraph footnotes" =:
            T.unlines [ "Multiparagraph[1] footnotes[2]"
                      , ""
                      , "[1] First footnote paragraph"
                      , ""
                      , "    Second footnote paragraph"
                      , "with continuation"
                      , ""
                      , "Not a note"
                      , "[2] Second footnote"
                      ] =?>
            para (text "Multiparagraph" <>
                  note (para "First footnote paragraph" <>
                        para "Second footnote paragraph\nwith continuation") <>
                  text " footnotes" <>
                  note (para "Second footnote")) <>
            para (text "Not a note")

          -- Verse requires precise indentation, so it is good to test indentation requirements
          , "Note continuation with verse" =:
            T.unlines [ "Foo[1]"
                      , ""
                      , "[1] Bar"
                      , ""
                      , "    > Baz"
                      ] =?>
            para ("Foo" <> note (para "Bar" <> lineBlock ["Baz"]))
          , "Footnote ending in self-terminating element and followed by paragraph" =:
            T.unlines [ "Foo[1]"
                      , ""
                      , "[1] > bar"
                      , "baz"
                      ] =?>
            para (str "Foo" <> note (lineBlock ["bar"])) <> para (str "baz")

          , "Footnote starting with empty line" =:
            T.unlines [ "Foo[1]"
                      , ""
                      , "[1]" -- No space character after note marker
                      , ""
                      , "    Bar"
                      ] =?>
            para (str "Foo" <> note (para $ text "Bar"))
          , "Indentation in footnote starting with empty line" =:
            T.unlines [ "Foo[1]"
                      , ""
                      , "[1]" -- No space character after note marker
                      , ""
                      , "   Bar"
                      ] =?>
            para (str "Foo" <> note mempty) <> blockQuote (para $ text "Bar")
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
          simpleTable [] [[plain "One", plain "Two"]]
        , "Table with multiple words" =:
          "One two | three four" =?>
          simpleTable [] [[plain "One two", plain "three four"]]
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
          simpleTable [] [[plain "One", plain "Two"],
                          [plain "Three", plain "Four"]]
        , "Table with one header" =:
          T.unlines
            [ "First || Second"
            , "Third | Fourth"
            ] =?>
          simpleTable [plain "First", plain "Second"] [[plain "Third", plain "Fourth"]]
        , "Table with two headers" =:
          T.unlines
            [ "First || header"
            , "Second || header"
            , "Foo | bar"
            ] =?>
          simpleTable [plain "First", plain "header"] [[plain "Second", plain "header"],
                                                       [plain "Foo", plain "bar"]]
        , "Header and footer reordering" =:
          T.unlines
            [ "Foo ||| bar"
            , "Baz || foo"
            , "Bar | baz"
            ] =?>
          simpleTable [plain "Baz", plain "foo"] [[plain "Bar", plain "baz"],
                                                  [plain "Foo", plain "bar"]]
        , "Table with caption" =:
          T.unlines
            [ "Foo || bar || baz"
            , "First | row | here"
            , "Second | row | there"
            , "|+ Table caption +|"
            ] =?>
          simpleTable' 3 (simpleCaption $ plain $ text "Table caption")
                       [plain "Foo", plain "bar", plain "baz"]
                       [[plain "First", plain "row", plain "here"],
                        [plain "Second", plain "row", plain "there"]]
        , "Table caption with +" =:
          T.unlines
            [ "Foo | bar"
            , "|+ Table + caption +|"
            ] =?>
          simpleTable' 2 (simpleCaption $ plain $ text "Table + caption")
                       []
                       [[plain "Foo", plain "bar"]]
        , "Caption without table" =:
          "|+ Foo bar baz +|" =?>
          simpleTable' 0 (simpleCaption $ plain $ text "Foo bar baz") [] []
        , "Table indented with space" =:
          T.unlines
            [ " Foo | bar"
            , " Baz | foo"
            , " Bar | baz"
            ] =?>
          simpleTable [] [[plain "Foo", plain "bar"],
                          [plain "Baz", plain "foo"],
                          [plain "Bar", plain "baz"]]
        , "Empty cells" =:
          T.unlines
            [ " | Foo"
            , " |"
            , " bar |"
            , " || baz"
            ] =?>
          simpleTable [plain "", plain "baz"] [[plain "", plain "Foo"],
                                               [plain "", plain ""],
                                               [plain "bar", plain ""]]
        , "Empty cell in the middle" =:
          T.unlines
            [ " 1 | 2 | 3"
            , " 4 |   | 6"
            , " 7 | 8 | 9"
            ] =?>
          simpleTable []
                      [[plain "1", plain "2", plain "3"],
                       [plain "4", mempty,    plain "6"],
                       [plain "7", plain "8", plain "9"]]
        , "Grid table" =:
          T.unlines
            [ "+-----+-----+"
            , "| foo | bar |"
            , "+-----+-----+"
            ] =?>
          simpleTable [] [[para "foo", para "bar"]]
        , "Grid table inside list" =:
          T.unlines
            [ " - +-----+-----+"
            , "   | foo | bar |"
            , "   +-----+-----+"
            ] =?>
          bulletList [simpleTable [] [[para "foo", para "bar"]]]
        , "Grid table with two rows" =:
          T.unlines
            [ "+-----+-----+"
            , "| foo | bar |"
            , "+-----+-----+"
            , "| bat | baz |"
            , "+-----+-----+"
            ] =?>
          simpleTable [] [[para "foo", para "bar"]
                         ,[para "bat", para "baz"]]
        , "Grid table inside grid table" =:
          T.unlines
            [ "+-----+"
            , "|+---+|"
            , "||foo||"
            , "|+---+|"
            , "+-----+"
            ] =?>
          simpleTable [] [[simpleTable [] [[para "foo"]]]]
        , "Grid table with example" =:
          T.unlines
            [ "+------------+"
            , "| <example>  |"
            , "| foo        |"
            , "| </example> |"
            , "+------------+"
            ] =?>
          simpleTable [] [[codeBlock "foo"]]
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
      , "Ordered list with roman numerals" =:
        T.unlines
          [ " i. First"
          , " ii. Second"
          , " iii. Third"
          , " iv. Fourth"
          ] =?>
        orderedListWith (1, LowerRoman, Period) [ para "First"
                                                , para "Second"
                                                , para "Third"
                                                , para "Fourth"
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
      , "Bullet list with last item empty" =:
        T.unlines
          [ " -"
          , ""
          , "foo"
          ] =?>
        bulletList [ mempty ] <>
        para "foo"
      , testGroup "Nested lists"
        [ "Nested bullet list" =:
          T.unlines [ " - Item1"
                    , "   - Item2"
                    , "     - Item3"
                    , "   - Item4"
                    , "     - Item5"
                    , " - Item6"
                    ] =?>
          bulletList [ para "Item1" <>
                       bulletList [ para "Item2" <>
                                    bulletList [ para "Item3" ]
                                  , para "Item4" <>
                                    bulletList [ para "Item5" ]
                                  ]
                     , para "Item6"
                     ]
        , "Nested ordered list" =:
          T.unlines [ " 1. Item1"
                    , "    1. Item2"
                    , "       1. Item3"
                    , "    2. Item4"
                    , "       1. Item5"
                    , " 2. Item6"
                    ] =?>
          orderedListWith (1, Decimal, Period) [ para "Item1" <>
                                                 orderedListWith (1, Decimal, Period) [ para "Item2" <>
                                                                                        orderedListWith (1, Decimal, Period) [ para "Item3" ]
                                                                                      , para "Item4" <>
                                                                                        orderedListWith (1, Decimal, Period) [ para "Item5" ]
                                                                                      ]
                                               , para "Item6"
                                               ]
        , "Mixed nested list" =:
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
      , "List continuation after nested list" =:
         T.unlines
           [ " - - foo"
           , ""
           , "   bar"
           ] =?>
         bulletList [ bulletList [ para "foo" ] <>
                      para "bar"
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
        , "No blank line after blockquote" =:
          T.unlines
            [ " - <quote>"
            , "   foo"
            , "   </quote>"
            , " - bar"
            ] =?>
          bulletList [ blockQuote $ para "foo", para "bar" ]
        , "One blank line after blockquote" =:
          T.unlines
            [ " - <quote>"
            , "   foo"
            , "   </quote>"
            , ""
            , " - bar"
            ] =?>
          bulletList [ blockQuote $ para "foo", para "bar" ]
        , "Two blank lines after blockquote" =:
          T.unlines
            [ " - <quote>"
            , "   foo"
            , "   </quote>"
            , ""
            , ""
            , " - bar"
            ] =?>
          bulletList [ blockQuote $ para "foo" ] <> bulletList [ para "bar" ]
        , "No blank line after verse" =:
          T.unlines
            [ " - > foo"
            , " - bar"
            ] =?>
          bulletList [ lineBlock [ "foo" ], para "bar" ]
        , "One blank line after verse" =:
          T.unlines
            [ " - > foo"
            , ""
            , " - bar"
            ] =?>
          bulletList [ lineBlock [ "foo" ], para "bar" ]
        , "Two blank lines after verse" =:
          T.unlines
            [ " - > foo"
            , ""
            , ""
            , " - bar"
            ] =?>
          bulletList [ lineBlock [ "foo" ] ] <> bulletList [ para "bar" ]
        ]
      , "List ending in self-terminating element and followed by paragraph" =:
        T.unlines [ " - > Foo"
                  , "bar"
                  ] =?>
        bulletList [lineBlock ["Foo"]] <> para (str "bar")
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
      , "Definition list term may include single colon" =:
        " foo:bar :: baz" =?>
        definitionList [ ("foo:bar", [ para "baz" ]) ]
      , "Definition list term with emphasis" =: " *Foo* :: bar\n" =?>
        definitionList [ (emph "Foo", [ para "bar" ]) ]
      , "Definition list term with :: inside code" =: " foo <code> :: </code> :: bar <code> :: </code> baz\n" =?>
        definitionList [ ("foo " <> code " :: ", [ para $ "bar " <> code " :: " <> " baz" ]) ]
      , "Multi-line definition lists" =:
        T.unlines
          [ " First term :: Definition of first term"
          , "and its continuation."
          , " Second term :: Definition of second term."
          ] =?>
        definitionList [ ("First term", [ para "Definition of first term\nand its continuation." ])
                       , ("Second term", [ para "Definition of second term." ])
                       ]
      , "Definition list with verse" =:
        T.unlines
          [ " First term :: Definition of first term"
          , "  > First verse"
          , "  > Second line of first verse"
          , ""
          , "               > Second verse"
          , "               > Second line of second verse"
          ] =?>
        definitionList [ ("First term", [ para "Definition of first term" <>
                                          lineBlock [ text "First verse"
                                                    , text "Second line of first verse"
                                                    ] <>
                                          lineBlock [ text "Second verse"
                                                    , text "Second line of second verse"
                                                    ]
                                        ])
                       ]
      , "Definition list with table" =:
        " foo :: bar | baz" =?>
        definitionList [ ("foo", [ simpleTable [] [[plain "bar", plain "baz"]]
                                 ])]
      , "Definition list with table inside bullet list" =:
        " - foo :: bar | baz" =?>
        bulletList [definitionList [ ("foo", [ simpleTable [] [[plain "bar", plain "baz"]]
                                             ])]]
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
      , "One-line nested definition list" =:
        " Foo :: bar :: baz" =?>
        definitionList [ ("Foo", [ definitionList [ ("bar", [ para "baz" ])]])]
      , "Nested definition list" =:
        T.unlines
        [ " First :: Second :: Third"
        , "          Fourth :: Fifth :: Sixth"
        , " Seventh :: Eighth"
        ] =?>
        definitionList [ ("First", [ definitionList [ ("Second", [ para "Third" ]),
                                                      ("Fourth", [ definitionList [ ("Fifth", [ para "Sixth"] ) ] ] ) ] ] )
                       , ("Seventh", [ para "Eighth" ])
                       ]
      , testGroup "Definition lists with multiple descriptions"
        [ "Correctly indented second description" =:
          T.unlines
          [ " First term :: first description"
          , "  :: second description"
          ] =?>
          definitionList [ ("First term", [ para "first description"
                                          , para "second description"
                                          ])
                         ]
        , "Incorrectly indented second description" =:
          T.unlines
          [ " First term :: first description"
          , " :: second description"
          ] =?>
          definitionList [ ("First term", [ para "first description" ])
                         , ("", [ para "second description" ])
                         ]
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
      , "Bullet list inside a tag" =:
        T.unlines
          [ "<quote>"
          , " - First"
          , ""
          , " - Second"
          , ""
          , " - Third"
          , "</quote>"
          ] =?>
        blockQuote (bulletList [ para "First"
                               , para "Second"
                               , para "Third"
                               ])
      , "Ordered list inside a tag" =:
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
      -- Regression test for a bug caught by round-trip test
      , "Do not consume whitespace while looking for end tag" =:
        T.unlines
          [ "<quote>"
          , " - <quote>"
          , "   foo"
          , "   </quote>"
          , " bar" -- Do not consume whitespace while looking for arbitrarily indented </quote>
          , "</quote>"
          ] =?>
        blockQuote (bulletList [ blockQuote $ para "foo" ] <> para "bar")

      , "Unclosed quote tag" =:
        T.unlines
          [ "<quote>"
          , "<verse>"
          , "</quote>"
          , "</verse>"
          ] =?>
        para "<quote>" <> lineBlock [ "</quote>" ]

      , "Unclosed quote tag inside list" =:
        T.unlines
          [ " - <quote>"
          , "   <verse>"
          , "   </quote>"
          , "   </verse>"
          ] =?>
        bulletList [ para "<quote>" <> lineBlock [ "</quote>" ] ]

      -- Allowing indented closing tags is dangerous,
      -- as they may terminate lists
      , "No indented closing tags" =:
        T.unlines
          [ "<quote>"
          , ""
          , " - Foo"
          , ""
          , "   </quote>"
          , ""
          , "   bar"
          , ""
          , "   <verse>"
          , "   </quote>"
          , "   </verse>"
          , "</quote>"
          ] =?>
        blockQuote (bulletList [ para "Foo" <> para "</quote>" <> para "bar" <> lineBlock [ "</quote>" ] ])
      ]
  ]

{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Docbook (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

docbook :: (ToPandoc a) => a -> Text
docbook = docbookWithOpts def{ writerWrapText = WrapNone }

docbookWithOpts :: ToPandoc a => WriterOptions -> a -> Text
docbookWithOpts opts = purely (writeDocbook4 opts) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test docbook "my test" $ X =?> Y

which is in turn shorthand for

  test docbook "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToText a, ToPandoc a)
     => String -> (a, Text) -> TestTree
(=:) = test docbook

lineblock :: Blocks
lineblock = para ("some text" <> linebreak <>
                  "and more lines" <> linebreak <>
                  "and again")

lineblock_out :: [Text]
lineblock_out = [ "<literallayout>some text"
                , "and more lines"
                , "and again</literallayout>"
                ]

tests :: [TestTree]
tests = [ testGroup "line blocks"
          [ "none"       =: para "This is a test"
                              =?> unlines
                                    [ "<para>"
                                    , "  This is a test"
                                    , "</para>"
                                    ]
          , "basic"      =: lineblock
                              =?> unlines lineblock_out
          , "blockquote" =: blockQuote lineblock
                              =?> unlines
                                    ( [ "<blockquote>" ] ++
                                      lineblock_out ++
                                      [ "</blockquote>" ]
                                    )
          , "footnote"   =: para ("This is a test" <>
                                  note lineblock <>
                                  " of footnotes")
                              =?> unlines
                                    ( [ "<para>"
                                      , "  This is a test<footnote>" ] ++
                                      lineblock_out ++
                                      [ "  </footnote> of footnotes"
                                      , "</para>" ]
                                    )
          ]
        , testGroup "divs"
          [ "admonition" =: divWith ("foo", ["warning"], []) (para "This is a test")
                              =?> unlines
                                    [ "<warning id=\"foo\">"
                                    , "  <para>"
                                    , "    This is a test"
                                    , "  </para>"
                                    , "</warning>"
                                    ]
          , "admonition-with-title" =:
                            divWith ("foo", ["attention"], []) (
                              divWith ("foo", ["title"], [])
                                (plain (text "This is title")) <>
                              para "This is a test"
                            )
                              =?> unlines
                                    [ "<attention id=\"foo\">"
                                    , "  <title>This is title</title>"
                                    , "  <para>"
                                    , "    This is a test"
                                    , "  </para>"
                                    , "</attention>"
                                    ]
          , "admonition-with-title-in-para" =:
                            divWith ("foo", ["attention"], []) (
                              divWith ("foo", ["title"], [])
                                (para "This is title") <>
                              para "This is a test"
                            )
                              =?> unlines
                                    [ "<attention id=\"foo\">"
                                    , "  <title>This is title</title>"
                                    , "  <para>"
                                    , "    This is a test"
                                    , "  </para>"
                                    , "</attention>"
                                    ]
          , "single-child" =:
                            divWith ("foo", [], []) (para "This is a test")
                              =?> unlines
                                    [ "<para id=\"foo\">"
                                    , "  This is a test"
                                    , "</para>"
                                    ]
          , "single-literal-child" =:
                            divWith ("foo", [], []) lineblock
                              =?> unlines
                                    [ "<literallayout id=\"foo\">some text"
                                    , "and more lines"
                                    , "and again</literallayout>"
                                    ]
          , "multiple-children" =:
                            divWith ("foo", [], []) (
                              para "This is a test" <>
                              para "This is an another test"
                            )
                              =?> unlines
                                    [ "<anchor id=\"foo\" />"
                                    , "<para>"
                                    , "  This is a test"
                                    , "</para>"
                                    , "<para>"
                                    , "  This is an another test"
                                    , "</para>"
                                    ]
          ]
        , testGroup "compact lists"
          [ testGroup "bullet"
            [ "compact"    =: bulletList [plain "a", plain "b", plain "c"]
                                =?> unlines
                                      [ "<itemizedlist spacing=\"compact\">"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      a"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      b"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      c"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "</itemizedlist>"
                                      ]
            , "loose"      =: bulletList [para "a", para "b", para "c"]
                                =?> unlines
                                      [ "<itemizedlist>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      a"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      b"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      c"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "</itemizedlist>"
                                      ]
            ]
          , testGroup "ordered"
            [ "compact"    =: orderedList [plain "a", plain "b", plain "c"]
                                =?> unlines
                                      [ "<orderedlist spacing=\"compact\">"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      a"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      b"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      c"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "</orderedlist>"
                                      ]
            , "loose"      =: orderedList [para "a", para "b", para "c"]
                                =?> unlines
                                      [ "<orderedlist>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      a"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      b"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "  <listitem>"
                                      , "    <para>"
                                      , "      c"
                                      , "    </para>"
                                      , "  </listitem>"
                                      , "</orderedlist>"
                                      ]
            ]
          , testGroup "definition"
            [ "compact"    =: definitionList [ ("an", [plain "apple" ])
                                             , ("a",  [plain "banana"])
                                             , ("an", [plain "orange"])]
                                =?> unlines
                                      [ "<variablelist spacing=\"compact\">"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      an"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        apple"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      a"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        banana"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      an"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        orange"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "</variablelist>"
                                      ]
            , "loose"      =: definitionList [ ("an", [para "apple" ])
                                             , ("a",  [para "banana"])
                                             , ("an", [para "orange"])]
                                =?> unlines
                                      [ "<variablelist>"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      an"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        apple"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      a"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        banana"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "  <varlistentry>"
                                      , "    <term>"
                                      , "      an"
                                      , "    </term>"
                                      , "    <listitem>"
                                      , "      <para>"
                                      , "        orange"
                                      , "      </para>"
                                      , "    </listitem>"
                                      , "  </varlistentry>"
                                      , "</variablelist>"
                                      ]
            ]
          ]
        , testGroup "writer options"
          [ testGroup "top-level division" $
            let
              headers =  header 1 (text "header1")
                      <> header 2 (text "header2")
                      <> header 3 (text "header3")

              docbookTopLevelDiv :: (ToPandoc a)
                                 => TopLevelDivision -> a -> Text
              docbookTopLevelDiv division =
                docbookWithOpts def{ writerTopLevelDivision = division }
            in
            [ test (docbookTopLevelDiv TopLevelSection) "sections as top-level" $
              headers =?>
              unlines [ "<sect1>"
                      , "  <title>header1</title>"
                      , "  <sect2>"
                      , "    <title>header2</title>"
                      , "    <sect3>"
                      , "      <title>header3</title>"
                      , "      <para>"
                      , "      </para>"
                      , "    </sect3>"
                      , "  </sect2>"
                      , "</sect1>"
                      ]
            , test (docbookTopLevelDiv TopLevelChapter) "chapters as top-level" $
              headers =?>
              unlines [ "<chapter>"
                      , "  <title>header1</title>"
                      , "  <sect1>"
                      , "    <title>header2</title>"
                      , "    <sect2>"
                      , "      <title>header3</title>"
                      , "      <para>"
                      , "      </para>"
                      , "    </sect2>"
                      , "  </sect1>"
                      , "</chapter>"
                      ]
            , test (docbookTopLevelDiv TopLevelPart) "parts as top-level" $
              headers =?>
              unlines [ "<part>"
                      , "  <title>header1</title>"
                      , "  <chapter>"
                      , "    <title>header2</title>"
                      , "    <sect1>"
                      , "      <title>header3</title>"
                      , "      <para>"
                      , "      </para>"
                      , "    </sect1>"
                      , "  </chapter>"
                      , "</part>"
                      ]
            , test (docbookTopLevelDiv TopLevelDefault) "default top-level" $
              headers =?>
              unlines [ "<sect1>"
                      , "  <title>header1</title>"
                      , "  <sect2>"
                      , "    <title>header2</title>"
                      , "    <sect3>"
                      , "      <title>header3</title>"
                      , "      <para>"
                      , "      </para>"
                      , "    </sect3>"
                      , "  </sect2>"
                      , "</sect1>"
                      ]
            ]
          ]
        ]

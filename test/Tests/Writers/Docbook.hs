{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Docbook (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

docbook :: (ToPandoc a) => a -> String
docbook = docbookWithOpts def{ writerWrapText = WrapNone }

docbook5 :: (ToPandoc a) => a -> String
docbook5 = docbook5WithOpts def{ writerWrapText = WrapNone }

docbookWithOpts :: ToPandoc a => WriterOptions -> a -> String
docbookWithOpts opts = unpack . purely (writeDocbook4 opts) . toPandoc

docbook5WithOpts :: ToPandoc a => WriterOptions -> a -> String
docbook5WithOpts opts = unpack . purely (writeDocbook5 opts) . toPandoc
{-
  "my test" =: X =?> Y

is shorthand for

  test docbook "my test" $ X =?> Y

which is in turn shorthand for

  test docbook "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test docbook

lineblock :: Blocks
lineblock = para ("some text" <> linebreak <>
                  "and more lines" <> linebreak <>
                  "and again")
lineblock_out :: [String]
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
                            divWith ("foo", ["note"], []) (
                              divWith ("foo", ["title"], [])
                                (plain (text "This is title")) <>
                              para "This is a test"
                            )
                              =?> unlines
                                    [ "<note id=\"foo\">"
                                    , "  <title>This is title</title>"
                                    , "  <para>"
                                    , "    This is a test"
                                    , "  </para>"
                                    , "</note>"
                                    ]
          , "admonition-with-title-in-para" =:
                            divWith ("foo", ["note"], []) (
                              divWith ("foo", ["title"], [])
                                (para "This is title") <>
                              para "This is a test"
                            )
                              =?> unlines
                                    [ "<note id=\"foo\">"
                                    , "  <title>This is title</title>"
                                    , "  <para>"
                                    , "    This is a test"
                                    , "  </para>"
                                    , "</note>"
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
                                 => TopLevelDivision -> a -> String
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
          , testGroup "section attributes" $
            let
              headers =  headerWith ("myid1",[],[("role","internal"),("xml:id","anotherid"),("dir","rtl")]) 1 "header1"
                      <> headerWith ("myid2",[],[("invalidname","value"),("arch","linux"),("dir","invaliddir")]) 1 "header2"
            in
            [ test docbook5 "sections with attributes (db5)" $
              headers =?>
              unlines [ "<section xmlns=\"http://docbook.org/ns/docbook\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xml:id=\"myid1\" role=\"internal\" dir=\"rtl\">"
                      , "  <title>header1</title>"
                      , "  <para>"
                      , "  </para>"
                      , "</section>"
                      , "<section xmlns=\"http://docbook.org/ns/docbook\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xml:id=\"myid2\">"
                      , "  <title>header2</title>"
                      , "  <para>"
                      , "  </para>"
                      , "</section>"
                      ]
            , test docbook "sections with attributes (db4)" $
              headers =?>
              unlines [ "<sect1 id=\"myid1\" role=\"internal\">"
                      , "  <title>header1</title>"
                      , "  <para>"
                      , "  </para>"
                      , "</sect1>"
                      , "<sect1 id=\"myid2\" arch=\"linux\">"
                      , "  <title>header2</title>"
                      , "  <para>"
                      , "  </para>"
                      , "</sect1>"
                      ]
            ]
        ]

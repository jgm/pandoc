{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Docbook (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

docbook :: (ToString a, ToPandoc a) => a -> String
docbook = writeDocbook def{ writerWrapText = False } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test docbook "my test" $ X =?> Y

which is in turn shorthand for

  test docbook "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
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

tests :: [Test]
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
        ]

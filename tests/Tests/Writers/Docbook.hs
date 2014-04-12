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
lineblock_out :: String
lineblock_out =   "<literallayout>some text\n" ++
                  "and more lines\n" ++
                  "and again</literallayout>"

tests :: [Test]
tests = [ testGroup "line blocks"
          [ "none"       =: para "This is a test"
                              =?> "<para>\n  This is a test\n</para>"
          , "basic"      =: lineblock
                              =?> lineblock_out
          , "blockquote" =: blockQuote lineblock
                              =?> ("<blockquote>\n" ++ lineblock_out ++ "\n</blockquote>")
          , "footnote"   =: para ("This is a test" <> note lineblock <> " of footnotes")
                              =?> ("<para>\n  This is a test<footnote>\n" ++
                                   lineblock_out ++
                                   "\n  </footnote> of footnotes\n</para>")
          ]
        ]

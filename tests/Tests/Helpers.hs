module Tests.Helpers where

import Text.Pandoc

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

data Expect = Inline  Inline
            | Inlines [Inline]
            | Block   Block
            | Blocks  [Block]

assertPandoc :: Expect -> Pandoc -> Assertion
assertPandoc (Inline  e) (Pandoc _ [Para [g]]) = e @=? g
assertPandoc (Inlines e) (Pandoc _ [Para g]  ) = e @=? g
assertPandoc (Block   e) (Pandoc _ [g]       ) = e @=? g
assertPandoc (Blocks  e) (Pandoc _ g         ) = e @=? g
assertPandoc _ _ = assertFailure "Wrong structure of Pandoc document."

latexTest :: String-> String -> Expect -> Test
latexTest = readerTestWithState defaultParserState readLaTeX

readerTestWithState :: ParserState -> (ParserState -> String -> Pandoc) -> String -> String -> Expect -> Test
readerTestWithState state reader name string exp = testCase name $ exp `assertPandoc` reader state string 


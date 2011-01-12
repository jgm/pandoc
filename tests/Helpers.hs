module Helpers where

import Text.Pandoc

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

data Expect = Inline  Inline
            | Inlines [Inline]
            | Block   Block
            | Blocks  [Block]

assertPandoc :: Expect -> Pandoc -> Assertion
assertPandoc (Inline e)  (Pandoc _ [Para [g]]) = e @=? g
assertPandoc (Inlines e) (Pandoc _ [Para g]  ) = e @=? g
assertPandoc (Block e)   (Pandoc _ [g]       ) = e @=? g
assertPandoc (Blocks e)  (Pandoc _ g         ) = e @=? g
assertPandoc _ _ = assertFailure "Wrong structur of Pandoc document."

latexTest :: String-> String -> Expect -> Test
latexTest = latexTestWithState defaultParserState

latexTestWithState :: ParserState -> String -> String -> Expect -> Test
latexTestWithState state name string exp = testCase name $ exp `assertPandoc` readLaTeX state string 

blocks :: [Block] -> Pandoc
blocks bs = Pandoc (Meta { docTitle = [], docAuthors = [], docDate = [] }) bs

block :: Block -> Pandoc
block b = blocks [b]

inlines :: [Inline] -> Pandoc
inlines is = block $ Para is

inline :: Inline -> Pandoc
inline i = inlines [i]

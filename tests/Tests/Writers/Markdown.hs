{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Markdown (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()
import qualified Data.Set as Set

markdown :: (ToString a, ToPandoc a) => a -> String
markdown = writeMarkdown (def { writerExtensions = Set.empty }) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test markdown "my test" $ X =?> Y

which is in turn shorthand for

  test markdown "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test markdown

-- Test that Pandoc emits backticks around code blocks if asked to do so
testBacktickCodeBlocks :: Test
testBacktickCodeBlocks = test markdown' "backtick code blocks" (codeBlock code =?> expected)
  where
    writerOpts = def { writerExtensions = Set.singleton Ext_backtick_code_blocks }
    markdown' = writeMarkdown writerOpts . toPandoc
    code = "let x = 1"
    backticks = "```"
    expected = unlines [backticks, code, backticks]

-- Test that Pandoc emits tiles around code blocks if asked to do so
testFencedCodeBlocks :: Test
testFencedCodeBlocks = test markdown' "fenced code blocks" (codeBlock code =?> expected)
  where
    writerOpts = def { writerExtensions = Set.singleton Ext_fenced_code_blocks }
    markdown' = writeMarkdown writerOpts . toPandoc
    code = "let x = 1"
    fence = "~~~~"
    expected = unlines [fence, code, fence]

tests :: [Test]
tests = [ "indented code after list"
             =: (orderedList [ para "one" <> para "two" ] <> codeBlock "test")
             =?> "1.  one\n\n    two\n\n<!-- -->\n\n    test"
        , "list with tight sublist"
             =: bulletList [ plain "foo" <> bulletList [ plain "bar" ],
                             plain "baz" ]
             =?> "-   foo\n    -   bar\n-   baz\n"
        , testBacktickCodeBlocks
        , testFencedCodeBlocks
        ]

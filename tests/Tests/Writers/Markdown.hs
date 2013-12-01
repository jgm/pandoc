{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Markdown (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

markdown :: (ToString a, ToPandoc a) => a -> String
markdown = writeMarkdown def . toPandoc

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

tests :: [Test]
tests = [ "indented code after list"
             =: (orderedList [ para "one" <> para "two" ] <> codeBlock "test")
             =?> "1.  one\n\n    two\n\n<!-- -->\n\n    test"
        , "list with tight sublist"
             =: bulletList [ plain "foo" <> bulletList [ plain "bar" ],
                             plain "baz" ]
             =?> "-   foo\n    -   bar\n-   baz\n"
        ]

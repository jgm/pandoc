{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Writers.ConTeXt (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers

context :: (ToString a, ToPandoc a) => a -> String
context = writeConTeXt defaultWriterOptions . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test context "my test" $ X =?> Y

which is in turn shorthand for

  test context "my test" (X,Y)
-}

infix 5 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test context

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with '}'" =: code "}" =?> "\\mono{\\letterclosebrace{}}"
          , "without '}'" =: code "]" =?> "\\type{]}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            header 1 "My header" =?> "\\subject{My header}"
          ]
        , testGroup "bullet lists"
          [ "nested" =:
            bulletList [plain (text "top")
                        ,bulletList [plain (text "next")
                         ,bulletList [plain (text "bot")]]]
              =?> [$lit|
\startitemize
\item
  top
\item
  \startitemize
  \item
    next
  \item
    \startitemize
    \item
      bot
    \stopitemize
  \stopitemize
\stopitemize|]
          ]
        ]


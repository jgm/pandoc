{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Writers.ConTeXt (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

context :: (ToString a, ToPandoc a) => a -> String
context = writeConTeXt defaultWriterOptions . toPandoc

context' :: (ToString a, ToPandoc a) => a -> String
context' = writeConTeXt defaultWriterOptions{ writerWrapText = False }
         . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test context "my test" $ X =?> Y

which is in turn shorthand for

  test context "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test context

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with '}'" =: code "}" =?> "\\mono{\\}}"
          , "without '}'" =: code "]" =?> "\\type{]}"
          , property "code property" $ \s -> null s ||
                if '{' `elem` s || '}' `elem` s
                   then (context' $ code s) == "\\mono{" ++
                             (context' $ str s) ++ "}"
                   else (context' $ code s) == "\\type{" ++ s ++ "}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            header 1 "My header" =?> "\\section[my-header]{My header}"
          ]
        , testGroup "bullet lists"
          [ "nested" =:
            bulletList [plain (text "top")
                        ,bulletList [plain (text "next")
                         ,bulletList [plain (text "bot")]]]
              =?> [_LIT|
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


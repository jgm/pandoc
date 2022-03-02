{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Ms (tests) where

import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Builder

infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeMs def . toPandoc))

tests :: [TestTree]
tests = [ testGroup "code blocks"
          [ "basic"
              =: codeBlock "hello"
              =?> unlines
              [ ".IP"
              , ".nf"
              , "\\f[C]"
              , "hello"
              , "\\f[]"
              , ".fi"]
          , "escape starting ."
              =: codeBlock ". hello"
              =?> unlines
              [ ".IP"
              , ".nf"
              , "\\f[C]"
              , "\\&. hello"
              , "\\f[]"
              , ".fi"]
          ]
        ]

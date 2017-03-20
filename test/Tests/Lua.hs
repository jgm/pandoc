{-# Language OverloadedStrings #-}
module Tests.Lua ( tests ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Pandoc.Builder
import Text.Pandoc.Lua

tests :: [TestTree]
tests =
  [ testCase "macro expansion via filter" $
    assertFilterConversion "a '{{helloworld}}' string is expanded"
      "strmacro.lua"
      (doc . para $ str "{{helloworld}}")
      (doc . para . emph $ str "Hello, World")

  , testCase "convert all plains to paras" $
    assertFilterConversion "plains become para"
      "plain-to-para.lua"
      (doc $ bulletList [plain (str "alfa"), plain (str "bravo")])
      (doc $ bulletList [para (str "alfa"), para (str "bravo")])

  , testCase "make hello world document" $
    assertFilterConversion "Document contains 'Hello, World!'"
      "hello-world-doc.lua"
      (doc . para $ str "Hey!" <> linebreak <> str "What's up?")
      (doc . para $ str "Hello," <> space <> str "World!")
  ]

assertFilterConversion :: String -> FilePath -> Pandoc -> Pandoc -> Assertion
assertFilterConversion msg filterPath docIn docExpected = do
  docRes <- runLuaFilter ("lua" </> filterPath) [] docIn
  assertEqual msg docExpected docRes

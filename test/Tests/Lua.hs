{-# Language OverloadedStrings #-}
module Tests.Lua ( tests ) where

import Control.Monad (when)
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (ioProperty, testProperty)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Definition (Block, Inline, Meta, Pandoc)
import Text.Pandoc.Builder ( (<>), bulletList, doc, emph, linebreak, rawBlock
                           , para, plain, space, str, strong)
import Text.Pandoc.Lua

import qualified Scripting.Lua as Lua

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

  , testCase "parse raw markdown blocks" $
    assertFilterConversion "raw markdown block is converted"
      "markdown-reader.lua"
      (doc $ rawBlock "markdown" "*charly* **delta**")
      (doc . para $ emph "charly" <> space <> strong "delta")

  , testProperty "inline elements can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Inline))

  , testProperty "block elements can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Block))

  , testProperty "meta blocks can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Meta))

  , testProperty "documents can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Pandoc))
  ]

assertFilterConversion :: String -> FilePath -> Pandoc -> Pandoc -> Assertion
assertFilterConversion msg filterPath docIn docExpected = do
  docRes <- runLuaFilter ("lua" </> filterPath) [] docIn
  assertEqual msg docExpected docRes

roundtripEqual :: (Eq a, Lua.StackValue a) => a -> IO Bool
roundtripEqual x = (x ==) <$> roundtripped
 where
  roundtripped :: (Lua.StackValue a) => IO a
  roundtripped = do
    lua <- Lua.newstate
    Lua.push lua x
    size <- Lua.gettop lua
    when (size /= 1) $
      error ("not exactly one element on the stack: " ++ show size)
    res <- Lua.peek lua (-1)
    retval <- case res of
                Nothing -> error "could not read from stack"
                Just y  -> return y
    Lua.close lua
    return retval

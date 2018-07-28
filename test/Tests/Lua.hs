{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Lua ( tests ) where

import Prelude
import Control.Monad (when)
import Data.Version (Version (versionBranch))
import System.FilePath ((</>))
import Test.Tasty (TestTree, localOption)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests (..), ioProperty, testProperty)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder (bulletList, divWith, doc, doubleQuoted, emph,
                            header, linebreak, para, plain, rawBlock,
                            singleQuoted, space, str, strong,
                            math, displayMath)
import Text.Pandoc.Class (runIOorExplode, setUserDataDir)
import Text.Pandoc.Definition (Block (BlockQuote, Div, Para), Inline (Emph, Str),
                               Attr, Meta, Pandoc, pandocTypesVersion)
import Text.Pandoc.Lua (runLuaFilter, runPandocLua)
import Text.Pandoc.Options (def)
import Text.Pandoc.Shared (pandocVersion)

import qualified Foreign.Lua as Lua

tests :: [TestTree]
tests = map (localOption (QuickCheckTests 20))
  [ testProperty "inline elements can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Inline))

  , testProperty "block elements can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Block))

  , testProperty "meta blocks can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Meta))

  , testProperty "documents can be round-tripped through the lua stack" $
    \x -> ioProperty (roundtripEqual (x::Pandoc))

  , testCase "macro expansion via filter" $
    assertFilterConversion "a '{{helloworld}}' string is expanded"
      "strmacro.lua"
      (doc . para $ str "{{helloworld}}")
      (doc . para . emph $ str "Hello, World")

  , testCase "convert all plains to paras" $
    assertFilterConversion "plains become para"
      "plain-to-para.lua"
      (doc $ bulletList [plain (str "alfa"), plain (str "bravo")])
      (doc $ bulletList [para (str "alfa"), para (str "bravo")])

  , testCase "convert display math to inline math" $
    assertFilterConversion "display math becomes inline math"
      "math.lua"
      (doc $ para (displayMath "5+5"))
      (doc $ para (math "5+5"))

  , testCase "make hello world document" $
    assertFilterConversion "Document contains 'Hello, World!'"
      "hello-world-doc.lua"
      (doc . para $ str "Hey!" <> linebreak <> str "What's up?")
      (doc . para $ str "Hello," <> space <> str "World!")

  , testCase "implicit doc filter" $
    assertFilterConversion "Document contains 'Hello, World!'"
      "implicit-doc-filter.lua"
      (doc . plain $ linebreak)
      (doc . para $ str "Hello," <> space <> str "World!")

  , testCase "parse raw markdown blocks" $
    assertFilterConversion "raw markdown block is converted"
      "markdown-reader.lua"
      (doc $ rawBlock "markdown" "*charly* **delta**")
      (doc . para $ emph "charly" <> space <> strong "delta")

  , testCase "allow shorthand functions for quote types" $
    assertFilterConversion "single quoted becomes double quoted string"
      "single-to-double-quoted.lua"
      (doc . para . singleQuoted $ str "simple")
      (doc . para . doubleQuoted $ str "simple")

  , testCase "Count inlines via metatable catch-all" $
    assertFilterConversion "filtering with metatable catch-all failed"
      "metatable-catch-all.lua"
      (doc . para $ "four words, three spaces")
      (doc . para $ str "7")

  , testCase "Count blocks via Block-specific catch-all" $
    assertFilterConversion "filtering with Block catch-all failed"
      "block-count.lua"
      (doc $ para "one" <> para "two")
      (doc $ para "2")

  , testCase "Convert header upper case" $
    assertFilterConversion "converting header to upper case failed"
      "uppercase-header.lua"
      (doc $ header 1 "les états-unis" <> para "text")
      (doc $ header 1 "LES ÉTATS-UNIS" <> para "text")

  , testCase "Attribute lists are convenient to use" $
    let kv_before = [("one", "1"), ("two", "2"), ("three", "3")]
        kv_after  = [("one", "eins"), ("three", "3"), ("five", "5")]
    in assertFilterConversion "Attr doesn't behave as expected"
      "attr-test.lua"
      (doc $ divWith ("", [], kv_before) (para "nil"))
      (doc $ divWith ("", [], kv_after) (para "nil"))

  , testCase "Test module pandoc.utils" $
    assertFilterConversion "pandoc.utils doesn't work as expected."
      "test-pandoc-utils.lua"
      (doc $ para "doesn't matter")
      (doc $ mconcat [ plain (str "blocks_to_inlines: OK")
                     , plain (str "hierarchicalize: OK")
                     , plain (str "normalize_date: OK")
                     , plain (str "pipe: OK")
                     , plain (str "failing pipe: OK")
                     , plain (str "read: OK")
                     , plain (str "failing read: OK")
                     , plain (str "sha1: OK")
                     , plain (str "stringify: OK")
                     , plain (str "to_roman_numeral: OK")
                     ])

  , testCase "Script filename is set" $
    assertFilterConversion "unexpected script name"
      "script-name.lua"
      (doc $ para "ignored")
      (doc $ para (str $ "lua" </> "script-name.lua"))

  , testCase "Pandoc version is set" . runPandocLua' $ do
      Lua.getglobal' "table.concat"
      Lua.getglobal "PANDOC_VERSION"
      Lua.push ("." :: String) -- separator
      Lua.call 2 1
      Lua.liftIO . assertEqual "pandoc version is wrong" pandocVersion
        =<< Lua.peek Lua.stackTop

  , testCase "Pandoc types version is set" . runPandocLua' $ do
      let versionNums = versionBranch pandocTypesVersion
      Lua.getglobal "PANDOC_API_VERSION"
      Lua.liftIO . assertEqual "pandoc-types version is wrong" versionNums
        =<< Lua.peek Lua.stackTop

  , testCase "Allow singleton inline in constructors" . runPandocLua' $ do
      Lua.liftIO . assertEqual "Not the exptected Emph" (Emph [Str "test"])
        =<< Lua.callFunc "pandoc.Emph" (Str "test")
      Lua.liftIO . assertEqual "Unexpected element" (Para [Str "test"])
        =<< Lua.callFunc "pandoc.Para" ("test" :: String)
      Lua.liftIO . assertEqual "Unexptected element"
        (BlockQuote [Para [Str "foo"]]) =<< (
        do
          Lua.getglobal' "pandoc.BlockQuote"
          Lua.push (Para [Str "foo"])
          _ <- Lua.call 1 1
          Lua.peek Lua.stackTop
        )

  , testCase "Elements with Attr have `attr` accessor" . runPandocLua' $ do
      Lua.push (Div ("hi", ["moin"], [])
                [Para [Str "ignored"]])
      Lua.getfield Lua.stackTop "attr"
      Lua.liftIO . assertEqual "no accessor" (("hi", ["moin"], []) :: Attr)
        =<< Lua.peek Lua.stackTop

  , testCase "informative error messages" . runPandocLua' $ do
      Lua.pushboolean True
      err <- Lua.peekEither Lua.stackTop :: Lua.Lua (Either String Pandoc)
      case err of
        Left msg -> do
          let expectedMsg = "Could not get Pandoc value: "
                            ++ "expected table but got boolean."
          Lua.liftIO $ assertEqual "unexpected error message" expectedMsg msg
        Right _ -> error "Getting a Pandoc element from a bool should fail."
  ]

assertFilterConversion :: String -> FilePath -> Pandoc -> Pandoc -> Assertion
assertFilterConversion msg filterPath docIn docExpected = do
  docEither <- runIOorExplode $ do
    setUserDataDir (Just "../data")
    runLuaFilter def ("lua" </> filterPath) [] docIn
  case docEither of
    Left _       -> fail "lua filter failed"
    Right docRes -> assertEqual msg docExpected docRes

roundtripEqual :: (Eq a, Lua.FromLuaStack a, Lua.ToLuaStack a) => a -> IO Bool
roundtripEqual x = (x ==) <$> roundtripped
 where
  roundtripped :: (Lua.FromLuaStack a, Lua.ToLuaStack a) => IO a
  roundtripped = runPandocLua' $ do
    oldSize <- Lua.gettop
    Lua.push x
    size <- Lua.gettop
    when (size - oldSize /= 1) $
      error ("not exactly one additional element on the stack: " ++ show size)
    res <- Lua.peekEither (-1)
    case res of
      Left e -> error (show e)
      Right y -> return y

runPandocLua' :: Lua.Lua a -> IO a
runPandocLua' op = runIOorExplode $ do
  setUserDataDir (Just "../data")
  res <- runPandocLua op
  case res of
    Left e -> error (show e)
    Right x -> return x

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Tests.Lua
   Copyright   : © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Unit and integration tests for pandoc's Lua subsystem.
-}
module Tests.Lua ( runLuaTest, tests ) where

import Prelude
import Control.Monad (when)
import System.FilePath ((</>))
import Test.Tasty (TestTree, localOption)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (QuickCheckTests (..), ioProperty, testProperty)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder (bulletList, definitionList, displayMath, divWith,
                            doc, doubleQuoted, emph, header, lineBlock,
                            linebreak, math, orderedList, para, plain, rawBlock,
                            singleQuoted, space, str, strong,
                            HasMeta (setMeta))
import Text.Pandoc.Class (runIOorExplode, setUserDataDir)
import Text.Pandoc.Definition (Block (BlockQuote, Div, Para), Inline (Emph, Str),
                               Attr, Meta, Pandoc, pandocTypesVersion)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Filter (Filter (LuaFilter), applyFilters)
import Text.Pandoc.Lua (runLua)
import Text.Pandoc.Options (def)
import Text.Pandoc.Shared (pandocVersion)

import qualified Control.Monad.Catch as Catch
import qualified Foreign.Lua as Lua
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

  , testCase "Smart constructors" $
    assertFilterConversion "smart constructors returned a wrong result"
      "smart-constructors.lua"
      (doc $ para "")
      (doc $ mconcat
       [ bulletList [para "Hello", para "World"]
       , definitionList [("foo", [para "placeholder"])]
       , lineBlock ["Moin", "Welt"]
       , orderedList [plain "one", plain "two"]
       ])

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

  , testCase "Filter list of inlines" $
      assertFilterConversion "List of inlines"
      "inlines-filter.lua"
      (doc $ para ("Hello," <> linebreak <> "World! Wassup?"))
      (doc $ para "Hello, World! Wassup?")

  , testCase "Filter list of blocks" $
      assertFilterConversion "List of blocks"
      "blocks-filter.lua"
      (doc $ para "one." <> para "two." <> para "three.")
      (doc $ plain "3")

  , testCase "Filter Meta" $
    let setMetaBefore = setMeta "old" ("old" :: T.Text)
                      . setMeta "bool" False
        setMetaAfter  = setMeta "new" ("new" :: T.Text)
                      . setMeta "bool" True
    in assertFilterConversion "Meta filtering"
      "meta.lua"
      (setMetaBefore . doc $ mempty)
      (setMetaAfter . doc $ mempty)

  , testCase "Script filename is set" $
    assertFilterConversion "unexpected script name"
      "script-name.lua"
      (doc $ para "ignored")
      (doc $ para (str $ T.pack $ "lua" </> "script-name.lua"))

  , testCase "Pandoc version is set" . runLuaTest $ do
      Lua.getglobal "PANDOC_VERSION"
      Lua.liftIO .
        assertEqual "pandoc version is wrong" (TE.encodeUtf8 pandocVersion)
        =<< Lua.tostring' Lua.stackTop

  , testCase "Pandoc types version is set" . runLuaTest $ do
      Lua.getglobal "PANDOC_API_VERSION"
      Lua.liftIO . assertEqual "pandoc-types version is wrong" pandocTypesVersion
        =<< Lua.peek Lua.stackTop

  , testCase "require file" $
    assertFilterConversion "requiring file failed"
      "require-file.lua"
      (doc $ para "ignored")
      (doc $ para (str . T.pack $ "lua" </> "require-file.lua"))

  , testCase "Allow singleton inline in constructors" . runLuaTest $ do
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

  , testCase "Elements with Attr have `attr` accessor" . runLuaTest $ do
      Lua.push (Div ("hi", ["moin"], [])
                [Para [Str "ignored"]])
      Lua.getfield Lua.stackTop "attr"
      Lua.liftIO . assertEqual "no accessor" (("hi", ["moin"], []) :: Attr)
        =<< Lua.peek Lua.stackTop

  , testCase "module `pandoc.system` is present" . runLuaTest $ do
      Lua.getglobal' "pandoc.system"
      ty <- Lua.ltype Lua.stackTop
      Lua.liftIO $ assertEqual "module should be a table" Lua.TypeTable ty

  , testCase "informative error messages" . runLuaTest $ do
      Lua.pushboolean True
      eitherPandoc <- Catch.try (Lua.peek Lua.stackTop :: Lua.Lua Pandoc)
      case eitherPandoc of
        Left (PandocLuaError msg) -> do
          let expectedMsg = "Could not get Pandoc value: "
                            <> "table expected, got boolean"
          Lua.liftIO $ assertEqual "unexpected error message" expectedMsg msg
        Left e -> error ("Expected a Lua error, but got " <> show e)
        Right _ -> error "Getting a Pandoc element from a bool should fail."
  ]

assertFilterConversion :: String -> FilePath -> Pandoc -> Pandoc -> Assertion
assertFilterConversion msg filterPath docIn expectedDoc = do
  actualDoc <- runIOorExplode $ do
    setUserDataDir (Just "../data")
    applyFilters def [LuaFilter ("lua" </> filterPath)] ["HTML"] docIn
  assertEqual msg expectedDoc actualDoc

roundtripEqual :: (Eq a, Lua.Peekable a, Lua.Pushable a) => a -> IO Bool
roundtripEqual x = (x ==) <$> roundtripped
 where
  roundtripped :: Lua.Peekable a => IO a
  roundtripped = runLuaTest $ do
    oldSize <- Lua.gettop
    Lua.push x
    size <- Lua.gettop
    when (size - oldSize /= 1) $
      error ("not exactly one additional element on the stack: " ++ show size)
    Lua.peek (-1)

runLuaTest :: Lua.Lua a -> IO a
runLuaTest op = runIOorExplode $ do
  setUserDataDir (Just "../data")
  res <- runLua op
  case res of
    Left e -> error (show e)
    Right x -> return x

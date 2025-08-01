{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Tests.Lua
   Copyright   : © 2017-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha
   Portability : portable

Unit and integration tests for pandoc's Lua subsystem.
-}
module Tests.Lua ( runLuaTest, tests ) where

import HsLua as Lua hiding (Operation (Div), error)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), Assertion, HasCallStack, assertEqual, testCase)
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder (bulletList, definitionList, displayMath, divWith,
                            doc, doubleQuoted, emph, header, lineBlock,
                            linebreak, math, orderedList, para, plain, rawBlock,
                            singleQuoted, space, str, strong,
                            HasMeta (setMeta))
import Text.Pandoc.Class ( runIOorExplode, setUserDataDir, setVerbosity )
import Text.Pandoc.Definition (Attr, Block (BlockQuote, Div, Para), Pandoc,
                               Inline (Emph, Str), pandocTypesVersion)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Logging (Verbosity (ERROR))
import Text.Pandoc.Lua (Global (..), applyFilter, runLua, setGlobals)
import Text.Pandoc.Options (def)
import Text.Pandoc.Version (pandocVersionText)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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
        assertEqual "pandoc version is wrong" (TE.encodeUtf8 pandocVersionText)
        =<< Lua.tostring' Lua.top

  , testCase "Pandoc types version is set" . runLuaTest $ do
      Lua.getglobal "PANDOC_API_VERSION"
      Lua.liftIO . assertEqual "pandoc-types version is wrong" pandocTypesVersion
        =<< Lua.peek Lua.top

  , testCase "require file" $
    assertFilterConversion "requiring file failed"
      "require-file.lua"
      (doc $ para "ignored")
      (doc $ para (str . T.pack $ "lua" </> "require-file.lua"))

  , testCase "Allow singleton inline in constructors" . runLuaTest $ do
      Lua.liftIO . assertEqual "Not the expected Emph"
        (Emph [Str "test"]) =<< do
        Lua.OK <- Lua.dostring "return pandoc.Emph"
        Lua.push @Inline (Str "test")
        Lua.call 1 1
        Lua.peek @Inline top
      Lua.liftIO . assertEqual "Unexpected element"
        (Para [Str "test"]) =<< do
        Lua.getglobal' "pandoc.Para"
        Lua.pushString "test"
        Lua.call 1 1
        Lua.peek @Block top
      Lua.liftIO . assertEqual "Unexptected element"
        (BlockQuote [Para [Str "foo"]]) =<< (
        do
          Lua.getglobal' "pandoc.BlockQuote"
          Lua.push (Para [Str "foo"])
          _ <- Lua.call 1 1
          Lua.peek @Block Lua.top
        )

  , testCase "Elements with Attr have `attr` accessor" . runLuaTest $ do
      Lua.push (Div ("hi", ["moin"], [])
                [Para [Str "ignored"]])
      Lua.getfield Lua.top "attr"
      Lua.liftIO . assertEqual "no accessor" (("hi", ["moin"], []) :: Attr)
        =<< Lua.peek @Attr Lua.top

  , testCase "module `pandoc.system` is present" . runLuaTest $ do
      Lua.getglobal' "pandoc.system"
      ty <- Lua.ltype Lua.top
      Lua.liftIO $ assertEqual "module should be a table" Lua.TypeTable ty

  , testGroup "global modules"
    [ testCase "module 'lpeg' is loaded into a global" . runLuaTest $ do
        s <- Lua.dostring "assert(type(lpeg)=='table')"
        Lua.liftIO $ Lua.OK @=? s

    , testCase "module 're' is loaded into a global" . runLuaTest $ do
        s <- Lua.dostring "assert(type(re)=='table')"
        Lua.liftIO $ Lua.OK @=? s

    , testCase "module 'lpeg' is available via `require`" . runLuaTest $ do
        s <- Lua.dostring
              "package.path = ''; package.cpath = ''; require 'lpeg'"
        Lua.liftIO $ Lua.OK @=? s

    , testCase "module 're' is available via `require`" . runLuaTest $ do
        s <- Lua.dostring
               "package.path = ''; package.cpath = ''; require 're'"
        Lua.liftIO $ Lua.OK @=? s
    ]

  , testCase "informative error messages" . runLuaTest $ do
      Lua.pushboolean True
      -- Lua.newtable
      eitherPandoc <- Catch.try (peek @Pandoc Lua.top)
      case eitherPandoc of
        Left (PandocLuaError msg) -> do
          let expectedMsg = "Pandoc expected, got boolean\n"
                <> "\twhile retrieving Pandoc"
          Lua.liftIO $ assertEqual "unexpected error message" expectedMsg msg
        Left e -> error ("Expected a Lua error, but got " <> show e)
        Right _ -> error "Getting a Pandoc element from a bool should fail."
  ]

assertFilterConversion :: String -> FilePath -> Pandoc -> Pandoc -> Assertion
assertFilterConversion msg filterPath docIn expectedDoc = do
  actualDoc <- runIOorExplode $ do
    setUserDataDir (Just "../data")
    applyFilter def ["HTML"] ("lua" </> filterPath) docIn
  assertEqual msg expectedDoc actualDoc

runLuaTest :: HasCallStack => Lua.LuaE PandocError a -> IO a
runLuaTest op = runIOorExplode $ do
  -- Disable printing of warnings on stderr: some tests will generate
  -- warnings, we don't want to see those messages.
  setVerbosity ERROR
  res <- runLua $ do
    setGlobals [ PANDOC_WRITER_OPTIONS def ]
    op
  case res of
    Left e -> error (show e)
    Right x -> return x

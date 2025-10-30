{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Writers.BBCode (tests) where

import Data.Maybe (isNothing)
import Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Writers.BBCode
import Text.Read (readMaybe)

bbcodeExt :: (ToPandoc a) => a -> Text
bbcodeExt x = purely (writeBBCodeCustom extendedSpec def) $ toPandoc x
 where
  extendedSpec :: FlavorSpec
  extendedSpec = officialSpec{wrapSpanDiv = wrapSpanDivGeneric}

bbcodeDefault
  , bbcodeSteam
  , bbcodePhpBB
  , bbcodeFluxBB
  , bbcodeHubzilla ::
    (ToPandoc a) => a -> Text
bbcodeDefault = purely (writeBBCode def) . toPandoc
bbcodeSteam = purely (writeBBCodeSteam def) . toPandoc
bbcodePhpBB = purely (writeBBCodePhpBB def) . toPandoc
bbcodeFluxBB = purely (writeBBCodeFluxBB def) . toPandoc
bbcodeHubzilla = purely (writeBBCodeHubzilla def) . toPandoc

infix 4 =:, `bbcode`, `steam`, `phpbb`, `fluxbb`, `hubzilla`
(=:)
  , bbcode
  , steam
  , phpbb
  , fluxbb
  , hubzilla ::
    (ToString a, ToPandoc a, HasCallStack) =>
    String ->
    (a, Text) ->
    TestTree
(=:) = test bbcodeExt
bbcode = test bbcodeDefault
steam = test bbcodeSteam
phpbb = test bbcodePhpBB
fluxbb = test bbcodeFluxBB
hubzilla = test bbcodeHubzilla

spanClasses :: [Text] -> Inlines -> Inlines
spanClasses cls = spanWith ("", cls, [])

spanAttrs :: [(Text, Text)] -> Inlines -> Inlines
spanAttrs kvList = spanWith ("", [], kvList)

divClasses :: [Text] -> Blocks -> Blocks
divClasses cls = divWith ("", cls, [])

divAttrs :: [(Text, Text)] -> Blocks -> Blocks
divAttrs kvList = divWith ("", [], kvList)

tests :: [TestTree]
tests =
  [ testGroup
      "spans classes"
      [ "left" =: spanClasses ["left"] "foo" =?> "foo"
      , "center" =: spanClasses ["center"] "foo" =?> "foo"
      , "right" =: spanClasses ["right"] "foo" =?> "foo"
      , "spoiler" =: spanClasses ["spoiler"] "foo" =?> "foo"
      , "box" =: spanClasses ["box"] "foo" =?> "foo"
      , "indent" =: spanClasses ["indent"] "foo" =?> "foo"
      ]
  , testGroup
      "spans attributes"
      [ testProperty "incorrect size ignored" . property $ do
          n <- arbitrary @String
          let nInt = readMaybe @Int n
          let actual = bbcodeExt (spanAttrs [("size", T.pack n)] "foo")
          pure $ isNothing nInt ==> actual === "foo"
      , testProperty "size<=0 ignored" . property $ do
          NonPositive n <- arbitrary @(NonPositive Int)
          let actual = bbcodeExt (spanAttrs [("size", tshow n)] "foo")
          pure $ actual === "foo"
      , testProperty "size>0" . property $ do
          Positive n <- arbitrary @(Positive Int)
          let actual = bbcodeExt (spanAttrs [("size", tshow n)] "foo")
          let expected = "[size=" <> tshow n <> "]" <> "foo[/size]"
          pure $ actual === expected
      , "size=20" =: spanAttrs [("size", "20")] "foo" =?> "[size=20]foo[/size]"
      , "color=#AAAAAA"
          =: spanAttrs [("color", "#AAAAAA")] "foo"
          =?> "[color=#AAAAAA]foo[/color]"
      , "spoiler ignored"
          =: spanAttrs [("spoiler", "name with spaces and ]brackets[]")] "foo"
          =?> "foo"
      , "font"
          =: spanAttrs [("font", "serif")] "foo"
          =?> "[font=serif]foo[/font]"
      , "align ignored"
          =: spanAttrs [("align", "left")] "foo"
          =?> "foo"
      , "box ignored"
          =: spanAttrs [("box", "center")] "foo"
          =?> "foo"
      ]
  , testGroup
      "divs classes"
      [ "left"
          =: divClasses ["left"] (para "foo")
          =?> "[left]foo\n\n[/left]"
      , "center"
          =: divClasses ["center"] (para "foo")
          =?> "[center]foo\n\n[/center]"
      , "right"
          =: divClasses ["right"] (para "foo")
          =?> "[right]foo\n\n[/right]"
      , "spoiler"
          =: divClasses ["spoiler"] (para "foo")
          =?> "[spoiler]foo\n\n[/spoiler]"
      , "box"
          =: divClasses ["box"] (para "foo")
          =?> "[box]foo\n\n[/box]"
      , "indent"
          =: divClasses ["indent"] (para "foo")
          =?> "[indent]foo\n\n[/indent]"
      ]
  , testGroup
      "divs attributes"
      [ testProperty "incorrect size ignored" . property $ do
          n <- arbitrary @String
          let nInt = readMaybe @Int n
          let actual = bbcodeExt (divAttrs [("size", T.pack n)] $ para "foo")
          pure $ isNothing nInt ==> actual === "foo\n"
      , testProperty "size<=0 ignored" . property $ do
          NonPositive n <- arbitrary @(NonPositive Int)
          let actual = bbcodeExt (divAttrs [("size", tshow n)] $ para "foo")
          pure $ actual === "foo\n"
      , testProperty "size>0" . property $ do
          Positive n <- arbitrary @(Positive Int)
          let actual = bbcodeExt (divAttrs [("size", tshow n)] $ para "foo")
          let expected = "[size=" <> tshow n <> "]" <> "foo\n\n[/size]"
          pure $ actual === expected
      , "size=20"
          =: divAttrs [("size", "20")] (para "foo")
          =?> "[size=20]foo\n\n[/size]"
      , "color=#AAAAAA"
          =: divAttrs [("color", "#AAAAAA")] (para "foo")
          =?> "[color=#AAAAAA]foo\n\n[/color]"
      , "spoiler"
          =: divAttrs
            [("spoiler", "name with spaces and ]brackets[]")]
            (para "foo")
          =?> "[spoiler=name with spaces and brackets]foo\n\n[/spoiler]"
      , "font"
          =: divAttrs [("font", "serif")] (para "foo")
          =?> "[font=serif]foo\n\n[/font]"
      , "align"
          =: divAttrs [("align", "left")] (para "foo")
          =?> "[align=left]foo\n\n[/align]"
      , "box"
          =: divAttrs [("box", "center")] (para "foo")
          =?> "[box=center]foo\n\n[/box]"
      ]
  , testGroup
      "default flavor"
      [ "link"
          `bbcode` link "https://example.com" "title" "label"
          =?> "[url=https://example.com]label[/url]"
      , "autolink"
          `bbcode` link "https://example.com" "title" "https://example.com"
          =?> "[url]https://example.com[/url]"
      , "email autolink"
          `bbcode` link
            "mailto:example@example.com"
            "title"
            "example@example.com"
          =?> "[email]example@example.com[/email]"
      , "named email"
          `bbcode` link "mailto:example@example.com" "title" "example email"
          =?> "[email=example@example.com]example email[/email]"
      , "h0" `bbcode` header 0 "heading 0" =?> "[u][b]heading 0[/b][/u]"
      , "h1" `bbcode` header 1 "heading 1" =?> "[u][b]heading 1[/b][/u]"
      , "h2" `bbcode` header 2 "heading 2" =?> "[b]heading 2[/b]"
      , "h3" `bbcode` header 3 "heading 3" =?> "[u]heading 3[/u]"
      , "h4" `bbcode` header 4 "heading 4" =?> "heading 4"
      , "h5" `bbcode` header 5 "heading 5" =?> "heading 5"
      ]
  , testGroup
      "steam"
      [ test bbcodeSteam "dename spoiler" $
          divAttrs [("spoiler", "bar")] (para "foo")
            =?> ("[spoiler]foo\n\n[/spoiler]" :: Text)
      , testProperty "ordered list styleless" . property $ do
          let listItems = [para "foo", para "bar", para "baz"]
          attrsRand <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
          let actual = bbcodeSteam $ orderedListWith attrsRand listItems
          let expected = "[olist]\n[*]foo\n\n[*]bar\n\n[*]baz\n\n[/olist]"
          pure $ actual === expected
      , "h0" `steam` header 0 "heading 0" =?> "[h1]heading 0[/h1]"
      , "h1" `steam` header 1 "heading 1" =?> "[h1]heading 1[/h1]"
      , "h2" `steam` header 2 "heading 2" =?> "[h2]heading 2[/h2]"
      , "h3" `steam` header 3 "heading 3" =?> "[h3]heading 3[/h3]"
      , "h4" `steam` header 4 "heading 4" =?> "[h3]heading 4[/h3]"
      , "code"
          `steam` codeWith ("id", ["haskell"], []) "map (2^) [1..5]"
          =?> "[noparse]map (2^) [1..5][/noparse]"
      ]
  , testGroup
      "phpBB"
      [ "image"
          `phpbb` imageWith
            ("id", [], [("width", "100")])
            "https://example.com"
            "title"
            "alt text"
          =?> "[img]https://example.com[/img]"
      ]
  , testGroup
      "FluxBB"
      [ "image"
          `fluxbb` imageWith
            ("id", [], [("width", "100")])
            "https://example.com"
            "title"
            "alt text"
          =?> "[img=alt text]https://example.com[/img]"
      , testProperty "ordered list" . property $ do
          let listItems = [para "foo", para "bar", para "baz"]
          attrsRand <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
          let actual = bbcodeFluxBB $ orderedListWith attrsRand listItems
          let opening = case attrsRand of
                (_, LowerAlpha, _) -> "[list=a]"
                (_, UpperAlpha, _) -> "[list=a]"
                _ -> "[list=1]"
          let expected = opening <> "\n[*]foo\n\n[*]bar\n\n[*]baz\n\n[/list]"
          pure $ actual === expected
      , "ulist > BlockQuote not rendered"
          `fluxbb` bulletList [blockQuote (para "foo") <> para "bar"]
          =?> "[list]\n[*]bar\n\n[/list]"
      , "code block"
          `fluxbb` codeBlockWith
            ("id", ["haskell"], [])
            ( T.intercalate "\n" $
                [ "vals ="
                , "  take 10"
                , "    . filter (\\x -> (x - 5) `mod` 3 == 0)"
                , "    $ map (2 ^) [1 ..]"
                ]
            )
          =?> T.intercalate
            "\n"
            [ "[code]vals ="
            , "  take 10"
            , "    . filter (\\x -> (x - 5) `mod` 3 == 0)"
            , "    $ map (2 ^) [1 ..]"
            , "[/code]"
            ]
      ]
  , testGroup
      "Hubzilla"
      [ "unordered list"
          `hubzilla` bulletList [para "foo", para "bar", para "baz"]
          =?> "[ul]\n[*]foo\n\n[*]bar\n\n[*]baz\n\n[/ul]"
      , testProperty "ordered list" . property $ do
          let listItems = [para "foo", para "bar", para "baz"]
          attrsRand <- (,,) <$> arbitrary <*> arbitrary <*> arbitrary
          let actual = bbcodeHubzilla $ orderedListWith attrsRand listItems
          let (opening, closing) = case attrsRand of
                (_, Decimal, _) -> ("[list=1]", "[/list]")
                (_, DefaultStyle, _) -> ("[ol]", "[/ol]")
                (_, Example, _) -> ("[ol]", "[/ol]")
                (_, LowerAlpha, _) -> ("[list=a]", "[/list]")
                (_, UpperAlpha, _) -> ("[list=A]", "[/list]")
                (_, LowerRoman, _) -> ("[list=i]", "[/list]")
                (_, UpperRoman, _) -> ("[list=I]", "[/list]")
          let expected =
                opening <> "\n[*]foo\n\n[*]bar\n\n[*]baz\n\n" <> closing
          pure $ actual === expected
      , "definition list"
          `hubzilla` definitionList
            [ ("term_foo", [para "def_foo1", para "def_foo2"])
            , ("term_bar", [para "def_bar1", para "def_bar2"])
            , ("term_baz", [para "def_baz1", para "def_baz2"])
            ]
          =?> mconcat
            [ "[dl terms=\"b\"]\n"
            , "[*= term_foo]\ndef_foo1\n\ndef_foo2\n\n"
            , "[*= term_bar]\ndef_bar1\n\ndef_bar2\n\n"
            , "[*= term_baz]\ndef_baz1\n\ndef_baz2\n\n"
            , "[/dl]"
            ]
      , "h0" `hubzilla` header 0 "heading 0" =?> "[h1]heading 0[/h1]"
      , "h1" `hubzilla` header 1 "heading 1" =?> "[h1]heading 1[/h1]"
      , "h2" `hubzilla` header 2 "heading 2" =?> "[h2]heading 2[/h2]"
      , "h3" `hubzilla` header 3 "heading 3" =?> "[h3]heading 3[/h3]"
      , "h4" `hubzilla` header 4 "heading 4" =?> "[h4]heading 4[/h4]"
      , "h5" `hubzilla` header 5 "heading 5" =?> "[h5]heading 5[/h5]"
      , "h6" `hubzilla` header 6 "heading 6" =?> "[h6]heading 6[/h6]"
      , "h7" `hubzilla` header 7 "heading 7" =?> "[h6]heading 7[/h6]"
      , "link"
          `hubzilla` link "https://example.com" "title" "label"
          =?> "[url=https://example.com]label[/url]"
      , "autolink"
          `hubzilla` link "https://example.com" "title" "https://example.com"
          =?> "[url]https://example.com[/url]"
      , "email autolink"
          `hubzilla` link
            "mailto:example@example.com"
            "title"
            "example@example.com"
          =?> "[url=mailto:example@example.com]example@example.com[/url]"
      , "named email"
          `hubzilla` link "mailto:example@example.com" "title" "example email"
          =?> "[url=mailto:example@example.com]example email[/url]"
      , "inline code"
          `hubzilla` ( "inline code: "
                        <> codeWith ("id", ["haskell"], []) "map (2^) [1..5]"
                     )
          =?> "inline code: [code]map (2^) [1..5][/code]"
      ]
  ]

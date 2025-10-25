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

extendedSpec :: FlavorSpec
extendedSpec = officialSpec{wrapSpanDiv = wrapSpanDivGeneric}

bbcode :: (ToPandoc a) => a -> Text
bbcode x = purely (writeBBCode_custom extendedSpec def) $ toPandoc x

bbcodeSteam :: (ToPandoc a) => a -> Text
bbcodeSteam x = purely (writeBBCode_steam def) $ toPandoc x

infix 4 =:
(=:) ::
  (ToString a, ToPandoc a, HasCallStack) =>
  String ->
  (a, Text) ->
  TestTree
(=:) = test bbcode

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
          let actual = bbcode (spanAttrs [("size", T.pack n)] "foo")
          pure $ isNothing nInt ==> actual === "foo"
      , testProperty "size<=0 ignored" . property $ do
          NonPositive n <- arbitrary @(NonPositive Int)
          let actual = bbcode (spanAttrs [("size", tshow n)] "foo")
          pure $ actual === "foo"
      , testProperty "size>0" . property $ do
          Positive n <- arbitrary @(Positive Int)
          let actual = bbcode (spanAttrs [("size", tshow n)] "foo")
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
          let actual = bbcode (divAttrs [("size", T.pack n)] $ para "foo")
          pure $ isNothing nInt ==> actual === "foo\n"
      , testProperty "size<=0 ignored" . property $ do
          NonPositive n <- arbitrary @(NonPositive Int)
          let actual = bbcode (divAttrs [("size", tshow n)] $ para "foo")
          pure $ actual === "foo\n"
      , testProperty "size>0" . property $ do
          Positive n <- arbitrary @(Positive Int)
          let actual = bbcode (divAttrs [("size", tshow n)] $ para "foo")
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
      "steam"
      [ test bbcodeSteam "steam dename spoiler" $
          divAttrs [("spoiler", "bar")] (para "foo")
            =?> ("[spoiler]foo\n\n[/spoiler]" :: Text)
      ]
  ]

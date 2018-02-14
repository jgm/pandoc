{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.RST (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Writers.RST

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeRST def . toPandoc))

tests :: [TestTree]
tests = [ testGroup "rubrics"
          [ "in list item" =:
              bulletList [header 2 (text "foo")] =?>
              "-  .. rubric:: foo"
          , "in definition list item" =:
              definitionList [(text "foo", [header 2 (text "bar"),
                                            para $ text "baz"])] =?>
              unlines
              [ "foo"
              , "    .. rubric:: bar"
              , ""
              , "    baz"]
          , "in block quote" =:
              blockQuote (header 1 (text "bar")) =?>
              "    .. rubric:: bar"
          , "with id" =:
              blockQuote (headerWith ("foo",[],[]) 1 (text "bar")) =?>
              unlines
              [ "    .. rubric:: bar"
              , "       :name: foo"]
          , "with id class" =:
              blockQuote (headerWith ("foo",["baz"],[]) 1 (text "bar")) =?>
              unlines
              [ "    .. rubric:: bar"
              , "       :name: foo"
              , "       :class: baz"]
          ]
        , testGroup "ligatures" -- handling specific sequences of blocks
          [ "a list is closed by a comment before a quote" =: -- issue 4248
            bulletList [plain "bulleted"] <> blockQuote (plain "quoted") =?>
              unlines
              [ "-  bulleted"
              , ""
              , ".."
              , ""
              , "    quoted"]
          ]
        , testGroup "inlines"
          [ "gives priority to strong style over emphasis" =: -- #4368
            emph (str "first" <> strong (str "second")) =?> "*first*\\ **second**",
            "filters out empty style inlines" =: -- #4434
            strong (str "") =?> ""
          ]
        , testGroup "flatten"
          [ testCase "drops all inner styles" $
            flatten (Strong [Emph [Strong [Str "s"]]]) @?=
            [Strong [Str "s"]]
          , testCase "drops outer styles that contain a link" $
            flatten (Strong [Emph [Link ("", [], [])[Str "s"] ("", "")]]) @?=
            [Strong [],Link ("",[],[]) [Str "s"] ("",""),Strong []]
          , testCase "preserves strong text in an emphasis " $
            flatten (Emph [Str "f", Str "s", Strong [Str "d"], Str "l"]) @?=
            [Emph [Str "f", Str "s"], Strong [Str "d"], Emph [Str "l"]]
          , testCase "drops emphasis in a strong inline" $
            flatten (Strong [Str "f", Str "s", Emph [Str "d"], Str "l"]) @?=
            [Strong [Str "f", Str "s", Str "d", Str "l"]]
          , testCase "does not break links" $
            flatten (Link ("", [], [])[Str "f", Strong [Str "d"]] ("", "")) @?=
            [Link ("", [], [])[Str "f", Str "d"] ("", "")]
          , testCase "keeps inlines not matched by dropStyle" $
            flatten (Strong [Str "f", Subscript [Str "d"], Str "l"]) @?=
            [Strong [Str "f", Subscript [Str "d"], Str "l"]]
          , testCase "keeps an image even when it does not contain inlines" $
            flatten (Image ("",[],[]) [] ("image5.jpeg","")) @?=
            [Image ("",[],[]) [] ("image5.jpeg","")]
          ]
        , testGroup "headings"
          [ "normal heading" =:
              header 1 (text "foo") =?>
              unlines
              [ "foo"
              , "==="]
          -- note: heading normalization is only done in standalone mode
          , test (purely (writeRST def{ writerTemplate = Just "$body$\n" }) . toPandoc)
            "heading levels" $
              header 1 (text "Header 1") <>
              header 3 (text "Header 2") <>
              header 2 (text "Header 2") <>
              header 1 (text "Header 1") <>
              header 4 (text "Header 2") <>
              header 5 (text "Header 3") <>
              header 3 (text "Header 2") =?>
              unlines
              [ "Header 1"
              , "========"
              , ""
              , "Header 2"
              , "--------"
              , ""
              , "Header 2"
              , "--------"
              , ""
              , "Header 1"
              , "========"
              , ""
              , "Header 2"
              , "--------"
              , ""
              , "Header 3"
              , "~~~~~~~~"
              , ""
              , "Header 2"
              , "--------"]
          , test (purely (writeRST def{ writerTemplate = Just "$body$\n" }) . toPandoc)
            "minimal heading levels" $
              header 2 (text "Header 1") <>
              header 3 (text "Header 2") <>
              header 2 (text "Header 1") <>
              header 4 (text "Header 2") <>
              header 5 (text "Header 3") <>
              header 3 (text "Header 2") =?>
              unlines
              [ "Header 1"
              , "========"
              , ""
              , "Header 2"
              , "--------"
              , ""
              , "Header 1"
              , "========"
              , ""
              , "Header 2"
              , "--------"
              , ""
              , "Header 3"
              , "~~~~~~~~"
              , ""
              , "Header 2"
              , "--------"]
          ]
        ]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.RST (tests) where

import Prelude
import Control.Monad.Identity
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Writers.RST
import qualified Data.Text as T

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeRST def . toPandoc))

testTemplate :: (ToString a, ToString c, ToPandoc a) =>
                String -> String -> (a, c) -> TestTree
testTemplate t = case runIdentity (compileTemplate [] (T.pack t)) of
    Left e -> error $ "Could not compile RST template: " ++ e
    Right templ -> test (purely (writeRST def{ writerTemplate = Just templ }) . toPandoc)

bodyTemplate :: Template T.Text
bodyTemplate = case runIdentity (compileTemplate [] "$body$\n") of
                    Left e      -> error $
                      "Could not compile RST bodyTemplate" ++ e
                    Right templ -> templ

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
              , "   .. rubric:: bar"
              , ""
              , "   baz"]
          , "in block quote" =:
              blockQuote (header 1 (text "bar")) =?>
              "   .. rubric:: bar"
          , "with id" =:
              blockQuote (headerWith ("foo",[],[]) 1 (text "bar")) =?>
              unlines
              [ "   .. rubric:: bar"
              , "      :name: foo"]
          , "with id class" =:
              blockQuote (headerWith ("foo",["baz"],[]) 1 (text "bar")) =?>
              unlines
              [ "   .. rubric:: bar"
              , "      :name: foo"
              , "      :class: baz"]
          ]
        , testGroup "ligatures" -- handling specific sequences of blocks
          [ "a list is closed by a comment before a quote" =: -- issue 4248
            bulletList [plain "bulleted"] <> blockQuote (plain "quoted") =?>
              unlines
              [ "-  bulleted"
              , ""
              , ".."
              , ""
              , "   quoted"]
          ]
        , testGroup "flatten"
          [ testCase "emerges nested styles as expected" $
            flatten (Emph [Str "1", Strong [Str "2"], Str "3"]) @?=
            [Emph [Str "1"], Strong [Str "2"], Emph [Str "3"]]
          , testCase "could introduce trailing spaces" $
            flatten (Emph [Str "f", Space, Strong [Str "2"]]) @?=
            [Emph [Str "f", Space], Strong [Str "2"]]
            -- the test above is the reason why we call
            -- stripLeadingTrailingSpace through transformNested after
            -- flatten
          , testCase "preserves empty parents" $
            flatten (Image ("",[],[]) [] ("loc","title")) @?=
            [Image ("",[],[]) [] ("loc","title")]
          ]
        , testGroup "inlines"
          [ "are removed when empty" =: -- #4434
            plain (strong (str "")) =?> ""
          , "do not cause the introduction of extra spaces when removed" =:
            plain (strong (str "") <> emph (str "text")) =?> "*text*"
          , "spaces are stripped at beginning and end" =:
            -- pandoc issue 4327 "The text within inline markup may not
            -- begin or end with whitespace"
            -- http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup
            strong (space <> str "text" <> space <> space) =?> "**text**"
          , "single space stripped" =:
            strong space =?> ""
          , "give priority to strong style over emphasis" =:
            strong (emph (strong (str "s"))) =?> "**s**"
          , "links are not elided by outer style" =:
            strong (emph (link "loc" "" (str "text"))) =?>
            "`text <loc>`__"
          , "RST inlines cannot start nor end with spaces" =:
            emph (str "f" <> space <> strong (str "d") <> space <> str "l") =?>
            "*f*\\ **d**\\ *l*"
          , "keeps quotes" =:
            strong (str "f" <> doubleQuoted (str "d") <> str "l") =?>
            "**f“d”l**"
          , "backslash inserted between str and code" =:
            str "/api?query=" <> code "foo" =?>
            "/api?query=\\ ``foo``"
          ]
        , testGroup "headings"
          [ "normal heading" =:
              header 1 (text "foo") =?>
              unlines
              [ "foo"
              , "==="]
          -- note: heading normalization is only done in standalone mode
          , test (purely (writeRST def{ writerTemplate = Just bodyTemplate })
                       . toPandoc)
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
          , test (purely (writeRST def{ writerTemplate = Just bodyTemplate }) . toPandoc)
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
        , testTemplate "$subtitle$\n" "subtitle" $
          setMeta "subtitle" ("subtitle" :: Inlines) (doc $ plain "") =?>
          ("subtitle" :: String)
        ]

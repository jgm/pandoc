{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.RST (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Text.Pandoc.Arbitrary()

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test (purely (writeRST def{ writerHighlight = True }) . toPandoc)

tests :: [Test]
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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.SILE (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

sile :: (ToPandoc a) => a -> String
sile = sileWithOpts def

sileWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
sileWithOpts opts = unpack . purely (writeSILE opts) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test sile "my test" $ X =?> Y

which is in turn shorthand for

  test sile "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test sile

tests :: [TestTree]
tests = [testGroup "BlockQuote"
          [ "simple" =: blockQuote (para "foo") =?>
            "\\begin{BlockQuote}\nfoo\n\\end{BlockQuote}"
          ]
        ,testGroup "BulletList"
          [ "simple" =: bulletList [para "foo", para "bar"] =?>
            "\\begin{BulletList}\n\\ListItem{foo}\n\\ListItem{bar}\n\\end{BulletList}"
          ]
        ,testGroup "CodeBlock"
          [ "simple" =: codeBlock "foo" =?>
            "\\begin{CodeBlock}\nfoo\n\\end{CodeBlock}"
          , "with id" =: codeBlockWith ("bar", ["stuff"], []) "foo" =?>
            "\\begin[id=bar,classes=\"stuff\"]{CodeBlock}\nfoo\n\\end{CodeBlock}"
          ]
        , testGroup "definition lists"
          [ "with internal link" =: definitionList [(link "#go" "" (str "testing"),
             [plain (text "hi there")])] =?>
            "\\begin[tight=true]{DefinitionList}\n\\term{\\pdf:link[id=go]{testing}}\n\\definition{hi there}\n\\end{DefinitionList}"
          ]
        , testGroup "Header"
          [ "chapter" =: header 0 (text "foo") =?>
            "\\Header[level=0,type=chapter]{foo}"
          , "section" =: header 1 (text "foo") =?>
            "\\Header[level=1,type=section]{foo}"
          , "subsection" =: header 2 (text "foo") =?>
            "\\Header[level=2,type=subsection]{foo}"
          -- , "part" =: header 0 (text "foo") =?>
          --   "\\Header[level=-1,type=part]{foo}"
          , "unnumbered with id note" =:
            headerWith ("foo",["unnumbered"],[]) 1
              (text "foo" <> note (plain $ text "bar")) =?>
            "\\Header[id=foo,classes=\"unnumbered\",level=1,type=section]{foo\\footnote{bar}}"
          , "in list item" =: bulletList [header 2 (text "foo")] =?>
            "\\begin{BulletList}\n\\ListItem{\\Header[level=2,type=subsection]{foo}}\n\\end{BulletList}"
          , "in definition list item" =:
            definitionList [(text "foo", [header 2 (text "bar"),
                                          para $ text "baz"])] =?>
            "\\begin{DefinitionList}\n\\term{foo}\n\\definition{\\Header[level=2,type=subsection]{bar}\n\nbaz}\n\\end{DefinitionList}"
          , "containing image" =: header 1 (image "imgs/foo.jpg" "" (text "Alt text")) =?>
            "\\Header[level=1,type=section]{\\img[src=imgs/foo.jpg]{Alt text}}"
          ]
        ]

{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org.Directive (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), tagSpan)
import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ testGroup "export options"
    [ "disable simple sub/superscript syntax" =:
        T.unlines [ "#+OPTIONS: ^:nil"
                  , "a^b"
                  ] =?>
        para "a^b"

    , "directly select drawers to be exported" =:
        T.unlines [ "#+OPTIONS: d:(\"IMPORTANT\")"
                  , ":IMPORTANT:"
                  , "23"
                  , ":END:"
                  , ":BORING:"
                  , "very boring"
                  , ":END:"
                  ] =?>
        divWith (mempty, ["IMPORTANT", "drawer"], mempty) (para "23")

    , "exclude drawers from being exported" =:
        T.unlines [ "#+OPTIONS: d:(not \"BORING\")"
                  , ":IMPORTANT:"
                  , "5"
                  , ":END:"
                  , ":BORING:"
                  , "very boring"
                  , ":END:"
                  ] =?>
        divWith (mempty, ["IMPORTANT", "drawer"], mempty) (para "5")

    , "don't include archive trees" =:
        T.unlines [ "#+OPTIONS: arch:nil"
                  , "* old  :ARCHIVE:"
                  ] =?>
        (mempty ::Blocks)

    , "include complete archive trees" =:
        T.unlines [ "#+OPTIONS: arch:t"
                  , "* old  :ARCHIVE:"
                  , "  boring"
                  ] =?>
        mconcat [ headerWith ("old", [], mempty) 1
                             ("old" <> space <> tagSpan "ARCHIVE")
                , para "boring"
                ]

    , "include archive tree header only" =:
        T.unlines [ "#+OPTIONS: arch:headline"
                  , "* old  :ARCHIVE:"
                  , "  boring"
                  ] =?>
        headerWith ("old", [], mempty) 1 ("old" <> space <> tagSpan "ARCHIVE")

    , "limit headline depth" =:
        T.unlines [ "#+OPTIONS: H:2"
                  , "* top-level section"
                  , "** subsection"
                  , "*** list item 1"
                  , "*** list item 2"
                  ] =?>
        mconcat [ headerWith ("top-level-section", [], [])    1 "top-level section"
                , headerWith ("subsection", [], []) 2 "subsection"
                , orderedList [ para "list item 1", para "list item 2" ]
                ]

    , "turn all headlines into lists" =:
        T.unlines [ "#+OPTIONS: H:0"
                  , "first block"
                  , "* top-level section 1"
                  , "** subsection"
                  , "* top-level section 2"
                  ] =?>
        mconcat [ para "first block"
                , orderedList
                  [ (para "top-level section 1" <>
                     orderedList [ para "subsection" ])
                  , para "top-level section 2" ]
                ]

    , "preserve linebreaks as hard breaks" =:
        T.unlines [ "#+OPTIONS: \\n:t"
                  , "first"
                  , "second"
                  ] =?>
        para ("first" <> linebreak <> "second")

    , "disable author export" =:
        T.unlines [ "#+OPTIONS: author:nil"
                  , "#+AUTHOR: ShyGuy"
                  ] =?>
        Pandoc nullMeta mempty

    , "disable creator export" =:
        T.unlines [ "#+OPTIONS: creator:nil"
                  , "#+creator: The Architect"
                  ] =?>
        Pandoc nullMeta mempty

    , "disable email export" =:
        T.unlines [ "#+OPTIONS: email:nil"
                  , "#+email: no-mail-please@example.com"
                  ] =?>
        Pandoc nullMeta mempty

    , "disable inclusion of todo keywords" =:
        T.unlines [ "#+OPTIONS: todo:nil"
                  , "** DONE todo export"
                  ] =?>
        headerWith ("todo-export", [], []) 2 "todo export"

    , "remove tags from headlines" =:
        T.unlines [ "#+OPTIONS: tags:nil"
                  , "* Headline :hello:world:"
                  ] =?>
        headerWith ("headline", [], mempty) 1 "Headline"
    ]
  ]

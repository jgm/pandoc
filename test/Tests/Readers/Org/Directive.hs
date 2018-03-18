{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org.Directive (tests) where

import Prelude
import Data.Time (UTCTime (UTCTime), secondsToDiffTime)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>), ToString, purely, test)
import Tests.Readers.Org.Shared ((=:), tagSpan)
import Text.Pandoc
import Text.Pandoc.Builder
import qualified Data.ByteString as BS
import qualified Data.Text as T

testWithFiles :: (ToString c)
              => [(FilePath, BS.ByteString)]
              -> String         -- ^ name of test case
              -> (T.Text, c)    -- ^ (input, expected value)
              -> TestTree
testWithFiles fileDefs = test (orgWithFiles fileDefs)
  where
orgWithFiles :: [(FilePath, BS.ByteString)] -> T.Text -> Pandoc
orgWithFiles fileDefs input =
  let readOrg' = readOrg def{ readerExtensions = getDefaultExtensions "org" }
  in flip purely input $ \inp -> do
    modifyPureState (\st -> st { stFiles = files fileDefs })
    readOrg' inp


files :: [(FilePath, BS.ByteString)] -> FileTree
files fileDefs =
  let dummyTime = UTCTime (ModifiedJulianDay 125) (secondsToDiffTime 0)
  in foldr (\(fp, bs) -> insertInFileTree fp (FileInfo dummyTime bs))
      mempty fileDefs

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
                  [ para "top-level section 1" <>
                     orderedList [ para "subsection" ]
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

  , testGroup "Include"
    [ testWithFiles [("./other.org", "content of other file\n")]
      "file inclusion"
      (T.unlines [ "#+include: \"other.org\"" ] =?>
       plain "content of other file")

    , testWithFiles [("./world.org", "World\n\n")]
      "Included file belongs to item"
      (T.unlines [ "- Hello,\n  #+include: \"world.org\"" ] =?>
       bulletList [para "Hello," <> para "World"])

    , testWithFiles [("./level3.org", "*** Level3\n\n")]
      "Default include preserves level"
      (T.unlines [ "#+include: \"level3.org\"" ] =?>
       headerWith ("level3", [], []) 3 "Level3")

    , testWithFiles [("./level3.org", "*** Level3\n\n")]
      "Minlevel shifts level"
      (T.unlines [ "#+include: \"level3.org\" :minlevel 1" ] =?>
       headerWith ("level3", [], []) 1 "Level3")

    , testWithFiles [("./src.hs", "putStrLn outString\n")]
      "Include file as source code snippet"
      (T.unlines [ "#+include: \"src.hs\" src haskell" ] =?>
       codeBlockWith ("", ["haskell"], []) "putStrLn outString\n")

    , testWithFiles [("./export-latex.org", "\\emph{Hello}\n")]
      "Include file as export snippet"
      (T.unlines [ "#+include: \"export-latex.org\" export latex" ] =?>
       rawBlock "latex" "\\emph{Hello}\n")

    , testWithFiles [("./subdir/foo-bar.latex", "foo\n"),
                     ("./hello.lisp", "(print \"Hello!\")\n")
                    ]
      "include directive is limited to one line"
      (T.unlines [ "#+INCLUDE: \"hello.lisp\" src lisp"
                 , "#+include: \"subdir/foo-bar.latex\" export latex"
                 , "bar"
                 ] =?>
       mconcat
         [ codeBlockWith ("", ["lisp"], []) "(print \"Hello!\")\n"
         , rawBlock "latex" "foo\n"
         , para "bar"
         ]
      )
    ]
  ]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Header
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org header blocks.
-}
module Tests.Readers.Org.Block.Header (tests) where

import Prelude
import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep, tagSpan)
import Text.Pandoc.Builder
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ "First Level Header" =:
      "* Headline\n" =?>
      headerWith ("headline", [], []) 1 "Headline"

  , "Third Level Header" =:
      "*** Third Level Headline\n" =?>
      headerWith ("third-level-headline", [], [])
                 3
                 ("Third" <> space <> "Level" <> space <> "Headline")

  , "Compact Headers with Paragraph" =:
      T.unlines [ "* First Level"
                , "** Second Level"
                , "   Text"
                ] =?>
      mconcat [ headerWith ("first-level", [], [])
                           1
                           ("First" <> space <> "Level")
              , headerWith ("second-level", [], [])
                           2
                           ("Second" <> space <> "Level")
              , para "Text"
              ]

  , "Separated Headers with Paragraph" =:
      T.unlines [ "* First Level"
                , ""
                , "** Second Level"
                , ""
                , "   Text"
                ] =?>
      mconcat [ headerWith ("first-level", [], [])
                           1
                           ("First" <> space <> "Level")
              , headerWith ("second-level", [], [])
                           2
                           ("Second" <> space <> "Level")
              , para "Text"
              ]

  , "Headers not preceded by a blank line" =:
      T.unlines [ "** eat dinner"
                , "Spaghetti and meatballs tonight."
                , "** walk dog"
                ] =?>
      mconcat [ headerWith ("eat-dinner", [], [])
                           2
                           ("eat" <> space <> "dinner")
              , para $ spcSep [ "Spaghetti", "and", "meatballs", "tonight." ]
              , headerWith ("walk-dog", [], [])
                           2
                           ("walk" <> space <> "dog")
              ]

  , testGroup "Todo keywords"
    [ "Header with known todo keyword" =:
        "* TODO header" =?>
        let todoSpan = spanWith ("", ["todo", "TODO"], []) "TODO"
        in headerWith ("header", [], []) 1 (todoSpan <> space <> "header")

    , "Header marked as done" =:
        "* DONE header" =?>
        let todoSpan = spanWith ("", ["done", "DONE"], []) "DONE"
        in headerWith ("header", [], []) 1 (todoSpan <> space <> "header")

    , "emphasis in first word" =:
        "** TODO /fix/ this" =?>
        let todoSpan = spanWith ("", ["todo", "TODO"], []) "TODO"
        in headerWith ("fix-this", [], [])
                      2
                      (todoSpan <> space <> emph "fix" <> space <> "this")

    , "Header with unknown todo keyword" =:
        "* WAITING header" =?>
        headerWith ("waiting-header", [], []) 1 "WAITING header"

    , "Custom todo keywords" =:
        T.unlines [ "#+TODO: WAITING CANCELLED"
                  , "* WAITING compile"
                  , "* CANCELLED lunch"
                  ] =?>
        let todoSpan = spanWith ("", ["todo", "WAITING"], []) "WAITING"
            doneSpan = spanWith ("", ["done", "CANCELLED"], []) "CANCELLED"
        in headerWith ("compile", [], []) 1 (todoSpan <> space <> "compile")
        <> headerWith ("lunch", [], []) 1 (doneSpan <> space <> "lunch")

    , "Custom todo keywords with multiple done-states" =:
        T.unlines [ "#+TODO: WAITING | DONE CANCELLED "
                  , "* WAITING compile"
                  , "* CANCELLED lunch"
                  , "* DONE todo-feature"
                  ] =?>
        let waiting = spanWith ("", ["todo", "WAITING"], []) "WAITING"
            cancelled = spanWith ("", ["done", "CANCELLED"], []) "CANCELLED"
            done = spanWith ("", ["done", "DONE"], []) "DONE"
        in headerWith ("compile", [], []) 1 (waiting <> space <> "compile")
        <> headerWith ("lunch", [], []) 1 (cancelled <> space <> "lunch")
        <> headerWith ("todo-feature", [], []) 1 (done <> space <> "todo-feature")
    ]

  , "Tagged headers" =:
      T.unlines [ "* Personal       :PERSONAL:"
                , "** Call Mom      :@PHONE:"
                , "** Call John     :@PHONE:JOHN: "
                ] =?>
      mconcat [ headerWith ("personal", [], [])
                           1
                           ("Personal " <> tagSpan "PERSONAL")
              , headerWith ("call-mom", [], [])
                           2
                           ("Call Mom " <> tagSpan "@PHONE")
              , headerWith ("call-john", [], [])
                           2
                           ("Call John " <> tagSpan "@PHONE" <> "\160" <> tagSpan "JOHN")
              ]

  , "Untagged header containing colons" =:
      "* This: is not: tagged" =?>
      headerWith ("this-is-not-tagged", [], []) 1 "This: is not: tagged"

  , "Untagged header time followed by colon" =:
      "** Meeting at 5:23: free food" =?>
      let attr = ("meeting-at-523-free-food", [], [])
      in headerWith attr 2 "Meeting at 5:23: free food"

  , "tag followed by text" =:
      "*** Looks like a :tag: but isn't" =?>
      let attr = ("looks-like-a-tag-but-isnt", [], [])
      in headerWith attr 3 "Looks like a :tag: but isn't"

  , "Header starting with strokeout text" =:
      T.unlines [ "foo"
                , ""
                , "* +thing+ other thing"
                ] =?>
      mconcat [ para "foo"
              , headerWith ("thing-other-thing", [], [])
                           1
                           (strikeout "thing" <> " other thing")
              ]

  , "Comment Trees" =:
      T.unlines [ "* COMMENT A comment tree"
                , "  Not much going on here"
                , "** This will be dropped"
                , "* Comment tree above"
                ] =?>
      headerWith ("comment-tree-above", [], []) 1 "Comment tree above"

  , "Nothing but a COMMENT header" =:
      "* COMMENT Test" =?>
      (mempty::Blocks)

  , "Tree with :noexport:" =:
      T.unlines [ "* Should be ignored :archive:noexport:old:"
                , "** Old stuff"
                , "   This is not going to be exported"
                ] =?>
      (mempty::Blocks)

  , "Subtree with :noexport:" =:
      T.unlines [ "* Exported"
                , "** This isn't exported :noexport:"
                , "*** This neither"
                , "** But this is"
                ] =?>
      mconcat [ headerWith ("exported", [], []) 1 "Exported"
              , headerWith ("but-this-is", [], []) 2 "But this is"
              ]

  , "Preferences are treated as header attributes" =:
      T.unlines [ "* foo"
                , "  :PROPERTIES:"
                , "  :custom_id: fubar"
                , "  :bar: baz"
                , "  :END:"
                ] =?>
      headerWith ("fubar", [], [("bar", "baz")]) 1 "foo"


  , "Headers marked with a unnumbered property get a class of the same name" =:
      T.unlines [ "* Not numbered"
                , "  :PROPERTIES:"
                , "  :UNNUMBERED: t"
                , "  :END:"
                ] =?>
      headerWith ("not-numbered", ["unnumbered"], []) 1 "Not numbered"

  , testGroup "planning information"
    [ "Planning info is not included in output" =:
      T.unlines [ "* important"
                , T.unwords
                  [ "CLOSED: [2018-09-05 Wed 13:58]"
                  , "DEADLINE: <2018-09-17 Mon>"
                  , "SCHEDULED: <2018-09-10 Mon>"
                  ]
                ] =?>
      headerWith ("important", [], []) 1 "important"

    , "Properties after planning info are recognized" =:
      T.unlines [ "* important "
                , "  " <> T.unwords
                  [ "CLOSED: [2018-09-05 Wed 13:58]"
                  , "DEADLINE: <2018-09-17 Mon>"
                  , "SCHEDULED: <2018-09-10 Mon>"
                  ]
                , "  :PROPERTIES:"
                , "  :custom_id: look"
                , "  :END:"
                ] =?>
      headerWith ("look", [], []) 1 "important"

    , "Planning info followed by test" =:
      T.unlines [ "* important "
                , "  " <> T.unwords
                  [ "CLOSED: [2018-09-05 Wed 13:58]"
                  , "DEADLINE: <2018-09-17 Mon>"
                  , "SCHEDULED: <2018-09-10 Mon>"
                  ]
                , "  :PROPERTIES:"
                , "  :custom_id: look"
                , "  :END:"
                ] =?>
      headerWith ("look", [], []) 1 "important"

    , "third and forth level headers" =:
      T.unlines [ "#+OPTIONS: p:t h:3"
                , "*** Third"
                , "    CLOSED: [2018-09-05 Wed 13:58]"
                , "    Text 3"
                , "**** Fourth"
                , "SCHEDULED: <2019-05-13 Mon 22:42>"
                , "Text 4"
                ] =?>
      mconcat
      [ headerWith ("third", [], mempty) 3 "Third"
      , plain $
        strong "CLOSED:" <> space <> emph (str "[2018-09-05 Wed 13:58]")
      , para "Text 3"
      , orderedList [
          mconcat
          [ para "Fourth"
          , plain $ strong "SCHEDULED:"
                    <> space
                    <> emph (str "<2019-05-13 Mon 22:42>")
          , para "Text 4"
          ]]
      ]
    ]
  ]

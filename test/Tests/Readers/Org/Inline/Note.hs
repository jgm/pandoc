{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Inline.Note
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of footnotes in org input.
-}
module Tests.Readers.Org.Inline.Note (tests) where

import Prelude
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:))
import Text.Pandoc.Builder
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ "Footnote" =:
      T.unlines [ "A footnote[1]"
                , ""
                , "[1] First paragraph"
                , ""
                , "second paragraph"
                ] =?>
      para (mconcat
            [ "A", space, "footnote"
            , note $ mconcat [ para ("First" <> space <> "paragraph")
                             , para ("second" <> space <> "paragraph")
                             ]
            ])

  , "Two footnotes" =:
      T.unlines [ "Footnotes[fn:1][fn:2]"
                , ""
                , "[fn:1] First note."
                , ""
                , "[fn:2] Second note."
                ] =?>
      para (mconcat
            [ "Footnotes"
            , note $ para ("First" <> space <> "note.")
            , note $ para ("Second" <> space <> "note.")
            ])

  , "Emphasized text before footnote" =:
      T.unlines [ "/text/[fn:1]"
                , ""
                , "[fn:1] unicorn"
                ] =?>
      para (mconcat
           [ emph "text"
           , note . para $ "unicorn"
           ])

  , "Footnote that starts with emphasized text" =:
      T.unlines [ "text[fn:1]"
                , ""
                , "[fn:1] /emphasized/"
                ] =?>
      para (mconcat
           [ "text"
           , note . para $ emph "emphasized"
           ])

  , "Footnote followed by header" =:
      T.unlines [ "Another note[fn:yay]"
                , ""
                , "[fn:yay] This is great!"
                , ""
                , "** Headline"
                ] =?>
      mconcat
      [ para (mconcat
              [ "Another", space, "note"
              , note $ para ("This" <> space <> "is" <> space <> "great!")
              ])
      , headerWith ("headline", [], []) 2 "Headline"
      ]

  , "Footnote followed by two blank lines" =:
      T.unlines [ "footnote[fn:blanklines]"
                , ""
                , "[fn:blanklines] followed by blank lines"
                , ""
                , ""
                , "next"
                ] =?>
      mconcat
      [ para ("footnote" <> note (para "followed by blank lines"))
      , para "next"
      ]
  ]

{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.MediaWiki
   Copyright   : © 2025 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : 
   Stability   : alpha
   Portability : portable

Tests for the MediaWiki reader.
-}

module Tests.Readers.MediaWiki (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

mw :: Text -> Pandoc
mw = purely $ readMediaWiki def

wikilink :: Text -> Inlines
wikilink dest = linkWith ("", ["wikilink"], []) (T.replace " " "_" dest) dest (text dest)

infix 4 =:
(=:) :: (ToString c, HasCallStack)
     => String -> (Text, c) -> TestTree
(=:) = test mw

tests :: [TestTree]
tests = [
  -- The "quotes" tests are adapted from tests for parsoid, MediaWiki's current
  -- wikitext parser. Cf. https://gerrit.wikimedia.org/r/plugins/gitiles/mediawiki/services/parsoid/+/refs/heads/master/tests/parser/quotes.txt
  testGroup "quotes"
    [ testGroup "intraword emphasis"
      [ "italic" =:
          "plain''italic''plain" =?>
          para ("plain" <> emph "italic" <> "plain")
      , "two italics" =:
          "plain''italic''plain''italic''plain" =?>
          para ("plain" <> emph "italic" <> "plain" <> emph "italic" <> "plain")
      , "bold" =:
          "plain'''bold'''plain" =?>
          para ("plain" <> strong "bold" <> "plain")
      , "two bolds" =:
          "plain'''bold'''plain'''bold'''plain" =?>
          para ("plain" <> strong "bold" <> "plain" <> strong "bold" <> "plain")
      , "bold and italic" =:
          "plain'''bold'''plain''italic''plain" =?>
          para ("plain" <> strong "bold" <> "plain" <> emph "italic" <> "plain")
      , "italic and bold" =:
          "plain''italic''plain'''bold'''plain" =?>
          para ("plain" <> emph "italic" <> "plain" <> strong "bold" <> "plain")
      , "italic with bold-italic" =:
          "plain''italic'''bold-italic'''italic''plain" =?>
          para ("plain" <> emph ("italic" <> strong "bold-italic" <> "italic") <> "plain")
      , "bold with bold-italic" =:
          "plain'''bold''bold-italic''bold'''plain" =?>
          para ("plain" <> strong ("bold" <> emph "bold-italic" <> "bold") <> "plain")
      , "bold-italic then italic" =:
          "plain'''''bold-italic'''italic''plain" =?>
          para ("plain" <> emph (strong "bold-italic" <> "italic") <> "plain")
      , "bold-italic then bold" =:
          "plain'''''bold-italic''bold'''plain" =?>
          para ("plain" <> strong (emph "bold-italic" <> "bold") <> "plain")
      , "italic then bold-italic" =:
          "plain''italic'''bold-italic'''''plain" =?>
          para ("plain" <> emph ("italic" <> strong "bold-italic") <> "plain")
      , "bold then bold-italic" =:
          "plain'''bold''bold-italic'''''plain" =?>
          para ("plain" <> strong ("bold" <> emph "bold-italic") <> "plain")
      ]
    , testGroup "possessives and italics"
      [ "simple" =:
          "In ''Flaming Pie'''s liner notes" =?>
          para ("In " <> emph "Flaming Pie'" <> "s liner notes")
      , "linked" =:
          "obtained by ''[[Lunar Prospector]]'''s gamma-ray spectrometer" =?>
          para ("obtained by " <> emph ((wikilink "Lunar Prospector") <> "'") <> "s gamma-ray spectrometer")
      , "with following italics" =:
          "''Sebastián Covarrubias''' ''Tesoro''" =?>
          para (emph "Sebastián Covarrubias'" <> " " <> emph "Tesoro")
      , "with internal link" =:
          "the ''Vocabolario dell'[[Accademia della Crusca]]'', for Italian" =?>
          para ("the " <> emph ("Vocabolario dell'" <> wikilink "Accademia della Crusca") <>
            ", for Italian")
      , "multiple" =:
          "'''This year''''s election ''should'' beat '''last year''''s." =?>
          para (strong "This year'" <> "s election " <> emph "should" <> " beat " <> strong "last year'" <> "s.")
      ]
    , testGroup "two-quote openings"
      [ "2 open 3 close" =:
          "''foo'''" =?>
          para (emph "foo'")
      , "2 open 4 close" =:
          "''foo''''" =?>
          para (emph "foo''")
      -- TODO line ends terminate emphases
      -- , "2 open 5 close" =:
      --     "''foo'''''" =?>
      --     para (emph "foo" <> strong "")
      ]
    , testGroup "three-quote openings"
      [ "3 open 2 close" =:
          "'''foo''" =?>
          para ("'" <> emph "foo")
      , "3 open 3 close" =:
          "'''foo'''" =?>
          para (strong "foo")
      , "3 open 4 close" =:
          "'''foo''''" =?>
          para (strong "foo'")
      -- TODO line ends terminate emphases
      -- , "3 open 5 close" =:
      --     "'''foo'''''" =?>
      --     para (strong "foo" <> emph "" )
      ]
    , testGroup "four-quote openings"
      [ "4 open 2 close" =:
          "''''foo''" =?>
          para ("''" <> emph "foo")
      , "4 open 3 close" =:
          "''''foo'''" =?>
          para ("'" <> strong "foo")
      , "4 open 4 close" =:
          "''''foo''''" =?>
          para ("'" <> strong "foo'")
      -- TODO line ends terminate emphases
      -- , "4 open 5 close" =:
      --     "''''foo'''''" =?>
      --     para ("'" <> strong "foo" <> emph "")
      ]
    , testGroup "five-quote openings"
      [ -- TODO line ends terminate emphases
        -- "5 open 2 close" =:
        --   "'''''foo''" =?>
        --   para (strong (emph "foo"))
        -- , "5 open 3 close" =:
        -- "'''''foo'''" =?>
        -- para (emph (strong "foo"))
        -- , "5 open 4 close" =:
        --   "'''''foo''''" =?>
        -- para (emph (strong "foo'"))
        "5 open 5 close" =:
          "'''''foo'''''" =?>
          para (emph (strong "foo"))
      , "5 open 6 close" =:
          "'''''foo''''''" =?>
          para (emph (strong "foo'"))
      ]
    , testGroup "multiple quote sequences"
      [ "2, 4, 2" =:
          "''foo''''bar''" =?>
          para (emph ("foo'" <> strong "bar"))
      , "2, 4, 2, more" =:
          "''foo''''bar'' something else" =?>
          para (emph ("foo'" <> strong "bar"))
      ]
    ]
  ]

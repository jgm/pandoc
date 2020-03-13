{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Figure
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org figures.
-}
module Tests.Readers.Org.Block.Figure (tests) where

import Prelude
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:))
import Text.Pandoc.Builder (image, imageWith, para)
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ "Figure" =:
      T.unlines [ "#+caption: A very courageous man."
                , "#+name: goodguy"
                , "[[file:edward.jpg]]"
                ] =?>
      para (image "edward.jpg" "fig:goodguy" "A very courageous man.")

  , "Figure with no name" =:
      T.unlines [ "#+caption: I've been through the desert on this"
                , "[[file:horse.png]]"
                ] =?>
      para (image "horse.png" "fig:" "I've been through the desert on this")

  , "Figure with `fig:` prefix in name" =:
      T.unlines [ "#+caption: Used as a metapher in evolutionary biology."
                , "#+name: fig:redqueen"
                , "[[./the-red-queen.jpg]]"
                ] =?>
      para (image "./the-red-queen.jpg" "fig:redqueen"
                  "Used as a metapher in evolutionary biology.")

  , "Figure with HTML attributes" =:
      T.unlines [ "#+CAPTION: mah brain just explodid"
                , "#+NAME: lambdacat"
                , "#+ATTR_HTML: :style color: blue :role button"
                , "[[file:lambdacat.jpg]]"
                ] =?>
      let kv = [("style", "color: blue"), ("role", "button")]
          name = "fig:lambdacat"
          caption = "mah brain just explodid"
      in para (imageWith (mempty, mempty, kv) "lambdacat.jpg" name caption)

  , "LaTeX attributes are ignored" =:
      T.unlines [ "#+CAPTION: Attribute after caption"
                , "#+ATTR_LATEX: :float nil"
                , "[[file:test.png]]"
                ] =?>
      para (image "test.png" "fig:" "Attribute after caption")

  , "Labelled figure" =:
      T.unlines [ "#+CAPTION: My figure"
                , "#+LABEL: fig:myfig"
                , "[[file:blub.png]]"
                ] =?>
      let attr = ("fig:myfig", mempty, mempty)
      in para (imageWith attr "blub.png" "fig:" "My figure")

  , "Figure with empty caption" =:
      T.unlines [ "#+CAPTION:"
                , "[[file:guess.jpg]]"
                ] =?>
      para (image "guess.jpg" "fig:" "")
  ]

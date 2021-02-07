{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Figure
   Copyright   : © 2014-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org figures.
-}
module Tests.Readers.Org.Block.Figure (tests) where

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
      T.unlines [ "#+caption: mah brain just explodid"
                , "#+name: lambdacat"
                , "#+attr_html: :style color: blue :role button"
                , "[[file:lambdacat.jpg]]"
                ] =?>
      let kv = [("style", "color: blue"), ("role", "button")]
          name = "fig:lambdacat"
          caption = "mah brain just explodid"
      in para (imageWith (mempty, mempty, kv) "lambdacat.jpg" name caption)

  , "LaTeX attributes are ignored" =:
      T.unlines [ "#+caption: Attribute after caption"
                , "#+attr_latex: :float nil"
                , "[[file:test.png]]"
                ] =?>
      para (image "test.png" "fig:" "Attribute after caption")

  , "Labelled figure" =:
      T.unlines [ "#+caption: My figure"
                , "#+label: fig:myfig"
                , "[[file:blub.png]]"
                ] =?>
      let attr = ("fig:myfig", mempty, mempty)
      in para (imageWith attr "blub.png" "fig:" "My figure")

  , "Figure with empty caption" =:
      T.unlines [ "#+caption:"
                , "[[file:guess.jpg]]"
                ] =?>
      para (image "guess.jpg" "fig:" "")
  ]

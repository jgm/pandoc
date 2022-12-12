{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Figure
   Copyright   : © 2014-2023 Albert Krewinkel
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
import Text.Pandoc.Builder ( emptyCaption, figure, figureWith, image
                           , plain, simpleCaption, simpleFigure )
import qualified Data.Text as T

tests :: [TestTree]
tests =
  [ "Figure" =:
      T.unlines [ "#+caption: A courageous man."
                , "#+name: ed"
                , "[[file:edward.jpg]]"
                ] =?>
      figure (plainCaption "A courageous man.")
             (plain $ image "edward.jpg" "ed" "")

  , "Figure with no name" =:
      T.unlines [ "#+caption: I've been through the desert on this"
                , "[[file:horse.png]]"
                ] =?>
      figure (plainCaption "I've been through the desert on this")
             (plain $ image "horse.png" "" "")

  , "Figure with `fig:` prefix in name" =:
      T.unlines [ "#+caption: Used as a metapher in evolutionary biology."
                , "#+name: fig:redqueen"
                , "[[./the-red-queen.jpg]]"
                ] =?>
      figure (plainCaption "Used as a metapher in evolutionary biology.")
             (plain $ image "./the-red-queen.jpg" "fig:redqueen" "")

  , "Figure with HTML attributes" =:
      T.unlines [ "#+caption: mah brain just explodid"
                , "#+name: lambdacat"
                , "#+attr_html: :style color: blue :role button"
                , "[[file:lambdacat.jpg]]"
                ] =?>
      let kv = [("style", "color: blue"), ("role", "button")]
          name = "lambdacat"
          capt = plain "mah brain just explodid"
      in figureWith (mempty, mempty, kv) (simpleCaption capt)
         (plain $ image "lambdacat.jpg" name "")

  , "LaTeX attributes are ignored" =:
      T.unlines [ "#+caption: Attribute after caption"
                , "#+attr_latex: :float nil"
                , "[[file:test.png]]"
                ] =?>
      simpleFigure "Attribute after caption"
                   "test.png" ""

  , "Labelled figure" =:
      T.unlines [ "#+caption: My figure"
                , "#+label: fig:myfig"
                , "[[file:blub.png]]"
                ] =?>
      figureWith ("fig:myfig", mempty, mempty)
                 (simpleCaption $ plain "My figure")
                 (plain (image "blub.png" "" ""))

  , "Figure with empty caption" =:
      T.unlines [ "#+caption:"
                , "[[file:guess.jpg]]"
                ] =?>
      figure emptyCaption (plain (image "guess.jpg" "" ""))
  ]
 where
  plainCaption = simpleCaption . plain

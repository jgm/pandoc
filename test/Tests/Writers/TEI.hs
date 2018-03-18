{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.TEI (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

{-
  "my test" =: X =?> Y

is shorthand for

  test html "my test" $ X =?> Y

which is in turn shorthand for

  test html "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeTEI def) . toPandoc)

tests :: [TestTree]
tests = [ testGroup "block elements"
          ["para"          =: para "Lorem ipsum cetera."
                           =?> "<p>Lorem ipsum cetera.</p>"
          ]
        , testGroup "inlines"
          [
            "Emphasis"      =:  emph "emphasized"
                            =?> "<p><hi rendition=\"simple:italic\">emphasized</hi></p>"
           ,"SingleQuoted"  =:  singleQuoted (text "quoted material")
                            =?> "<p><quote>quoted material</quote></p>"
           ,"DoubleQuoted"  =:  doubleQuoted (text "quoted material")
                            =?> "<p><quote>quoted material</quote></p>"
           ,"NestedQuoted"  =:  doubleQuoted (singleQuoted (text "quoted material"))
                            =?> "<p><quote><quote>quoted material</quote></quote></p>"
          ]
         ]

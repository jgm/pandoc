{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Markua (tests) where

import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
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
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeMarkua def) . toPandoc)

tests :: [TestTree]
tests = [ testGroup "simple blurb/aside"
          ["blurb"          =: divWith ("",["blurb"],[]) (bulletList [para "blurb content"])
                           =?> "B> * blurb content"
          ,"aside"          =: divWith ("",["aside"],[]) (bulletList [para "aside list"])
                           =?> "A> * aside list"
          ]
         ,testGroup "multiclass blurb/aside"
          ["blurb"          =: divWith ("",["blurb", "otherclass"],[]) (bulletList [para "blurb content"])
                           =?> "B> * blurb content"
          ,"aside"          =: divWith ("",["otherclass", "aside"],[]) (bulletList [para "aside list"])
                           =?> "A> * aside list"
          ]
         ]

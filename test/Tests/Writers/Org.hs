{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Org (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writeOrg def . toPandoc))

tests :: [TestTree]
tests = [ testGroup "links"
          -- See http://orgmode.org/manual/Internal-links.html#Internal-links
          [ "simple link"
              =: link "/url" "" "foo"
              =?> "[[/url][foo]]"
          , "internal link to anchor"
              =: link "#my-custom-id" "" "#my-custom-id"
              =?> "[[#my-custom-id]]"
          ]
        ]

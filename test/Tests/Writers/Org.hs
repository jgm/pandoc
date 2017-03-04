{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Org (tests) where

import Test.Framework
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test (purely (writeOrg def . toPandoc))

tests :: [Test]
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

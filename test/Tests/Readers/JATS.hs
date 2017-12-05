{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.JATS (tests) where

import Data.Text (Text)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

jats :: Text -> Pandoc
jats = purely $ readJATS def

tests :: [TestTree]
tests = [ testGroup "inline code"
          [ test jats "basic" $ "<p>\n  <monospace>@&amp;</monospace>\n</p>" =?> para (code "@&")
          ]
        , testGroup "images"
          [ test jats "basic" $ "<graphic mimetype=\"image\" mime-subtype=\"\" xlink:href=\"/url\" xlink:title=\"title\" />"
            =?> image "/url" "title" mempty
          ]
        ]

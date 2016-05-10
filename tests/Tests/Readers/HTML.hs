{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.HTML (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Error

html :: String -> Pandoc
html = handleError . readHtml def

tests :: [Test]
tests = [ testGroup "base tag"
          [ test html "simple" $
            "<head><base href=\"http://www.w3schools.com/images/foo\" ></head><body><img src=\"stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/images/stickman.gif" "" (text "Stickman"))
          , test html "slash at end of base" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/images/stickman.gif" "" (text "Stickman"))
          , test html "slash at beginning of href" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"/stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://www.w3schools.com/stickman.gif" "" (text "Stickman"))
          , test html "absolute URL" $
            "<head><base href=\"http://www.w3schools.com/images/\" ></head><body><img src=\"http://example.com/stickman.gif\" alt=\"Stickman\"></head>" =?>
            plain (image "http://example.com/stickman.gif" "" (text "Stickman"))
          ]
        ]

{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.HTML (tests) where

import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Data.Text (Text)

html :: Text -> Pandoc
html = purely $ readHtml def

tests :: [TestTree]
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
        , testGroup "anchors"
          [ test html "anchor without href" $ "<a name=\"anchor\"/>" =?>
            plain (spanWith ("anchor",[],[]) mempty)
          ]
        , testGroup "lang"
          [ test html "lang on <html>" $ "<html lang=\"es\">hola" =?>
            setMeta "lang" (text "es") (doc (plain (text "hola")))
          , test html "xml:lang on <html>" $ "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"es\"><head></head><body>hola</body></html>" =?>
            setMeta "lang" (text "es") (doc (plain (text "hola")))
          ]
        ]

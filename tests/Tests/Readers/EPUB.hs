module Tests.Readers.EPUB (tests) where

import Text.Pandoc.Options
import Test.Framework
import Test.HUnit (assertBool)
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Readers.EPUB
import Text.Pandoc.MediaBag (MediaBag, mediaDirectory)
import Control.Applicative

getMediaBag :: FilePath -> IO MediaBag
getMediaBag fp = snd . readEPUB def <$> BL.readFile fp

testMediaBag :: FilePath -> [(String, String, Int)] -> IO ()
testMediaBag fp bag = do
  actBag <- (mediaDirectory <$> getMediaBag fp)
  assertBool (show "MediaBag did not match:\nExpected: "
             ++ show bag
             ++ "\nActual: "
             ++ show actBag)
             (actBag == bag)

featuresBag :: [(String, String, Int)]
featuresBag = [("img/ElementaryMathExample.png","image/png",1331),("img/Maghreb1.png","image/png",2520),("img/check.gif","image/gif",1340),("img/check.jpg","image/jpeg",2661),("img/check.png","image/png",2815),("img/cichons_diagram.png","image/png",7045),("img/complex_number.png","image/png",5238),("img/multiscripts_and_greek_alphabet.png","image/png",10060)]

tests :: [Test]
tests =
  [ testGroup "EPUB Mediabag"
    [ testCase "features bag"
      (testMediaBag "epub/features.epub" featuresBag)
    ]
  ]

module Tests.Readers.EPUB (tests) where

import Text.Pandoc.Options
import Test.Framework
import Test.HUnit (assertBool)
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Readers.EPUB
import Text.Pandoc.MediaBag (MediaBag, mediaDirectory)
import qualified Text.Pandoc.Class as P

getMediaBag :: FilePath -> IO MediaBag
getMediaBag fp = do
  bs <- BL.readFile fp
  snd <$> (P.runIOorExplode $ P.withMediaBag $ readEPUB def bs)

testMediaBag :: FilePath -> [(String, String, Int)] -> IO ()
testMediaBag fp bag = do
  actBag <- (mediaDirectory <$> getMediaBag fp)
  assertBool (show "MediaBag did not match:\nExpected: "
             ++ show bag
             ++ "\nActual: "
             ++ show actBag)
             (actBag == bag)

featuresBag :: [(String, String, Int)]
featuresBag = [("img/check.gif","image/gif",1340)
              ,("img/check.jpg","image/jpeg",2661)
              ,("img/check.png","image/png",2815)
              ,("img/multiscripts_and_greek_alphabet.png","image/png",10060)
              ]

tests :: [Test]
tests =
  [ testGroup "EPUB Mediabag"
    [ testCase "features bag"
      (testMediaBag "epub/img.epub" featuresBag)
    ]
  ]

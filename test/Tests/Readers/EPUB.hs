{- |
   Module      : Tests.Readers.EPUB
   Copyright   : © 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.eu>
   Stability   : alpha
   Portability : portable

Tests for the EPUB mediabag.
-}
module Tests.Readers.EPUB (tests) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Pandoc.Class as P
import Text.Pandoc.MediaBag (MediaBag, mediaDirectory)
import Text.Pandoc.Options
import Text.Pandoc.Readers.EPUB

getMediaBag :: FilePath -> IO MediaBag
getMediaBag fp = do
  bs <- BL.readFile fp
  P.runIOorExplode $ do
    readEPUB def bs
    P.getMediaBag

testMediaBag :: FilePath -> [(String, String, Int)] -> IO ()
testMediaBag fp bag = do
  actBag <- mediaDirectory <$> getMediaBag fp
  assertBool (show "MediaBag did not match:\nExpected: "
             ++ show bag
             ++ "\nActual: "
             ++ show actBag)
             (actBag == packBag bag)
  where
    packBag = map $ \(x, y, z) -> (x, T.pack y, z)

featuresBag :: [(String, String, Int)]
featuresBag = [("img/check.gif","image/gif",1340)
              ,("img/check.jpg","image/jpeg",2661)
              ,("img/check.png","image/png",2815)
              ,("img/multiscripts_and_greek_alphabet.png","image/png",10060)
              ]

-- with additional meta tag for cover in EPUB2 format
epub3CoverBag :: [(String, String, Int)]
epub3CoverBag = [("wasteland-cover.jpg","image/jpeg", 16586)]

epub3NoCoverBag :: [(String, String, Int)]
epub3NoCoverBag = [("img/check.gif","image/gif",1340)
                  ,("img/check.jpg","image/jpeg",2661)
                  ,("img/check.png","image/png",2815)
                  ]

-- content.opf uses the word `picture` to refer to the cover as much as validly possible
-- to check if references are resolved correctly
epub2PictureBag :: [(String, String, Int)]
epub2PictureBag = [("image/image.jpg","image/jpeg",9713)]

-- content.opf contains the word `cover` as much as possible, to check if possible multiple matches cause errors
epub2CoverBag :: [(String, String, Int)]
epub2CoverBag = [("image/cover.jpg","image/jpeg",9713)]

epub2NoCoverBag :: [(String, String, Int)]
epub2NoCoverBag = []

tests :: [TestTree]
tests =
  [ testGroup "EPUB Mediabag"
    [ testCase "features bag"
      (testMediaBag "epub/img.epub" featuresBag),
      testCase "EPUB3 cover bag"
      (testMediaBag "epub/wasteland.epub" epub3CoverBag),
      testCase "EPUB3 no cover bag"
      (testMediaBag "epub/img_no_cover.epub" epub3NoCoverBag),
      testCase "EPUB2 picture bag"
      (testMediaBag "epub/epub2_picture.epub" epub2PictureBag),
      testCase "EPUB2 cover bag"
      (testMediaBag "epub/epub2_cover.epub" epub2CoverBag),
      testCase "EPUB2 no cover bag"
      (testMediaBag "epub/epub2_no_cover.epub" epub2NoCoverBag)
    ]
  ]

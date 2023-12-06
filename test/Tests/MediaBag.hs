{-# LANGUAGE OverloadedStrings #-}
module Tests.MediaBag (tests) where

import Test.Tasty
import Test.Tasty.HUnit
-- import Tests.Helpers
import Text.Pandoc.Class.IO (extractMedia)
import Text.Pandoc.Class (fillMediaBag, runIOorExplode)
import System.IO.Temp (withTempDirectory)
import Text.Pandoc.Shared (inDirectory)
import System.FilePath
import Text.Pandoc.Builder as B
import System.Directory (doesFileExist, copyFile)

tests :: [TestTree]
tests = [
  testCase "test fillMediaBag & extractMedia" $
      withTempDirectory "." "extractMediaTest" $ \tmpdir -> inDirectory tmpdir $ do
        copyFile "../../test/lalune.jpg" "moon.jpg"
        let d = B.doc $
                  B.para (B.image "../../test/lalune.jpg" "" mempty) <>
                  B.para (B.image "moon.jpg" "" mempty) <>
                  B.para (B.image "data:image/png;base64,cHJpbnQgImhlbGxvIgo=;.lua+%2f%2e%2e%2f%2e%2e%2fa%2elua" "" mempty) <>
                  B.para (B.image "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" "" mempty)
        runIOorExplode $ do
          fillMediaBag d
          extractMedia "foo" d
        exists1 <- doesFileExist ("foo" </> "moon.jpg")
        assertBool "file in directory is not extracted with original name" exists1
        exists2 <- doesFileExist ("foo" </> "f9d88c3dbe18f6a7f5670e994a947d51216cdf0e.jpg")
        assertBool "file above directory is not extracted with hashed name" exists2
        exists3 <- doesFileExist ("foo" </> "2a0eaa89f43fada3e6c577beea4f2f8f53ab6a1d.lua")
        exists4 <- doesFileExist "a.lua"
        assertBool "data uri with malicious payload gets written outside of destination dir"
          (exists3 && not exists4)
        exists5 <- doesFileExist ("foo" </> "d5fceb6532643d0d84ffe09c40c481ecdf59e15a.gif")
        assertBool "data uri with gif is not properly decoded" exists5
        -- double-encoded version:
        let e = B.doc $
                  B.para (B.image "data:image/png;base64,cHJpbnQgInB3bmVkIgo=;.lua+%252f%252e%252e%252f%252e%252e%252fb%252elua" "" mempty)
        runIOorExplode $ do
          fillMediaBag e
          extractMedia "bar" e
        exists6 <- doesFileExist ("bar" </> "772ceca21a2751863ec46cb23db0e7fc35b9cff8.png")
        exists7 <- doesFileExist "b.lua"
        assertBool "data uri with double-encoded malicious payload gets written outside of destination dir"
          (exists6 && not exists7)
  ]

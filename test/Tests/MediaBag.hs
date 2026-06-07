{-# LANGUAGE OverloadedStrings #-}
module Tests.MediaBag (tests) where

import Test.Tasty
import Test.Tasty.HUnit
-- import Tests.Helpers
import Text.Pandoc.Class.IO (extractMedia)
import Text.Pandoc.Class (fillMediaBag, runIOorExplode)
import System.IO.Temp (withTempDirectory)
import System.FilePath
import Text.Pandoc.Builder as B
import qualified Data.Text as T
import System.Directory (doesFileExist, makeAbsolute)

tests :: [TestTree]
tests = [
  testCase "test fillMediaBag & extractMedia" $
      withTempDirectory "." "extractMediaTest" $ \tmpdir -> do
        -- Use absolute paths so the test does not need to change
        -- the process-wide current directory (which is not thread-safe
        -- and breaks other tests running in parallel).
        absTmpdir <- makeAbsolute tmpdir
        absLalune <- makeAbsolute "lalune.jpg"
        let d = B.doc $
                  -- simple relative path -> extracted with original name
                  B.para (B.image "lalune.jpg" "" mempty) <>
                  -- absolute path -> extracted with hashed name
                  B.para (B.image (T.pack absLalune) "" mempty) <>
                  B.para (B.image "data:image/png;base64,cHJpbnQgImhlbGxvIgo=;.lua+%2f%2e%2e%2f%2e%2e%2fa%2elua" "" mempty) <>
                  B.para (B.image "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7" "" mempty)
        let fooDir = absTmpdir </> "foo"
        runIOorExplode $ do
          fillMediaBag d
          extractMedia fooDir d
        exists1 <- doesFileExist (fooDir </> "lalune.jpg")
        assertBool "file in directory is not extracted with original name" exists1
        exists2 <- doesFileExist (fooDir </> "f9d88c3dbe18f6a7f5670e994a947d51216cdf0e.jpg")
        assertBool "file with absolute path is not extracted with hashed name" exists2
        exists3 <- doesFileExist (fooDir </> "2a0eaa89f43fada3e6c577beea4f2f8f53ab6a1d.png")
        exists4 <- doesFileExist (absTmpdir </> "a.lua")
        assertBool "data uri with malicious payload gets written outside of destination dir"
          (exists3 && not exists4)
        exists5 <- doesFileExist (fooDir </> "d5fceb6532643d0d84ffe09c40c481ecdf59e15a.gif")
        assertBool "data uri with gif is not properly decoded" exists5
        -- double-encoded version:
        let e = B.doc $
                  B.para (B.image "data:image/png;base64,cHJpbnQgInB3bmVkIgo=;.lua+%252f%252e%252e%252f%252e%252e%252fb%252elua" "" mempty)
        let barDir = absTmpdir </> "bar"
        runIOorExplode $ do
          fillMediaBag e
          extractMedia barDir e
        exists6 <- doesFileExist (barDir </> "772ceca21a2751863ec46cb23db0e7fc35b9cff8.png")
        exists7 <- doesFileExist (absTmpdir </> "b.lua")
        assertBool "data uri with double-encoded malicious payload gets written outside of destination dir"
          (exists6 && not exists7)
  ]

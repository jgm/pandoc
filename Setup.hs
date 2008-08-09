import Distribution.Simple
import Distribution.PackageDescription ( emptyHookedBuildInfo )
import Control.Exception ( bracket_ )
import System.Process ( runCommand, waitForProcess )
import System.FilePath ( (</>) )
import System.Directory ( getCurrentDirectory, setCurrentDirectory, findExecutable, doesFileExist )
import System.IO ( stderr )
import System.Exit
import Data.Maybe ( fromJust )

main = defaultMainWithHooks (simpleUserHooks {runTests = runTestSuite, preConf = checkReferenceODT})

-- | Run test suite.
runTestSuite _ _ _ _ = do
  inDirectory "tests" $ runCommand "runhaskell RunTests.hs" >>= waitForProcess
  return ()

-- | If reference.odt does not exist, build it.
checkReferenceODT _ _ = do
  refODTexists <- doesFileExist ("odt-styles" </> "reference.odt")
  if refODTexists
     then return ()
     else makeReferenceODT
  return emptyHookedBuildInfo

-- | Create reference.odt by zipping up sources in odt-styles directory.
makeReferenceODT :: IO ()
makeReferenceODT = do
  zipPathMaybe <- findExecutable "zip"
  if zipPathMaybe == Nothing
     then error $ "The 'zip' command, which is needed to build reference.odt\n" ++
                  "from sources in the odt-styles directory, was not found.\n" ++
                  "Try again after installing zip (http://www.info-zip.org/Zip.html).\n" ++
                  "Or use the pandoc source tarball, which contains a prebuilt reference.odt."
     else do
       putStrLn "Creating reference.odt:"
       inDirectory "odt-styles" $ do
         ec <- runCommand "zip -9 -r reference.odt *" >>= waitForProcess
         case ec of
              ExitSuccess -> putStrLn "Done."
              _           -> error "Error creating ODT."

-- | Perform an IO action in a directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory dir action = do
  oldDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory oldDir) action


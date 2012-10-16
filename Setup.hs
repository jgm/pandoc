{-# LANGUAGE ScopedTypeVariables #-}
import Distribution.Simple
import Distribution.Simple.Setup
         (copyDest, copyVerbosity, fromFlag, installVerbosity, BuildFlags(..))
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import Distribution.Simple.LocalBuildInfo
         (LocalBuildInfo(..), absoluteInstallDirs)
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.Utils (installOrdinaryFiles)
import Prelude hiding (catch)
import System.Process ( rawSystem )
import System.FilePath ( (</>) )
import System.Exit

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks {
      runTests  = runTestSuite
    , postBuild = makeManPages 
    , postCopy = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ copyVerbosity flags)
              (fromFlag $ copyDest flags)
    , postInst = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
    , copyHook = \pkgdescr ->
         (copyHook simpleUserHooks) pkgdescr{ executables =
            [x | x <- executables pkgdescr, exeName x /= "make-pandoc-man-pages"] }
    , instHook = \pkgdescr ->
         (instHook simpleUserHooks) pkgdescr{ executables =
            [x | x <- executables pkgdescr, exeName x /= "make-pandoc-man-pages"] }
    }
  exitWith ExitSuccess

-- | Run test suite.
runTestSuite :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO a
runTestSuite args _ pkg lbi = do
  let testDir = buildDir lbi </> "test-pandoc"
  testDir' <- canonicalizePath testDir
  let testArgs = "--timeout=5" : concatMap (\arg -> ["-t",arg]) args
  if any id [buildable (buildInfo exe) | exe <- executables pkg, exeName exe == "test-pandoc"]
     then inDirectory "tests" $ rawSystem (testDir' </> "test-pandoc") testArgs >>= exitWith
     else do
         putStrLn "Build pandoc with the 'tests' flag to run tests"
         exitWith $ ExitFailure 3

-- | Build man pages from markdown sources in man/
makeManPages :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
makeManPages _ flags _ _ = do
  let verbosity = fromFlag $ buildVerbosity flags
  let args = ["--verbose" | verbosity /= silent]
  rawSystem ("dist" </> "build" </> "make-pandoc-man-pages" </> "make-pandoc-man-pages")
      args >>= exitWith

manpages :: [FilePath]
manpages = ["man1" </> "pandoc.1"
           ,"man5" </> "pandoc_markdown.5"]

manDir :: FilePath
manDir = "man"

installManpages :: PackageDescription -> LocalBuildInfo
                -> Verbosity -> CopyDest -> IO ()
installManpages pkg lbi verbosity copy =
  installOrdinaryFiles verbosity (mandir (absoluteInstallDirs pkg lbi copy))
             (zip (repeat manDir) manpages)

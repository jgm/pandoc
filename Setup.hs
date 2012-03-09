import Distribution.Simple
import Distribution.Simple.Setup
         (copyDest, copyVerbosity, fromFlag, installVerbosity, BuildFlags(..))
import Distribution.PackageDescription
         (PackageDescription(..), Executable(..), BuildInfo(..))
import Distribution.Simple.LocalBuildInfo
         (LocalBuildInfo(..), absoluteInstallDirs)
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Simple.GHC (ghcPackageDbOptions)
import Distribution.Simple.InstallDirs (mandir, bindir, CopyDest (NoCopyDest))
import Distribution.Simple.Utils (installOrdinaryFiles)
import Control.Exception ( bracket_ )
import Control.Monad ( unless )
import System.Process ( rawSystem, runCommand, waitForProcess )
import System.FilePath ( (</>) )
import System.Directory
import System.Exit
import System.Time
import System.IO.Error ( isDoesNotExistError )
import Data.Maybe ( catMaybes )
import Data.List ( (\\) )

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
makeManPages _ flags _ lbi = do
  ds1 <- modifiedDependencies (manDir </> "man1" </> "pandoc.1")
    ["README", manDir </> "man1" </> "pandoc.1.template"]
  ds2 <- modifiedDependencies (manDir </> "man5" </> "pandoc_markdown.5")
    ["README", manDir </> "man5" </> "pandoc_markdown.5.template"]

  let distPref  = fromFlag (buildDistPref flags)
      packageDB =
          withPackageDB lbi
           ++ [SpecificPackageDB $ distPref </> "package.conf.inplace"]
      
      verbosity = fromFlag $ buildVerbosity flags

      args = makeGhcArgs (ghcPackageDbOptions packageDB)
             ++ ["MakeManPage.hs"]
      args' = if verbosity == silent
                then args
                else args ++ ["--verbose"]
  -- Don't run MakeManPage.hs unless we have to
  unless (null ds1 && null ds2) $ do
    rawSystem "runghc" args' >>= exitWith

-- format arguments to runghc that we wish to pass to ghc
-- normally runghc gets it right, unless the argument does
-- not begin with a '-' charecter, so we need to give clear
-- directions.
makeGhcArgs :: [String] -> [String]
makeGhcArgs = map ("--ghc-arg="++)

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

-- | Returns a list of 'dependencies' that have been modified after 'file'.
modifiedDependencies :: FilePath -> [FilePath] -> IO [FilePath]
modifiedDependencies file dependencies = do
  fileModTime <- catch (getModificationTime file) $
                 \e -> if isDoesNotExistError e
                          then return (TOD 0 0)   -- the minimum ClockTime
                          else ioError e
  depModTimes <- mapM getModificationTime dependencies
  let modified = zipWith (\dep time -> if time > fileModTime then Just dep else Nothing) dependencies depModTimes
  return $ catMaybes modified

-- | Perform an IO action in a directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory dir action = do
  oldDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir) (setCurrentDirectory oldDir) action


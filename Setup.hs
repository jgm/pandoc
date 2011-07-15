import Distribution.Simple
import Distribution.Simple.Setup
         (copyDest, copyVerbosity, fromFlag, installVerbosity, BuildFlags(..))
import Distribution.PackageDescription
         (PackageDescription(..), Executable(..), BuildInfo(..))
import Distribution.Simple.LocalBuildInfo
         (LocalBuildInfo(..), absoluteInstallDirs)
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Simple.InstallDirs (mandir, bindir, CopyDest (NoCopyDest))
import Distribution.Simple.Utils (copyFiles)
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
    , postCopy = \ _ flags pkg lbi -> do
         installManpages pkg lbi (fromFlag $ copyVerbosity flags)
              (fromFlag $ copyDest flags)
         installScripts pkg lbi (fromFlag $ copyVerbosity flags)
              (fromFlag $ copyDest flags)
    , postInst = \ _ flags pkg lbi -> do
         installManpages pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
         installScripts pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
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
  ds1 <- modifiedDependencies (manDir </> "man1" </> "pandoc.1")
    ["README", manDir </> "man1" </> "pandoc.1.template"]
  ds2 <- modifiedDependencies (manDir </> "man1" </> "markdown2pdf.1")
    [manDir </> "man1" </> "markdown2pdf.1.md"]
  ds3 <- modifiedDependencies (manDir </> "man5" </> "pandoc_markdown.5")
    ["README", manDir </> "man5" </> "pandoc_markdown.5.template"]
  let cmd  = "runghc -package-conf=dist/package.conf.inplace MakeManPage.hs"
  let cmd' = if verbosity == silent
                then cmd
                else cmd ++ " --verbose"
  -- Don't run MakeManPage.hs unless we have to
  unless (null ds1 && null ds2 && null ds3) $
    runCommand cmd' >>= waitForProcess >>= exitWith

manpages :: [FilePath]
manpages = ["man1" </> "pandoc.1"
           ,"man1" </> "markdown2pdf.1"
           ,"man5" </> "pandoc_markdown.5"]

manDir :: FilePath
manDir = "man"

installScripts :: PackageDescription -> LocalBuildInfo
               -> Verbosity -> CopyDest -> IO ()
installScripts pkg lbi verbosity copy =
  copyFiles verbosity (bindir (absoluteInstallDirs pkg lbi copy))
      (zip (repeat ".") (wrappers \\ exes))
    where exes = map exeName $ filter isBuildable $ executables pkg
          isBuildable = buildable . buildInfo
          wrappers = ["markdown2pdf"]

installManpages :: PackageDescription -> LocalBuildInfo
                -> Verbosity -> CopyDest -> IO ()
installManpages pkg lbi verbosity copy =
  copyFiles verbosity (mandir (absoluteInstallDirs pkg lbi copy))
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


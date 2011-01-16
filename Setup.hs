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
import System.Process ( rawSystem, runCommand, runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.IO ( stderr )
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
  let testArgs = concatMap (\arg -> ["-t",arg]) args
  if any id [buildable (buildInfo exe) | exe <- executables pkg, exeName exe == "test-pandoc"]
     then inDirectory "tests" $ rawSystem (testDir' </> "test-pandoc") testArgs >>= exitWith
     else do
         putStrLn "Build pandoc with the 'tests' flag to run tests"
         exitWith $ ExitFailure 3

-- | Build man pages from markdown sources in man/man1/.
makeManPages :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
makeManPages _ flags _ bi = do
  let pandocPath = (buildDir bi) </> "pandoc" </> "pandoc"
  let verbosity = fromFlag $ buildVerbosity flags
  -- make markdown2pdf.1 from markdown2pdf.1.md
  makeManPage pandocPath verbosity "markdown2pdf.1"
  -- make pandoc.1 from README
  let pandocpage = manDir </> "pandoc.1"
  modifiedDeps <- modifiedDependencies pandocpage ["README"]
  unless (null modifiedDeps) $ do
    let cmd  = "runghc -package-conf=dist/package.conf.inplace MakeManPage.hs"
    ec <- runCommand cmd >>= waitForProcess
    case ec of
         ExitSuccess   -> unless (verbosity == silent) $
                            putStrLn $ "Created " ++ pandocpage
         ExitFailure n -> putStrLn ("Error creating " ++ pandocpage ++
                               ". Exit code = " ++ show n) >> exitWith ec

manpages :: [FilePath]
manpages = ["pandoc.1", "markdown2pdf.1"]

manDir :: FilePath
manDir = "man" </> "man1"

-- | Build a man page from markdown source in man/man1.
makeManPage :: FilePath -> Verbosity -> FilePath -> IO ()
makeManPage pandoc verbosity manpage = do
  let page = manDir </> manpage
  let source = page <.> "md"
  modifiedDeps <- modifiedDependencies page [source]
  unless (null modifiedDeps) $ do
    ec <- runProcess pandoc ["-s", "-S", "-r", "markdown", "-w", "man",
                "--template=templates/man.template", "-o", page, source]
                Nothing Nothing Nothing Nothing (Just stderr) >>= waitForProcess
    case ec of
         ExitSuccess   -> unless (verbosity == silent) $
                             putStrLn $ "Created " ++ page
         ExitFailure n -> putStrLn ("Error creating " ++ page ++
                             ". Exit code = " ++ show n) >> exitWith ec

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
  copyFiles verbosity (mandir (absoluteInstallDirs pkg lbi copy) </> "man1")
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


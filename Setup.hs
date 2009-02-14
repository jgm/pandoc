import Distribution.Simple
import Distribution.Package ( pkgName )
import Distribution.Simple.LocalBuildInfo ( packageDeps )
import Distribution.PackageDescription ( emptyHookedBuildInfo )
import Control.Exception ( bracket_ )
import Control.Monad ( unless )
import System.Process ( runCommand, runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.IO ( stderr )
import System.Exit
import System.Time
import System.IO.Error ( isDoesNotExistError )
import Data.Maybe ( fromJust, isNothing, catMaybes )

main = do
  defaultMainWithHooks $ simpleUserHooks { runTests  = runTestSuite
                                         , postBuild = makeManPages }
  exitWith ExitSuccess

-- | Run test suite.
runTestSuite _ _ _ local = do
  let highlightingSupport = (PackageName "highlighting-kate") `elem` (map pkgName $ packageDeps local)
  let testArgs = if highlightingSupport then ["lhs"] else []
  let testCmd  = "runhaskell -i.. RunTests.hs " ++ unwords testArgs
  inDirectory "tests" $ runCommand testCmd >>= waitForProcess >>= exitWith

-- | Build man pages from markdown sources in man/man1/.
makeManPages _ _ _ _ = do
  mapM_ makeManPage ["pandoc.1", "hsmarkdown.1", "html2markdown.1", "markdown2pdf.1"]

-- | Build a man page from markdown source in man/man1.
makeManPage manpage = do
  let manDir = "man" </> "man1"
  let pandoc = "dist" </> "build" </> "pandoc" </> "pandoc"
  let page = manDir </> manpage
  let source = manDir </> manpage <.> "md"
  modifiedDeps <- modifiedDependencies page [source]
  unless (null modifiedDeps) $ do
    ec <- runProcess pandoc ["-s", "-S", "-r", "markdown", "-w", "man", "-o", page, source]
                Nothing Nothing Nothing Nothing (Just stderr) >>= waitForProcess
    case ec of
         ExitSuccess -> putStrLn $ "Created " ++ manDir </> manpage
         _           -> do putStrLn $ "Error creating " ++ manDir </> manpage
                           exitWith ec

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


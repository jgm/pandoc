import Distribution.Simple
import Distribution.PackageDescription ( emptyHookedBuildInfo )
import Control.Exception ( bracket_ )
import System.Process ( runCommand, runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.IO ( stderr )
import System.Exit
import System.Time
import System.IO.Error ( isDoesNotExistError )
import Data.Maybe ( fromJust, isNothing, catMaybes )

main = defaultMainWithHooks $
       simpleUserHooks { runTests  = runTestSuite
                       , preConf   = checkReferenceODT
                       , postBuild = makeManPages }

-- | Run test suite.
runTestSuite _ _ _ _ = do
  inDirectory "tests" $ runCommand "runhaskell RunTests.hs" >>= waitForProcess
  return ()

-- | If reference.odt needs rebuilding, build it.
checkReferenceODT _ _ = inDirectory "odt-styles" $ do
  let refodt = "reference.odt"
  let deps   = [ "meta.xml", "content.xml", "settings.xml", "META-INF/manifest.xml",
                 "Thumbnails/thumbnail.png", "styles.xml", "mimetype" ]
  modifiedDeps <- modifiedDependencies refodt deps
  if null modifiedDeps
     then return ()
     else makeReferenceODT modifiedDeps
  return emptyHookedBuildInfo

-- | Create reference.odt by zipping up sources in odt-styles directory.
makeReferenceODT :: [FilePath] -> IO ()
makeReferenceODT sources = do
  zipPathMaybe <- findExecutable "zip"
  if isNothing zipPathMaybe
     then error $ "The 'zip' command, which is needed to build reference.odt\n" ++
                  "from sources in the odt-styles directory, was not found.\n" ++
                  "Try again after installing zip (http://www.info-zip.org/Zip.html).\n" ++
                  "Or use the pandoc source tarball, which contains a prebuilt reference.odt."
     else do
       putStrLn "Creating reference.odt:"
       ec <- runProcess (fromJust zipPathMaybe) (["-9", "-r", "reference.odt"] ++ sources)
                 Nothing Nothing Nothing Nothing (Just stderr) >>= waitForProcess
       case ec of
            ExitSuccess -> return ()
            _           -> error "Error creating ODT."

-- | Build man pages from markdown sources in man/man1/.
makeManPages _ _ _ _ = do
  mapM makeManPage ["pandoc.1", "hsmarkdown.1", "markdown2pdf.1", "html2markdown.1"]
  return ()

-- | Build a man page from markdown source in man/man1.
makeManPage manpage = do
  let manDir = "man" </> "man1"
  let pandoc = "dist" </> "build" </> "pandoc" </> "pandoc"
  let page = manDir </> manpage
  let source = manDir </> manpage <.> "md"
  modifiedDeps <- modifiedDependencies page [source]
  if null modifiedDeps
     then return ()
     else do
       ec <- runProcess pandoc ["-s", "-r", "markdown", "-w", "man", "-o", page, source]
                   Nothing Nothing Nothing Nothing (Just stderr) >>= waitForProcess
       case ec of
            ExitSuccess -> putStrLn $ "Created " ++ manDir </> manpage
            _           -> error $ "Error creating " ++ manDir </> manpage

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


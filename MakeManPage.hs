-- Create pandoc.1 man page from README
import Text.Pandoc
import Data.ByteString.UTF8 (toString, fromString)
import Data.Char (toUpper)
import qualified Data.ByteString as B
import Control.Monad
import System.FilePath
import System.Environment (getArgs)
import Text.Pandoc.Shared (normalize)
import System.Directory (getModificationTime)
import System.IO.Error (isDoesNotExistError)
import System.Time (ClockTime(..))
import Data.Maybe (catMaybes)

main = do
  rmContents <- liftM toString $ B.readFile "README"
  let (Pandoc meta blocks) = readMarkdown defaultParserState rmContents
  let manBlocks = removeSect [Str "Wrappers"]
                $ removeSect [Str "Pandoc's",Space,Str "markdown"] blocks
  let syntaxBlocks = extractSect [Str "Pandoc's",Space,Str "markdown"] blocks
  args <- getArgs
  let verbose = "--verbose" `elem` args
  makeManPage verbose ("man" </> "man1" </> "pandoc.1")
      meta manBlocks
  makeManPage verbose ("man" </> "man5" </> "pandoc_markdown.5")
      meta syntaxBlocks
  let markdown2pdfpage = "man" </> "man1" </> "markdown2pdf.1"
  modDeps <- modifiedDependencies markdown2pdfpage [markdown2pdfpage <.> "md"]
  unless (null modDeps) $ do
    mpdfContents <- liftM toString $ B.readFile $ markdown2pdfpage <.> "md"
    templ <- liftM toString $ B.readFile $ "templates" </> "default.man"
    let doc = readMarkdown defaultParserState{ stateStandalone = True }
                                             mpdfContents
    writeManPage markdown2pdfpage templ doc
    when verbose $
      putStrLn $ "Created " ++ markdown2pdfpage

makeManPage :: Bool -> FilePath -> Meta -> [Block] -> IO ()
makeManPage verbose page meta blocks = do
  let templ = page <.> "template"
  modDeps <- modifiedDependencies page ["README", templ]
  unless (null modDeps) $ do
    manTemplate <- liftM toString $ B.readFile templ
    writeManPage page manTemplate (Pandoc meta blocks)
    when verbose $
      putStrLn $ "Created " ++ page

writeManPage :: FilePath -> String -> Pandoc -> IO ()
writeManPage page templ doc = do
  let opts = defaultWriterOptions{ writerStandalone = True
                                 , writerTemplate = templ }
  let manPage = writeMan opts $
                    bottomUp (concatMap removeLinks) $
                    bottomUp  capitalizeHeaders doc
  B.writeFile page $ fromString manPage

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

removeLinks :: Inline -> [Inline]
removeLinks (Link l _) = l
removeLinks x = [x]

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 xs) = Header 1 $ bottomUp capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x

removeSect :: [Inline] -> [Block] -> [Block]
removeSect ils (Header 1 x:xs) | normalize x == normalize ils =
  dropWhile (not . isHeader1) xs
removeSect ils (x:xs) = x : removeSect ils xs
removeSect _ [] = []

extractSect :: [Inline] -> [Block] -> [Block]
extractSect ils (Header 1 z:xs) | normalize z == normalize ils =
  bottomUp promoteHeader $ takeWhile (not . isHeader1) xs
    where promoteHeader (Header n x) = Header (n-1) x
          promoteHeader x            = x
extractSect ils (x:xs) = extractSect ils xs
extractSect _ [] = []

isHeader1 :: Block -> Bool
isHeader1 (Header 1 _) = True
isHeader1 _            = False


{-# LANGUAGE CPP #-}
-- Create pandoc.1 man and pandoc_markdown.5 man pages from README
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Char (toUpper)
import Control.Monad
import System.FilePath
import System.Environment (getArgs)
import Text.Pandoc.Shared (normalize)
import Data.Maybe ( catMaybes )
import Prelude hiding (catch)
import Control.Exception ( catch )
import System.IO.Error ( isDoesNotExistError )
#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime(..))
#else
import System.Time (ClockTime(..))
#endif
import System.Directory

main :: IO ()
main = do
  ds1 <- modifiedDependencies ("man" </> "man1" </> "pandoc.1")
    ["README", "man" </> "man1" </> "pandoc.1.template"]
  ds2 <- modifiedDependencies ("man" </> "man5" </> "pandoc_markdown.5")
    ["README", "man" </> "man5" </> "pandoc_markdown.5.template"]

  unless (null ds1 && null ds2) $ do
    rmContents <- UTF8.readFile "README"
    let (Pandoc meta blocks) = normalize $ readMarkdown def rmContents
    let manBlocks = removeSect [Str "Wrappers"]
                  $ removeSect [Str "Pandoc's",Space,Str "markdown"] blocks
    let syntaxBlocks = extractSect [Str "Pandoc's",Space,Str "markdown"] blocks
    args <- getArgs
    let verbose = "--verbose" `elem` args
    unless (null ds1) $
      makeManPage verbose ("man" </> "man1" </> "pandoc.1") meta manBlocks
    unless (null ds2) $
      makeManPage verbose ("man" </> "man5" </> "pandoc_markdown.5") meta syntaxBlocks

makeManPage :: Bool -> FilePath -> Meta -> [Block] -> IO ()
makeManPage verbose page meta blocks = do
  let templ = page <.> "template"
  manTemplate <- UTF8.readFile templ
  writeManPage page manTemplate (Pandoc meta blocks)
  when verbose $ putStrLn $ "Created " ++ page

writeManPage :: FilePath -> String -> Pandoc -> IO ()
writeManPage page templ doc = do
  let version = pandocVersion
  let opts = def{ writerStandalone = True
                , writerTemplate = templ
                , writerVariables = [("version",version)] }
  let manPage = writeMan opts $
                    bottomUp (concatMap removeLinks) $
                    bottomUp  capitalizeHeaders doc
  UTF8.writeFile page manPage

removeLinks :: Inline -> [Inline]
removeLinks (Link l _) = l
removeLinks x = [x]

capitalizeHeaders :: Block -> Block
capitalizeHeaders (Header 1 attr xs) = Header 1 attr $ bottomUp capitalize xs
capitalizeHeaders x = x

capitalize :: Inline -> Inline
capitalize (Str xs) = Str $ map toUpper xs
capitalize x = x

removeSect :: [Inline] -> [Block] -> [Block]
removeSect ils (Header 1 _ x:xs) | x == ils =
  dropWhile (not . isHeader1) xs
removeSect ils (x:xs) = x : removeSect ils xs
removeSect _ [] = []

extractSect :: [Inline] -> [Block] -> [Block]
extractSect ils (Header 1 _ z:xs) | z == ils =
  bottomUp promoteHeader $ takeWhile (not . isHeader1) xs
    where promoteHeader (Header n attr x) = Header (n-1) attr x
          promoteHeader x            = x
extractSect ils (x:xs) = extractSect ils xs
extractSect _ [] = []

isHeader1 :: Block -> Bool
isHeader1 (Header 1 _ _ ) = True
isHeader1 _               = False


-- | Returns a list of 'dependencies' that have been modified after 'file'.
modifiedDependencies :: FilePath -> [FilePath] -> IO [FilePath]
modifiedDependencies file dependencies = do
  fileModTime <- catch (getModificationTime file) $
                 \e -> if isDoesNotExistError e
#if MIN_VERSION_directory(1,2,0)
                          then return (UTCTime (toEnum 0) 0)   -- the minimum ClockTime
#else
                          then return (TOD 0 0)   -- the minimum ClockTime
#endif
                          else ioError e
  depModTimes <- mapM getModificationTime dependencies
  let modified = zipWith (\dep time -> if time > fileModTime then Just dep else Nothing) dependencies depModTimes
  return $ catMaybes modified


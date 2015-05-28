import System.Environment
import System.Directory
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as BS
import qualified Control.Exception as E
import System.IO.Error (isDoesNotExistError)
import System.FilePath

mkzip :: String -> IO ()
mkzip fmt = do
  let dir    = "data" </> fmt
      output = "data" </> "reference" <.> fmt
  cd <- getCurrentDirectory
  setCurrentDirectory dir
  archive <- addFilesToArchive [OptRecursive] emptyArchive ["."]
  setCurrentDirectory cd
  removeIfExists output
  BS.writeFile output $ fromArchive archive

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `E.catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = E.throwIO e

main :: IO ()
main = getArgs >>= mkzip . (!!0)

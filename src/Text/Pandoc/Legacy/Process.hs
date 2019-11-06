module Text.Pandoc.Legacy.Process (pipeProcess)
where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Exit (ExitCode)
import qualified Text.Pandoc.Process as TP

pipeProcess
    :: Maybe [(String, String)]
    -> FilePath
    -> [String]
    -> BL.ByteString
    -> IO (ExitCode,BL.ByteString)
pipeProcess mbenv cmd = TP.pipeProcess (fmap go <$> mbenv) cmd . fmap T.pack
  where
    go (x, y) = (T.pack x, T.pack y)

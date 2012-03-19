{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.PDF
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( tex2pdf ) where

import System.IO.Temp
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Exit (ExitCode (..))
import System.FilePath
import System.Directory
import System.Process
import Control.Exception (evaluate)
import System.IO (hClose)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Text.Pandoc.UTF8 as UTF8
import Control.Monad (unless)
import Data.List (isInfixOf)

tex2pdf :: String       -- ^ tex program (pdflatex, lualatex, xelatex)
        -> String       -- ^ latex source
        -> IO (Either ByteString ByteString)
tex2pdf program source = withSystemTempDirectory "tex2pdf" $ \tmpdir ->
  tex2pdf' tmpdir program source

tex2pdf' :: FilePath                        -- ^ temp directory for output
         -> String                          -- ^ tex program
         -> String                          -- ^ tex source
         -> IO (Either ByteString ByteString)
tex2pdf' tmpDir program source = do
  let numruns = if "\\tableofcontents" `isInfixOf` source
                   then 3  -- to get page numbers
                   else 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- runTeXProgram program numruns tmpDir source
  let msg = "Error producing PDF from TeX source."
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> return $ Left $
                                     msg <> "\n" <> extractMsg log'
       (ExitSuccess, Nothing)  -> return $ Left msg
       (ExitSuccess, Just pdf) -> return $ Right pdf

(<>) :: ByteString -> ByteString -> ByteString
(<>) = B.append

-- parsing output

extractMsg :: ByteString -> ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)

-- running tex programs

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
runTeXProgram :: String -> Int -> FilePath -> String
              -> IO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram program runsLeft tmpDir source = do
    let file = tmpDir </> "input.tex"
    exists <- doesFileExist file
    unless exists $ UTF8.writeFile file source
    let programArgs = ["-halt-on-error", "-interaction", "nonstopmode",
         "-output-directory", tmpDir, file]
    (exit, out, err) <- readCommand program programArgs
    if runsLeft > 1
       then runTeXProgram program (runsLeft - 1) tmpDir source
       else do
         let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
         pdfExists <- doesFileExist pdfFile
         pdf <- if pdfExists
                   then Just `fmap` B.readFile pdfFile
                   else return Nothing
         return (exit, out <> err, pdf)

-- utility functions

-- Run a command and return exitcode, contents of stdout, and
-- contents of stderr. (Based on
-- 'readProcessWithExitCode' from 'System.Process'.)
readCommand :: FilePath                            -- ^ command to run
            -> [String]                            -- ^ any arguments
            -> IO (ExitCode,ByteString,ByteString) -- ^ exit, stdout, stderr
readCommand cmd args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar
    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh
    _ <- forkIO $ evaluate (B.length out) >> putMVar outMVar ()
    -- fork off a thread to start consuming stderr
    err  <- B.hGetContents errh
    _ <- forkIO $ evaluate (B.length err) >> putMVar outMVar ()
    -- now write and flush any input
    hClose inh -- done with stdin
    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    -- wait on the process
    ex <- waitForProcess pid
    return (ex, out, err)

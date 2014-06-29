{-
Copyright (C) 2013-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Process
   Copyright   : Copyright (C) 2013-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

ByteString variant of 'readProcessWithExitCode'.
-}
module Text.Pandoc.Process (pipeProcess)
where
import System.Process
import System.Exit (ExitCode (..))
import Control.Exception
import System.IO (hClose, hFlush)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL

{- |
Version of 'System.Process.readProcessWithExitCode' that uses lazy bytestrings
instead of strings and allows setting environment variables.

@readProcessWithExitCode@ creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.

If an asynchronous exception is thrown to the thread executing
@readProcessWithExitCode@, the forked process will be terminated and
@readProcessWithExitCode@ will wait (block) until the process has been
terminated.
-}

pipeProcess
    :: Maybe [(String, String)] -- ^ environment variables
    -> FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> BL.ByteString            -- ^ standard input
    -> IO (ExitCode,BL.ByteString,BL.ByteString) -- ^ exitcode, stdout, stderr
pipeProcess mbenv cmd args input =
    mask $ \restore -> do
      (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args)
                                                   { env     = mbenv,
                                                     std_in  = CreatePipe,
                                                     std_out = CreatePipe,
                                                     std_err = CreatePipe }
      flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- BL.hGetContents outh
        waitOut <- forkWait $ evaluate $ BL.length out

        -- fork off a thread to start consuming stderr
        err <- BL.hGetContents errh
        waitErr <- forkWait $ evaluate $ BL.length err

        -- now write and flush any input
        let writeInput = do
              unless (BL.null input) $ do
                BL.hPutStr inh input
                hFlush inh
              hClose inh

        writeInput

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

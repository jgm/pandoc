{-
Copyright (C) 2013-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2013-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

ByteString variant of 'readProcessWithExitCode'.
-}
module Text.Pandoc.Process (pipeProcess)
where
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import System.Exit (ExitCode (..))
import System.IO (hClose, hFlush)
import System.Process

{- |
Version of 'System.Process.readProcessWithExitCode' that uses lazy bytestrings
instead of strings and allows setting environment variables.

@readProcessWithExitCode@ creates an external process, reads its
standard output strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process
and the standard output.  stderr is inherited from the parent.

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
    -> IO (ExitCode,BL.ByteString) -- ^ exitcode, stdout
pipeProcess mbenv cmd args input =
    mask $ \restore -> do
      (Just inh, Just outh, Nothing, pid) <- createProcess (proc cmd args)
                                                   { env     = mbenv,
                                                     std_in  = CreatePipe,
                                                     std_out = CreatePipe,
                                                     std_err = Inherit }
      flip onException
        (do hClose inh; hClose outh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- BL.hGetContents outh
        waitOut <- forkWait $ evaluate $ BL.length out

        -- now write and flush any input
        let writeInput = do
              unless (BL.null input) $ do
                BL.hPutStr inh input
                hFlush inh
              hClose inh

        writeInput

        -- wait on the output
        waitOut

        hClose outh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)

{- |
   Module      : Text.Pandoc.Process
   Copyright   : Copyright (C) 2013-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

ByteString variant of 'readProcessWithExitCode'.
-}
module Text.Pandoc.Process (pipeProcess)
where
import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar,
                           takeMVar)
import Control.Exception (SomeException (..))
import qualified Control.Exception as E
import Control.Monad (unless)
import Control.DeepSeq (rnf)
import qualified Data.ByteString.Lazy as BL
import Foreign.C (Errno (Errno), ePIPE)
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.Exit (ExitCode (..))
import System.IO (hClose)
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

This function was adapted from @readProcessWithExitCode@ of module
System.Process, package process-1.6.3.0. The original code is BSD
licensed and © University of Glasgow 2004-2008.
-}
pipeProcess
    :: Maybe [(String, String)] -- ^ environment variables
    -> FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> BL.ByteString            -- ^ standard input
    -> IO (ExitCode,BL.ByteString) -- ^ exitcode, stdout
pipeProcess mbenv cmd args input = do
    let cp_opts = (proc cmd args)
                  { env     = mbenv
                  , std_in  = CreatePipe
                  , std_out = CreatePipe
                  , std_err = Inherit
                  }
    withCreateProcess cp_opts $
      \mbInh mbOuth _ pid -> do
        let (inh, outh) =
             case (mbInh, mbOuth) of
                  (Just i, Just o) -> (i, o)
                  (Nothing, _)     -> error "withCreateProcess no inh"
                  (_, Nothing)     -> error "withCreateProcess no outh"

        out <- BL.hGetContents outh

        -- fork off threads to start consuming stdout & stderr
        withForkWait (E.evaluate $ rnf out) $ \waitOut -> do

          -- now write any input
          unless (BL.null input) $
            ignoreSigPipe $ BL.hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut

          hClose outh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out)

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
-- This function was copied verbatim from module System.Process of package
-- process-1.6.3.0.
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  E.mask $ \restore -> do
    tid <- forkIO $ E.try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either E.throwIO return
    restore (body wait) `E.onException` killThread tid

-- This function was copied verbatim from module System.Process of package
-- process-1.6.3.0.
ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = E.handle $ \e ->
  case e of
    IOError { ioe_type  = ResourceVanished
            , ioe_errno = Just ioe }
      | Errno ioe == ePIPE -> return ()
    _ -> E.throwIO e

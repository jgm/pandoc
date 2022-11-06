{- |
   Module      : PandocCLI.Server
   Copyright   : © 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley@edu>

Placeholder module to be used when pandoc is compiled without server
support.
-}
module PandocCLI.Server
  ( runCGI
  , runServer
  )
where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

-- | Placeholder function for the CGI server; prints an error message
-- and exists with error code.
runCGI :: IO ()
runCGI = serverUnsupported

-- | Placeholder function for the HTTP server; prints an error message
-- and exists with error code.
runServer :: [String] -> IO ()
runServer _args = serverUnsupported

serverUnsupported :: IO ()
serverUnsupported = do
  hPutStrLn stderr $ "Server mode unsupported.\n" <>
                     "Pandoc was not compiled with the 'server' flag."
  exitWith $ ExitFailure 4

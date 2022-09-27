{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Main
   Copyright   : © 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley@edu>

Functions for the pandoc server CLI.
-}
module PandocCLI.Server
  ( runCGI
  , runServer
  )
where
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Timeout (timeout)
import Safe (readDef)
import System.Environment (lookupEnv)
import Text.Pandoc.Server (ServerOpts(..), parseServerOptsFromArgs, app)

-- | Runs the CGI server.
runCGI :: IO ()
runCGI = do
  cgiTimeout <- maybe 2 (readDef 2) <$> lookupEnv "PANDOC_SERVER_TIMEOUT"
  CGI.run (timeout cgiTimeout app)

-- | Runs the HTTP server.
runServer :: [String] -> IO ()
runServer args = do
  sopts <- parseServerOptsFromArgs args
  Warp.run (serverPort sopts) (timeout (serverTimeout sopts) app)

{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import qualified Control.Exception as E
import Text.Pandoc.App (convertWithOpts, defaultOpts, options, parseOptions)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runScript)
import Text.Pandoc.Server (ServerOpts(..), parseServerOpts, app)
import Safe (readDef)
import System.Environment (getProgName, lookupEnv)
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Timeout (timeout)

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  cgiTimeout <- maybe 2 (readDef 2) <$> lookupEnv "PANDOC_SERVER_TIMEOUT"
  case prg of
    "pandoc-server.cgi" -> CGI.run (timeout cgiTimeout app)
    "pandoc-server" -> do
      sopts <- parseServerOpts
      Warp.run (serverPort sopts) (timeout (serverTimeout sopts) app)
    "pandoc-lua" -> runScript
    _ -> parseOptions options defaultOpts >>= convertWithOpts

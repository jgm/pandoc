module Main where

import PandocServer (app)
import Text.Pandoc (pandocVersion)
import Control.Monad (when)
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Timeout (timeout)
import System.Environment (getProgName)
import Options.Applicative
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.Text as T

data Opts = Opts
  { optPort :: Warp.Port,
    optTimeout :: Int, -- seconds
    optVersion :: Bool }

options :: Parser Opts
options = Opts
  <$> option auto
         ( long "port"
         <> value 3030
         <> metavar "PORT"
         <> help "Port to serve on" )
  <*> option auto
         ( long "timeout"
         <> value 2
         <> metavar "SECONDS"
         <> help "Seconds timeout" )
  <*> flag False True
         ( long "version"
         <> help "Print version" )

main :: IO ()
main = do
  progname <- getProgName
  let optspec = info (options <**> helper)
       ( fullDesc
       <> progDesc "Run a pandoc server"
       <> header "pandoc-server - text conversion server" )
  opts <- execParser optspec

  when (optVersion opts) $ do
    putStrLn $ progname <> " " <> T.unpack pandocVersion
    exitWith ExitSuccess

  let port = optPort opts
  let app' = timeout (optTimeout opts) app
  if progname == "pandoc-server.cgi"
     then -- operate as a CGI script
       CGI.run app'
     else -- operate as a persistent server
       Warp.run port app'

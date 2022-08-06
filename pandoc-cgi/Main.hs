module Main where

import PandocCGI (app)
import Network.Wai.Handler.CGI (run)
import Network.Wai.Middleware.Timeout (timeout)

main :: IO ()
main = run $ timeout 2 app

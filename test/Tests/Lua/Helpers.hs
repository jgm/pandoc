{- |
   Module      : Tests.Lua.Helpers
   Copyright   : © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test helpers for Lua.
-}
module Tests.Lua.Helpers ( runLuaTest ) where

import Foreign.Lua (Lua)
import Text.Pandoc.Class (runIOorExplode, setUserDataDir)
import Text.Pandoc.Lua (runLua)

runLuaTest :: Lua a -> IO a
runLuaTest op = runIOorExplode $ do
  setUserDataDir (Just "../data")
  res <- runLua op
  case res of
    Left e -> error (show e)
    Right x -> return x

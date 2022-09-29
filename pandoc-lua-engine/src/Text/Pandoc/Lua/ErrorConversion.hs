{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.ErrorConversion
   Copyright   : © 2020-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Define how Lua errors are converted into @'PandocError'@ Haskell
exceptions, and /vice versa/.
-}
module Text.Pandoc.Lua.ErrorConversion
  ( addContextToException
  ) where

import HsLua (LuaError, LuaE, resultToEither, runPeek, top)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Marshal.PandocError (pushPandocError, peekPandocError)

import qualified Data.Text as T
import qualified HsLua as Lua

addContextToException :: ()
addContextToException = undefined

-- | Retrieve a @'PandocError'@ from the Lua stack.
popPandocError :: LuaE PandocError PandocError
popPandocError = do
  errResult <- runPeek $ peekPandocError top
  case resultToEither errResult of
    Right x -> return x
    Left err -> return $ PandocLuaError (T.pack err)

-- Ensure conversions between Lua errors and 'PandocError' exceptions
-- are possible.
instance LuaError PandocError where
  popException = popPandocError
  pushException = pushPandocError
  luaException = PandocLuaError . T.pack

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.PandocError
   Copyright   : © 2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling of @'PandocError'@ values.
-}
module Text.Pandoc.Lua.Marshaling.PandocError
  ( peekPandocError
  , pushPandocError
  )
  where

import Foreign.Lua (Lua, Peekable, Pushable, StackIndex)
import Text.Pandoc.Error (PandocError (PandocLuaError))

import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Userdata as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil
import qualified Text.Pandoc.UTF8 as UTF8

-- | Userdata name used by Lua for the @PandocError@ type.
pandocErrorName :: String
pandocErrorName = "pandoc error"

-- | Peek a @'PandocError'@ element to the Lua stack.
pushPandocError :: PandocError -> Lua ()
pushPandocError = Lua.pushAnyWithMetatable pushPandocErrorMT
  where
    pushPandocErrorMT = Lua.ensureUserdataMetatable pandocErrorName $
      LuaUtil.addFunction "__tostring" __tostring

-- | Retrieve a @'PandocError'@ from the Lua stack.
peekPandocError :: StackIndex -> Lua PandocError
peekPandocError idx = Lua.ltype idx >>= \case
  Lua.TypeUserdata -> do
    errMb <- Lua.toAnyWithName idx pandocErrorName
    return $ case errMb of
      Just err -> err
      Nothing  -> PandocLuaError "could not retrieve original error"
  _ -> do
    Lua.pushvalue idx
    msg <- Lua.state >>= \l -> Lua.liftIO (Lua.errorMessage l)
    return $ PandocLuaError (UTF8.toText msg)

-- | Convert to string.
__tostring :: PandocError -> Lua String
__tostring = return . show

--
-- Instances
--

instance Pushable PandocError where
  push = pushPandocError

instance Peekable PandocError where
  peek = peekPandocError

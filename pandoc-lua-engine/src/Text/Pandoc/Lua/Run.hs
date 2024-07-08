{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{- |
   Module      : Text.Pandoc.Lua.Run
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Run code in the Lua interpreter.
-}
module Text.Pandoc.Lua.Run
  ( runLua
  , runLuaNoEnv
  , runLuaWith
  ) where

import Control.Monad.Catch (try)
import Control.Monad.Trans (MonadIO (..))
import HsLua as Lua hiding (try)
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (initLua)
import Text.Pandoc.Lua.PandocLua (PandocLua (..), liftPandocLua)

-- | Run the Lua interpreter, using pandoc's default way of environment
-- initialization.
runLua :: (PandocMonad m, MonadIO m)
       => LuaE PandocError a -> m (Either PandocError a)
runLua action = do
  runPandocLuaWith Lua.run . try $ do
    initLua
    liftPandocLua action

runLuaWith :: (PandocMonad m, MonadIO m)
           => GCManagedState -> LuaE PandocError a -> m (Either PandocError a)
runLuaWith luaState action = do
  runPandocLuaWith (withGCManagedState luaState) . try $ do
    initLua
    liftPandocLua action

-- | Like 'runLua', but ignores all environment variables like @LUA_PATH@.
runLuaNoEnv :: (PandocMonad m, MonadIO m)
            => LuaE PandocError a -> m (Either PandocError a)
runLuaNoEnv action = do
  runPandocLuaWith Lua.run . try $ do
    liftPandocLua $ do
      -- This is undocumented, but works -- the code is adapted from the
      -- `lua.c` sources for the default interpreter.
      Lua.pushboolean True
      Lua.setfield Lua.registryindex "LUA_NOENV"
    initLua
    liftPandocLua action

-- | Evaluate a @'PandocLua'@ computation, running all contained Lua
-- operations.
runPandocLuaWith :: (PandocMonad m, MonadIO m)
                 => (forall b. LuaE PandocError b -> IO b)
                 -> PandocLua a
                 -> m a
runPandocLuaWith runner pLua = do
  origState <- getCommonState
  globals <- defaultGlobals
  (result, newState) <- liftIO . runner . unPandocLua $ do
    putCommonState origState
    liftPandocLua $ setGlobals globals
    r <- pLua
    c <- getCommonState
    return (r, c)
  putCommonState newState
  return result

-- | Global variables which should always be set.
defaultGlobals :: PandocMonad m => m [Global]
defaultGlobals = do
  commonState <- getCommonState
  return
    [ PANDOC_API_VERSION
    , PANDOC_STATE commonState
    , PANDOC_VERSION
    ]

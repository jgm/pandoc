{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.PandocLua
   Copyright   : © 2020-2023 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

PandocMonad instance which allows execution of Lua operations and which
uses Lua to handle state.
-}
module Text.Pandoc.Lua.PandocLua
  ( PandocLua (..)
  , liftPandocLua
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO)
import HsLua as Lua
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Lua.Marshal.CommonState (peekCommonState, pushCommonState)
import Text.Pandoc.Lua.Marshal.PandocError (peekPandocError, pushPandocError)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Text.Pandoc.Class.IO as IO

-- | Type providing access to both, pandoc and Lua operations.
newtype PandocLua a = PandocLua { unPandocLua :: LuaE PandocError a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadThrow
    )

-- | Lift a @'Lua'@ operation into the @'PandocLua'@ type.
liftPandocLua :: LuaE PandocError a -> PandocLua a
liftPandocLua = PandocLua

instance {-# OVERLAPPING #-} Exposable PandocError (PandocLua NumResults) where
  partialApply _narg = liftLua . unPandocLua

instance Pushable a => Exposable PandocError (PandocLua a) where
  partialApply _narg x = 1 <$ (liftLua (unPandocLua x >>= Lua.push))

instance MonadError PandocError PandocLua where
  catchError = Catch.catch
  throwError = Catch.throwM

instance PandocMonad PandocLua where
  lookupEnv = IO.lookupEnv
  getCurrentTime = IO.getCurrentTime
  getCurrentTimeZone = IO.getCurrentTimeZone
  newStdGen = IO.newStdGen
  newUniqueHash = IO.newUniqueHash

  openURL = IO.openURL

  readFileLazy = IO.readFileLazy
  readFileStrict = IO.readFileStrict
  readStdinStrict = IO.readStdinStrict
  svgToPng = IO.svgToPng

  glob = IO.glob
  fileExists = IO.fileExists
  getDataFileName = IO.getDataFileName
  getModificationTime = IO.getModificationTime

  getCommonState = PandocLua $ do
    Lua.getglobal "PANDOC_STATE"
    forcePeek $ peekCommonState Lua.top `lastly` pop 1
  putCommonState cst = PandocLua $ do
    pushCommonState cst
    Lua.setglobal "PANDOC_STATE"

  logOutput = IO.logOutput

-- | Retrieve a @'PandocError'@ from the Lua stack.
popPandocError :: LuaE PandocError PandocError
popPandocError = do
  errResult <- runPeek $ peekPandocError top `lastly` pop 1
  case resultToEither errResult of
    Right x  -> return x
    Left err -> return $ PandocLuaError (T.pack err)

-- | Conversions between Lua errors and 'PandocError' exceptions.
instance LuaError PandocError where
  popException = popPandocError
  pushException = pushPandocError
  luaException = PandocLuaError . T.pack

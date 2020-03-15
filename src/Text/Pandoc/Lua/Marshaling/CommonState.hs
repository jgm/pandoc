{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.CommonState
   Copyright   : © 2012-2020 John MacFarlane
                 © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Instances to marshal (push) and unmarshal (peek) the common state.
-}
module Text.Pandoc.Lua.Marshaling.CommonState () where

import Foreign.Lua (Lua, Peekable, Pushable)
import Foreign.Lua.Types.Peekable (reportValueOnFailure)
import Foreign.Lua.Userdata (ensureUserdataMetatable, pushAnyWithMetatable,
                             toAnyWithName)
import Text.Pandoc.Class (CommonState (..))
import Text.Pandoc.Logging (LogMessage, showLogMessage)
import Text.Pandoc.Lua.Marshaling.AnyValue (AnyValue (..))

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Name used by Lua for the @CommonState@ type.
commonStateTypeName :: String
commonStateTypeName = "Pandoc CommonState"

instance Peekable CommonState where
  peek idx = reportValueOnFailure commonStateTypeName
             (`toAnyWithName` commonStateTypeName) idx

instance Pushable CommonState where
  push st = pushAnyWithMetatable pushCommonStateMetatable st
   where
    pushCommonStateMetatable = ensureUserdataMetatable commonStateTypeName $ do
      LuaUtil.addFunction "__index" indexCommonState
      LuaUtil.addFunction "__pairs" pairsCommonState

indexCommonState :: CommonState -> AnyValue -> Lua Lua.NumResults
indexCommonState st (AnyValue idx) = Lua.ltype idx >>= \case
  Lua.TypeString -> 1 <$ (Lua.peek idx >>= pushField)
  _ -> 1 <$ Lua.pushnil
 where
  pushField :: Text.Text -> Lua ()
  pushField name = case lookup name commonStateFields of
    Just pushValue -> pushValue st
    Nothing -> Lua.pushnil

pairsCommonState :: CommonState -> Lua Lua.NumResults
pairsCommonState st = do
  Lua.pushHaskellFunction nextFn
  Lua.pushnil
  Lua.pushnil
  return 3
 where
  nextFn :: AnyValue -> AnyValue -> Lua Lua.NumResults
  nextFn _ (AnyValue idx) =
    Lua.ltype idx >>= \case
      Lua.TypeNil -> case commonStateFields of
        []  -> 2 <$ (Lua.pushnil *> Lua.pushnil)
        (key, pushValue):_ -> 2 <$ (Lua.push key *> pushValue st)
      Lua.TypeString -> do
        key <- Lua.peek idx
        case tail $ dropWhile ((/= key) . fst) commonStateFields of
          []                     -> 2 <$ (Lua.pushnil *> Lua.pushnil)
          (nextKey, pushValue):_ -> 2 <$ (Lua.push nextKey *> pushValue st)
      _ -> 2 <$ (Lua.pushnil *> Lua.pushnil)

commonStateFields :: [(Text.Text, CommonState -> Lua ())]
commonStateFields =
  [ ("input_files", Lua.push . stInputFiles)
  , ("output_file", Lua.push . Lua.Optional . stOutputFile)
  , ("log", Lua.push . stLog)
  , ("request_headers", Lua.push . Map.fromList . stRequestHeaders)
  , ("resource_path", Lua.push . stResourcePath)
  , ("source_url", Lua.push . Lua.Optional . stSourceURL)
  , ("user_data_dir", Lua.push . Lua.Optional . stUserDataDir)
  , ("trace", Lua.push . stTrace)
  , ("verbosity", Lua.push . show . stVerbosity)
  ]

-- | Name used by Lua for the @CommonState@ type.
logMessageTypeName :: String
logMessageTypeName = "Pandoc LogMessage"

instance Peekable LogMessage where
  peek idx = reportValueOnFailure logMessageTypeName
             (`toAnyWithName` logMessageTypeName) idx

instance Pushable LogMessage where
  push msg = pushAnyWithMetatable pushLogMessageMetatable msg
   where
    pushLogMessageMetatable = ensureUserdataMetatable logMessageTypeName $
      LuaUtil.addFunction "__tostring" tostringLogMessage

tostringLogMessage :: LogMessage -> Lua Text.Text
tostringLogMessage = return . showLogMessage

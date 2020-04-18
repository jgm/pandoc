{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Version
   Copyright   : © 2019-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling of @'Version'@s. The marshaled elements can be compared using
default comparison operators (like @>@ and @<=@).
-}
module Text.Pandoc.Lua.Marshaling.Version
  ( peekVersion
  , pushVersion
  )
  where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Version (Version (..), makeVersion, parseVersion, showVersion)
import Foreign.Lua (Lua, Optional (..), NumResults,
                    Peekable, Pushable, StackIndex)
import Foreign.Lua.Types.Peekable (reportValueOnFailure)
import Foreign.Lua.Userdata (ensureUserdataMetatable, pushAnyWithMetatable,
                             toAnyWithName)
import Safe (atMay, lastMay)
import Text.Pandoc.Lua.Marshaling.AnyValue (AnyValue (..))
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Push a @'Version'@ element to the Lua stack.
pushVersion :: Version -> Lua ()
pushVersion version = pushAnyWithMetatable pushVersionMT version
 where
  pushVersionMT = ensureUserdataMetatable versionTypeName $ do
    LuaUtil.addFunction "__eq" __eq
    LuaUtil.addFunction "__le" __le
    LuaUtil.addFunction "__lt" __lt
    LuaUtil.addFunction "__len" __len
    LuaUtil.addFunction "__index" __index
    LuaUtil.addFunction "__pairs" __pairs
    LuaUtil.addFunction "__tostring" __tostring

instance Pushable Version where
  push = pushVersion

peekVersion :: StackIndex -> Lua Version
peekVersion idx = Lua.ltype idx >>= \case
  Lua.TypeString -> do
    versionStr <- Lua.peek idx
    let parses = readP_to_S parseVersion versionStr
    case lastMay parses of
      Just (v, "") -> return v
      _  -> Lua.throwMessage $ "could not parse as Version: " ++ versionStr

  Lua.TypeUserdata ->
    reportValueOnFailure versionTypeName
                         (`toAnyWithName` versionTypeName)
                         idx
  Lua.TypeNumber -> do
    n <- Lua.peek idx
    return (makeVersion [n])

  Lua.TypeTable ->
    makeVersion <$> Lua.peek idx

  _ ->
    Lua.throwMessage "could not peek Version"

instance Peekable Version where
  peek = peekVersion

-- | Name used by Lua for the @CommonState@ type.
versionTypeName :: String
versionTypeName = "HsLua Version"

__eq :: Version -> Version -> Lua Bool
__eq v1 v2 = return (v1 == v2)

__le :: Version -> Version -> Lua Bool
__le v1 v2 = return (v1 <= v2)

__lt :: Version -> Version -> Lua Bool
__lt v1 v2 = return (v1 < v2)

-- | Get number of version components.
__len :: Version -> Lua Int
__len = return . length . versionBranch

-- | Access fields.
__index :: Version -> AnyValue -> Lua NumResults
__index v (AnyValue k) = do
  ty <- Lua.ltype k
  case ty of
    Lua.TypeNumber -> do
      n <- Lua.peek k
      let versionPart = atMay (versionBranch v) (n - 1)
      Lua.push (Lua.Optional versionPart)
      return 1
    Lua.TypeString -> do
      (str :: Text) <- Lua.peek k
      if str == "must_be_at_least"
        then 1 <$ Lua.pushHaskellFunction must_be_at_least
        else 1 <$ Lua.pushnil
    _ -> 1 <$ Lua.pushnil

-- | Create iterator.
__pairs :: Version -> Lua NumResults
__pairs v = do
  Lua.pushHaskellFunction nextFn
  Lua.pushnil
  Lua.pushnil
  return 3
 where
  nextFn :: AnyValue -> Optional Int -> Lua Lua.NumResults
  nextFn _ (Optional key) =
    case key of
      Nothing -> case versionBranch v of
                   []  -> 2 <$ (Lua.pushnil *> Lua.pushnil)
                   n:_ -> 2 <$ (Lua.push (1 :: Int) *> Lua.push n)
      Just n  -> case atMay (versionBranch v) n of
                   Nothing -> 2 <$ (Lua.pushnil *> Lua.pushnil)
                   Just b  -> 2 <$ (Lua.push (n + 1) *> Lua.push b)

-- | Convert to string.
__tostring :: Version -> Lua String
__tostring v = return (showVersion v)

-- | Default error message when a version is too old. This message is
-- formatted in Lua with the expected and actual versions as arguments.
versionTooOldMessage :: String
versionTooOldMessage = "expected version %s or newer, got %s"

-- | Throw an error if this version is older than the given version.
-- FIXME: This function currently requires the string library to be
-- loaded.
must_be_at_least :: Version -> Version -> Optional String -> Lua NumResults
must_be_at_least actual expected optMsg = do
  let msg = fromMaybe versionTooOldMessage (fromOptional optMsg)
  if expected <= actual
    then return 0
    else do
      Lua.getglobal' "string.format"
      Lua.push msg
      Lua.push (showVersion expected)
      Lua.push (showVersion actual)
      Lua.call 3 1
      Lua.error

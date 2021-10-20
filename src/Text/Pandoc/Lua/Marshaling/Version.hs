{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Version
   Copyright   : © 2019-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling of @'Version'@s. The marshaled elements can be compared using
default comparison operators (like @>@ and @<=@).
-}
module Text.Pandoc.Lua.Marshaling.Version
  ( peekVersion
  , pushVersion
  , peekVersionFuzzy
  )
  where

import Data.Maybe (fromMaybe)
import Data.Version (Version (..), makeVersion, parseVersion, showVersion)
import HsLua as Lua
import Safe (lastMay)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Text.Pandoc.UTF8 as UTF8

instance Peekable Version where
  peek = forcePeek . peekVersionFuzzy

instance Pushable Version where
  push = pushVersion

-- | Push a @'Version'@ element to the Lua stack.
pushVersion :: LuaError e => Pusher e Version
pushVersion = pushUD typeVersion

peekVersionFuzzy :: LuaError e => Peeker e Version
peekVersionFuzzy idx = retrieving "Version" $ liftLua (Lua.ltype idx) >>= \case
  Lua.TypeUserdata -> peekVersion idx
  Lua.TypeString -> do
    versionStr <- peekString idx
    let parses = readP_to_S parseVersion versionStr
    case lastMay parses of
      Just (v, "") -> return v
      _  -> Lua.failPeek $
            UTF8.fromString $ "could not parse as Version: " ++ versionStr

  Lua.TypeNumber -> do
    (makeVersion . (:[])) <$> peekIntegral idx

  Lua.TypeTable ->
    makeVersion <$> peekList peekIntegral idx

  _ ->
    Lua.failPeek "could not peek Version"

peekVersion :: LuaError e => Peeker e Version
peekVersion = peekUD typeVersion

typeVersion :: LuaError e => DocumentedType e Version
typeVersion = deftype "Version"
  [ operation Eq $ defun "__eq"
    ### liftPure2 (==)
    <#> parameter peekVersionFuzzy "Version" "v1" ""
    <#> parameter peekVersionFuzzy "Version" "v2" ""
    =#> functionResult pushBool "boolean" "true iff v1 == v2"
  , operation Lt $ defun "__lt"
    ### liftPure2 (<)
    <#> parameter peekVersionFuzzy "Version" "v1" ""
    <#> parameter peekVersionFuzzy "Version" "v2" ""
    =#> functionResult pushBool "boolean" "true iff v1 < v2"
  , operation Le $ defun "__le"
    ### liftPure2 (<=)
    <#> parameter peekVersionFuzzy "Version" "v1" ""
    <#> parameter peekVersionFuzzy "Version" "v2" ""
    =#> functionResult pushBool "boolean" "true iff v1 <= v2"
  , operation Len $ defun "__len"
    ### liftPure (length . versionBranch)
    <#> parameter peekVersionFuzzy "Version" "v1" ""
    =#> functionResult pushIntegral "integer" "number of version components"
  , operation Tostring $ defun "__tostring"
    ### liftPure showVersion
    <#> parameter peekVersionFuzzy "Version" "version" ""
    =#> functionResult pushString "string" "stringified version"
  ]
  [ method $ defun "must_be_at_least"
    ### must_be_at_least
    <#> parameter peekVersionFuzzy "Version" "self" "version to check"
    <#> parameter peekVersionFuzzy "Version" "reference" "minimum version"
    <#> optionalParameter peekString "string" "msg" "alternative message"
    =?> "Returns no result, and throws an error if this version is older than reference"
  ]

-- | Throw an error if this version is older than the given version.
-- FIXME: This function currently requires the string library to be
-- loaded.
must_be_at_least :: LuaError e
                 => Version -> Version -> Maybe String
                 -> LuaE e NumResults
must_be_at_least actual expected mMsg = do
  let msg = fromMaybe versionTooOldMessage mMsg
  if expected <= actual
    then return 0
    else do
      Lua.getglobal' "string.format"
      Lua.push msg
      Lua.push (showVersion expected)
      Lua.push (showVersion actual)
      Lua.call 3 1
      Lua.error

-- | Default error message when a version is too old. This message is
-- formatted in Lua with the expected and actual versions as arguments.
versionTooOldMessage :: String
versionTooOldMessage = "expected version %s or newer, got %s"

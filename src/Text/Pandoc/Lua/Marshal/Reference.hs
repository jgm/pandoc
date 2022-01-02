{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.ReaderOptions
   Copyright   : © 2012-2022 John MacFarlane
                 © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshal citeproc 'Reference' values.
-}
module Text.Pandoc.Lua.Marshal.Reference
  ( pushReference
  ) where

import Citeproc.Types
  ( Date (..), DateParts (..), ItemId (..), Name (..), Reference (..)
  , Val (..), Variable, fromVariable
  )
import Control.Monad (forM_)
import HsLua hiding (Name, Reference, pushName, peekName)
import Text.Pandoc.Builder (Inlines, toList)
import Text.Pandoc.Lua.Marshal.Inline (pushInlines)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)

import qualified Data.Map as Map

-- | Pushes a ReaderOptions value as userdata object.
pushReference :: LuaError e => Pusher e (Reference Inlines)
pushReference reference = do
  pushAsTable [ ("id", pushItemId . referenceId)
              , ("type", pushText . referenceType)
              ]
              reference
  forM_ (Map.toList $ referenceVariables reference) $ \(var, val) -> do
    pushVariable var
    pushVal val
    rawset (nth 3)

-- | Pushes an 'ItemId' as a string.
pushItemId :: Pusher e ItemId
pushItemId = pushText . unItemId

-- | Pushes a person's 'Name' as a table.
pushName :: LuaError e => Pusher e Name
pushName = pushAsTable
  [ ("family"                , pushTextOrNil . nameFamily)
  , ("given"                 , pushTextOrNil . nameGiven)
  , ("dropping-particle"     , pushTextOrNil . nameDroppingParticle)
  , ("non-dropping-particle" , pushTextOrNil . nameNonDroppingParticle)
  , ("suffix"                , pushTextOrNil . nameSuffix)
  , ("literal"               , pushTextOrNil . nameLiteral)
  , ("comma-suffix"          , pushBoolOrNil . nameCommaSuffix)
  , ("static-ordering"       , pushBoolOrNil . nameStaticOrdering)
  ]
  where
    pushTextOrNil = \case
      Nothing -> pushnil
      Just xs -> pushText xs

-- | Pushes a boolean, but uses @nil@ instead of @false@; table fields
-- are not set unless the value is true.
pushBoolOrNil :: Pusher e Bool
pushBoolOrNil = \case
  False -> pushnil
  True  -> pushBool True

-- | Pushes a 'Variable' as string.
pushVariable :: Pusher e Variable
pushVariable = pushText . fromVariable

-- | Pushes a 'Val', i.e., a variable value.
pushVal :: LuaError e => Pusher e (Val Inlines)
pushVal = \case
  TextVal t -> pushText t
  FancyVal inlns -> pushInlines $ toList inlns
  NumVal i       -> pushIntegral i
  NamesVal names -> pushPandocList pushName names
  DateVal date   -> pushDate date

-- | Pushes a 'Date' as table.
pushDate :: LuaError e => Pusher e Date
pushDate = pushAsTable
  [ ("date-parts", pushPandocList pushDateParts . dateParts)
  , ("circa", pushBoolOrNil . dateCirca)
  , ("season", maybe pushnil pushIntegral . dateSeason)
  , ("literal", maybe pushnil pushText . dateLiteral)
  ]
 where
   -- date parts are lists of Int values
   pushDateParts (DateParts dp) = pushPandocList pushIntegral dp

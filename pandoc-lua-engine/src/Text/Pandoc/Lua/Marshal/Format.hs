{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Format
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling functions and instance for format related types, including
'Extensions' and 'ExtensionConfig'.
-}
module Text.Pandoc.Lua.Marshal.Format
  ( peekExtensions
  , pushExtensions
  , peekExtensionsConfig
  , peekFlavoredFormat
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>))
import Data.Maybe (fromMaybe)
import HsLua
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Extensions
  ( Extension, Extensions, extensionsFromList
  , getDefaultExtensions, readExtension )
import Text.Pandoc.Format
  ( ExtensionsConfig (..), ExtensionsDiff (..), FlavoredFormat (..)
  , diffExtensions, parseFlavoredFormat)
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua))

-- | Retrieves an 'Extensions' set from the Lua stack.
peekExtension :: LuaError e => Peeker e Extension
peekExtension idx = do
  extString <- peekString idx
  return $ readExtension extString
{-# INLINE peekExtension #-}

-- | Retrieves an 'Extensions' set from the Lua stack.
peekExtensions :: LuaError e => Peeker e Extensions
peekExtensions = fmap extensionsFromList . peekList peekExtension
{-# INLINE peekExtensions #-}

-- | Pushes a set of 'Extensions' to the top of the Lua stack.
pushExtensions :: LuaError e => Pusher e Extensions
pushExtensions = pushViaJSON
{-# INLINE pushExtensions #-}

instance Peekable Extensions where
  safepeek = peekExtensions

instance Pushable Extensions where
  push = pushExtensions

-- | Retrieves an 'ExtensionsConfig' value from the Lua stack.
peekExtensionsConfig :: LuaError e => Peeker e ExtensionsConfig
peekExtensionsConfig idx = do
  diff <- peekExtensionsDiff idx
  return $ ExtensionsConfig
    { extsDefault   = extsToEnable diff
    , extsSupported = extsToEnable diff <> extsToDisable diff
    }

instance Peekable ExtensionsConfig where
  safepeek = peekExtensionsConfig

peekExtensionsDiff :: LuaError e => Peeker e ExtensionsDiff
peekExtensionsDiff = typeChecked "table" istable $ \idx ->
      (do
          en <- peekFieldRaw (emptyOr (fmap Just . peekExtensions)) "enable" idx
          di <- peekFieldRaw (emptyOr (fmap Just . peekExtensions)) "disable" idx
          if (en, di) == (Nothing, Nothing)
            then failPeek "At least on of  'enable' and 'disable' must be set"
            else return $
                 ExtensionsDiff (fromMaybe mempty en) (fromMaybe mempty di))
  <|> -- two lists of extensions; the first is list assumed to contain those
      -- extensions to be enabled
      (uncurry ExtensionsDiff <$!> peekPair peekExtensions peekExtensions idx)
  <|> (do
          let
          exts <- peekKeyValuePairs peekExtension peekEnabled idx
          let enabled  = extensionsFromList . map fst $ filter snd exts
          let disabled = extensionsFromList . map fst $ filter (not . snd) exts
          return $ ExtensionsDiff enabled disabled)

-- | Retrieves the activation status of an extension. True or the string
-- @'enable'@ for activated, False or 'disable' for disabled.
peekEnabled :: LuaError e => Peeker e Bool
peekEnabled idx' = liftLua (ltype idx') >>= \case
  TypeBoolean -> peekBool idx'
  TypeString  -> peekText idx' >>= \case
                   "disable" -> pure False
                   "enable"  -> pure True
                   _         -> failPeek "expected 'disable' or 'enable'"
  _ -> failPeek "expected boolean or string"

-- | Retrieves a flavored format from the Lua stack.
peekFlavoredFormat :: Peeker PandocError FlavoredFormat
peekFlavoredFormat idx = retrieving "flavored format" $
  liftLua (ltype idx) >>= \case
  TypeString -> peekText idx >>= liftLua . unPandocLua . parseFlavoredFormat
  TypeTable -> do
    let diffFor format idx' = peekExtensionsDiff idx' <|>
          (getDefaultExtensions format `diffExtensions`) <$>
          (typeChecked "table" istable peekExtensions idx')
    format   <- peekFieldRaw peekText "format" idx
    extsDiff <- peekFieldRaw (emptyOr (diffFor format)) "extensions" idx
    return (FlavoredFormat format extsDiff)
  _ -> failPeek =<< typeMismatchMessage "string or table" idx

-- | Returns 'mempty' if the given stack index is @nil@, and the result
-- of the peeker otherwise.
emptyOr :: Monoid a => Peeker e a -> Peeker e a
emptyOr p idx = do
  nil <- liftLua (isnil idx)
  if nil
    then pure mempty
    else p idx

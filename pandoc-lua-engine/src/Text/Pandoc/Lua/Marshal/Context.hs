{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Context
   Copyright   : © 2012-2022 John MacFarlane
                 © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for doctemplates Context and its components.
-}
module Text.Pandoc.Lua.Marshal.Context
  ( peekContext
  , pushContext
  ) where

import Control.Monad ((<$!>))
import Data.Text (Text)
import HsLua as Lua
import HsLua.Module.DocLayout (peekDoc, pushDoc)
import Text.DocTemplates (Context(..), Val(..))

instance Pushable (Context Text) where
  push = pushContext

instance Pushable (Val Text) where
  push = pushVal

-- | Retrieves a template context from the Lua stack.
peekContext :: LuaError e => Peeker e (Context Text)
peekContext idx = Context <$!> peekMap peekText peekVal idx

-- | Pushes a template context to the Lua stack.
pushContext :: LuaError e => Pusher e (Context Text)
pushContext = pushMap pushText pushVal . unContext

pushVal :: LuaError e => Pusher e (Val Text)
pushVal = \case
  NullVal     -> Lua.pushnil
  BoolVal b   -> Lua.pushBool b
  MapVal ctx  -> pushContext ctx
  ListVal xs  -> pushList pushVal xs
  SimpleVal d -> pushDoc d

peekVal :: LuaError e => Peeker e (Val Text)
peekVal idx = liftLua (ltype idx) >>= \case
  TypeNil      -> pure NullVal
  TypeBoolean  -> BoolVal <$!> peekBool idx
  TypeNumber   -> SimpleVal <$!> peekDoc idx
  TypeString   -> SimpleVal <$!> peekDoc idx
  TypeTable    -> do
    len <- liftLua $ Lua.rawlen idx
    if len <= 0
      then MapVal <$!> peekContext idx
      else ListVal <$!> peekList peekVal idx
  TypeUserdata -> SimpleVal <$!> peekDoc idx
  _ -> failPeek =<<
       typeMismatchMessage "Doc, string, boolean, table, or nil" idx

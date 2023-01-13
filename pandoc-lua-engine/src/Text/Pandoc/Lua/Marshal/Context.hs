{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Context
   Copyright   : © 2012-2023 John MacFarlane
                 © 2017-2023 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for doctemplates Context and its components.
-}
module Text.Pandoc.Lua.Marshal.Context
  ( peekContext
  , pushContext
  ) where

import Control.Monad (when, (<$!>))
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
pushContext ctx = do
  pushMap pushText pushVal $ unContext ctx
  created <- Lua.newmetatable "pandoc Context"
  when created $ do
    pushName "__concat"
    pushHaskellFunction $ do
      c1 <- forcePeek $ peekContext (nthBottom 1)
      c2 <- forcePeek $ peekContext (nthBottom 2)
      pushContext (c1 <> c2)
      return 1
    rawset (nth 3)
  setmetatable (nth 2)

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

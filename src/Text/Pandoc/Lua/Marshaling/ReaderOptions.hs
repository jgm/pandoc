{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.ReaderOptions
   Copyright   : © 2012-2020 John MacFarlane
                 © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for ReaderOptions and its components.
-}
module Text.Pandoc.Lua.Marshaling.ReaderOptions () where

import Data.Data (showConstr, toConstr)
import Foreign.Lua (Lua, Pushable)
import Text.Pandoc.Extensions (Extensions)
import Text.Pandoc.Lua.Marshaling.AnyValue (AnyValue (..))
import Text.Pandoc.Lua.Marshaling.CommonState ()
import Text.Pandoc.Options (ReaderOptions (..), TrackChanges)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

--
-- Reader Options
--
instance Pushable Extensions where
  push exts = Lua.push (show exts)

instance Pushable TrackChanges where
  push = Lua.push . showConstr . toConstr

instance Pushable ReaderOptions where
  push ro = do
    let ReaderOptions
          (extensions            :: Extensions)
          (standalone            :: Bool)
          (columns               :: Int)
          (tabStop               :: Int)
          (indentedCodeClasses   :: [Text.Text])
          (abbreviations         :: Set.Set Text.Text)
          (defaultImageExtension :: Text.Text)
          (trackChanges          :: TrackChanges)
          (stripComments         :: Bool)
          = ro
    Lua.newtable
    LuaUtil.addField "extensions" extensions
    LuaUtil.addField "standalone" standalone
    LuaUtil.addField "columns" columns
    LuaUtil.addField "tab_stop" tabStop
    LuaUtil.addField "indented_code_classes" indentedCodeClasses
    LuaUtil.addField "abbreviations" abbreviations
    LuaUtil.addField "default_image_extension" defaultImageExtension
    LuaUtil.addField "track_changes" trackChanges
    LuaUtil.addField "strip_comments" stripComments

    -- add metatable
    let indexReaderOptions :: AnyValue -> AnyValue -> Lua Lua.NumResults
        indexReaderOptions _tbl (AnyValue key) = do
          Lua.ltype key >>= \case
            Lua.TypeString -> Lua.peek key >>= \case
              ("defaultImageExtension" :: Text.Text)
                                    -> Lua.push defaultImageExtension
              "indentedCodeClasses" -> Lua.push indentedCodeClasses
              "stripComments" -> Lua.push stripComments
              "tabStop" -> Lua.push tabStop
              "trackChanges" -> Lua.push trackChanges
              _ -> Lua.pushnil
            _ -> Lua.pushnil
          return 1
    Lua.newtable
    LuaUtil.addFunction "__index" indexReaderOptions
    Lua.setmetatable (Lua.nthFromTop 2)

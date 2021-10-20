{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.ReaderOptions
   Copyright   : © 2012-2021 John MacFarlane
                 © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for ReaderOptions and its components.
-}
module Text.Pandoc.Lua.Marshaling.ReaderOptions
  ( peekReaderOptions
  , pushReaderOptions
  ) where

import HsLua as Lua
import Text.Pandoc.Lua.Marshaling.List (pushPandocList)
import Text.Pandoc.Options (ReaderOptions (..))

--
-- Reader Options
--

peekReaderOptions :: LuaError e => Peeker e ReaderOptions
peekReaderOptions = peekUD typeReaderOptions

pushReaderOptions :: LuaError e => Pusher e ReaderOptions
pushReaderOptions = pushUD typeReaderOptions

typeReaderOptions :: LuaError e => DocumentedType e ReaderOptions
typeReaderOptions = deftype "pandoc ReaderOptions"
  [ operation Tostring luaShow
  ]
  [ readonly "extensions" ""
      ( pushString . show
      , readerExtensions)
  , readonly "standalone" ""
      ( pushBool
      , readerStandalone)
  , readonly "columns" ""
      ( pushIntegral
      , readerColumns)
  , readonly "tab_stop" ""
      ( pushIntegral
      , readerTabStop)
  , readonly "indented_code_classes" ""
      ( pushPandocList pushText
      , readerIndentedCodeClasses)
  , readonly "abbreviations" ""
      ( pushSet pushText
      , readerAbbreviations)
  , readonly "track_changes" ""
      ( pushString . show
      , readerTrackChanges)
  , readonly "strip_comments" ""
      ( pushBool
      , readerStripComments)
  , readonly "default_image_extension" ""
      ( pushText
      , readerDefaultImageExtension)
  ]

luaShow :: LuaError e => DocumentedFunction e
luaShow = defun "__tostring"
  ### liftPure show
  <#> udparam typeReaderOptions "state" "object to print in native format"
  =#> functionResult pushString "string" "Haskell representation"

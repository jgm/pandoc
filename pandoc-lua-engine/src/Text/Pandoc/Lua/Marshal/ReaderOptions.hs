{-# LANGUAGE CPP                  #-}
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

Marshaling instance for ReaderOptions and its components.
-}
module Text.Pandoc.Lua.Marshal.ReaderOptions
  ( peekReaderOptions
  , pushReaderOptions
  , pushReaderOptionsReadonly
  ) where

import Data.Default (def)
import HsLua as Lua
#if !MIN_VERSION_hslua(2,2,0)
import HsLua.Aeson (peekViaJSON, pushViaJSON)
#endif
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Options (ReaderOptions (..))

--
-- Reader Options
--

-- | Retrieve a ReaderOptions value, either from a normal ReaderOptions
-- value, from a read-only object, or from a table with the same
-- keys as a ReaderOptions object.
peekReaderOptions :: LuaError e => Peeker e ReaderOptions
peekReaderOptions = retrieving "ReaderOptions" . \idx ->
  liftLua (ltype idx) >>= \case
    TypeUserdata -> choice [ peekUD typeReaderOptions
                           , peekUD typeReaderOptionsReadonly
                           ]
                           idx
    TypeTable    -> peekReaderOptionsTable idx
    _            -> failPeek =<<
                    typeMismatchMessage "ReaderOptions userdata or table" idx

-- | Pushes a ReaderOptions value as userdata object.
pushReaderOptions :: LuaError e => Pusher e ReaderOptions
pushReaderOptions = pushUD typeReaderOptions

-- | Pushes a ReaderOptions object, but makes it read-only.
pushReaderOptionsReadonly :: LuaError e => Pusher e ReaderOptions
pushReaderOptionsReadonly = pushUD typeReaderOptionsReadonly

-- | ReaderOptions object type for read-only values.
typeReaderOptionsReadonly :: LuaError e => DocumentedType e ReaderOptions
typeReaderOptionsReadonly = deftype "ReaderOptions (read-only)"
  [ operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeReaderOptions "opts" "options to print in native format"
    =#> functionResult pushString "string" "Haskell representation"
  , operation Newindex $ lambda
    ### (failLua "This ReaderOptions value is read-only.")
    =?> "Throws an error when called, i.e., an assignment is made."
  ]
  readerOptionsMembers

-- | 'ReaderOptions' object type.
typeReaderOptions :: LuaError e => DocumentedType e ReaderOptions
typeReaderOptions = deftype "ReaderOptions"
  [ operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeReaderOptions "opts" "options to print in native format"
    =#> functionResult pushString "string" "Haskell representation"
  ]
  readerOptionsMembers

-- | Member properties of 'ReaderOptions' Lua values.
readerOptionsMembers :: LuaError e
                     => [Member e (DocumentedFunction e) ReaderOptions]
readerOptionsMembers =
  [ property "abbreviations" ""
      (pushSet pushText, readerAbbreviations)
      (peekSet peekText, \opts x -> opts{ readerAbbreviations = x })
  , property "columns" ""
      (pushIntegral, readerColumns)
      (peekIntegral, \opts x -> opts{ readerColumns = x })
  , property "default_image_extension" ""
      (pushText, readerDefaultImageExtension)
      (peekText, \opts x -> opts{ readerDefaultImageExtension = x })
  , property "extensions" ""
      (pushViaJSON, readerExtensions)
      (peekViaJSON, \opts x -> opts{ readerExtensions = x })
  , property "indented_code_classes" ""
      (pushPandocList pushText, readerIndentedCodeClasses)
      (peekList peekText, \opts x -> opts{ readerIndentedCodeClasses = x })
  , property "standalone" ""
      (pushBool, readerStandalone)
      (peekBool, \opts x -> opts{ readerStandalone = x })
  , property "strip_comments" ""
      (pushBool, readerStripComments)
      (peekBool, \opts x -> opts{ readerStripComments = x })
  , property "tab_stop" ""
      (pushIntegral, readerTabStop)
      (peekIntegral, \opts x -> opts{ readerTabStop = x })
  , property "track_changes" ""
      (pushViaJSON, readerTrackChanges)
      (choice [peekRead, peekViaJSON], \opts x -> opts{ readerTrackChanges = x })
  ]

-- | Retrieves a 'ReaderOptions' object from a table on the stack, using
-- the default values for all missing fields.
--
-- Internally, this pushes the default reader options, sets each
-- key/value pair of the table in the userdata value, then retrieves the
-- object again. This will update all fields and complain about unknown
-- keys.
peekReaderOptionsTable :: LuaError e => Peeker e ReaderOptions
peekReaderOptionsTable idx = retrieving "ReaderOptions (table)" $ do
  liftLua $ do
    absidx <- absindex idx
    pushUD typeReaderOptions def
    let setFields = do
          next absidx >>= \case
            False -> return () -- all fields were copied
            True -> do
              pushvalue (nth 2) *> insert (nth 2)
              settable (nth 4) -- set in userdata object
              setFields
    pushnil -- first key
    setFields
  peekUD typeReaderOptions top `lastly` pop 1

instance Pushable ReaderOptions where
  push = pushReaderOptions

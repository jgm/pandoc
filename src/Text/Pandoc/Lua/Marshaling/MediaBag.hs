{- |
   Module      : Text.Pandoc.Lua.Marshaling.MediaBag
   Copyright   : © 2012-2020 John MacFarlane
                 © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Instances to marshal (push) and unmarshal (peek) media data.
-}
module Text.Pandoc.Lua.Marshaling.MediaBag (pushIterator) where

import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import Foreign.Lua (Lua, NumResults, Peekable, Pushable, StackIndex)
import Foreign.Lua.Types.Peekable (reportValueOnFailure)
import Foreign.Lua.Userdata (ensureUserdataMetatable, pushAnyWithMetatable,
                             toAnyWithName)
import Text.Pandoc.MediaBag (MediaBag, mediaItems)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Lua.Marshaling.AnyValue (AnyValue (..))

import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua as Lua
import qualified Foreign.Storable as Storable

-- | A list of 'MediaBag' items.
newtype MediaItems = MediaItems [(String, MimeType, BL.ByteString)]

instance Pushable MediaItems where
  push = pushMediaItems

instance Peekable MediaItems where
  peek = peekMediaItems

-- | Push an iterator triple to be used with Lua's @for@ loop construct.
-- Each iterator invokation returns a tripple consisting of an item's
-- filename, MIME type, and content.
pushIterator :: MediaBag -> Lua NumResults
pushIterator mb = do
  Lua.pushHaskellFunction nextItem
  Lua.push (MediaItems $ mediaItems mb)
  Lua.pushnil
  return 3

-- | Lua type name for @'MediaItems'@.
mediaItemsTypeName :: String
mediaItemsTypeName = "pandoc MediaItems"

-- | Push a @MediaItems@ element to the stack.
pushMediaItems :: MediaItems -> Lua ()
pushMediaItems xs = pushAnyWithMetatable pushMT xs
 where
  pushMT = ensureUserdataMetatable mediaItemsTypeName (return ())

-- | Retrieve a @MediaItems@ element from the stack.
peekMediaItems :: StackIndex -> Lua MediaItems
peekMediaItems = reportValueOnFailure mediaItemsTypeName
                 (`toAnyWithName` mediaItemsTypeName)

-- | Retrieve a list of items from an iterator state, return the first
-- item (if present), and advance the state.
nextItem :: Ptr (StablePtr MediaItems) -> AnyValue -> Lua NumResults
nextItem ptr _ = do
  (MediaItems items) <- Lua.liftIO $ deRefStablePtr =<< Storable.peek ptr
  case items of
    [] -> 2 <$ (Lua.pushnil *> Lua.pushnil)
    (key, mt, content):xs -> do
      Lua.liftIO $ Storable.poke ptr =<< newStablePtr (MediaItems xs)
      Lua.push key
      Lua.push mt
      Lua.push content
      return 3

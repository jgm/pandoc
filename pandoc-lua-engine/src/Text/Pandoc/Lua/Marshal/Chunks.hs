{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.Chunks
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling chunks, i.e., pandoc subdocuments.
-}
module Text.Pandoc.Lua.Marshal.Chunks
  ( peekChunk
  , pushChunk
  , peekChunkedDoc
  , pushChunkedDoc
  ) where

import Control.Monad ((<$!>))
import Data.Tree (Tree (..))
import HsLua
import Text.Pandoc.Chunks (Chunk (..), ChunkedDoc (..), SecInfo (..))
import Text.Pandoc.Lua.Marshal.AST

-- | Retrieves a 'Chunk' from the Lua stack.
peekChunk :: LuaError e => Peeker e Chunk
peekChunk = peekUD typeChunk

-- | Pushes a 'Chunk' to the top of the Lua stack.
pushChunk :: LuaError e => Pusher e Chunk
pushChunk = pushUD typeChunk

typeChunk :: LuaError e => DocumentedType e Chunk
typeChunk = deftype "pandoc.Chunk"
  [ operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeChunk "chunk" "chunk to print in native format"
    =#> functionResult pushString "string" "Haskell representation"
  ]
  [ property "heading"
    "heading text"
    (pushInlines, chunkHeading)
    (peekInlinesFuzzy, \chunk inlns -> chunk{ chunkHeading = inlns })

  , property "id"
    "identifier"
    (pushText, chunkId)
    (peekText, \chunk ident -> chunk{ chunkId = ident })

  , property "level"
    "level of topmost heading in chunk"
    (pushIntegral, chunkLevel)
    (peekIntegral, \chunk level -> chunk{ chunkLevel = level })

  , property "number"
    "chunk number"
    (pushIntegral, chunkNumber)
    (peekIntegral, \chunk number -> chunk{ chunkNumber = number })

  , property "section_number"
    "hierarchical section number"
    (pushMaybe pushText, chunkSectionNumber)
    (peekMaybe peekText, \chunk secnum -> chunk{ chunkSectionNumber = secnum })

  , property "path"
    "target filepath for this chunk"
    (pushString, chunkPath)
    (peekString, \chunk path -> chunk{ chunkPath = path })

  , property "up"
    "link to the enclosing section chunk, if any"
    (pushMaybe pushChunk, chunkUp)
    (peekMaybe peekChunk, \chunk up -> chunk{ chunkUp = up })

  , property "prev"
    "link to the previous section, if any"
    (pushMaybe pushChunk, chunkPrev)
    (peekMaybe peekChunk, \chunk prev -> chunk{ chunkPrev = prev })

  , property "next"
    "link to the next section, if any"
    (pushMaybe pushChunk, chunkNext)
    (peekMaybe peekChunk, \chunk next' -> chunk{ chunkNext = next' })

  , property "unlisted"
    ( "whether the section in this chunk should be listed in the TOC" <>
      "even if the chunk has no section number" )
    (pushBool, chunkUnlisted)
    (peekBool, \chunk unlisted -> chunk { chunkUnlisted = unlisted })

  , property "contents"
    "the chunk's block contents"
    (pushBlocks, chunkContents)
    (peekBlocksFuzzy, \chunk blks -> chunk{ chunkContents = blks })
  ]

-- | Retrieves a 'ChunkedDoc' from the Lua stack.
peekChunkedDoc :: LuaError e => Peeker e ChunkedDoc
peekChunkedDoc = peekUD typeChunkedDoc

-- | Pushes a 'ChunkedDoc to the top of the Lua stack.
pushChunkedDoc :: LuaError e => Pusher e ChunkedDoc
pushChunkedDoc = pushUD typeChunkedDoc

-- | Lua type for 'ChunkedDoc' values.
typeChunkedDoc :: LuaError e => DocumentedType e ChunkedDoc
typeChunkedDoc = deftype "pandoc.ChunkedDoc"
  []
  [ readonly "chunks"
    "list of chunks that make up the document"
    (pushList pushChunk, chunkedChunks)

  , readonly "meta"
    "the document's metadata"
    (pushMeta, chunkedMeta)

  , readonly "toc"
    "table of contents information"
    (pushTocTree, chunkedTOC)
  ]

-- | Pushes a TOC tree to the stack. The resulting object is a list with
-- the top-level entry placed at index @0@ and all subentries as the
-- rest of the list.
pushTocTree :: LuaError e => Pusher e (Tree SecInfo)
pushTocTree (Node secInfo subSecInfo) = do
  pushList pushTocTree subSecInfo
  pushSecInfo secInfo
  rawseti (nth 2) 0

pushSecInfo :: LuaError e => Pusher e SecInfo
pushSecInfo = pushAsTable
  [ ("title"  , pushInlines . secTitle)
  , ("number" , maybe pushnil pushText . secNumber)
  , ("id"     , pushText . secId)
  , ("path"   , pushText . secPath)
  , ("level"  , pushIntegral . secLevel)
  ]

peekMaybe :: LuaError e => Peeker e a -> Peeker e (Maybe a)
peekMaybe p idx = liftLua (isnoneornil idx) >>= \case
  True  -> pure Nothing
  False -> Just <$!> p idx

pushMaybe :: LuaError e => Pusher e a -> Pusher e (Maybe a)
pushMaybe = maybe pushnil

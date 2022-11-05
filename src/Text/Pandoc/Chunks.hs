{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Text.Pandoc.Chunks
   Copyright   : Copyright (C) 2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions and types for splitting a Pandoc into subdocuments,
e.g. for conversion into a set of HTML pages.
-}
module Text.Pandoc.Chunks
  ( Chunk(..)
  , ChunkedDoc(..)
  , PathTemplate(..)
  , splitIntoChunks
  ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared (makeSections, stringify)
import Text.Pandoc.Walk (Walkable(..))
import Data.Text (Text)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List (find)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.HTML.TagSoup (Tag (TagOpen), fromAttrib, parseTags)
import Text.Pandoc.Writers.Shared (toTOCTree, SecInfo(..))
import Data.Tree (Tree)

-- | Split 'Pandoc' into 'Chunk's, e.g. for conversion into
-- a set of HTML pages or EPUB chapters.
splitIntoChunks :: PathTemplate -- ^ Template for filepath
                -> Bool -- ^ Number sections
                -> Maybe Int -- ^ Base heading level
                -> Int -- ^ Chunk level -- level of section to split at
                -> Pandoc
                -> ChunkedDoc
splitIntoChunks pathTemplate numberSections mbBaseLevel
                chunkLevel (Pandoc meta blocks) =
   fixInternalReferences .
   (\chunks -> ChunkedDoc{ chunkedMeta = meta
                         , chunkedChunks = chunks
                         , chunkedTOC = toTOCTree
                              (concatMap chunkContents chunks) }) .
   makeChunks chunkLevel pathTemplate .
   addNavigation Nothing Nothing .
   makeSections numberSections mbBaseLevel $ blocks

-- | Fix internal references so they point to the path of the chunk.
fixInternalReferences :: ChunkedDoc -> ChunkedDoc
fixInternalReferences chunkedDoc =
  walk rmNavAttrs $ walk fixInternalRefs $
    chunkedDoc{ chunkedTOC = newTOC
              , chunkedChunks = newChunks }
 where
  newTOC = fromMaybe (chunkedTOC chunkedDoc) $
             traverse addSecPath (chunkedTOC chunkedDoc)

  newChunks = map fixNav (chunkedChunks chunkedDoc)

  fixNav chunk =
    chunk{ chunkNext = chunkNext chunk >>= toNavLink
         , chunkPrev = chunkPrev chunk >>= toNavLink
         , chunkUp = chunkUp chunk >>= toNavLink
         }

  toNavLink id' =
    case M.lookup id' refMap of
      Nothing -> Just $ "#" <> id'
      Just fp -> Just $ T.pack fp <> "#" <> id'

  addSecPath :: SecInfo -> Maybe SecInfo
  addSecPath secinfo =
    case M.lookup (secId secinfo) refMap of
      Nothing -> Just secinfo
      Just fp -> Just $ secinfo{ secPath = T.pack fp }

  -- Remove some attributes we added just to construct chunkNext etc.
  rmNavAttrs :: Block -> Block
  rmNavAttrs (Div (ident,classes,kvs) bs) =
    Div (ident,classes,filter (not . isNavAttr) kvs) bs
  rmNavAttrs b = b

  isNavAttr :: (Text,Text) -> Bool
  isNavAttr ("nav-prev",_) = True
  isNavAttr ("nav-next",_) = True
  isNavAttr ("nav-up",_)  = True
  isNavAttr ("nav-path",_)  = True
  isNavAttr _ = False

  fixInternalRefs :: Inline -> Inline
  fixInternalRefs il@(Link attr ils (src,tit))
    = case T.uncons src of
        Just ('#', ident) -> Link attr ils (src', tit)
          where src' = case M.lookup ident refMap of
                         Just fp -> T.pack fp <> src
                         Nothing -> src
        _ -> il
  fixInternalRefs il = il

  refMap = foldr chunkToRefs mempty (chunkedChunks chunkedDoc)

  chunkToRefs chunk m =
    let idents = chunkId chunk : getIdents (chunkContents chunk)
    in  foldr (\ident -> M.insert ident (chunkPath chunk)) m idents

  getIdents bs = query getBlockIdent bs ++ query getInlineIdent bs

  getBlockIdent :: Block -> [Text]
  getBlockIdent (Div (ident, _, _) _)
    | not (T.null ident) = [ident]
  getBlockIdent (Header _ (ident, _, _) _)
    | not (T.null ident) = [ident]
  getBlockIdent (Table (ident,_,_) _ _ _ _ _)
    | not (T.null ident) = [ident]
  getBlockIdent (RawBlock fmt raw)
    | isHtmlFormat fmt
    = foldr (\tag ->
                case tag of
                  TagOpen{} ->
                    case fromAttrib "id" tag of
                      "" -> id
                      x  -> (x:)
                  _ -> id)
        [] (parseTags raw)
  getBlockIdent _ = []

  getInlineIdent :: Inline -> [Text]
  getInlineIdent (Span (ident, _, _) _)
    | not (T.null ident) = [ident]
  getInlineIdent (Link (ident, _, _) _ _)
    | not (T.null ident) = [ident]
  getInlineIdent (Image (ident, _, _) _ _)
    | not (T.null ident) = [ident]
  getInlineIdent (RawInline fmt raw)
    | isHtmlFormat fmt
    = foldr (\tag ->
                case tag of
                  TagOpen{} ->
                    case fromAttrib "id" tag of
                      "" -> id
                      x  -> (x:)
                  _ -> id)
        [] (parseTags raw)
  getInlineIdent _ = []

  isHtmlFormat :: Format -> Bool
  isHtmlFormat (Format "html") = True
  isHtmlFormat (Format "html4") = True
  isHtmlFormat (Format "html5") = True
  isHtmlFormat _ = False


makeChunks :: Int -> PathTemplate -> [Block] -> [Chunk]
makeChunks chunkLevel pathTemplate = secsToChunks 1
 where
  isChunkHeader :: Block -> Bool
  isChunkHeader (Div (_,"section":_,_) (Header n _ _:_)) = n <= chunkLevel
  isChunkHeader _ = False

  secsToChunks :: Int -> [Block] -> [Chunk]
  secsToChunks chunknum bs =
    case break isChunkHeader bs of
      ([], []) -> []
      ([], (d@(Div attr@(_,"section":_,_) (h@(Header lvl _ _) : bs')) : rest))
        | chunkLevel == lvl ->
          -- If the header is of the same level as chunks, create a chunk
          toChunk chunknum d :
            secsToChunks (chunknum + 1) rest
        | chunkLevel > lvl ->
          case break isChunkHeader bs' of
            (xs, ys) -> toChunk chunknum (Div attr (h:xs)) :
                          secsToChunks (chunknum + 1) (ys ++ rest)
      (xs, ys) -> toChunk chunknum
                     (Div ("",["preamble"],[]) xs) :
                    secsToChunks (chunknum + 1) ys

  toChunk :: Int -> Block -> Chunk
  toChunk chunknum
    (Div (divid,"section":classes,kvs) (h@(Header _ _ ils) : bs)) =
    Chunk
      { chunkHeading = ils
      , chunkId = divid
      , chunkNumber = chunknum
      , chunkPath = chunkpath
      , chunkUp = lookup "nav-up" kvs
      , chunkPrev = lookup "nav-prev" kvs
      , chunkNext = lookup "nav-next" kvs
      , chunkContents =
         [Div (divid,"section":classes,kvs') (h : bs)]
      }
     where kvs' = kvs ++ [("nav-path", T.pack chunkpath)]
           chunkpath = resolvePathTemplate pathTemplate chunknum
                        (stringify ils)
                        divid
                        (fromMaybe "" (lookup "number" kvs))
  toChunk chunknum (Div ("",["preamble"],[]) bs) =
    Chunk
      { chunkHeading = []
      , chunkId = ""
      , chunkNumber = chunknum
      , chunkPath = resolvePathTemplate pathTemplate chunknum
                        "" "" ""
      , chunkUp = Nothing
      , chunkPrev = Nothing
      , chunkNext = Nothing
      , chunkContents = bs
      }
  toChunk _ b = error $ "toChunk called on inappropriate block " <> show b
  -- should not happen

-- | Add nav-up, nav-prev, nav-next attributes to each section Div
-- in a document.
addNavigation :: Maybe Text -> Maybe Text -> [Block] -> [Block]
addNavigation mbUpId mbPrevId (Div (ident, "section":classes, kvs) bs : xs) =
  Div (ident, "section":classes, kvs ++ navattrs) bs' :
    addNavigation mbUpId (Just ident) xs
 where
  bs' = addNavigation (Just ident) Nothing bs
  navattrs = maybe [] (\x -> [("nav-up", x)]) mbUpId
          ++ maybe [] (\x -> [("nav-prev", x)]) mbPrevId
          ++ maybe [] (\x -> [("nav-next", x)]) mbNextId
  mbNextId = find isSectionDiv bs >>= extractId
  isSectionDiv (Div (_,"section":_,_) _) = True
  isSectionDiv _ = False
  extractId (Div (id',_,_) _) = Just id'
  extractId _ = Nothing
addNavigation mbUpId mbPrevId (x:xs) = x : addNavigation mbUpId mbPrevId xs
addNavigation _ _ [] = []

resolvePathTemplate :: PathTemplate
                    -> Int -- ^ Chunk number
                    -> Text -- ^ Stringified heading text
                    -> Text -- ^ Section identifier
                    -> Text -- ^ Section number
                    -> FilePath
resolvePathTemplate (PathTemplate templ) chunknum headingText ident secnum =
  T.unpack .
  T.replace "%n" (T.pack $ printf "%03d" chunknum) .
  T.replace "%s" secnum .
  T.replace "%h" headingText .
  T.replace "%i" ident $
  templ

-- | A 'PathTemplate' is a FilePath in which certain codes
-- will be substituted with information from a 'Chunk'.
-- @%n@ will be replaced with the chunk number
-- (padded with leading 0s to 3 digits),
-- @%s@ with the section number of the heading,
-- @%h@ with the (stringified) heading text,
-- @%i@ with the section identifier.
-- For example, @"section-%s-%i.html"@ might be resolved to
-- @"section-1.2-introduction.html"@.
newtype PathTemplate =
  PathTemplate { unPathTemplate :: Text }
  deriving (Show, IsString)

-- | A part of a document (typically a chapter or section, or
-- the part of a section before its subsections).
data Chunk =
  Chunk
  { chunkHeading :: [Inline]
  , chunkId :: Text
  , chunkNumber :: Int
  , chunkPath :: FilePath
  , chunkUp :: Maybe Text
  , chunkPrev :: Maybe Text
  , chunkNext :: Maybe Text
  , chunkContents :: [Block]
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Walkable Inline Chunk where
  query f chunk = query f (chunkContents chunk)
  walk f chunk = chunk{ chunkContents = walk f (chunkContents chunk) }
  walkM f chunk = do
    contents <- walkM f (chunkContents chunk)
    return chunk{ chunkContents = contents }

instance Walkable Block Chunk where
  query f chunk = query f (chunkContents chunk)
  walk f chunk = chunk{ chunkContents = walk f (chunkContents chunk) }
  walkM f chunk = do
    contents <- walkM f (chunkContents chunk)
    return chunk{ chunkContents = contents }

-- | A 'Pandoc' broken into 'Chunk's for writing to separate files.
data ChunkedDoc =
  ChunkedDoc
  { chunkedMeta :: Meta
  , chunkedTOC :: Tree SecInfo
  , chunkedChunks :: [Chunk]
  }

instance Walkable Inline ChunkedDoc where
  query f doc = query f (chunkedChunks doc) <> query f (chunkedMeta doc)
  walk f doc = doc{ chunkedMeta = walk f (chunkedMeta doc)
                  , chunkedChunks = walk f (chunkedChunks doc)
                  }
  walkM f doc = do
    meta' <- walkM f (chunkedMeta doc)
    chunks' <- walkM f (chunkedChunks doc)
    return $ doc{ chunkedMeta = meta'
                , chunkedChunks = chunks' }

instance Walkable Block ChunkedDoc where
  query f doc = query f (chunkedChunks doc) <> query f (chunkedMeta doc)
  walk f doc = doc{ chunkedMeta = walk f (chunkedMeta doc)
                  , chunkedChunks = walk f (chunkedChunks doc)
                  }
  walkM f doc = do
    meta' <- walkM f (chunkedMeta doc)
    chunks' <- walkM f (chunkedChunks doc)
    return $ doc{ chunkedMeta = meta'
                , chunkedChunks = chunks' }


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
   Module      : Text.Pandoc.Chunks
   Copyright   : Copyright (C) 2022-2023 John MacFarlane
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
  , toTOCTree
  , tocToList
  , SecInfo(..)
  ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared (makeSections, stringify, inlineListToIdentifier)
import Text.Pandoc.Walk (Walkable(..))
import Data.Text (Text)
import Text.Printf (printf)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.HTML.TagSoup (Tag (TagOpen), fromAttrib, parseTags)
import Data.Tree (Tree(..))

-- | Split 'Pandoc' into 'Chunk's, e.g. for conversion into
-- a set of HTML pages or EPUB chapters.
splitIntoChunks :: PathTemplate -- ^ Template for filepath
                -> Bool -- ^ Number sections
                -> Maybe Int -- ^ Base heading level
                -> Int -- ^ Chunk level -- level of section to split at
                -> Pandoc
                -> ChunkedDoc
splitIntoChunks pathTemplate numberSections mbBaseLevel
                chunklev (Pandoc meta blocks) =
   addNav .
   fixInternalReferences .
   walk rmNavAttrs .
   (\chunks -> ChunkedDoc{ chunkedMeta = meta
                         , chunkedChunks = chunks
                         , chunkedTOC = toTOCTree' chunks }) .
   makeChunks chunklev pathTemplate meta .
   makeSections numberSections mbBaseLevel $ blocks

-- | Add chunkNext, chunkPrev, chunkUp
addNav :: ChunkedDoc -> ChunkedDoc
addNav chunkedDoc =
  chunkedDoc{ chunkedChunks =
     addNext . addPrev . addUp $ chunkedChunks chunkedDoc }

addUp :: [Chunk] -> [Chunk]
addUp (c : d : ds)
  | chunkLevel c < chunkLevel d
    = c : addUp (d{ chunkUp = Just c } : ds)
  | chunkLevel c == chunkLevel d
    = c : addUp (d{ chunkUp = chunkUp c} : ds)
addUp (c:cs) = c : addUp cs
addUp [] = []

addNext :: [Chunk] -> [Chunk]
addNext cs = zipWith go cs (map Just (tail cs) ++ [Nothing])
 where
  go c nxt = c{ chunkNext = nxt }

addPrev :: [Chunk] -> [Chunk]
addPrev cs = zipWith go cs (Nothing : map Just cs)
 where
  go c prev = c{ chunkPrev = prev }

-- | Fix internal references so they point to the path of the chunk.
fixInternalReferences :: ChunkedDoc -> ChunkedDoc
fixInternalReferences chunkedDoc = walk fixInternalRefs chunkedDoc
 where
  fixInternalRefs :: Inline -> Inline
  fixInternalRefs il@(Link attr ils (src,tit))
    = case T.uncons src of
        Just ('#', ident) -> Link attr ils (src', tit)
          where src' = case M.lookup ident refMap of
                         Just chunk -> T.pack (chunkPath chunk) <> src
                         Nothing -> src
        _ -> il
  fixInternalRefs il = il

  refMap = foldr chunkToRefs mempty (chunkedChunks chunkedDoc)

  chunkToRefs chunk m =
    let idents = chunkId chunk : getIdents (chunkContents chunk)
    in  foldr (\ident -> M.insert ident chunk) m idents

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


makeChunks :: Int -> PathTemplate -> Meta -> [Block] -> [Chunk]
makeChunks chunklev pathTemplate meta = secsToChunks 1
 where
  isChunkHeader :: Block -> Bool
  isChunkHeader (Div (_,"section":_,_) (Header n _ _:_)) = n <= chunklev
  isChunkHeader _ = False

  secsToChunks :: Int -> [Block] -> [Chunk]
  secsToChunks chunknum bs =
    case break isChunkHeader bs of
      ([], []) -> []
      ([], (d@(Div attr@(_,"section":_,_) (h@(Header lvl _ _) : bs')) : rest))
        | chunklev == lvl ->
          -- If the header is of the same level as chunks, create a chunk
          toChunk chunknum d :
            secsToChunks (chunknum + 1) rest
        | chunklev > lvl ->
          case break isChunkHeader bs' of
            (xs, ys) -> toChunk chunknum (Div attr (h:xs)) :
                          secsToChunks (chunknum + 1) (ys ++ rest)
      (xs, ys) -> toChunk chunknum
                     (Div ("",["preamble"],[]) xs) :
                    secsToChunks (chunknum + 1) ys

  toChunk :: Int -> Block -> Chunk
  toChunk chunknum
    (Div (divid,"section":classes,kvs) (h@(Header lvl _ ils) : bs)) =
    Chunk
      { chunkHeading = ils
      , chunkId = divid
      , chunkLevel = lvl
      , chunkNumber = chunknum
      , chunkSectionNumber = secnum
      , chunkPath = chunkpath
      , chunkUp = Nothing
      , chunkNext = Nothing
      , chunkPrev = Nothing
      , chunkUnlisted = "unlisted" `elem` classes
      , chunkContents =
         [Div (divid,"section":classes,kvs') (h : bs)]
      }
     where kvs' = kvs ++ [("nav-path", T.pack chunkpath)]
           secnum = lookup "number" kvs
           chunkpath = resolvePathTemplate pathTemplate chunknum
                        (stringify ils)
                        divid
                        (fromMaybe "" secnum)
  toChunk chunknum (Div ("",["preamble"],[]) bs) =
    Chunk
      { chunkHeading = docTitle meta
      , chunkId = inlineListToIdentifier mempty $ docTitle meta
      , chunkLevel = 0
      , chunkNumber = chunknum
      , chunkSectionNumber = Nothing
      , chunkPath = resolvePathTemplate pathTemplate chunknum
                        (stringify (docTitle meta))
                        (inlineListToIdentifier mempty (docTitle meta))
                        "0"
      , chunkUp = Nothing
      , chunkPrev = Nothing
      , chunkNext = Nothing
      , chunkUnlisted = False
      , chunkContents = bs
      }
  toChunk _ b = error $ "toChunk called on inappropriate block " <> show b
  -- should not happen


-- Remove some attributes we added just to construct chunkNext etc.
rmNavAttrs :: Block -> Block
rmNavAttrs (Div (ident,classes,kvs) bs) =
  Div (ident,classes,filter (not . isNavAttr) kvs) bs
 where
  isNavAttr (k,_) = "nav-" `T.isPrefixOf` k
rmNavAttrs b = b

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
  , chunkLevel :: Int
  , chunkNumber :: Int
  , chunkSectionNumber :: Maybe Text
  , chunkPath :: FilePath
  , chunkUp :: Maybe Chunk
  , chunkPrev :: Maybe Chunk
  , chunkNext :: Maybe Chunk
  , chunkUnlisted :: Bool
  , chunkContents :: [Block]
  }
  deriving (Show, Eq, Generic)

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
  } deriving (Show, Eq, Generic)

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

-- | Data for a section in a hierarchical document.
data SecInfo =
  SecInfo
  { secTitle :: [Inline]
  , secNumber :: Maybe Text
  , secId :: Text
  , secPath :: Text
  , secLevel :: Int
  } deriving (Show, Eq, Generic)

instance Walkable Inline SecInfo where
  query f sec = query f (secTitle sec)
  walk f sec = sec{ secTitle = walk f (secTitle sec) }
  walkM f sec = do
    st <- walkM f (secTitle sec)
    return sec{ secTitle = st }

-- | Create tree of sections with titles, links, and numbers,
-- in a form that can be turned into a table of contents.
-- Presupposes that the '[Block]' is the output of 'makeSections'.
toTOCTree :: [Block] -> Tree SecInfo
toTOCTree =
  Node SecInfo{ secTitle = []
              , secNumber = Nothing
              , secId = ""
              , secPath = ""
              , secLevel = 0 } . foldr go []
 where
  go :: Block -> [Tree SecInfo] -> [Tree SecInfo]
  go (Div (ident,_,_) (Header lev (_,classes,kvs) ils : subsecs))
    | not (isNothing (lookup "number" kvs) && "unlisted" `elem` classes)
    = ((Node SecInfo{ secTitle = ils
                    , secNumber = lookup "number" kvs
                    , secId = ident
                    , secPath = ""
                    , secLevel = lev } (foldr go [] subsecs)) :)
  go (Div _ [d@Div{}]) = go d -- #8402
  go _ = id

toTOCTree' :: [Chunk] -> Tree SecInfo
toTOCTree' =
  Node SecInfo{ secTitle = []
              , secNumber = Nothing
              , secId = ""
              , secPath = ""
              , secLevel = 0 } . getNodes . filter (not . skippable)
 where
  skippable c = isNothing (chunkSectionNumber c) && chunkUnlisted c
  getNodes :: [Chunk] -> [Tree SecInfo]
  getNodes (c:cs) =
    let (as, bs) = span (\d -> chunkLevel d > chunkLevel c) cs
        secinfo = SecInfo{ secTitle = chunkHeading c,
                           secNumber = chunkSectionNumber c,
                           secId = chunkId c,
                           secPath = T.pack $ chunkPath c,
                           secLevel = chunkLevel c }
     in Node secinfo (getNodes as) : getNodes bs
  getNodes [] = []

-- | Creates a TOC link to the respective document section.
tocEntryToLink :: SecInfo -> [Inline]
tocEntryToLink secinfo = headerLink
 where
  addNumber  = case secNumber secinfo of
                 Just num -> (Span ("",["toc-section-number"],[])
                               [Str num] :) . (Space :)
                 Nothing -> id
  clean (Link _ xs _) = xs
  clean (Note _) = []
  clean x = [x]
  ident = secId secinfo
  headerText = addNumber $ walk (concatMap clean) (secTitle secinfo)
  headerLink = if T.null ident
                  then headerText
                  else [Link ("toc-" <> ident, [], [])
                         headerText (secPath secinfo <> "#" <> ident, "")]

-- | Generate a table of contents of the given depth.
tocToList :: Int -> Tree SecInfo -> Block
tocToList tocDepth (Node _ subtrees)
  = BulletList (toItems subtrees)
 where
  toItems = map go . filter isBelowTocDepth
  isBelowTocDepth (Node sec _) = secLevel sec <= tocDepth
  go (Node secinfo xs) =
    Plain (tocEntryToLink secinfo) : [BulletList (toItems xs) | not (null xs)]

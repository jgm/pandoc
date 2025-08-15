{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.ChunkedHTML
   Copyright   : Copyright (C) 2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to "chunked" HTML (a folder of
linked HTML documents, split by sections.
-}
module Text.Pandoc.Writers.ChunkedHTML (
  writeChunkedHTML
  ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions(..))
import Text.Pandoc.Shared (stringify, tshow)
import Text.Pandoc.Class (PandocMonad, getPOSIXTime, runPure,
                          fetchItem, insertMedia, getMediaBag)
import Text.Pandoc.MediaBag (mediaItems)
import qualified Data.ByteString.Lazy as BL
import Text.Pandoc.Chunks (splitIntoChunks, Chunk(..), ChunkedDoc(..),
                           SecInfo(..), tocToList)
import Text.Pandoc.URI (isURI)
import Data.Text (Text)
import Data.Tree
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Codec.Archive.Zip (Entry, addEntryToArchive, emptyArchive, toEntry,
                          fromArchive)
import qualified Data.Map as M
import Text.DocTemplates (Context(..), Val(..))
import Text.DocLayout (literal)
import Text.Pandoc.Writers.Shared (defField)
import Data.Aeson (toJSON, encode)
import System.FilePath (isRelative, normalise)
import Data.List (isInfixOf)
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.Templates (compileTemplate, WithDefaultPartials(..))
import Control.Monad.Except (throwError)
import Text.Pandoc.Error

-- | Splits document into HTML chunks, dividing them by section,
-- and returns a zip archive of a folder of files.
writeChunkedHTML :: PandocMonad m
                 => WriterOptions -> Pandoc -> m BL.ByteString
writeChunkedHTML opts (Pandoc meta blocks) = do
  walkM addMedia (Pandoc meta blocks)
  epochtime <- floor <$> getPOSIXTime
  let toMediaEntry (fp, _mt, bs) = toEntry fp epochtime bs
  mediaEntries <- map toMediaEntry . mediaItems <$> getMediaBag
  let chunkedDoc = splitIntoChunks (writerChunkTemplate opts)
                     True
                     (Just 1)
                     (writerSplitLevel opts)
                     (Pandoc meta blocks)
  let topChunk =
        Chunk
          { chunkHeading = docTitle meta
          , chunkId = "top"
          , chunkLevel = 0
          , chunkNumber = 0
          , chunkSectionNumber = Nothing
          , chunkPath = "index.html"
          , chunkUp = Nothing
          , chunkPrev = Nothing
          , chunkNext = case chunkedChunks chunkedDoc of
                          [] -> Nothing
                          (x:_) -> Just x
          , chunkUnlisted = True
          , chunkContents = mempty
          }

  let chunks = map (\x -> case chunkUp x of
                             Nothing -> x{ chunkUp = Just topChunk }
                             _ -> x)
               $ case chunkedChunks chunkedDoc of
                   [] -> []
                   (x:xs) -> x{ chunkPrev = Just topChunk } : xs

  let Node secinfo secs = chunkedTOC chunkedDoc
  let tocTree = Node secinfo{ secTitle = docTitle meta,
                              secPath = "index.html" } secs
  let tree = buildTOC opts tocTree
  renderedTOC <- writeHtml5String opts{ writerTemplate = Nothing }
                    (Pandoc nullMeta [tree])
  -- see #8915 -- we need to set the math variable in the top chunk:
  res <- runWithDefaultPartials $ compileTemplate "mathvar" "$math$"
  mathVar <- case res of
    Left e   -> throwError $ PandocTemplateError (T.pack e)
    Right t  -> return t
  tocMathVariable <- writeHtml5String opts{ writerTemplate = Just mathVar }
                    (Pandoc meta (tree:blocks))
  let opts' = opts{ writerVariables =
                        defField "table-of-contents" renderedTOC
                      . defField "math" tocMathVariable
                      $ writerVariables opts }
  entries <- mapM (chunkToEntry opts' meta topChunk) (topChunk : chunks)
  let sitemap = toEntry "sitemap.json" epochtime
                  (encode $ toJSON $ tocTreeToContext tocTree)
  let archive = foldr addEntryToArchive emptyArchive
                 (sitemap : entries ++ mediaEntries)
  return $ fromArchive archive


-- We include in the zip only local media that is in the working directory
-- or below.
addMedia :: PandocMonad m => Inline -> m Inline
addMedia il@(Image _ _ (src,_))
  | not (isURI src)
  , fp <- normalise (T.unpack src)
  , isRelative fp
  , not (".." `isInfixOf` fp) = do
  (bs, mbMime) <- fetchItem (T.pack fp)
  insertMedia fp mbMime (BL.fromStrict bs)
  return il
addMedia il = return il

buildTOC :: WriterOptions -> Tree SecInfo -> Block
buildTOC opts = tocToList (writerNumberSections opts) (writerTOCDepth opts)

chunkToEntry :: PandocMonad m
             => WriterOptions -> Meta -> Chunk -> Chunk -> m Entry
chunkToEntry opts meta topChunk chunk = do
  html <- writeHtml5String opts' (Pandoc meta' blocks)
  epochtime <- floor <$> getPOSIXTime
  let htmlLBS = BL.fromStrict $ TE.encodeUtf8 html
  return $ toEntry (chunkPath chunk) epochtime htmlLBS
 where
  opts' = opts{ writerVariables =
                  addContextVars opts' topChunk chunk $ writerVariables opts }
  meta' = setMeta "pagetitle" (MetaString (stringify $ chunkHeading chunk)) meta
  blocks = chunkContents chunk

tocTreeToContext :: Tree SecInfo -> Context Text
tocTreeToContext (Node secinfo subs) =
  Context $ M.fromList
  [ ("section", MapVal $ secInfoToContext secinfo)
  , ("subsections", ListVal $ map (MapVal . tocTreeToContext) subs)
  ]

secInfoToContext :: SecInfo -> Context Text
secInfoToContext sec =
  Context $ M.fromList
  [ ("title", SimpleVal $ literal $ stringify $ secTitle sec)
  , ("number", maybe NullVal (SimpleVal . literal) (secNumber sec))
  , ("id", SimpleVal $ literal $ secId sec)
  , ("path", SimpleVal $ literal $ secPath sec)
  , ("level", SimpleVal $ literal $ tshow $ secLevel sec)
  ]

addContextVars
  :: WriterOptions -> Chunk -> Chunk -> Context Text -> Context Text
addContextVars opts topChunk chunk context =
     maybe id (defField "next" . navlinks) (chunkNext chunk)
   . maybe id (defField "previous" . navlinks) (chunkPrev chunk)
   . maybe id (defField "up" . navlinks) (chunkUp chunk)
   . maybe id (defField "top" . navlinks) (if chunk == topChunk
                                              then Nothing
                                              else Just topChunk)
   . defField "toc" (chunk == topChunk && writerTableOfContents opts)
    $ context
 where
  navlinks ch = toMapVal [("url", formatPath ch), ("title", formatHeading ch)]
  toMapVal = MapVal . Context . M.fromList
  formatPath = SimpleVal . literal . T.pack . chunkPath
  formatHeading ch = SimpleVal . literal . either (const "") id . runPure $
    writeHtml5String opts{ writerTemplate = Nothing }
      (Pandoc nullMeta [Plain $ chunkHeading ch])

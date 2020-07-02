{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.Ipynb
   Copyright   : Copyright (C) 2019-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) writer for pandoc.

-}
module Text.Pandoc.Writers.Ipynb ( writeIpynb )
where
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Data.Ipynb as Ipynb
import Text.Pandoc.Walk (walkM)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Logging
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Shared (safeRead, isURI)
import Text.Pandoc.Writers.Shared (metaToContext')
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty (Config(..), defConfig,
           encodePretty', keyOrder, Indent(Spaces))
import Text.DocLayout (literal)

writeIpynb :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeIpynb opts d = do
  notebook <- pandocToNotebook opts d
  return $ TE.decodeUtf8 . BL.toStrict . encodePretty' defConfig{
             confIndent  = Spaces 1,
             confTrailingNewline = True,
             confCompare = keyOrder
               [ "cells", "nbformat", "nbformat_minor",
                 "cell_type", "output_type",
                 "execution_count", "metadata",
                 "outputs", "source",
                 "data", "name", "text" ] }
         $ notebook

pandocToNotebook :: PandocMonad m
                 => WriterOptions -> Pandoc -> m (Notebook NbV4)
pandocToNotebook opts (Pandoc meta blocks) = do
  let blockWriter bs = literal <$> writeMarkdown
           opts{ writerTemplate = Nothing } (Pandoc nullMeta bs)
  let inlineWriter ils = literal . T.stripEnd <$> writeMarkdown
           opts{ writerTemplate = Nothing } (Pandoc nullMeta [Plain ils])
  let jupyterMeta =
        case lookupMeta "jupyter" meta of
          Just (MetaMap m) -> Meta m
          _ -> mempty
  let nbformat =
         case (lookupMeta "nbformat" jupyterMeta,
               lookupMeta "nbformat_minor" jupyterMeta) of
               (Just (MetaInlines [Str "4"]), Just (MetaInlines [Str y])) ->
                 case safeRead y of
                        Just z  -> (4, z)
                        Nothing -> (4, 5)
               _                -> (4, 5) -- write as v4.5
  metadata' <- toJSON <$> metaToContext' blockWriter inlineWriter
                 (B.deleteMeta "nbformat" .
                  B.deleteMeta "nbformat_minor" $
                  jupyterMeta)
  -- convert from a Value (JSON object) to a M.Map Text Value:
  let metadata = case fromJSON metadata' of
                   Error _ -> mempty -- TODO warning here? shouldn't happen
                   Success x -> x
  cells <- extractCells opts blocks
  return $ Notebook{
       notebookMetadata = metadata
     , notebookFormat = nbformat
     , notebookCells = cells }

addAttachment :: PandocMonad m
              => Inline
              -> StateT (M.Map Text MimeBundle) m Inline
addAttachment (Image attr lab (src,tit))
  | not (isURI src) = do
  (img, mbmt) <- fetchItem src
  let mt = fromMaybe "application/octet-stream" mbmt
  modify $ M.insert src
          (MimeBundle (M.insert mt (BinaryData img) mempty))
  return $ Image attr lab ("attachment:" <> src, tit)
addAttachment x = return x

extractCells :: PandocMonad m => WriterOptions -> [Block] -> m [Ipynb.Cell a]
extractCells _ [] = return []
extractCells opts (Div (_id,classes,kvs) xs : bs)
  | "cell" `elem` classes
  , "markdown" `elem` classes = do
      let meta = pairsToJSONMeta kvs
      (newdoc, attachments) <-
        runStateT (walkM addAttachment (Pandoc nullMeta xs)) mempty
      source <- writeMarkdown opts{ writerTemplate = Nothing } newdoc
      (Ipynb.Cell{
          cellType = Markdown
        , cellSource = Source $ breakLines $ T.stripEnd source
        , cellMetadata = meta
        , cellAttachments = if M.null attachments
                               then Nothing
                               else Just attachments } :)
            <$> extractCells opts bs
  | "cell" `elem` classes
  , "code" `elem` classes = do
      let (codeContent, rest) =
            case xs of
               (CodeBlock _ t : ys) -> (t, ys)
               ys                   -> (mempty, ys)
      let meta = pairsToJSONMeta kvs
      outputs <- catMaybes <$> mapM blockToOutput rest
      let exeCount = lookup "execution_count" kvs >>= safeRead
      (Ipynb.Cell{
          cellType = Ipynb.Code {
                codeExecutionCount = exeCount
              , codeOutputs = outputs
              }
        , cellSource = Source $ breakLines codeContent
        , cellMetadata = meta
        , cellAttachments = Nothing } :) <$> extractCells opts bs
  | "cell" `elem` classes
  , "raw" `elem` classes =
      case consolidateAdjacentRawBlocks xs of
        [RawBlock (Format f) raw] -> do
          let format' =
                case T.toLower f of
                  "html"     -> "text/html"
                  "revealjs" -> "text/html"
                  "latex"    -> "text/latex"
                  "markdown" -> "text/markdown"
                  "rst"      -> "text/x-rst"
                  _          -> f
          (Ipynb.Cell{
              cellType = Raw
            , cellSource = Source $ breakLines raw
            , cellMetadata = if format' == "ipynb" -- means no format given
                                then mempty
                                else M.insert "format"
                                       (Aeson.String format') mempty
            , cellAttachments = Nothing } :) <$> extractCells opts bs
        _ -> extractCells opts bs
extractCells opts (CodeBlock (_id,classes,kvs) raw : bs)
  | "code" `elem` classes = do
      let meta = pairsToJSONMeta kvs
      let exeCount = lookup "execution_count" kvs >>= safeRead
      (Ipynb.Cell{
          cellType = Ipynb.Code {
                codeExecutionCount = exeCount
              , codeOutputs = []
              }
        , cellSource = Source $ breakLines raw
        , cellMetadata = meta
        , cellAttachments = Nothing } :) <$> extractCells opts bs
extractCells opts (b:bs) = do
      let isCodeOrDiv (CodeBlock (_,cl,_) _) = "code" `elem` cl
          isCodeOrDiv (Div (_,cl,_) _)       = "cell" `elem` cl
          isCodeOrDiv _                      = False
      let (mds, rest) = break isCodeOrDiv bs
      extractCells opts (Div ("",["cell","markdown"],[]) (b:mds) : rest)

blockToOutput :: PandocMonad m => Block -> m (Maybe (Output a))
blockToOutput (Div (_,["output","stream",sname],_) (CodeBlock _ t:_)) =
  return $ Just
         $ Stream{ streamName = sname
               , streamText = Source (breakLines t) }
blockToOutput (Div (_,["output","error"],kvs) (CodeBlock _ t:_)) =
  return $ Just
         $ Err{ errName = fromMaybe mempty (lookup "ename" kvs)
              , errValue = fromMaybe mempty (lookup "evalue" kvs)
              , errTraceback = breakLines t }
blockToOutput (Div (_,["output","execute_result"],kvs) bs) = do
  (data', metadata') <- extractData bs
  return $ Just
         $ ExecuteResult{ executeCount = fromMaybe 0 $
                          lookup "execution_count" kvs >>= safeRead
                        , executeData = data'
                        , executeMetadata = pairsToJSONMeta kvs <> metadata'}
blockToOutput (Div (_,["output","display_data"],kvs) bs) = do
  (data', metadata') <- extractData bs
  return $ Just
         $ DisplayData { displayData = data'
                       , displayMetadata = pairsToJSONMeta kvs <> metadata'}
blockToOutput _ = return Nothing

extractData :: PandocMonad m => [Block] -> m (MimeBundle, JSONMeta)
extractData bs = do
  (mmap, meta) <- foldM go mempty $ consolidateAdjacentRawBlocks bs
  return (MimeBundle mmap, meta)
  where
    go (mmap, meta) b@(Para [Image (_,_,kvs) _ (src,_)]) = do
      (img, mbmt) <- fetchItem src
      case mbmt of
        Just mt -> return
          (M.insert mt (BinaryData img) mmap,
           meta <> pairsToJSONMeta kvs)
        Nothing -> (mmap, meta) <$ report (BlockNotRendered b)
    go (mmap, meta) b@(CodeBlock (_,["json"],_) code) =
      case decode (UTF8.fromTextLazy $ TL.fromStrict code) of
        Just v  -> return
                    (M.insert "application/json" (JsonData v) mmap, meta)
        Nothing -> (mmap, meta) <$ report (BlockNotRendered b)
    go (mmap, meta) (CodeBlock ("",[],[]) code) =
       return (M.insert "text/plain" (TextualData code) mmap, meta)
    go (mmap, meta) (RawBlock (Format "html") raw) =
       return (M.insert "text/html" (TextualData raw) mmap, meta)
    go (mmap, meta) (RawBlock (Format "latex") raw) =
       return (M.insert "text/latex" (TextualData raw) mmap, meta)
    go (mmap, meta) (Div _ bs') = foldM go (mmap, meta) bs'
    go (mmap, meta) b = (mmap, meta) <$ report (BlockNotRendered b)

pairsToJSONMeta :: [(Text, Text)] -> JSONMeta
pairsToJSONMeta kvs =
  M.fromList [(k, case Aeson.decode (UTF8.fromTextLazy $ TL.fromStrict v) of
                           Just val -> val
                           Nothing  -> String v)
             | (k,v) <- kvs
             , k /= "execution_count"
             ]

consolidateAdjacentRawBlocks :: [Block] -> [Block]
consolidateAdjacentRawBlocks [] = []
consolidateAdjacentRawBlocks (RawBlock f1 x : RawBlock f2 y : xs)
  | f1 == f2
  = consolidateAdjacentRawBlocks (RawBlock f1 (x <> "\n" <> y) : xs)
consolidateAdjacentRawBlocks (x : xs) =
  x : consolidateAdjacentRawBlocks xs

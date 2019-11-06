{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers.Ipynb
   Copyright   : Copyright (C) 2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Ipynb (Jupyter notebook JSON format) reader for pandoc.
-}
module Text.Pandoc.Readers.Ipynb ( readIpynb )
where
import Prelude
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Text.Pandoc.Legacy.Options
import qualified Data.Scientific as Scientific
import qualified Text.Pandoc.Legacy.Builder as B -- TODO text: remove Legacy
import Text.Pandoc.Legacy.Logging
import Text.Pandoc.Legacy.Definition -- TODO text: remove Legacy
import Data.Ipynb as Ipynb
import Text.Pandoc.Class
import Text.Pandoc.Legacy.MIME (extensionFromMimeType)
import Text.Pandoc.UTF8
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Legacy.Error
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Aeson as Aeson
import Control.Monad.Except (throwError)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import qualified Text.Pandoc.UTF8 as UTF8

readIpynb :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readIpynb opts t = do
  let src = BL.fromStrict (TE.encodeUtf8 t)
  case eitherDecode src of
    Right (notebook4 :: Notebook NbV4) -> notebookToPandoc opts notebook4
    Left _ ->
      case eitherDecode src of
        Right (notebook3 :: Notebook NbV3) -> notebookToPandoc opts notebook3
        Left err -> throwError $ PandocIpynbDecodingError err

notebookToPandoc :: PandocMonad m
                 => ReaderOptions -> Notebook a -> m Pandoc
notebookToPandoc opts notebook = do
  let cells = notebookCells notebook
  let (fmt,fmtminor) = notebookFormat notebook
  let m = M.insert "nbformat" (MetaString $ show fmt) $
          M.insert "nbformat_minor" (MetaString $ show fmtminor) $
          jsonMetaToMeta (notebookMetadata notebook)
  let lang = case M.lookup "kernelspec" m of
                   Just (MetaMap ks) ->
                      case M.lookup "language" ks of
                         Just (MetaString l) -> l
                         _ -> "python"
                   _ -> "python"
  bs <- mconcat <$> mapM (cellToBlocks opts lang) cells
  let Pandoc _ blocks = B.doc bs
  return $ Pandoc (Meta $ M.insert "jupyter" (MetaMap m) mempty) blocks

cellToBlocks :: PandocMonad m
             => ReaderOptions -> String -> Cell a -> m B.Blocks
cellToBlocks opts lang c = do
  let Source ts = cellSource c
  let source = mconcat ts
  let kvs = jsonMetaToPairs (cellMetadata c)
  let attachments = maybe mempty M.toList $ cellAttachments c
  mapM_ addAttachment attachments
  case cellType c of
    Ipynb.Markdown -> do
      Pandoc _ bs <- walk fixImage <$> readMarkdown opts source
      return $ B.divWith ("",["cell","markdown"],kvs)
             $ B.fromList bs
    Ipynb.Heading lev -> do
      Pandoc _ bs <- readMarkdown opts
        (T.replicate lev "#" <> " " <> source)
      return $ B.divWith ("",["cell","markdown"],kvs)
             $ B.fromList bs
    Ipynb.Raw -> do
      -- we use ipynb to indicate no format given (a wildcard in nbformat)
      let format = fromMaybe "ipynb" $ lookup "format" kvs
      let format' =
            case format of
              "text/html"       -> "html"
              "text/latex"      -> "latex"
              "application/pdf" -> "latex"
              "text/markdown"   -> "markdown"
              "text/x-rsrt"     -> "rst"
              _                 -> format
      return $ B.divWith ("",["cell","raw"],kvs) $ B.rawBlock format'
             $ T.unpack source
    Ipynb.Code{ codeOutputs = outputs, codeExecutionCount = ec } -> do
      outputBlocks <- mconcat <$> mapM outputToBlock outputs
      let kvs' = maybe kvs (\x -> ("execution_count", show x):kvs) ec
      return $ B.divWith ("",["cell","code"],kvs') $
        B.codeBlockWith ("",[lang],[]) (T.unpack source)
        <> outputBlocks

-- Remove attachment: prefix from images...
fixImage :: Inline -> Inline
fixImage (Image attr lab (src,tit))
  | "attachment:" `isPrefixOf` src = Image attr lab (drop 11 src, tit)
fixImage x = x

addAttachment :: PandocMonad m => (Text, MimeBundle) -> m ()
addAttachment (fname, mimeBundle) = do
  let fp = T.unpack fname
  case M.toList (unMimeBundle mimeBundle) of
    (mimeType, BinaryData bs):_ ->
      insertMedia fp (Just $ T.unpack mimeType) (BL.fromStrict bs)
    (mimeType, TextualData t):_ ->
      insertMedia fp (Just $ T.unpack mimeType)
          (BL.fromStrict $ TE.encodeUtf8 t)
    (mimeType, JsonData v):_ ->
      insertMedia fp (Just $ T.unpack mimeType) (encode v)
    [] -> report $ CouldNotFetchResource fp "no attachment"

outputToBlock :: PandocMonad m => Output a -> m B.Blocks
outputToBlock Stream{ streamName = sName,
                      streamText = Source text } = do
  return $ B.divWith ("",["output","stream",T.unpack sName],[])
         $ B.codeBlock $ T.unpack . mconcat $ text
outputToBlock DisplayData{ displayData = data',
                            displayMetadata = metadata' } =
  B.divWith ("",["output", "display_data"],[]) <$>
    handleData metadata' data'
outputToBlock ExecuteResult{ executeCount = ec,
                              executeData = data',
                              executeMetadata = metadata' } =
  B.divWith ("",["output", "execute_result"],[("execution_count",show ec)])
    <$> handleData metadata' data'
outputToBlock Err{ errName = ename,
                   errValue = evalue,
                   errTraceback = traceback } = do
  return $ B.divWith ("",["output","error"],
                         [("ename",T.unpack ename),
                          ("evalue",T.unpack evalue)])
         $ B.codeBlock $ T.unpack . T.unlines $ traceback

-- We want to display the richest output possible given
-- the output format.
handleData :: PandocMonad m
           => JSONMeta -> MimeBundle -> m B.Blocks
handleData metadata (MimeBundle mb) =
  mconcat <$> mapM dataBlock (M.toList mb)

  where

    dataBlock :: PandocMonad m => (MimeType, MimeData) -> m B.Blocks
    dataBlock (mt, BinaryData bs)
     | "image/" `T.isPrefixOf` mt
      = do
      -- normally metadata maps from mime types to key-value map;
      -- but not always...
      let meta = case M.lookup mt metadata of
                   Just v@(Object{}) ->
                     case fromJSON v of
                       Success m' -> m'
                       Error _   -> mempty
                   _ -> mempty
      let metaPairs = jsonMetaToPairs meta
      let bl = BL.fromStrict bs
      -- SHA1 hash for filename
      let mt' = T.unpack mt
      let fname = showDigest (sha1 bl) ++
            case extensionFromMimeType mt' of
              Nothing  -> ""
              Just ext -> '.':ext
      insertMedia fname (Just mt') bl
      return $ B.para $ B.imageWith ("",[],metaPairs) fname "" mempty
     | otherwise = return mempty

    dataBlock ("text/html", TextualData t)
      = return $ B.rawBlock "html" $ T.unpack t

    dataBlock ("text/latex", TextualData t)
      = return $ B.rawBlock "latex" $ T.unpack t

    dataBlock ("text/plain", TextualData t) =
      return $ B.codeBlock $ T.unpack t

    dataBlock (_, JsonData v) =
      return $ B.codeBlockWith ("",["json"],[]) $ toStringLazy $ encode v

    dataBlock _ = return mempty

jsonMetaToMeta :: JSONMeta -> M.Map String MetaValue
jsonMetaToMeta = M.mapKeys T.unpack . M.map valueToMetaValue
  where
    valueToMetaValue :: Value -> MetaValue
    valueToMetaValue x@(Object{}) =
      case fromJSON x of
        Error s -> MetaString s
        Success jm' -> MetaMap $ jsonMetaToMeta jm'
    valueToMetaValue x@(Array{}) =
      case fromJSON x of
        Error s -> MetaString s
        Success xs -> MetaList $ map valueToMetaValue xs
    valueToMetaValue (Bool b) = MetaBool b
    valueToMetaValue (String t) = MetaString (T.unpack t)
    valueToMetaValue (Number n)
      | Scientific.isInteger n = MetaString (show (floor n :: Integer))
      | otherwise              = MetaString (show n)
    valueToMetaValue Aeson.Null = MetaString ""

jsonMetaToPairs :: JSONMeta -> [(String, String)]
jsonMetaToPairs = M.toList . M.mapKeys T.unpack . M.map
  (\case
      String t
        | not (T.all isDigit t)
        , t /= "true"
        , t /= "false"
                 -> T.unpack t
      x          -> UTF8.toStringLazy $ Aeson.encode x)

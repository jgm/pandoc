{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.App.Input
   Copyright   : © 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley@edu>

Read from the file system into a pandoc document.
-}
module Text.Pandoc.App.Input
  ( InputParameters (..)
  , readInput
  ) where

import Control.Monad ((>=>))
import Control.Monad.Except (throwError, catchError)
import Data.Text (Text)
import Network.URI (URI (..), parseURI)
import Text.Pandoc.Class ( PandocMonad, openURL
                         , readFileStrict, readStdinStrict, report)
import Text.Pandoc.Definition (Pandoc (..), Attr, Block (..), Inline (..))
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging (LogMessage (..))
import Text.Pandoc.MIME (getCharset, MimeType)
import Text.Pandoc.Options (Extensions, ReaderOptions (..))
import Text.Pandoc.Readers (Reader (..))
import Text.Pandoc.Shared (tabFilter, textToIdentifier, tshow, uriPathToPath)
import Text.Pandoc.Walk (walk)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TSE
import qualified Data.Text.Encoding.Error as TSE

-- | Settings specifying how and which input should be processed.
data InputParameters m = InputParameters
  { inputReader         :: Reader m
  , inputReaderName     :: Text
  , inputReaderOptions  :: ReaderOptions
  , inputSources        :: [FilePath]
  , inputSpacesPerTab   :: Maybe Int
  , inputFileScope      :: Bool
  }

-- | Read all input into a pandoc document.
readInput :: PandocMonad m => InputParameters m -> m Pandoc
readInput params = do
  let sources = inputSources params
  let readerName = inputReaderName params
  let readerOpts = inputReaderOptions params
  let convertTabs :: Text -> Text
      convertTabs = tabFilter $ case inputSpacesPerTab params of
        Nothing -> 0
        Just ts -> if readerName `elem` ["t2t", "man", "tsv"]
                   then 0
                   else ts

  inputs <- readSources sources

  case inputReader params of
    TextReader r
      | readerName == "json" ->
          mconcat <$> mapM (inputToText convertTabs >=> r readerOpts . (:[]))
                           inputs
      | inputFileScope params ->
          mconcat <$> mapM
              (\source -> do
                  (fp, txt) <- inputToText convertTabs source
                  adjustLinksAndIds (readerExtensions readerOpts)
                    (T.pack fp) (map (T.pack . fst) inputs)
                    <$> r readerOpts [(fp, txt)])
              inputs
      | otherwise -> mapM (inputToText convertTabs) inputs >>= r readerOpts
    ByteStringReader r ->
      mconcat <$> mapM (r readerOpts . inputToLazyByteString) inputs

readSources :: PandocMonad m
            => [FilePath] -> m [(FilePath, (BS.ByteString, Maybe MimeType))]
readSources srcs =
  mapM (\fp -> do t <- readSource fp
                  return (if fp == "-" then "" else fp, t)) srcs

-- | Read input from a resource, i.e., either a file, a URL, or stdin
-- (@-@).
readSource :: PandocMonad m
           => FilePath -> m (BS.ByteString, Maybe MimeType)
readSource "-" = (,Nothing) <$> readStdinStrict
readSource src =
  case parseURI src of
    Just u | uriScheme u `elem` ["http:","https:"] -> openURL (T.pack src)
           | uriScheme u == "file:" ->
               (,Nothing) <$>
                 readFileStrict (uriPathToPath $ T.pack $ uriPath u)
    _       -> (,Nothing) <$> readFileStrict src

utf8ToText :: PandocMonad m => FilePath -> BS.ByteString -> m Text
utf8ToText fp bs =
  case TSE.decodeUtf8' . dropBOM $ bs of
    Left (TSE.DecodeError _ (Just w)) ->
      case BS.elemIndex w bs of
        Just offset -> throwError $ PandocUTF8DecodingError (T.pack fp) offset w
        Nothing -> throwError $ PandocUTF8DecodingError (T.pack fp) 0 w
    Left e -> throwError $ PandocAppError (tshow e)
    Right t -> return t
 where
   dropBOM bs' =
     if "\xEF\xBB\xBF" `BS.isPrefixOf` bs'
        then BS.drop 3 bs'
        else bs'

inputToText :: PandocMonad m
            => (Text -> Text)
            -> (FilePath, (BS.ByteString, Maybe MimeType))
            -> m (FilePath, Text)
inputToText convTabs (fp, (bs,mt)) =
  (fp,) . convTabs . T.filter (/='\r') <$>
  case mt >>= getCharset of
    Just "UTF-8"      -> utf8ToText fp bs
    Just "ISO-8859-1" -> return $ T.pack $ B8.unpack bs
    Just charset      -> throwError $ PandocUnsupportedCharsetError charset
    Nothing           -> catchError
                           (utf8ToText fp bs)
                           (\case
                              PandocUTF8DecodingError{} -> do
                                report $ NotUTF8Encoded
                                  (if null fp
                                      then "input"
                                      else fp)
                                return $ T.pack $ B8.unpack bs
                              e -> throwError e)

inputToLazyByteString :: (FilePath, (BS.ByteString, Maybe MimeType))
                      -> BL.ByteString
inputToLazyByteString (_, (bs,_)) = BL.fromStrict bs

adjustLinksAndIds :: Extensions -> Text -> [Text] -> Pandoc -> Pandoc
adjustLinksAndIds exts thisfile allfiles
  | length allfiles > 1 = addDiv . walk fixInline . walk fixBlock
  | otherwise           = id
 where
  toIdent :: Text -> Text
  toIdent = textToIdentifier exts . T.intercalate "__" .
            T.split (\c -> c == '/' || c == '\\')

  addDiv :: Pandoc -> Pandoc
  addDiv (Pandoc m bs)
    | T.null thisfile = Pandoc m bs
    | otherwise = Pandoc m [Div (toIdent thisfile,[],[]) bs]

  fixBlock :: Block -> Block
  fixBlock (CodeBlock attr t) = CodeBlock (fixAttrs attr) t
  fixBlock (Header lev attr ils) = Header lev (fixAttrs attr) ils
  fixBlock (Table attr cap cols th tbs tf) =
     Table (fixAttrs attr) cap cols th tbs tf
  fixBlock (Div attr bs) = Div (fixAttrs attr) bs
  fixBlock x = x

  -- add thisfile as prefix of identifier
  fixAttrs :: Attr -> Attr
  fixAttrs (i,cs,kvs)
    | T.null i = (i,cs,kvs)
    | otherwise =
        (T.intercalate "__"
          (filter (not . T.null) [toIdent thisfile, i]),
        cs, kvs)

  -- if URL begins with file from allfiles, convert to
  -- an internal link with the appropriate identifier
  fixURL :: Text -> Text
  fixURL u =
    let (a,b) = T.break (== '#') u
        filepart = if T.null a
                      then toIdent thisfile
                      else toIdent a
        fragpart = T.dropWhile (== '#') b
     in if T.null a || a `elem` allfiles
           then "#" <> T.intercalate "__"
                         (filter (not . T.null) [filepart, fragpart])
           else u

  fixInline :: Inline -> Inline
  fixInline (Code attr t) = Code (fixAttrs attr) t
  fixInline (Link attr ils (url,tit)) =
    Link (fixAttrs attr) ils (fixURL url,tit)
  fixInline (Image attr ils (url,tit)) =
    Image (fixAttrs attr) ils (fixURL url,tit)
  fixInline (Span attr ils) = Span (fixAttrs attr) ils
  fixInline x = x

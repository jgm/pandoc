{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.App.Input
   Copyright   : © 2006-2024 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley@edu>

Read from the file system into a pandoc document.
-}
module Text.Pandoc.App.Input
  ( InputParameters (..)
  , readInput
  ) where

import Control.Monad ((>=>), when)
import Control.Monad.Except (throwError, catchError)
import Data.Text (Text)
import Network.URI (URI (..), parseURI)
import Text.Pandoc.Transforms (adjustLinksAndIds)
import Text.Pandoc.Class ( PandocMonad, openURL, toTextM
                         , readFileStrict, readStdinStrict, report)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging (LogMessage (..))
import Text.Pandoc.MIME (getCharset, MimeType)
import Text.Pandoc.Options (ReaderOptions (..))
import Text.Pandoc.Readers (Reader (..))
import Text.Pandoc.Shared (tabFilter)
import Text.Pandoc.URI (uriPathToPath)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

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

inputToText :: PandocMonad m
            => (Text -> Text)
            -> (FilePath, (BS.ByteString, Maybe MimeType))
            -> m (FilePath, Text)
inputToText convTabs (fp, (bs,mt)) =
  (fp,) . convTabs . T.filter (/='\r') <$>
  case mt >>= getCharset of
    Just "UTF-8"      -> toTextM fp bs
    Just "ISO-8859-1" -> return $ T.pack $ B8.unpack bs
    Just charset      -> throwError $ PandocUnsupportedCharsetError charset
    Nothing           -> catchError
                           (toTextM fp bs)
                           (\case
                              PandocUTF8DecodingError{} -> do
                                when (hasKnownSignature bs) $
                                  throwError $
                                    PandocInputNotTextError (T.pack fp)
                                report $ NotUTF8Encoded
                                  (if null fp
                                      then "input"
                                      else fp)
                                return $ T.pack $ B8.unpack bs
                              e -> throwError e)
 where
  -- "50 4B 03 04" is zip file signature
  isZip bs' = "\x50\x4B\x03\x04" `BS.isPrefixOf` bs'
  -- "25 50 44 46 2D" is PDF file signature
  isPDF bs' = "\x25\x50\x44\x46\x2D" `BS.isPrefixOf` bs'
  -- "D0 CF 11 E0 A1 B1 1A E1" is Compound File Binary Format signature used in
  -- variety of old Microsoft formats (.doc and .xls among others)
  isCFBF bs' = "\xD0\xCF\x11\xE0\xA1\xB1\x1A\xE1" `BS.isPrefixOf` bs'
  -- "41 54 26 54 46 4F 52 4D ?? ?? ?? ?? 44 4A 56" is DjVu signature
  isDjVu bs' = case BS.stripPrefix "\x41\x54\x26\x54\x46\x4F\x52\x4D" bs' of
    Nothing -> False
    Just x -> BS.isPrefixOf "\x44\x4A\x56" $ BS.drop 4 x

  hasKnownSignature bs' = any ($ bs') [isZip, isPDF, isCFBF, isDjVu]

inputToLazyByteString :: (FilePath, (BS.ByteString, Maybe MimeType))
                      -> BL.ByteString
inputToLazyByteString (_, (bs,_)) = BL.fromStrict bs

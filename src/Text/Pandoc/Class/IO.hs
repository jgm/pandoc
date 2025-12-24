{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Class.IO
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

Default ways to perform @'PandocMonad'@ actions in a @'MonadIO'@ type.

These functions are used to make the @'PandocIO'@ type an instance of
@'PandocMonad'@, but can be reused for any other MonadIO-conforming
types.
-}
module Text.Pandoc.Class.IO
  ( fileExists
  , getCurrentTime
  , getCurrentTimeZone
  , getDataFileName
  , getModificationTime
  , glob
  , logOutput
  , logIOError
  , lookupEnv
  , newStdGen
  , newUniqueHash
  , openURL
  , readFileLazy
  , readFileStrict
  , readStdinStrict
  , extractMedia
  , writeMedia
 ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, pack, unpack)
import Data.Time (TimeZone, UTCTime)
import Data.Unique (hashUnique)
#ifdef PANDOC_HTTP_SUPPORT
import Data.ByteString.Lazy (toChunks)
import System.Environment (getEnv)
import Data.Default (def)
import Network.Connection (TLSSettings(..))
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import System.X509 (getSystemCertificateStore)
import Network.HTTP.Client
       (httpLbs, Manager, responseBody, responseHeaders,
        Request(port, host, requestHeaders), parseUrlThrow, newManager, HttpException)
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Header ( hContentType )
import Network.Socket (withSocketsDo)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad ( getsCommonState, modifyCommonState )
import qualified Data.CaseInsensitive as CI
#endif
import Network.URI (URI(..), parseURI, unEscapeString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, normalise)
import qualified System.FilePath.Posix as Posix
import System.IO (stderr)
import System.IO.Error
import System.Random (StdGen)
import Text.Pandoc.Class.PandocMonad
       (PandocMonad,
        getMediaBag, report, extractURIData)
import Text.Pandoc.Definition (Pandoc, Inline (Image))
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging (LogMessage (..), messageVerbosity, showLogMessage)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.MediaBag (MediaBag, MediaItem(..), lookupMedia, mediaItems)
import Text.Pandoc.Walk (walk)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Time
import qualified Data.Time.LocalTime
import qualified Data.Unique
import qualified System.Directory
import qualified System.Environment as Env
import qualified System.FilePath.Glob
import qualified System.Random
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef EMBED_DATA_FILES
import qualified Paths_pandoc as Paths
#endif

-- | Utility function to lift IO errors into 'PandocError's.
liftIOError :: (PandocMonad m, MonadIO m) => (String -> IO a) -> String -> m a
liftIOError f u = do
  res <- liftIO $ tryIOError $ f u
  case res of
         Left e  -> throwError $ PandocIOError (pack u) e
         Right r -> return r

-- | Show potential IO errors to the user continuing execution anyway
logIOError :: (PandocMonad m, MonadIO m) => IO () -> m ()
logIOError f = do
  res <- liftIO $ tryIOError f
  case res of
    Left e -> report $ IgnoredIOError $ pack $ E.displayException e
    Right _ -> pure ()

-- | Lookup an environment variable in the programs environment.
lookupEnv :: MonadIO m => Text -> m (Maybe Text)
lookupEnv = fmap (fmap pack) . liftIO . Env.lookupEnv . unpack

-- | Get the current (UTC) time.
getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

-- | Get the locale's time zone.
getCurrentTimeZone :: MonadIO m => m TimeZone
getCurrentTimeZone = liftIO Data.Time.LocalTime.getCurrentTimeZone

-- | Return a new generator for random numbers.
newStdGen :: MonadIO m => m StdGen
newStdGen = liftIO System.Random.newStdGen

-- | Return a new unique integer.
newUniqueHash :: MonadIO m => m Int
newUniqueHash = hashUnique <$> liftIO Data.Unique.newUnique

#ifdef PANDOC_HTTP_SUPPORT
getManager :: (PandocMonad m, MonadIO m) => m Manager
getManager = do
  mbManager <- getsCommonState stManager
  disableCertificateValidation <- getsCommonState stNoCheckCertificate
  case mbManager of
    Just manager -> pure manager
    Nothing -> do
      manager <- liftIO $ do
        certificateStore <- getSystemCertificateStore
        let tlsSettings = TLSSettings $
               (TLS.defaultParamsClient "localhost.localdomain" "80")
                  { TLS.clientSupported = def{ TLS.supportedCiphers =
                                               TLS.ciphersuite_default
                                             , TLS.supportedExtendedMainSecret =
                                                TLS.AllowEMS }
                  , TLS.clientShared = def
                      { TLS.sharedCAStore = certificateStore
                      , TLS.sharedValidationCache =
                          if disableCertificateValidation
                             then TLS.ValidationCache
                                   (\_ _ _ -> return TLS.ValidationCachePass)
                                   (\_ _ _ -> return ())
                             else def
                      }
                  }
        let tlsManagerSettings = mkManagerSettings tlsSettings  Nothing
        newManager tlsManagerSettings
      modifyCommonState $ \st -> st{ stManager = Just manager }
      pure manager
#endif

openURL :: (PandocMonad m, MonadIO m) => Text -> m (B.ByteString, Maybe MimeType)
openURL u
 | Just (URI{ uriScheme = "data:",
              uriPath = upath }) <- parseURI (T.unpack u)
     = pure $ extractURIData upath
#ifdef PANDOC_HTTP_SUPPORT
 | otherwise = do
     let toReqHeader (n, v) = (CI.mk (UTF8.fromText n), UTF8.fromText v)
     customHeaders <- map toReqHeader <$> getsCommonState stRequestHeaders
     report $ Fetching u
     manager <- getManager
     res <- liftIO $ E.try $ withSocketsDo $ do
       proxy <- tryIOError (getEnv "http_proxy")
       let addProxy' x = case proxy of
                            Left _ -> return x
                            Right pr -> parseUrlThrow pr >>= \r ->
                                return (addProxy (host r) (port r) x)
       req <- parseUrlThrow (unpack u) >>= addProxy'
       let req' = req{requestHeaders = customHeaders ++ requestHeaders req}
       resp <- httpLbs req' manager
       return (B.concat $ toChunks $ responseBody resp,
               UTF8.toText `fmap` lookup hContentType (responseHeaders resp))

     case res of
          Right r -> return r
          Left (e :: HttpException)
                  -> throwError $ PandocHttpError u (T.pack (show e))
#else
 | otherwise =
     throwError $ PandocHttpError u "pandoc was compiled without HTTP support"
#endif

-- | Read the lazy ByteString contents from a file path, raising an error on
-- failure.
readFileLazy :: (PandocMonad m, MonadIO m) => FilePath -> m BL.ByteString
readFileLazy s = liftIOError BL.readFile s

-- | Read the strict ByteString contents from a file path,
-- raising an error on failure.
readFileStrict :: (PandocMonad m, MonadIO m) => FilePath -> m B.ByteString
readFileStrict s = liftIOError B.readFile s

-- | Read the strict ByteString contents from stdin, raising
-- an error on failure.
readStdinStrict :: (PandocMonad m, MonadIO m) => m B.ByteString
readStdinStrict = liftIOError (const B.getContents) "stdin"

-- | Return a list of paths that match a glob, relative to the working
-- directory. See 'System.FilePath.Glob' for the glob syntax.
glob :: (PandocMonad m, MonadIO m) => String -> m [FilePath]
glob = liftIOError System.FilePath.Glob.glob

-- | Returns True if file exists.
fileExists :: (PandocMonad m, MonadIO m) => FilePath -> m Bool
fileExists = liftIOError System.Directory.doesFileExist

-- | Returns the path of data file.
getDataFileName :: (PandocMonad m, MonadIO m) => FilePath -> m FilePath
#ifdef EMBED_DATA_FILES
getDataFileName = return
#else
getDataFileName = liftIOError Paths.getDataFileName
#endif

-- | Return the modification time of a file.
getModificationTime :: (PandocMonad m, MonadIO m) => FilePath -> m UTCTime
getModificationTime = liftIOError System.Directory.getModificationTime

-- | Output a log message.
logOutput :: (PandocMonad m, MonadIO m) => LogMessage -> m ()
logOutput msg = liftIO $ do
  UTF8.hPutStr stderr $
      "[" <> T.pack (show (messageVerbosity msg)) <> "] "
  alertIndent $ T.lines $ showLogMessage msg

-- | Prints the list of lines to @stderr@, indenting every but the first
-- line by two spaces.
alertIndent :: [Text] -> IO ()
alertIndent [] = return ()
alertIndent (l:ls) = do
  UTF8.hPutStrLn stderr l
  mapM_ go ls
  where go l' = do UTF8.hPutStr stderr "  "
                   UTF8.hPutStrLn stderr l'

-- | Extract media from the mediabag into a directory.
extractMedia :: (PandocMonad m, MonadIO m) => FilePath -> Pandoc -> m Pandoc
extractMedia dir d = do
  media <- getMediaBag
  let items = mediaItems media
  if null items
    then return d
    else do
      mapM_ (writeMedia dir) items
      return $ walk (adjustImagePath dir media) d

-- | Write the contents of a media bag to a path.
-- If the path contains URI escape sequences (percent-encoding),
-- these are resolved.
writeMedia :: (PandocMonad m, MonadIO m)
           => FilePath
           -> (FilePath, MimeType, BL.ByteString)
           -> m ()
writeMedia dir (fp, _mt, bs) = do
  -- we normalize to get proper path separators for the platform
  -- we unescape URI encoding, but given how insertMedia
  -- is written, we shouldn't have any % in a canonical media name...
  let fullpath = normalise $ dir </> unEscapeString fp
  liftIOError (createDirectoryIfMissing True) (takeDirectory fullpath)
  report $ Extracting (T.pack fullpath)
  logIOError $ BL.writeFile fullpath bs

-- | If the given Inline element is an image with a @src@ path equal to
-- one in the list of @paths@, then prepends @dir@ to the image source;
-- returns the element unchanged otherwise.
adjustImagePath :: FilePath -> MediaBag -> Inline -> Inline
adjustImagePath dir mediabag (Image attr lab (src, tit)) =
  case lookupMedia (T.unpack src) mediabag of
    Nothing -> Image attr lab (src, tit)
    Just item ->
      let fullpath = dir Posix.</> mediaPath item
      in  Image attr lab (T.pack fullpath, tit)
adjustImagePath _ _ x = x

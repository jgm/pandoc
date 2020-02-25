{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Class.PandocIO
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

This module defines @'PandocIO'@, an IO-based instance of the
@'PandocMonad'@ type class. File, data, and network access all are run
using IO operators.
-}
module Text.Pandoc.Class.PandocIO
  ( getPOSIXTime
  , getZonedTime
  , readFileFromDirs
  , report
  , setTrace
  , setRequestHeader
  , getLog
  , setVerbosity
  , getVerbosity
  , getMediaBag
  , setMediaBag
  , insertMedia
  , setUserDataDir
  , getUserDataDir
  , fetchItem
  , getInputFiles
  , setInputFiles
  , getOutputFile
  , setOutputFile
  , setResourcePath
  , getResourcePath
  , PandocIO(..)
  , runIO
  , runIOorExplode
  , extractMedia
 ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Lazy (toChunks)
import Data.Default
import Data.Text (Text)
import Data.Unique (hashUnique)
import Network.HTTP.Client
       (httpLbs, responseBody, responseHeaders,
        Request(port, host, requestHeaders), parseRequest, newManager)
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Types.Header ( hContentType )
import Network.Socket (withSocketsDo)
import Network.URI ( unEscapeString )
import Prelude
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>), takeDirectory, normalise)
import System.IO (stderr)
import System.IO.Error
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Text.Pandoc.Walk (walk)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Time as IO (getCurrentTime)
import qualified Data.Time.LocalTime as IO (getCurrentTimeZone)
import qualified Data.Unique as IO (newUnique)
import qualified System.Directory as Directory
import qualified System.Directory as IO (getModificationTime)
import qualified System.Environment as IO (lookupEnv)
import qualified System.FilePath.Glob as IO (glob)
import qualified System.Random as IO (newStdGen)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef EMBED_DATA_FILES
import qualified Paths_pandoc as Paths
#endif

-- | Evaluate a 'PandocIO' operation.
runIO :: PandocIO a -> IO (Either PandocError a)
runIO ma = flip evalStateT def $ runExceptT $ unPandocIO ma

-- | Evaluate a 'PandocIO' operation, handling any errors
-- by exiting with an appropriate message and error status.
runIOorExplode :: PandocIO a -> IO a
runIOorExplode ma = runIO ma >>= handleError

newtype PandocIO a = PandocIO {
  unPandocIO :: ExceptT PandocError (StateT CommonState IO) a
  } deriving ( MonadIO
             , Functor
             , Applicative
             , Monad
             , MonadError PandocError
             )

-- | Utility function to lift IO errors into 'PandocError's.
liftIOError :: (String -> IO a) -> String -> PandocIO a
liftIOError f u = do
  res <- liftIO $ tryIOError $ f u
  case res of
         Left e  -> throwError $ PandocIOError (T.pack u) e
         Right r -> return r

-- | Show potential IO errors to the user continuing execution anyway
logIOError :: IO () -> PandocIO ()
logIOError f = do
  res <- liftIO $ tryIOError f
  case res of
    Left e -> report $ IgnoredIOError $ T.pack $ E.displayException e
    Right _ -> pure ()

instance PandocMonad PandocIO where
  lookupEnv = fmap (fmap T.pack) . liftIO . IO.lookupEnv . T.unpack
  getCurrentTime = liftIO IO.getCurrentTime
  getCurrentTimeZone = liftIO IO.getCurrentTimeZone
  newStdGen = liftIO IO.newStdGen
  newUniqueHash = hashUnique <$> liftIO IO.newUnique

  openURL u
   | Just u'' <- T.stripPrefix "data:" u = do
       let mime     = T.takeWhile (/=',') u''
       let contents = UTF8.fromString $
                       unEscapeString $ T.unpack $ T.drop 1 $ T.dropWhile (/=',') u''
       return (decodeLenient contents, Just mime)
   | otherwise = do
       let toReqHeader (n, v) = (CI.mk (UTF8.fromText n), UTF8.fromText v)
       customHeaders <- map toReqHeader <$> getsCommonState stRequestHeaders
       disableCertificateValidation <- getsCommonState stNoCheckCertificate
       report $ Fetching u
       res <- liftIO $ E.try $ withSocketsDo $ do
         let parseReq = parseRequest
         proxy <- tryIOError (getEnv "http_proxy")
         let addProxy' x = case proxy of
                              Left _ -> return x
                              Right pr -> parseReq pr >>= \r ->
                                  return (addProxy (host r) (port r) x)
         req <- parseReq (T.unpack u) >>= addProxy'
         let req' = req{requestHeaders = customHeaders ++ requestHeaders req}
         resp <- newManager (mkManagerSettings  (TLSSettingsSimple disableCertificateValidation False False) Nothing) >>= httpLbs req'
         return (B.concat $ toChunks $ responseBody resp,
                 UTF8.toText `fmap` lookup hContentType (responseHeaders resp))

       case res of
            Right r -> return r
            Left e  -> throwError $ PandocHttpError u e

  readFileLazy s = liftIOError BL.readFile s
  readFileStrict s = liftIOError B.readFile s

  glob = liftIOError IO.glob
  fileExists = liftIOError Directory.doesFileExist
#ifdef EMBED_DATA_FILES
  getDataFileName = return
#else
  getDataFileName = liftIOError Paths.getDataFileName
#endif
  getModificationTime = liftIOError IO.getModificationTime
  getCommonState = PandocIO $ lift get
  putCommonState x = PandocIO $ lift $ put x
  logOutput msg = liftIO $ do
    UTF8.hPutStr stderr $
        "[" ++ show (messageVerbosity msg) ++ "] "
    alertIndent $ T.lines $ showLogMessage msg

-- | Prints the list of lines to @stderr@, indenting every but the first
-- line by two spaces.
alertIndent :: [Text] -> IO ()
alertIndent [] = return ()
alertIndent (l:ls) = do
  UTF8.hPutStrLn stderr $ T.unpack l
  mapM_ go ls
  where go l' = do UTF8.hPutStr stderr "  "
                   UTF8.hPutStrLn stderr $ T.unpack l'

-- | Extract media from the mediabag into a directory.
extractMedia :: FilePath -> Pandoc -> PandocIO Pandoc
extractMedia dir d = do
  media <- getMediaBag
  case [fp | (fp, _, _) <- mediaDirectory media] of
        []  -> return d
        fps -> do
          mapM_ (writeMedia dir media) fps
          return $ walk (adjustImagePath dir fps) d

-- | Write the contents of a media bag to a path.
writeMedia :: FilePath -> MediaBag -> FilePath -> PandocIO ()
writeMedia dir mediabag subpath = do
  -- we join and split to convert a/b/c to a\b\c on Windows;
  -- in zip containers all paths use /
  let fullpath = dir </> unEscapeString (normalise subpath)
  let mbcontents = lookupMedia subpath mediabag
  case mbcontents of
       Nothing -> throwError $ PandocResourceNotFound $ T.pack subpath
       Just (_, bs) -> do
         report $ Extracting $ T.pack fullpath
         liftIOError (createDirectoryIfMissing True) (takeDirectory fullpath)
         logIOError $ BL.writeFile fullpath bs

-- | If the given Inline element is an image with a @src@ path equal to
-- one in the list of @paths@, then prepends @dir@ to the image source;
-- returns the element unchanged otherwise.
adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | T.unpack src `elem` paths = Image attr lab (T.pack dir <> "/" <> src, tit)
adjustImagePath _ _ x = x

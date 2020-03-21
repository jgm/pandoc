{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Class
   Copyright   : Copyright (C) 2016-17 Jesse Rosenthal, John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

This module defines a type class, 'PandocMonad', for pandoc readers
and writers. A pure instance 'PandocPure' and an impure instance
'PandocIO' are provided.  This allows users of the library to choose
whether they want conversions to perform IO operations (such as
reading include files or images).
-}

module Text.Pandoc.Class ( PandocMonad(..)
                         , CommonState(..)
                         , PureState(..)
                         , getPureState
                         , getsPureState
                         , putPureState
                         , modifyPureState
                         , getPOSIXTime
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
                         , PandocPure(..)
                         , FileTree
                         , FileInfo(..)
                         , addToFileTree
                         , insertInFileTree
                         , runIO
                         , runIOorExplode
                         , runPure
                         , readDefaultDataFile
                         , readDataFile
                         , fetchMediaResource
                         , fillMediaBag
                         , extractMedia
                         , toLang
                         , setTranslations
                         , translateTerm
                         , Translations
                         ) where

import Codec.Archive.Zip
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString.Base64 (decodeLenient)
import Data.ByteString.Lazy (toChunks)
import Data.Default
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Time.LocalTime (TimeZone, utc)
import Data.Unique (hashUnique)
import Data.Word (Word8)
import Network.HTTP.Client
       (httpLbs, responseBody, responseHeaders,
        Request(port, host, requestHeaders), parseRequest, newManager)
import Network.HTTP.Client.Internal (addProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header ( hContentType )
import Network.Socket (withSocketsDo)
import Network.URI ( unEscapeString )
import Prelude
import System.Directory (createDirectoryIfMissing, getDirectoryContents,
                         doesDirectoryExist)
import System.Environment (getEnv)
import System.FilePath ((</>), takeDirectory, normalise)
import System.FilePath.Glob (match, compile)
import System.IO (stderr)
import System.IO.Error
import System.Random (StdGen, next, mkStdGen)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Text.Pandoc.Translations (Translations)
import Text.Pandoc.Walk (walk)
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
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
         resp <- newManager tlsManagerSettings >>= httpLbs req'
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

alertIndent :: [T.Text] -> IO ()
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

-- Write the contents of a media bag to a path.
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

adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | T.unpack src `elem` paths = Image attr lab (T.pack dir <> "/" <> src, tit)
adjustImagePath _ _ x = x

-- | The 'PureState' contains ersatz representations
-- of things that would normally be obtained through IO.
data PureState = PureState { stStdGen     :: StdGen
                           , stWord8Store :: [Word8] -- should be
                                                     -- infinite,
                                                     -- i.e. [1..]
                           , stUniqStore  :: [Int] -- should be
                                                   -- infinite and
                                                   -- contain every
                                                   -- element at most
                                                   -- once, e.g. [1..]
                           , stEnv :: [(T.Text, T.Text)]
                           , stTime :: UTCTime
                           , stTimeZone :: TimeZone
                           , stReferenceDocx :: Archive
                           , stReferencePptx :: Archive
                           , stReferenceODT :: Archive
                           , stFiles :: FileTree
                           , stUserDataFiles :: FileTree
                           , stCabalDataFiles :: FileTree
                           }

instance Default PureState where
  def = PureState { stStdGen = mkStdGen 1848
                  , stWord8Store = [1..]
                  , stUniqStore = [1..]
                  , stEnv = [("USER", "pandoc-user")]
                  , stTime = posixSecondsToUTCTime 0
                  , stTimeZone = utc
                  , stReferenceDocx = emptyArchive
                  , stReferencePptx = emptyArchive
                  , stReferenceODT = emptyArchive
                  , stFiles = mempty
                  , stUserDataFiles = mempty
                  , stCabalDataFiles = mempty
                  }


getPureState :: PandocPure PureState
getPureState = PandocPure $ lift $ lift get

getsPureState :: (PureState -> a) -> PandocPure a
getsPureState f = f <$> getPureState

putPureState :: PureState -> PandocPure ()
putPureState ps= PandocPure $ lift $ lift $ put ps

modifyPureState :: (PureState -> PureState) -> PandocPure ()
modifyPureState f = PandocPure $ lift $ lift $ modify f


data FileInfo = FileInfo { infoFileMTime :: UTCTime
                         , infoFileContents :: B.ByteString
                         }

newtype FileTree = FileTree {unFileTree :: M.Map FilePath FileInfo}
  deriving (Semigroup, Monoid)

getFileInfo :: FilePath -> FileTree -> Maybe FileInfo
getFileInfo fp tree =
  M.lookup (makeCanonical fp) (unFileTree tree)

-- | Add the specified file to the FileTree. If file
-- is a directory, add its contents recursively.
addToFileTree :: FileTree -> FilePath -> IO FileTree
addToFileTree tree fp = do
  isdir <- doesDirectoryExist fp
  if isdir
     then do -- recursively add contents of directories
       let isSpecial ".." = True
           isSpecial "."  = True
           isSpecial _    = False
       fs <- (map (fp </>) . filter (not . isSpecial)) <$> getDirectoryContents fp
       foldM addToFileTree tree fs
     else do
       contents <- B.readFile fp
       mtime <- IO.getModificationTime fp
       return $ insertInFileTree fp FileInfo{ infoFileMTime = mtime
                                            , infoFileContents = contents } tree

-- | Insert an ersatz file into the 'FileTree'.
insertInFileTree :: FilePath -> FileInfo -> FileTree -> FileTree
insertInFileTree fp info (FileTree treemap) =
  FileTree $ M.insert (makeCanonical fp) info treemap

newtype PandocPure a = PandocPure {
  unPandocPure :: ExceptT PandocError
                  (StateT CommonState (State PureState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError PandocError
             )

-- Run a 'PandocPure' operation.
runPure :: PandocPure a -> Either PandocError a
runPure x = flip evalState def $
            flip evalStateT def $
            runExceptT $
            unPandocPure x

instance PandocMonad PandocPure where
  lookupEnv s = do
    env <- getsPureState stEnv
    return (lookup s env)

  getCurrentTime = getsPureState stTime

  getCurrentTimeZone = getsPureState stTimeZone

  newStdGen = do
    g <- getsPureState stStdGen
    let (_, nxtGen) = next g
    modifyPureState $ \st -> st { stStdGen = nxtGen }
    return g

  newUniqueHash = do
    uniqs <- getsPureState stUniqStore
    case uniqs of
      u : us -> do
        modifyPureState $ \st -> st { stUniqStore = us }
        return u
      _ -> throwError $ PandocShouldNeverHappenError
                        "uniq store ran out of elements"
  openURL u = throwError $ PandocResourceNotFound u
  readFileLazy fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return (BL.fromStrict bs)
      Nothing -> throwError $ PandocResourceNotFound $ T.pack fp
  readFileStrict fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return bs
      Nothing -> throwError $ PandocResourceNotFound $ T.pack fp

  glob s = do
    FileTree ftmap <- getsPureState stFiles
    return $ filter (match (compile s)) $ M.keys ftmap

  fileExists fp = do
    fps <- getsPureState stFiles
    case getFileInfo fp fps of
         Nothing -> return False
         Just _  -> return True

  getDataFileName fp = return $ "data/" ++ fp

  getModificationTime fp = do
    fps <- getsPureState stFiles
    case infoFileMTime <$> getFileInfo fp fps of
      Just tm -> return tm
      Nothing -> throwError $ PandocIOError (T.pack fp)
                    (userError "Can't get modification time")

  getCommonState = PandocPure $ lift get
  putCommonState x = PandocPure $ lift $ put x

  logOutput _msg = return ()

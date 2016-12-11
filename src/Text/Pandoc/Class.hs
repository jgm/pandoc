{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TypeSynonymInstances,
FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}

{-
Copyright (C) 2016 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Class
   Copyright   : Copyright (C) 2016 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Typeclass for pandoc readers and writers, allowing both IO and pure instances.
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
                         , warning
                         , warningWithPos
                         , getWarnings
                         , getMediaBag
                         , setMediaBag
                         , insertMedia
                         , getInputFiles
                         , getOutputFile
                         , PandocIO(..)
                         , PandocPure(..)
                         , FileInfo(..)
                         , runIO
                         , runIOorExplode
                         , runPure
                         , withMediaBag
                         , withWarningsToStderr
                         ) where

import Prelude hiding (readFile)
import System.Random (StdGen, next, mkStdGen)
import qualified System.Random as IO (newStdGen)
import Codec.Archive.Zip (Archive, fromArchive, emptyArchive)
import Data.Unique (hashUnique)
import qualified Data.Unique as IO (newUnique)
import qualified Text.Pandoc.Shared as IO ( fetchItem
                                          , fetchItem'
                                          , readDataFile
                                          , warn)
import Text.Pandoc.Compat.Time (UTCTime)
import Text.Pandoc.Parsing (ParserT, SourcePos)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds
                             , posixSecondsToUTCTime
                             , POSIXTime )
import Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime, utc)
import qualified Data.Time.LocalTime as IO (getCurrentTimeZone)
import Text.Pandoc.MIME (MimeType, getMimeType)
import Text.Pandoc.MediaBag (MediaBag)
import qualified Text.Pandoc.MediaBag as MB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified System.Environment as IO (lookupEnv)
import System.FilePath.Glob (match, compile)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as IO (glob)
import qualified System.Directory as IO (getModificationTime)
import Control.Monad as M (fail)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT)
import Control.Monad.RWS (RWST)
import Data.Word (Word8)
import Data.Default
import System.IO.Error
import qualified Data.Map as M
import Text.Pandoc.Error

class (Functor m, Applicative m, Monad m, MonadError PandocError m)
      => PandocMonad m where
  lookupEnv :: String -> m (Maybe String)
  getCurrentTime :: m UTCTime
  getCurrentTimeZone :: m TimeZone
  newStdGen :: m StdGen
  newUniqueHash :: m Int
  readFileLazy :: FilePath -> m BL.ByteString
  readDataFile :: Maybe FilePath
               -> FilePath
               -> m B.ByteString
  fetchItem :: Maybe String
            -> String
            -> m (Either E.SomeException (B.ByteString, Maybe MimeType))
  fetchItem' :: MediaBag
             -> Maybe String
             -> String
             -> m (Either E.SomeException (B.ByteString, Maybe MimeType))
  glob :: String -> m [FilePath]
  getModificationTime :: FilePath -> m UTCTime
  getCommonState :: m CommonState
  putCommonState :: CommonState -> m ()

  getsCommonState :: (CommonState -> a) -> m a
  getsCommonState f = f <$> getCommonState

  modifyCommonState :: (CommonState -> CommonState) -> m ()
  modifyCommonState f = getCommonState >>= putCommonState . f

-- Functions defined for all PandocMonad instances

warning :: PandocMonad m => String -> m ()
warning msg = modifyCommonState $ \st -> st{stWarnings = msg : stWarnings st}

getWarnings :: PandocMonad m => m [String]
getWarnings = getsCommonState stWarnings

setMediaBag :: PandocMonad m => MediaBag -> m ()
setMediaBag mb = modifyCommonState $ \st -> st{stMediaBag = mb}

getMediaBag :: PandocMonad m => m MediaBag
getMediaBag = getsCommonState stMediaBag

insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp mime bs =
    modifyCommonState $ \st ->
      st{stMediaBag = MB.insertMedia fp mime bs (stMediaBag st) }

getInputFiles :: PandocMonad m => m (Maybe [FilePath])
getInputFiles = getsCommonState stInputFiles

getOutputFile :: PandocMonad m => m (Maybe FilePath)
getOutputFile = getsCommonState stOutputFile

getPOSIXTime :: (PandocMonad m) => m POSIXTime
getPOSIXTime = utcTimeToPOSIXSeconds <$> getCurrentTime

getZonedTime :: (PandocMonad m) => m ZonedTime
getZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

warningWithPos :: PandocMonad m
               => SourcePos
               -> String
               -> ParserT s st m ()
warningWithPos pos msg = lift $ warning $ msg ++ " " ++ show pos

--

data CommonState = CommonState { stWarnings :: [String]
                               , stMediaBag :: MediaBag
                               , stInputFiles :: Maybe [FilePath]
                               , stOutputFile :: Maybe FilePath
                               }

instance Default CommonState where
  def = CommonState { stWarnings = []
                    , stMediaBag = mempty
                    , stInputFiles = Nothing
                    , stOutputFile = Nothing
                    }

runIO :: PandocIO a -> IO (Either PandocError a)
runIO ma = flip evalStateT def $ runExceptT $ unPandocIO ma

withMediaBag :: PandocMonad m => m a ->  m (a, MediaBag)
withMediaBag ma = ((,)) <$> ma <*> getMediaBag

withWarningsToStderr :: PandocIO a -> PandocIO a
withWarningsToStderr f = do
  x <- f
  getWarnings >>= mapM_ IO.warn
  return x

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

instance PandocMonad PandocIO where
  lookupEnv = liftIO . IO.lookupEnv
  getCurrentTime = liftIO IO.getCurrentTime
  getCurrentTimeZone = liftIO IO.getCurrentTimeZone
  newStdGen = liftIO IO.newStdGen
  newUniqueHash = hashUnique <$> (liftIO IO.newUnique)
  readFileLazy s = do
    eitherBS <- liftIO (tryIOError $ BL.readFile s)
    case eitherBS of
      Right bs -> return bs
      Left _ -> throwError $ PandocFileReadError s
  -- TODO: Make this more sensitive to the different sorts of failure
  readDataFile mfp fname = do
    eitherBS <- liftIO (tryIOError $ IO.readDataFile mfp fname)
    case eitherBS of
      Right bs -> return bs
      Left _ -> throwError $ PandocFileReadError fname
  fetchItem ms s = liftIO $ IO.fetchItem ms s
  fetchItem' mb ms s = liftIO $ IO.fetchItem' mb ms s
  glob = liftIO . IO.glob
  getModificationTime fp = do
    eitherMtime <- liftIO (tryIOError $ IO.getModificationTime fp)
    case eitherMtime of
      Right mtime -> return mtime
      Left _ -> throwError $ PandocFileReadError fp
  getCommonState = PandocIO $ lift get
  putCommonState x = PandocIO $ lift $ put x

data PureState = PureState { stStdGen     :: StdGen
                           , stWord8Store :: [Word8] -- should be
                                                     -- inifinite,
                                                     -- i.e. [1..]
                           , stUniqStore  :: [Int] -- should be
                                                   -- inifinite and
                                                   -- contain every
                                                   -- element at most
                                                   -- once, e.g. [1..]
                           , stEnv :: [(String, String)]
                           , stTime :: UTCTime
                           , stTimeZone :: TimeZone
                           , stReferenceDocx :: Archive
                           , stReferenceODT :: Archive
                           , stFiles :: FileTree
                           , stUserDataDir :: FileTree
                           , stCabalDataDir :: FileTree
                           , stFontFiles :: [FilePath]
                           }

instance Default PureState where
  def = PureState { stStdGen = mkStdGen 1848
                  , stWord8Store = [1..]
                  , stUniqStore = [1..]
                  , stEnv = [("USER", "pandoc-user")]
                  , stTime = posixSecondsToUTCTime 0
                  , stTimeZone = utc
                  , stReferenceDocx = emptyArchive
                  , stReferenceODT = emptyArchive
                  , stFiles = mempty
                  , stUserDataDir = mempty
                  , stCabalDataDir = mempty
                  , stFontFiles = []
                  }


getPureState :: PandocPure PureState
getPureState = PandocPure $ lift $ lift $ get

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
  deriving (Monoid)

getFileInfo :: FilePath -> FileTree -> Maybe FileInfo
getFileInfo fp tree = M.lookup fp $ unFileTree tree


newtype PandocPure a = PandocPure {
  unPandocPure :: ExceptT PandocError
                  (StateT CommonState (State PureState)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError PandocError
             )

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
      _ -> M.fail "uniq store ran out of elements"
  readFileLazy fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return (BL.fromStrict bs)
      Nothing -> throwError $ PandocFileReadError fp
  readDataFile Nothing "reference.docx" = do
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceDocx
  readDataFile Nothing "reference.odt" = do
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceODT
  readDataFile Nothing fname = do
    let fname' = if fname == "MANUAL.txt" then fname else "data" </> fname
    BL.toStrict <$> (readFileLazy fname')
  readDataFile (Just userDir) fname = do
    userDirFiles <- getsPureState stUserDataDir
    case infoFileContents <$> (getFileInfo (userDir </> fname) userDirFiles) of
      Just bs -> return bs
      Nothing -> readDataFile Nothing fname
  fetchItem _ fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> (getFileInfo fp fps) of
      Just bs -> return (Right (bs, getMimeType fp))
      Nothing -> return (Left $ E.toException $ PandocFileReadError fp)

  fetchItem' media sourceUrl nm = do
    case MB.lookupMedia nm media of
      Nothing -> fetchItem sourceUrl nm
      Just (mime, bs) -> return (Right (B.concat $ BL.toChunks bs, Just mime))

  glob s = do
    fontFiles <- getsPureState stFontFiles
    return (filter (match (compile s)) fontFiles)

  getModificationTime fp = do
    fps <- getsPureState stFiles
    case infoFileMTime <$> (getFileInfo fp fps) of
      Just tm -> return tm
      Nothing -> throwError $ PandocFileReadError fp

  getCommonState = PandocPure $ lift $ get
  putCommonState x = PandocPure $ lift $ put x

instance PandocMonad m => PandocMonad (ParserT s st m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  readFileLazy = lift . readFileLazy
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  fetchItem media = lift . fetchItem media
  fetchItem' media sourceUrl = lift . fetchItem' media sourceUrl
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState

instance PandocMonad m => PandocMonad (ReaderT r m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  readFileLazy = lift . readFileLazy
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  fetchItem media = lift . fetchItem media
  fetchItem' media sourceUrl = lift . fetchItem' media sourceUrl
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState

instance (PandocMonad m, Monoid w) => PandocMonad (WriterT w m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  readFileLazy = lift . readFileLazy
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  fetchItem media = lift . fetchItem media
  fetchItem' media sourceUrl = lift . fetchItem' media sourceUrl
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState

instance (PandocMonad m, Monoid w) => PandocMonad (RWST r w st m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  readFileLazy = lift . readFileLazy
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  fetchItem media = lift . fetchItem media
  fetchItem' media sourceUrl = lift . fetchItem' media sourceUrl
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState

instance PandocMonad m => PandocMonad (StateT st m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  readFileLazy = lift . readFileLazy
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  fetchItem media = lift . fetchItem media
  fetchItem' media sourceUrl = lift . fetchItem' media sourceUrl
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState


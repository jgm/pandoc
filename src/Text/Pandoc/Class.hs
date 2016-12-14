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
                         , insertDeferredMedia
                         , fetchItem
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
import qualified Text.Pandoc.Shared as IO ( readDataFile
                                          , warn
                                          , openURL )
import Text.Pandoc.Compat.Time (UTCTime)
import Text.Pandoc.Parsing (ParserT, SourcePos)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Text.Pandoc.MIME (MimeType, getMimeType)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds
                             , posixSecondsToUTCTime
                             , POSIXTime )
import Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime, utc)
import Network.URI ( escapeURIString, nonStrictRelativeTo,
                     unEscapeString, parseURIReference, isAllowedInURI,
                     parseURI, URI(..) )
import qualified Data.Time.LocalTime as IO (getCurrentTimeZone)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia)
import qualified Text.Pandoc.MediaBag as MB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.Environment as IO (lookupEnv)
import System.FilePath.Glob (match, compile)
import System.FilePath ((</>), takeExtension, dropExtension)
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
import Data.Monoid

class (Functor m, Applicative m, Monad m, MonadError PandocError m)
      => PandocMonad m where
  lookupEnv :: String -> m (Maybe String)
  getCurrentTime :: m UTCTime
  getCurrentTimeZone :: m TimeZone
  newStdGen :: m StdGen
  newUniqueHash :: m Int
  openURL :: String -> m (B.ByteString, Maybe MimeType)
  readFileLazy :: FilePath -> m BL.ByteString
  readFileStrict :: FilePath -> m B.ByteString
  readDataFile :: Maybe FilePath
               -> FilePath
               -> m B.ByteString
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
setMediaBag mb = modifyCommonState $
                 \st -> st{stDeferredMediaBag = DeferredMediaBag mb mempty}

getMediaBag :: PandocMonad m => m MediaBag
getMediaBag = fetchDeferredMedia >> (dropDeferredMedia <$> getsCommonState stDeferredMediaBag)

insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp mime bs = do
  (DeferredMediaBag mb dm) <- getsCommonState stDeferredMediaBag
  let mb' = MB.insertMedia fp mime bs mb
  modifyCommonState $ \st -> st{stDeferredMediaBag =DeferredMediaBag mb' dm }

insertDeferredMedia :: PandocMonad m => FilePath -> m ()
insertDeferredMedia fp = do
  (DeferredMediaBag mb dm) <- getsCommonState stDeferredMediaBag
  modifyCommonState $
    \st -> st{stDeferredMediaBag = DeferredMediaBag mb ((DeferredMediaPath fp) : dm)}

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

newtype DeferredMediaPath = DeferredMediaPath {unDefer :: String}
                          deriving (Show, Eq)

data DeferredMediaBag = DeferredMediaBag MediaBag [DeferredMediaPath]
                      deriving (Show)

instance Monoid DeferredMediaBag where
  mempty = DeferredMediaBag mempty mempty
  mappend (DeferredMediaBag mb lst) (DeferredMediaBag mb' lst') =
    DeferredMediaBag (mb <> mb') (lst <> lst')

fetchDeferredMedia' :: PandocMonad m => m MediaBag
fetchDeferredMedia' = do
  (DeferredMediaBag mb defMedia) <- getsCommonState stDeferredMediaBag
  fetchedMedia <- mapM (\dfp -> fetchItem Nothing (unDefer dfp)) defMedia
  return $ foldr
    (\(dfp, (bs, mbMime)) mb' ->
       MB.insertMedia (unDefer dfp) mbMime (BL.fromStrict bs) mb')
    mb
    (zip defMedia fetchedMedia)

fetchDeferredMedia :: PandocMonad m => m ()
fetchDeferredMedia = fetchDeferredMedia' >>= setMediaBag

dropDeferredMedia :: DeferredMediaBag -> MediaBag
dropDeferredMedia (DeferredMediaBag mb _) = mb


data CommonState = CommonState { stWarnings :: [String]
                               , stDeferredMediaBag :: DeferredMediaBag
                               , stInputFiles :: Maybe [FilePath]
                               , stOutputFile :: Maybe FilePath
                               }

instance Default CommonState where
  def = CommonState { stWarnings = []
                    , stDeferredMediaBag = mempty
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
  openURL u = do
    eitherRes <- liftIO $ (tryIOError $ IO.openURL u)
    case eitherRes of
          Right (Right res) -> return res
          Right (Left _)    -> throwError $ PandocFileReadError u
          Left  _           -> throwError $ PandocFileReadError u
  readFileLazy s = do
    eitherBS <- liftIO (tryIOError $ BL.readFile s)
    case eitherBS of
      Right bs -> return bs
      Left _ -> throwError $ PandocFileReadError s
  readFileStrict s = do
    eitherBS <- liftIO (tryIOError $ B.readFile s)
    case eitherBS of
      Right bs -> return bs
      Left _ -> throwError $ PandocFileReadError s
  -- TODO: Make this more sensitive to the different sorts of failure
  readDataFile mfp fname = do
    eitherBS <- liftIO (tryIOError $ IO.readDataFile mfp fname)
    case eitherBS of
      Right bs -> return bs
      Left _ -> throwError $ PandocFileReadError fname
  glob = liftIO . IO.glob
  getModificationTime fp = do
    eitherMtime <- liftIO (tryIOError $ IO.getModificationTime fp)
    case eitherMtime of
      Right mtime -> return mtime
      Left _ -> throwError $ PandocFileReadError fp
  getCommonState = PandocIO $ lift get
  putCommonState x = PandocIO $ lift $ put x


-- | Specialized version of parseURIReference that disallows
-- single-letter schemes.  Reason:  these are usually windows absolute
-- paths.
parseURIReference' :: String -> Maybe URI
parseURIReference' s =
  case parseURIReference s of
       Just u
         | length (uriScheme u) > 2  -> Just u
         | null (uriScheme u)        -> Just u  -- protocol-relative
       _                             -> Nothing

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: PandocMonad m
          => Maybe String
          -> String
          -> m (B.ByteString, Maybe MimeType)
fetchItem sourceURL s = do
  mediabag <- dropDeferredMedia <$> getsCommonState stDeferredMediaBag
  case lookupMedia s mediabag of
       Just (mime, bs) -> return $ (BL.toStrict bs, Just mime)
       Nothing ->
        case (sourceURL >>= parseURIReference' .
                             ensureEscaped, ensureEscaped s) of
          (Just u, s') -> -- try fetching from relative path at source
             case parseURIReference' s' of
                  Just u' -> openURL $ show $ u' `nonStrictRelativeTo` u
                  Nothing -> openURL s' -- will throw error
          (Nothing, s'@('/':'/':_)) ->  -- protocol-relative URI
             case parseURIReference' s' of
                  Just u' -> openURL $ show $ u' `nonStrictRelativeTo` httpcolon
                  Nothing -> openURL s' -- will throw error
          (Nothing, s') ->
             case parseURI s' of  -- requires absolute URI
                  -- We don't want to treat C:/ as a scheme:
                  Just u' | length (uriScheme u') > 2 -> openURL (show u')
                  Just u' | uriScheme u' == "file:" ->
                       readLocalFile $ dropWhile (=='/') (uriPath u')
                  _ -> readLocalFile fp -- get from local file system
         where readLocalFile f = do
                 cont <- readFileStrict f
                 return (cont, mime)
               httpcolon = URI{ uriScheme = "http:",
                                uriAuthority = Nothing,
                                uriPath = "",
                                uriQuery = "",
                                uriFragment = "" }
               dropFragmentAndQuery = takeWhile (\c -> c /= '?' && c /= '#')
               fp = unEscapeString $ dropFragmentAndQuery s
               mime = case takeExtension fp of
                           ".gz" -> getMimeType $ dropExtension fp
                           ".svgz" -> getMimeType $ dropExtension fp ++ ".svg"
                           x     -> getMimeType x
               ensureEscaped = escapeURIString isAllowedInURI . map convertSlash
               convertSlash '\\' = '/'
               convertSlash x    = x

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
  openURL _ = throwError $ PandocSomeError "Cannot open URL in PandocPure"
  readFileLazy fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return (BL.fromStrict bs)
      Nothing -> throwError $ PandocFileReadError fp
  readFileStrict fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return bs
      Nothing -> throwError $ PandocFileReadError fp
  readDataFile Nothing "reference.docx" = do
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceDocx
  readDataFile Nothing "reference.odt" = do
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceODT
  readDataFile Nothing fname = do
    let fname' = if fname == "MANUAL.txt" then fname else "data" </> fname
    readFileStrict fname'
  readDataFile (Just userDir) fname = do
    userDirFiles <- getsPureState stUserDataDir
    case infoFileContents <$> (getFileInfo (userDir </> fname) userDirFiles) of
      Just bs -> return bs
      Nothing -> readDataFile Nothing fname

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
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  readDataFile mbuserdir = lift . readDataFile mbuserdir
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
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  readDataFile mbuserdir = lift . readDataFile mbuserdir
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
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  readDataFile mbuserdir = lift . readDataFile mbuserdir
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
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  readDataFile mbuserdir = lift . readDataFile mbuserdir
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
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  readDataFile mbuserdir = lift . readDataFile mbuserdir
  glob = lift . glob
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState


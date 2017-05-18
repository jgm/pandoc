{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TypeSynonymInstances,
FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts,
StandaloneDeriving #-}

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
                         , readFileFromDirs
                         , report
                         , getLog
                         , setVerbosity
                         , getMediaBag
                         , setMediaBag
                         , insertMedia
                         , fetchItem
                         , getInputFiles
                         , getOutputFile
                         , setResourcePath
                         , getResourcePath
                         , PandocIO(..)
                         , PandocPure(..)
                         , FileTree(..)
                         , FileInfo(..)
                         , runIO
                         , runIOorExplode
                         , runPure
                         , withMediaBag
                         , fillMediaBag
                         , extractMedia
                         ) where

import Prelude hiding (readFile)
import System.Random (StdGen, next, mkStdGen)
import qualified System.Random as IO (newStdGen)
import Codec.Archive.Zip (Archive, fromArchive, emptyArchive)
import Data.Unique (hashUnique)
import qualified Data.Unique as IO (newUnique)
import qualified Text.Pandoc.Shared as IO ( readDataFile
                                          , openURL )
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Compat.Time (UTCTime)
import Text.Pandoc.Logging
import Text.Parsec (ParsecT)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Text.Pandoc.MIME (MimeType, getMimeType, extensionFromMimeType)
import Text.Pandoc.Definition
import Data.Char (toLower)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds
                             , posixSecondsToUTCTime
                             , POSIXTime )
import Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime, utc)
import Network.URI ( escapeURIString, nonStrictRelativeTo,
                     unEscapeString, parseURIReference, isAllowedInURI,
                     parseURI, URI(..) )
import qualified Data.Time.LocalTime as IO (getCurrentTimeZone)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, extractMediaBag,
                             mediaDirectory)
import Text.Pandoc.Walk (walkM, walk)
import qualified Text.Pandoc.MediaBag as MB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.Environment as IO (lookupEnv)
import System.FilePath.Glob (match, compile)
import System.FilePath ((</>), (<.>), takeExtension, dropExtension, isRelative)
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
import System.IO (stderr, nativeNewline)
import qualified Data.Map as M
import Text.Pandoc.Error

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
  readDataFile :: Maybe FilePath -> FilePath -> m B.ByteString
  glob :: String -> m [FilePath]
  getModificationTime :: FilePath -> m UTCTime
  getCommonState :: m CommonState
  putCommonState :: CommonState -> m ()

  getsCommonState :: (CommonState -> a) -> m a
  getsCommonState f = f <$> getCommonState

  modifyCommonState :: (CommonState -> CommonState) -> m ()
  modifyCommonState f = getCommonState >>= putCommonState . f

  logOutput :: LogMessage -> m ()

-- Functions defined for all PandocMonad instances

setVerbosity :: PandocMonad m => Verbosity -> m ()
setVerbosity verbosity =
  modifyCommonState $ \st -> st{ stVerbosity = verbosity }

getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

report :: PandocMonad m => LogMessage -> m ()
report msg = do
  verbosity <- getsCommonState stVerbosity
  let level = messageVerbosity msg
  when (level <= verbosity) $ do
    logOutput msg
  unless (level == DEBUG) $
    modifyCommonState $ \st -> st{ stLog = msg : stLog st }

setMediaBag :: PandocMonad m => MediaBag -> m ()
setMediaBag mb = modifyCommonState $ \st -> st{stMediaBag = mb}

getMediaBag :: PandocMonad m => m MediaBag
getMediaBag = getsCommonState stMediaBag

insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp mime bs = do
  mb <- getsCommonState stMediaBag
  let mb' = MB.insertMedia fp mime bs mb
  modifyCommonState $ \st -> st{stMediaBag = mb' }

getInputFiles :: PandocMonad m => m (Maybe [FilePath])
getInputFiles = getsCommonState stInputFiles

getOutputFile :: PandocMonad m => m (Maybe FilePath)
getOutputFile = getsCommonState stOutputFile

setResourcePath :: PandocMonad m => [FilePath] -> m ()
setResourcePath ps = modifyCommonState $ \st -> st{stResourcePath = ps}

getResourcePath :: PandocMonad m => m [FilePath]
getResourcePath = getsCommonState stResourcePath

getPOSIXTime :: (PandocMonad m) => m POSIXTime
getPOSIXTime = utcTimeToPOSIXSeconds <$> getCurrentTime

getZonedTime :: (PandocMonad m) => m ZonedTime
getZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

-- | Read file, checking in any number of directories.
readFileFromDirs :: PandocMonad m => [FilePath] -> FilePath -> m (Maybe String)
readFileFromDirs [] _ = return Nothing
readFileFromDirs (d:ds) f = catchError
    ((Just . UTF8.toStringLazy) <$> readFileLazy (d </> f))
    (\_ -> readFileFromDirs ds f)

--

data CommonState = CommonState { stLog          :: [LogMessage]
                               , stMediaBag     :: MediaBag
                               , stInputFiles   :: Maybe [FilePath]
                               , stOutputFile   :: Maybe FilePath
                               , stResourcePath :: [FilePath]
                               , stVerbosity    :: Verbosity
                               }

instance Default CommonState where
  def = CommonState { stLog = []
                    , stMediaBag = mempty
                    , stInputFiles = Nothing
                    , stOutputFile = Nothing
                    , stResourcePath = ["."]
                    , stVerbosity = WARNING
                    }

runIO :: PandocIO a -> IO (Either PandocError a)
runIO ma = flip evalStateT def $ runExceptT $ unPandocIO ma

withMediaBag :: PandocMonad m => m a ->  m (a, MediaBag)
withMediaBag ma = ((,)) <$> ma <*> getMediaBag

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

liftIOError :: (String -> IO a) -> String -> PandocIO a
liftIOError f u = do
  res <- liftIO $ tryIOError $ f u
  case res of
         Left e -> throwError $ PandocIOError u e
         Right r -> return r

instance PandocMonad PandocIO where
  lookupEnv = liftIO . IO.lookupEnv
  getCurrentTime = liftIO IO.getCurrentTime
  getCurrentTimeZone = liftIO IO.getCurrentTimeZone
  newStdGen = liftIO IO.newStdGen
  newUniqueHash = hashUnique <$> (liftIO IO.newUnique)
  openURL u = do
    report $ Fetching u
    res <- liftIO (IO.openURL u)
    case res of
         Right r -> return r
         Left e  -> throwError $ PandocHttpError u e
  readFileLazy s = liftIOError BL.readFile s
  readFileStrict s = liftIOError B.readFile s
  readDataFile mfp fname = liftIOError (IO.readDataFile mfp) fname
  glob = liftIO . IO.glob
  getModificationTime fp = liftIOError IO.getModificationTime fp
  getCommonState = PandocIO $ lift get
  putCommonState x = PandocIO $ lift $ put x
  logOutput msg = liftIO $ do
    UTF8.hPutStr stderr nativeNewline $ "[" ++
       (map toLower $ show (messageVerbosity msg)) ++ "] "
    alertIndent $ lines $ showLogMessage msg

alertIndent :: [String] -> IO ()
alertIndent [] = return ()
alertIndent (l:ls) = do
  UTF8.hPutStrLn stderr nativeNewline l
  mapM_ go ls
  where go l' = do UTF8.hPutStr stderr nativeNewline "! "
                   UTF8.hPutStrLn stderr nativeNewline l'

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
  mediabag <- getMediaBag
  case lookupMedia s mediabag of
    Just (mime, bs) -> return $ (BL.toStrict bs, Just mime)
    Nothing -> downloadOrRead sourceURL s

downloadOrRead :: PandocMonad m
               => Maybe String
               -> String
               -> m (B.ByteString, Maybe MimeType)
downloadOrRead sourceURL s = do
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
             resourcePath <- getResourcePath
             cont <- if isRelative f
                        then withPaths resourcePath readFileStrict f
                        else readFileStrict f
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

withPaths :: PandocMonad m => [FilePath] -> (FilePath -> m a) -> FilePath -> m a
withPaths [] _ fp = throwError $ PandocResourceNotFound fp
withPaths (p:ps) action fp =
  catchError (action (p </> fp))
             (\_ -> withPaths ps action fp)

-- | Traverse tree, filling media bag.
fillMediaBag :: PandocMonad m => Maybe String -> Pandoc -> m Pandoc
fillMediaBag sourceURL d = walkM handleImage d
  where handleImage :: PandocMonad m => Inline -> m Inline
        handleImage (Image attr lab (src, tit)) = catchError
          (do (bs, mt) <- fetchItem sourceURL src
              let ext = fromMaybe (takeExtension src)
                          (mt >>= extensionFromMimeType)
              let bs' = BL.fromChunks [bs]
              let basename = showDigest $ sha1 bs'
              let fname = basename <.> ext
              insertMedia fname mt bs'
              return $ Image attr lab (fname, tit))
          (\e -> do
              case e of
                PandocResourceNotFound _ -> do
                  report $ CouldNotFetchResource src
                            "replacing image with description"
                  -- emit alt text
                  return $ Span ("",["image"],[]) lab
                PandocHttpError u er -> do
                  report $ CouldNotFetchResource u
                            (show er ++ "\rReplacing image with description.")
                  -- emit alt text
                  return $ Span ("",["image"],[]) lab
                _ -> throwError e)
        handleImage x = return x

-- | Extract media from the mediabag into a directory.
extractMedia :: FilePath -> Pandoc -> PandocIO Pandoc
extractMedia dir d = do
  media <- getMediaBag
  case [fp | (fp, _, _) <- mediaDirectory media] of
        []  -> return d
        fps -> do
          liftIO $ extractMediaBag True dir media
          return $ walk (adjustImagePath dir fps) d

adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | src `elem` paths = Image attr lab (dir ++ "/" ++ src, tit)
adjustImagePath _ _ x = x

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
  openURL u = throwError $ PandocResourceNotFound u
  readFileLazy fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return (BL.fromStrict bs)
      Nothing -> throwError $ PandocResourceNotFound fp
  readFileStrict fp = do
    fps <- getsPureState stFiles
    case infoFileContents <$> getFileInfo fp fps of
      Just bs -> return bs
      Nothing -> throwError $ PandocResourceNotFound fp
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
      Nothing -> throwError $ PandocIOError fp
                    (userError "Can't get modification time")

  getCommonState = PandocPure $ lift $ get
  putCommonState x = PandocPure $ lift $ put x

  logOutput _msg = return ()

instance PandocMonad m => PandocMonad (ParsecT s st m) where
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
  logOutput = lift . logOutput

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
  logOutput = lift . logOutput

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
  logOutput = lift . logOutput

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
  logOutput = lift . logOutput

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
  logOutput = lift . logOutput


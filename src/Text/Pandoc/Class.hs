{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Copyright (C) 2016-17 Jesse Rosenthal <jrosenthal@jhu.edu>
and John MacFarlane.

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
                         , addToFileTree
                         , runIO
                         , runIOorExplode
                         , runPure
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
import Text.Parsec (ParsecT, getPosition, sourceLine, sourceName)
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
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Text.Pandoc.Walk (walkM, walk)
import qualified Text.Pandoc.MediaBag as MB
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified System.Environment as IO (lookupEnv)
import System.FilePath.Glob (match, compile)
import System.Directory (createDirectoryIfMissing, getDirectoryContents,
                          doesDirectoryExist)
import System.FilePath ((</>), (<.>), takeDirectory,
         takeExtension, dropExtension, isRelative, normalise)
import qualified System.FilePath.Glob as IO (glob)
import qualified System.Directory as IO (getModificationTime)
import Control.Monad as M (fail)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer (WriterT)
import Control.Monad.RWS (RWST)
import Data.Word (Word8)
import Data.Default
import System.IO.Error
import System.IO (stderr)
import qualified Data.Map as M
import Text.Pandoc.Error
import qualified Debug.Trace

-- | The PandocMonad typeclass contains all the potentially
-- IO-related functions used in pandoc's readers and writers.
-- Instances of this typeclass may implement these functions
-- in IO (as in 'PandocIO') or using an internal state that
-- represents a file system, time, and so on (as in 'PandocPure').
class (Functor m, Applicative m, Monad m, MonadError PandocError m)
      => PandocMonad m where
  -- | Lookup an environment variable.
  lookupEnv :: String -> m (Maybe String)
  -- | Get the current (UTC) time.
  getCurrentTime :: m UTCTime
  -- | Get the locale's time zone.
  getCurrentTimeZone :: m TimeZone
  -- | Return a new generator for random numbers.
  newStdGen :: m StdGen
  -- | Return a new unique integer.
  newUniqueHash :: m Int
  -- | Retrieve contents and mime type from a URL, raising
  -- an error on failure.
  openURL :: String -> m (B.ByteString, Maybe MimeType)
  -- | Read the lazy ByteString contents from a file path,
  -- raising an error on failure.
  readFileLazy :: FilePath -> m BL.ByteString
  -- | Read the strict ByteString contents from a file path,
  -- raising an error on failure.
  readFileStrict :: FilePath -> m B.ByteString
  -- | Read file from specified user data directory or,
  -- if not found there, from Cabal data directory.
  readDataFile :: Maybe FilePath -> FilePath -> m B.ByteString
  -- | Return a list of paths that match a glob, relative to
  -- the working directory.  See 'System.FilePath.Glob' for
  -- the glob syntax.
  glob :: String -> m [FilePath]
  -- | Return the modification time of a file.
  getModificationTime :: FilePath -> m UTCTime
  -- | Get the value of the 'CommonState' used by all instances
  -- of 'PandocMonad'.
  getCommonState :: m CommonState
  -- | Set the value of the 'CommonState' used by all instances
  -- of 'PandocMonad'.
  -- | Get the value of a specific field of 'CommonState'.
  putCommonState :: CommonState -> m ()
  -- | Get the value of a specific field of 'CommonState'.
  getsCommonState :: (CommonState -> a) -> m a
  getsCommonState f = f <$> getCommonState
  -- | Modify the 'CommonState'.
  modifyCommonState :: (CommonState -> CommonState) -> m ()
  modifyCommonState f = getCommonState >>= putCommonState . f
  -- Output a log message.
  logOutput :: LogMessage -> m ()
  -- Output a debug message to sterr, using 'Debug.Trace.trace',
  -- if tracing is enabled.  Note: this writes to stderr even in
  -- pure instances.
  trace :: String -> m ()
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ Debug.Trace.trace ("[trace] " ++ msg) (return ())

-- * Functions defined for all PandocMonad instances

-- | Set the verbosity level.
setVerbosity :: PandocMonad m => Verbosity -> m ()
setVerbosity verbosity =
  modifyCommonState $ \st -> st{ stVerbosity = verbosity }

-- Get the accomulated log messages (in temporal order).
getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

-- | Log a message using 'logOutput'.  Note that
-- 'logOutput' is called only if the verbosity
-- level exceeds the level of the message, but
-- the message is added to the list of log messages
-- that will be retrieved by 'getLog' regardless
-- of its verbosity level.
report :: PandocMonad m => LogMessage -> m ()
report msg = do
  verbosity <- getsCommonState stVerbosity
  let level = messageVerbosity msg
  when (level <= verbosity) $ logOutput msg
  modifyCommonState $ \st -> st{ stLog = msg : stLog st }

-- | Determine whether tracing is enabled.  This affects
-- the behavior of 'trace'.  If tracing is not enabled,
-- 'trace' does nothing.
setTrace :: PandocMonad m => Bool -> m ()
setTrace useTracing = modifyCommonState $ \st -> st{stTrace = useTracing}

-- | Initialize the media bag.
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

getPOSIXTime :: PandocMonad m => m POSIXTime
getPOSIXTime = utcTimeToPOSIXSeconds <$> getCurrentTime

getZonedTime :: PandocMonad m => m ZonedTime
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

-- | 'CommonState' represents state that is used by all
-- instances of 'PandocMonad'.  Normally users should not
-- need to interact with it directly; instead, auxiliary
-- functions like 'setVerbosity' and 'withMediaBag' should be used.
data CommonState = CommonState { stLog          :: [LogMessage]
                                 -- ^ A list of log messages in reverse order
                               , stMediaBag     :: MediaBag
                                 -- ^ Media parsed from binary containers
                               , stInputFiles   :: Maybe [FilePath]
                                 -- ^ List of input files from command line
                               , stOutputFile   :: Maybe FilePath
                                 -- ^ Output file from command line
                               , stResourcePath :: [FilePath]
                                 -- ^ Path to search for resources like
                                 -- included images
                               , stVerbosity    :: Verbosity
                                 -- ^ Verbosity level
                               , stTrace        :: Bool
                                 -- ^ Controls whether tracing messages are
                                 -- issued.
                               }

instance Default CommonState where
  def = CommonState { stLog = []
                    , stMediaBag = mempty
                    , stInputFiles = Nothing
                    , stOutputFile = Nothing
                    , stResourcePath = ["."]
                    , stVerbosity = WARNING
                    , stTrace = False
                    }

-- | Evaluate a 'PandocIO' operation.
runIO :: PandocIO a -> IO (Either PandocError a)
runIO ma = flip evalStateT def $ runExceptT $ unPandocIO ma

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
  newUniqueHash = hashUnique <$> liftIO IO.newUnique
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
    UTF8.hPutStr stderr $ "[" ++
       map toLower (show (messageVerbosity msg)) ++ "] "
    alertIndent $ lines $ showLogMessage msg

alertIndent :: [String] -> IO ()
alertIndent [] = return ()
alertIndent (l:ls) = do
  UTF8.hPutStrLn stderr l
  mapM_ go ls
  where go l' = do UTF8.hPutStr stderr "! "
                   UTF8.hPutStrLn stderr l'

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
    Just (mime, bs) -> return (BL.toStrict bs, Just mime)
    Nothing -> downloadOrRead sourceURL s

downloadOrRead :: PandocMonad m
               => Maybe String
               -> String
               -> m (B.ByteString, Maybe MimeType)
downloadOrRead sourceURL s =
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

-- | Traverse tree, filling media bag for any images that
-- aren't already in the media bag.
fillMediaBag :: PandocMonad m => Maybe String -> Pandoc -> m Pandoc
fillMediaBag sourceURL d = walkM handleImage d
  where handleImage :: PandocMonad m => Inline -> m Inline
        handleImage (Image attr lab (src, tit)) = catchError
          (do mediabag <- getMediaBag
              case lookupMedia src mediabag of
                Just (_, _) -> return $ Image attr lab (src, tit)
                Nothing -> do
                  (bs, mt) <- downloadOrRead sourceURL src
                  let ext = fromMaybe (takeExtension src)
                              (mt >>= extensionFromMimeType)
                  let bs' = BL.fromChunks [bs]
                  let basename = showDigest $ sha1 bs'
                  let fname = basename <.> ext
                  insertMedia fname mt bs'
                  return $ Image attr lab (fname, tit))
          (\e ->
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
          mapM_ (writeMedia dir media) fps
          return $ walk (adjustImagePath dir fps) d

writeMedia :: FilePath -> MediaBag -> FilePath -> PandocIO ()
writeMedia dir mediabag subpath = do
  -- we join and split to convert a/b/c to a\b\c on Windows;
  -- in zip containers all paths use /
  let fullpath = dir </> normalise subpath
  let mbcontents = lookupMedia subpath mediabag
  case mbcontents of
       Nothing -> throwError $ PandocResourceNotFound subpath
       Just (_, bs) -> do
         report $ Extracting fullpath
         liftIO $ do
           createDirectoryIfMissing True $ takeDirectory fullpath
           BL.writeFile fullpath bs

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
  deriving (Monoid)

getFileInfo :: FilePath -> FileTree -> Maybe FileInfo
getFileInfo fp tree = M.lookup fp $ unFileTree tree

-- | Add the specified file to the FileTree. If file
-- is a directory, add its contents recursively.
addToFileTree :: FileTree -> FilePath -> IO FileTree
addToFileTree (FileTree treemap) fp = do
  isdir <- doesDirectoryExist fp
  if isdir
     then do -- recursively add contents of directories
       let isSpecial ".." = True
           isSpecial "."  = True
           isSpecial _    = False
       fs <- (map (fp </>) . filter (not . isSpecial)) <$> getDirectoryContents fp
       foldM addToFileTree (FileTree treemap) fs
     else do
       contents <- B.readFile fp
       mtime <- IO.getModificationTime fp
       return $ FileTree $
                M.insert fp FileInfo{ infoFileMTime = mtime
                                    , infoFileContents = contents } treemap

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
  readDataFile Nothing "reference.docx" =
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceDocx
  readDataFile Nothing "reference.odt" =
    (B.concat . BL.toChunks . fromArchive) <$> getsPureState stReferenceODT
  readDataFile Nothing fname = do
    let fname' = if fname == "MANUAL.txt" then fname else "data" </> fname
    readFileStrict fname'
  readDataFile (Just userDir) fname = do
    userDirFiles <- getsPureState stUserDataDir
    case infoFileContents <$> getFileInfo (userDir </> fname) userDirFiles of
      Just bs -> return bs
      Nothing -> readDataFile Nothing fname

  glob s = do
    FileTree ftmap <- getsPureState stFiles
    return $ filter (match (compile s)) $ M.keys ftmap

  getModificationTime fp = do
    fps <- getsPureState stFiles
    case infoFileMTime <$> getFileInfo fp fps of
      Just tm -> return tm
      Nothing -> throwError $ PandocIOError fp
                    (userError "Can't get modification time")

  getCommonState = PandocPure $ lift get
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
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ do
      pos <- getPosition
      Debug.Trace.trace
        ("[trace] Parsed " ++ msg ++ " at line " ++
            show (sourceLine pos) ++
            if sourceName pos == "chunk"
               then " of chunk"
               else "")
        (return ())
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

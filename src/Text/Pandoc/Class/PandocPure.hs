{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Class.PandocPure
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

This module defines a pure instance 'PandocPure' of the @'PandocMonad'@
typeclass. This instance is useful for testing, or when all IO access is
prohibited for security reasons.
-}
module Text.Pandoc.Class.PandocPure
  ( PureState(..)
  , getPureState
  , getsPureState
  , putPureState
  , modifyPureState
  , PandocPure(..)
  , FileTree
  , FileInfo(..)
  , addToFileTree
  , insertInFileTree
  , runPure
  ) where

import Codec.Archive.Zip
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Time.LocalTime (TimeZone, utc)
import Data.Word (Word8)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.FilePath.Glob (match, compile)
import System.Random (StdGen, split, mkStdGen)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.Directory as Directory (getModificationTime)

-- | The 'PureState' contains ersatz representations
-- of things that would normally be obtained through IO.
data PureState = PureState
  { stStdGen     :: StdGen
  , stWord8Store :: [Word8]    -- ^ should be infinite, i.e. [1..]
  , stUniqStore  :: [Int]      -- ^ should be infinite and contain every
                               -- element at most once, e.g. [1..]
  , stEnv :: [(Text, Text)]
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
  def = PureState
        { stStdGen = mkStdGen 1848
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


-- | Retrieve the underlying state of the @'PandocPure'@ type.
getPureState :: PandocPure PureState
getPureState = PandocPure $ lift $ lift get

-- | Retrieve a value from the underlying state of the @'PandocPure'@
-- type.
getsPureState :: (PureState -> a) -> PandocPure a
getsPureState f = f <$> getPureState

-- | Set a new state for the @'PandocPure'@ type.
putPureState :: PureState -> PandocPure ()
putPureState ps= PandocPure $ lift $ lift $ put ps

-- | Modify the underlying state of the @'PandocPure'@ type.
modifyPureState :: (PureState -> PureState) -> PandocPure ()
modifyPureState f = PandocPure $ lift $ lift $ modify f

-- | Captures all file-level information necessary for a @'PandocMonad'@
-- conforming mock file system.
data FileInfo = FileInfo
  { infoFileMTime :: UTCTime
  , infoFileContents :: B.ByteString
  }

-- | Basis of the mock file system used by @'PandocPure'@.
newtype FileTree = FileTree { unFileTree :: M.Map FilePath FileInfo }
  deriving (Semigroup, Monoid)

-- | Retrieve @'FileInfo'@ of the given @'FilePath'@ from a
-- @'FileTree'@.
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
       fs <- map (fp </>) . filter (not . isSpecial) <$> getDirectoryContents fp
       foldM addToFileTree tree fs
     else do
       contents <- B.readFile fp
       mtime <- Directory.getModificationTime fp
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

-- | Run a 'PandocPure' operation.
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
    oldGen <- getsPureState stStdGen
    let (genToStore, genToReturn) = split oldGen
    modifyPureState $ \st -> st { stStdGen = genToStore }
    return genToReturn

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

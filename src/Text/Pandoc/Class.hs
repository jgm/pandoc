{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}

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
                         , Testing
                         , TestState(..)
                         , TestEnv(..)
                         , getPOSIXTime
                         ) where

import Prelude hiding (readFile, fail)
import qualified Control.Monad as M (fail)
import System.Random (StdGen, next)
import qualified System.Random as IO (newStdGen)
import Codec.Archive.Zip (Archive, fromArchive)
import Data.Unique (hashUnique)
import qualified Data.Unique as IO (newUnique)
import qualified Text.Pandoc.Shared as IO ( fetchItem
                                          , fetchItem'
                                          , getDefaultReferenceDocx
                                          , getDefaultReferenceODT
                                          , warn
                                          , readDataFile)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia)
import Text.Pandoc.Compat.Time (UTCTime)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds, POSIXTime )
import Text.Pandoc.MIME (MimeType, getMimeType)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified System.Environment as IO (lookupEnv)
import System.FilePath.Glob (match, compile)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as IO (glob)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)
import Data.Word (Word8)
import Data.Typeable

class Monad m => PandocMonad m where
  lookupEnv :: String -> m (Maybe String)
  getCurrentTime :: m UTCTime
  getDefaultReferenceDocx :: Maybe FilePath -> m Archive
  getDefaultReferenceODT :: Maybe FilePath -> m Archive
  newStdGen :: m StdGen
  newUniqueHash :: m Int
  readFileLazy :: FilePath -> m BL.ByteString
  readDataFile :: Maybe FilePath -> FilePath -> m B.ByteString
  fetchItem :: Maybe String ->
               String -> 
               m (Either E.SomeException (B.ByteString, Maybe MimeType))
  fetchItem' :: MediaBag ->
                Maybe String ->
                String -> 
                m (Either E.SomeException (B.ByteString, Maybe MimeType))
  warn :: String -> m ()
  fail :: String -> m b
  glob :: String -> m [FilePath]

--Some functions derived from Primitives:

getPOSIXTime :: (PandocMonad m) => m POSIXTime
getPOSIXTime = utcTimeToPOSIXSeconds <$> getCurrentTime

instance PandocMonad IO where  
  lookupEnv = IO.lookupEnv
  getCurrentTime = IO.getCurrentTime
  getDefaultReferenceDocx = IO.getDefaultReferenceDocx
  getDefaultReferenceODT = IO.getDefaultReferenceODT
  newStdGen = IO.newStdGen
  newUniqueHash = hashUnique <$> IO.newUnique
  readFileLazy = BL.readFile
  readDataFile = IO.readDataFile
  fail = M.fail
  fetchItem = IO.fetchItem
  fetchItem' = IO.fetchItem'
  warn = IO.warn
  glob = IO.glob



data TestState = TestState { stStdGen     :: StdGen
                           , stWord8Store :: [Word8] -- should be
                                                     -- inifinite,
                                                     -- i.e. [1..]
                           , stWarnings   :: [String]
                           , stUniqStore  :: [Int] -- should be
                                                   -- inifinite and
                                                   -- contain every
                                                   -- element at most
                                                   -- once, e.g. [1..]
                           }

data TestEnv = TestEnv { envEnv :: [(String, String)]
                       , envTime :: UTCTime
                       , envReferenceDocx :: Archive
                       , envReferenceODT :: Archive
                       , envFiles :: [(FilePath, B.ByteString)]
                       , envUserDataDir :: [(FilePath, B.ByteString)]
                       , envCabalDataDir :: [(FilePath, B.ByteString)]
                       , envFontFiles :: [FilePath]
                       }

data TestException = TestException
  deriving (Show, Typeable)

instance E.Exception TestException

type Testing = ReaderT TestEnv (State TestState)

instance PandocMonad Testing where
  lookupEnv s = do
    env <- asks envEnv
    return (lookup s env)

  getCurrentTime = asks envTime

  getDefaultReferenceDocx _ = asks envReferenceDocx

  getDefaultReferenceODT _ = asks envReferenceODT

  newStdGen = do
    g <- gets stStdGen
    let (_, nxtGen) = next g
    modify $ \st -> st { stStdGen = nxtGen }
    return g

  newUniqueHash = do
    uniqs <- gets stUniqStore
    case uniqs of
      u : us -> do
        modify $ \st -> st { stUniqStore = us }
        return u
      _ -> M.fail "uniq store ran out of elements"

  readFileLazy fp =   do
    fps <- asks envFiles
    case lookup fp fps of
      Just bs -> return (BL.fromStrict bs)
      Nothing -> error "openFile: does not exist"

  readDataFile Nothing "reference.docx" = do
    (B.concat . BL.toChunks . fromArchive) <$> (getDefaultReferenceDocx Nothing)
  readDataFile Nothing "reference.odt" = do
    (B.concat . BL.toChunks . fromArchive) <$> (getDefaultReferenceODT Nothing)
  readDataFile Nothing fname = do
    let fname' = if fname == "MANUAL.txt" then fname else "data" </> fname
    BL.toStrict <$> (readFileLazy fname')
  readDataFile (Just userDir) fname = do
    userDirFiles <- asks envUserDataDir
    case lookup (userDir </> fname) userDirFiles of
      Just bs -> return bs
      Nothing -> readDataFile Nothing fname

  fail = M.fail

  fetchItem _ fp = do
    fps <- asks envFiles
    case lookup fp fps of
      Just bs -> return (Right (bs, getMimeType fp))
      Nothing -> return (Left $ E.toException TestException)

  fetchItem' media sourceUrl nm = do
    case lookupMedia nm media of
      Nothing -> fetchItem sourceUrl nm
      Just (mime, bs) -> return (Right (B.concat $ BL.toChunks bs, Just mime))

  warn s =  modify $ \st -> st { stWarnings = s : stWarnings st }

  glob s = do
    fontFiles <- asks envFontFiles
    return (filter (match (compile s)) fontFiles)

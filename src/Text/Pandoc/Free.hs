{-# LANGUAGE DeriveFunctor #-}

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
   Module      : Text.Pandoc.Free
   Copyright   : Copyright (C) 2016 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Pure implementations of the IO monads used in Pandoc's readers and writers.
-}

module Text.Pandoc.Free ( PandocActionF(..)
                        , PandocAction
                        , runIO
                        , liftF
                        --
                        , lookupEnv
                        , getCurrentTime
                        , getPOSIXTime
                        , getDefaultReferenceDocx
                        , getDefaultReferenceODT
                        , newStdGen
                        , newUnique
                        , newUUID
                        , readFileStrict
                        , readFileLazy
                        , readFileUTF8
                        , readDataFile
                        , fetchItem
                        , fetchItem'
                        , warn
                        , fail
                        , newIORef
                        , modifyIORef
                        , readIORef
                        , namesMatching
                        ) where

import Prelude hiding (readFile, fail)
import qualified Control.Monad as M (fail)
import System.Random (StdGen)
import qualified System.Random as IO (newStdGen)
import Codec.Archive.Zip (Archive)
import Data.Unique (Unique)
import qualified Data.Unique as IO (newUnique)
import qualified Text.Pandoc.Shared as IO ( fetchItem
                                          , fetchItem'
                                          , getDefaultReferenceDocx
                                          , getDefaultReferenceODT
                                          , warn
                                          , readDataFile)
import Text.Pandoc.MediaBag (MediaBag)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Time.Clock.POSIX as IO (getPOSIXTime)
import Text.Pandoc.Compat.Time (UTCTime)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Text.Pandoc.MIME (MimeType)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Free
import qualified Control.Exception as E
import qualified System.Environment as IO (lookupEnv)
import Data.IORef (IORef)
import qualified Data.IORef as IO (newIORef, modifyIORef, readIORef)
import Text.Pandoc.UUID (UUID)
import qualified Text.Pandoc.UUID as IO (getRandomUUID)
import qualified Text.Pandoc.UTF8 as UTF8 (readFile)
import qualified System.FilePath.Glob as IO (namesMatching)

data PandocActionF ref nxt =
  LookupEnv String (Maybe String -> nxt)
  | GetCurrentTime (UTCTime -> nxt)
  | GetPOSIXTime (POSIXTime -> nxt)
  | GetDefaultReferenceDocx (Maybe FilePath) (Archive -> nxt)
  | GetDefaultReferenceODT (Maybe FilePath) (Archive -> nxt)
  | NewStdGen (StdGen -> nxt)
  | NewUnique (Unique -> nxt)
  | NewUUID (UUID -> nxt)
  | ReadFileStrict FilePath (B.ByteString -> nxt)
  | ReadFileLazy FilePath (BL.ByteString -> nxt)
  | ReadFileUTF8 FilePath (String -> nxt)
  | ReadDataFile (Maybe FilePath) FilePath (B.ByteString -> nxt)
  | FetchItem (Maybe String) (String)
    (Either E.SomeException (B.ByteString, Maybe MimeType) -> nxt)
  | FetchItem' MediaBag (Maybe String) (String)
    (Either E.SomeException (B.ByteString, Maybe MimeType) -> nxt)
  | NewIORef ref (IORef ref -> nxt)
  | ModifyIORef (IORef ref) (ref -> ref) nxt
  | ReadIORef (IORef ref) (ref -> nxt)
  | NamesMatching String ([FilePath] -> nxt)
  | Warn String nxt
  | Fail String
  deriving Functor

type PandocAction a = Free (PandocActionF a)

lookupEnv :: String -> PandocAction a (Maybe String)
lookupEnv s = liftF $ LookupEnv s id

getCurrentTime :: PandocAction a UTCTime
getCurrentTime = liftF $ GetCurrentTime id

getPOSIXTime :: PandocAction a POSIXTime
getPOSIXTime = liftF $ GetPOSIXTime id

getDefaultReferenceDocx :: Maybe FilePath -> PandocAction a Archive
getDefaultReferenceDocx fp = liftF $ GetDefaultReferenceDocx fp id

getDefaultReferenceODT :: Maybe FilePath -> PandocAction a Archive
getDefaultReferenceODT fp = liftF $ GetDefaultReferenceODT fp id

newStdGen :: PandocAction a StdGen
newStdGen = liftF $ NewStdGen id

newUnique :: PandocAction a Unique
newUnique = liftF $ NewUnique id

newUUID :: PandocAction a UUID
newUUID = liftF $ NewUUID id

readFileStrict :: FilePath -> PandocAction a B.ByteString
readFileStrict fp = liftF $ ReadFileStrict fp id

readFileLazy :: FilePath -> PandocAction a BL.ByteString
readFileLazy fp = liftF $ ReadFileLazy fp id

readFileUTF8 :: FilePath -> PandocAction a String
readFileUTF8 fp = liftF $ ReadFileUTF8 fp id

readDataFile :: Maybe FilePath -> FilePath -> PandocAction a B.ByteString
readDataFile mfp fp = liftF $ ReadDataFile mfp fp id

fetchItem :: Maybe String ->
             String -> 
             PandocAction a (Either E.SomeException (B.ByteString, Maybe MimeType))
fetchItem ms s = liftF $ FetchItem ms s id


fetchItem' :: MediaBag ->
              Maybe String ->
              String -> 
              PandocAction a (Either E.SomeException (B.ByteString, Maybe MimeType))
fetchItem' mb ms s = liftF $ FetchItem' mb ms s id

warn :: String -> PandocAction a ()
warn s = liftF $ Warn s ()

fail :: String -> PandocAction a b
fail s = liftF $ Fail s

newIORef :: a -> PandocAction a (IORef a)
newIORef v = liftF $ NewIORef v id

modifyIORef :: (IORef a) -> (a -> a) -> PandocAction a ()
modifyIORef ref f = liftF $ ModifyIORef ref f ()

readIORef :: (IORef a) -> PandocAction a a
readIORef ref = liftF $ ReadIORef ref id

namesMatching :: String -> PandocAction a [FilePath]
namesMatching s = liftF $ NamesMatching s id

runIO :: PandocAction ref nxt -> IO nxt
runIO (Free (LookupEnv s f)) = IO.lookupEnv s >>= runIO . f
runIO (Free (GetCurrentTime f)) = IO.getCurrentTime >>= runIO . f
runIO (Free (GetPOSIXTime f)) = IO.getPOSIXTime >>= runIO . f
runIO (Free (GetDefaultReferenceDocx mfp f)) =
  IO.getDefaultReferenceDocx mfp >>= runIO . f
runIO (Free (GetDefaultReferenceODT mfp f)) =
  IO.getDefaultReferenceODT mfp >>= runIO . f
runIO (Free (NewStdGen f)) = IO.newStdGen >>= runIO . f
runIO (Free (NewUnique f)) = IO.newUnique >>= runIO . f
runIO (Free (NewUUID f))   = IO.getRandomUUID >>= runIO . f
runIO (Free (ReadFileStrict fp f)) = B.readFile fp >>= runIO . f
runIO (Free (ReadFileLazy fp f)) = BL.readFile fp >>= runIO . f
runIO (Free (ReadFileUTF8 fp f)) = UTF8.readFile fp >>= runIO . f
runIO (Free (ReadDataFile mfp fp f)) = IO.readDataFile mfp fp >>= runIO . f
runIO (Free (Fail s)) = M.fail s
runIO (Free (FetchItem sourceUrl nm f)) =
  IO.fetchItem sourceUrl nm >>= runIO . f
runIO (Free (FetchItem' media sourceUrl nm f)) =
  IO.fetchItem' media sourceUrl nm >>= runIO . f
runIO (Free (Warn s nxt)) = IO.warn s >> runIO nxt
runIO (Free (NewIORef v f)) = IO.newIORef v >>= runIO . f
runIO (Free (ModifyIORef ref f nxt)) = IO.modifyIORef ref f >> runIO nxt
runIO (Free (ReadIORef ref f)) = IO.readIORef ref >>= runIO . f
runIO (Free (NamesMatching s f)) = IO.namesMatching s >>= runIO . f
runIO (Pure r) = return r

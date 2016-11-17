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
                        , runTest
                        , TestState(..)
                        , TestEnv(..)
                        , liftF
                        --
                        , lookupEnv
                        , getCurrentTime
                        , getPOSIXTime
                        , getDefaultReferenceDocx
                        , getDefaultReferenceODT
                        , newStdGen
                        , newUniqueHash
                        , newUUID
                        , readFileStrict
                        , readFileLazy
                        , readFileUTF8
                        , readDataFile
                        , fetchItem
                        , fetchItem'
                        , warn
                        , fail
                        , glob
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
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Data.Time.Clock.POSIX as IO (getPOSIXTime)
import Text.Pandoc.Compat.Time (UTCTime)
import qualified Text.Pandoc.Compat.Time as IO (getCurrentTime)
import Text.Pandoc.MIME (MimeType, getMimeType)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Free
import qualified Control.Exception as E
import qualified System.Environment as IO (lookupEnv)
import Text.Pandoc.UUID
import qualified Text.Pandoc.UUID as IO (getRandomUUID)
import qualified Text.Pandoc.UTF8 as UTF8 (readFile, toString)
import System.FilePath.Glob (match, compile)
import System.FilePath ((</>))
import qualified System.FilePath.Glob as IO (glob)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)
import Data.Word (Word8)

data PandocActionF nxt =
  LookupEnv String (Maybe String -> nxt)
  | GetCurrentTime (UTCTime -> nxt)
  | GetPOSIXTime (POSIXTime -> nxt)
  | GetDefaultReferenceDocx (Maybe FilePath) (Archive -> nxt)
  | GetDefaultReferenceODT (Maybe FilePath) (Archive -> nxt)
  | NewStdGen (StdGen -> nxt)
  | NewUniqueHash (Int -> nxt)
  | NewUUID (UUID -> nxt)
  | ReadFileStrict FilePath (B.ByteString -> nxt)
  | ReadFileLazy FilePath (BL.ByteString -> nxt)
  | ReadFileUTF8 FilePath (String -> nxt)
  | ReadDataFile (Maybe FilePath) FilePath (B.ByteString -> nxt)
  | FetchItem (Maybe String) (String)
    (Either E.SomeException (B.ByteString, Maybe MimeType) -> nxt)
  | FetchItem' MediaBag (Maybe String) (String)
    (Either E.SomeException (B.ByteString, Maybe MimeType) -> nxt)
  | Glob String ([FilePath] -> nxt)
  | Warn String nxt
  | Fail String
  deriving Functor

type PandocAction = Free PandocActionF

lookupEnv :: String -> PandocAction (Maybe String)
lookupEnv s = liftF $ LookupEnv s id

getCurrentTime :: PandocAction UTCTime
getCurrentTime = liftF $ GetCurrentTime id

getPOSIXTime :: PandocAction POSIXTime
getPOSIXTime = liftF $ GetPOSIXTime id

getDefaultReferenceDocx :: Maybe FilePath -> PandocAction Archive
getDefaultReferenceDocx fp = liftF $ GetDefaultReferenceDocx fp id

getDefaultReferenceODT :: Maybe FilePath -> PandocAction Archive
getDefaultReferenceODT fp = liftF $ GetDefaultReferenceODT fp id

newStdGen :: PandocAction StdGen
newStdGen = liftF $ NewStdGen id

newUniqueHash :: PandocAction Int
newUniqueHash = liftF $ NewUniqueHash id

newUUID :: PandocAction UUID
newUUID = liftF $ NewUUID id

readFileStrict :: FilePath -> PandocAction B.ByteString
readFileStrict fp = liftF $ ReadFileStrict fp id

readFileLazy :: FilePath -> PandocAction BL.ByteString
readFileLazy fp = liftF $ ReadFileLazy fp id

readFileUTF8 :: FilePath -> PandocAction String
readFileUTF8 fp = liftF $ ReadFileUTF8 fp id

readDataFile :: Maybe FilePath -> FilePath -> PandocAction B.ByteString
readDataFile mfp fp = liftF $ ReadDataFile mfp fp id

fetchItem :: Maybe String ->
             String -> 
             PandocAction (Either E.SomeException (B.ByteString, Maybe MimeType))
fetchItem ms s = liftF $ FetchItem ms s id


fetchItem' :: MediaBag ->
              Maybe String ->
              String -> 
              PandocAction (Either E.SomeException (B.ByteString, Maybe MimeType))
fetchItem' mb ms s = liftF $ FetchItem' mb ms s id

warn :: String -> PandocAction ()
warn s = liftF $ Warn s ()

fail :: String -> PandocAction b
fail s = liftF $ Fail s

glob :: String -> PandocAction [FilePath]
glob s = liftF $ Glob s id

runIO :: PandocAction nxt -> IO nxt
runIO (Free (LookupEnv s f)) = IO.lookupEnv s >>= runIO . f
runIO (Free (GetCurrentTime f)) = IO.getCurrentTime >>= runIO . f
runIO (Free (GetPOSIXTime f)) = IO.getPOSIXTime >>= runIO . f
runIO (Free (GetDefaultReferenceDocx mfp f)) =
  IO.getDefaultReferenceDocx mfp >>= runIO . f
runIO (Free (GetDefaultReferenceODT mfp f)) =
  IO.getDefaultReferenceODT mfp >>= runIO . f
runIO (Free (NewStdGen f)) = IO.newStdGen >>= runIO . f
runIO (Free (NewUniqueHash f)) = hashUnique <$> IO.newUnique >>= runIO . f
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
runIO (Free (Glob s f)) = IO.glob s >>= runIO . f
runIO (Pure r) = return r

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
  deriving (Show)

instance E.Exception TestException

type Testing = ReaderT TestEnv (State TestState) 

runTest :: PandocAction nxt -> Testing nxt
runTest (Free (LookupEnv s f)) = do
  env <- asks envEnv
  return (lookup s env) >>= runTest . f
runTest (Free (GetCurrentTime f)) =
  asks envTime >>= runTest . f
runTest (Free (GetPOSIXTime f)) =
  (utcTimeToPOSIXSeconds <$> asks envTime) >>= runTest . f
runTest (Free (GetDefaultReferenceDocx _ f)) =
  asks envReferenceDocx >>= runTest . f
runTest (Free (GetDefaultReferenceODT _ f)) =
  asks envReferenceODT >>= runTest . f
runTest (Free (NewStdGen f)) = do
  g <- gets stStdGen
  let (_, nxtGen) = next g
  modify $ \st -> st { stStdGen = nxtGen }
  return g >>= runTest . f
runTest (Free (NewUniqueHash f)) = do
  uniqs <- gets stUniqStore
  case uniqs of
    u : us -> do
      modify $ \st -> st { stUniqStore = us }
      return u >>= runTest . f
    _ -> M.fail "uniq store ran out of elements"
runTest (Free (NewUUID f)) = do
  word8s <- gets stWord8Store
  case word8s of
    -- note we use f' because f is a param of the function
    a:b:c:d:e:f':g:h:i:j:k:l:m:n:o:p:remaining -> do
      modify $ \st -> st { stWord8Store = remaining }
      return (UUID a b c d e f' g h i j k l m n o p) >>= runTest . f
    _ -> M.fail "word8 supply was not infinite"
runTest (Free (ReadFileStrict fp f)) = do
  fps <- asks envFiles
  case lookup fp fps of
    Just bs -> return bs >>= runTest . f
    Nothing -> error "openFile: does not exist"
runTest (Free (ReadFileLazy fp f)) = do
  fps <- asks envFiles
  case lookup fp fps of
    Just bs -> return (BL.fromStrict bs) >>= runTest . f
    Nothing -> error "openFile: does not exist"
runTest (Free (ReadFileUTF8 fp f)) = do
  fps <- asks envFiles
  case lookup fp fps of
    Just bs -> return (UTF8.toString bs) >>= runTest . f
    Nothing -> error "openFile: does not exist"
-- A few different cases of readDataFile to reimplement, for when
-- there is no filepath and it falls through to readDefaultDataFile
runTest (Free (ReadDataFile Nothing "reference.docx" f)) = do
  (B.concat . BL.toChunks . fromArchive) <$>
    (runTest $ getDefaultReferenceDocx Nothing) >>=
    runTest . f
runTest (Free (ReadDataFile Nothing "reference.odt" f)) = do
  (B.concat . BL.toChunks . fromArchive) <$>
    (runTest $ getDefaultReferenceODT Nothing) >>=
    runTest . f
runTest (Free (ReadDataFile Nothing fname f)) = do
  let fname' = if fname == "MANUAL.txt" then fname else "data" </> fname
  runTest (readFileStrict fname') >>= runTest . f
runTest (Free (ReadDataFile (Just userDir) fname f)) = do
  userDirFiles <- asks envUserDataDir
  case lookup (userDir </> fname) userDirFiles of
    Just bs -> return bs >>= runTest . f
    Nothing -> runTest (readDataFile Nothing fname) >>= runTest . f
runTest (Free (Fail s)) = M.fail s
runTest (Free (FetchItem _ fp f)) = do
  fps <- asks envFiles
  case lookup fp fps of
    Just bs -> return (Right (bs, getMimeType fp)) >>= runTest . f
    Nothing -> return (Left $ E.toException TestException) >>= runTest . f
runTest (Free (FetchItem' media sourceUrl nm f)) = do
  case lookupMedia nm media of
    Nothing -> runTest (fetchItem sourceUrl nm) >>= runTest . f
    Just (mime, bs) -> return (Right (B.concat $ BL.toChunks bs, Just mime)) >>= runTest . f
runTest (Free (Warn s nxt)) = do
  modify $ \st -> st { stWarnings = s : stWarnings st }
  runTest nxt
runTest (Free (Glob s f)) = do
  fontFiles <- asks envFontFiles
  return (filter (match (compile s)) fontFiles) >>= runTest . f
runTest (Pure r) = return r




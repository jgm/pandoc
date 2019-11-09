{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Text.Pandoc.Legacy.Class
  ( TP.PandocMonad ( TP.getCommonState
                   , TP.putCommonState
                   , TP.getCurrentTime
                   , TP.readFileLazy
                   , TP.newStdGen
                   , TP.getModificationTime
                   , TP.getsCommonState
                   , TP.fileExists
                   , TP.readFileStrict
                   , TP.modifyCommonState
                   )
  , lookupEnv
  , openURL
  , TP.glob
  , trace
  , insertMedia
  , TP.CommonState
  , pattern CommonState
  , stInputFiles
  , stOutputFile
  , stLog
  , stTranslations
  , stRequestHeaders
  , stResourcePath
  , stSourceURL
  , stUserDataDir
  , stTrace
  , stMediaBag
  , stVerbosity
  , TP.PureState
  , pattern PureState
  , stStdGen
  , stWord8Store
  , stUniqStore
  , stEnv
  , stTime
  , stTimeZone
  , stReferenceDocx
  , stReferencePptx
  , stReferenceODT
  , stFiles
  , stUserDataFiles
  , stCabalDataFiles   
  , TP.getPureState
  , TP.getsPureState
  , TP.putPureState
  , TP.modifyPureState
  , TP.getPOSIXTime
  , TP.getZonedTime
  , readFileFromDirs
  , TP.report
  , TP.setTrace
  , setRequestHeader
  , TP.getLog
  , TP.setVerbosity
  , TP.getVerbosity
  , TP.getMediaBag
  , TP.setMediaBag
  , TP.setUserDataDir
  , TP.getUserDataDir
  , fetchItem
  , TP.getInputFiles
  , TP.setInputFiles
  , TP.getOutputFile
  , TP.setOutputFile
  , TP.setResourcePath
  , TP.getResourcePath
  , TP.PandocIO
  , TP.PandocPure
  , TP.FileTree
  , TP.FileInfo(..)
  , TP.addToFileTree
  , TP.insertInFileTree
  , TP.runIO
  , TP.runIOorExplode
  , TP.runPure
  , TP.readDefaultDataFile
  , TP.readDataFile
  , fetchMediaResource
  , TP.fillMediaBag
  , TP.extractMedia
  , toLang
  , TP.setTranslations
  , translateTerm
  , TP.Translations
  ) where

import qualified Text.Pandoc.Class as TP
import Text.Pandoc.Class (PandocMonad, FileTree)
import Text.Pandoc.Logging (LogMessage, Verbosity)
import System.Random (StdGen)
import Data.Word (Word8)
import Data.Time (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Text.Pandoc.Translations (Term, Translations)
import Codec.Archive.Zip (Archive)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.BCP47 (Lang)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.ByteString as B
import Text.Pandoc.Legacy.MIME (MimeType)

pattern CommonState :: [LogMessage] -> Maybe FilePath -> Maybe String -> [(String, String)] -> MediaBag -> Maybe (Lang, Maybe Translations) -> [FilePath] -> Maybe FilePath -> [FilePath] -> Verbosity -> Bool -> TP.CommonState
pattern CommonState
  { stLog
  , stUserDataDir
  , stSourceURL
  , stRequestHeaders
  , stMediaBag
  , stTranslations
  , stInputFiles
  , stOutputFile
  , stResourcePath
  , stVerbosity
  , stTrace } <- TP.CommonState stLog
                                stUserDataDir
                                (fmap T.unpack -> stSourceURL)
                                (fmap (\(x, y) -> (T.unpack x, T.unpack y)) -> stRequestHeaders)
                                stMediaBag
                                stTranslations
                                stInputFiles
                                stOutputFile
                                stResourcePath
                                stVerbosity
                                stTrace
  where
    CommonState a b c = TP.CommonState a b (T.pack <$> c) . fmap (\(x, y) -> (T.pack x, T.pack y))

pattern PureState :: StdGen -> [Word8] -> [Int] -> [(String, String)] -> UTCTime -> TimeZone -> Archive -> Archive -> Archive -> FileTree -> FileTree -> FileTree -> TP.PureState
pattern PureState
  { stStdGen
  , stWord8Store
  , stUniqStore
  , stEnv
  , stTime
  , stTimeZone
  , stReferenceDocx
  , stReferencePptx
  , stReferenceODT
  , stFiles
  , stUserDataFiles
  , stCabalDataFiles } <- TP.PureState stStdGen
                                       stWord8Store
                                       stUniqStore
                                       (fmap (\(x, y) -> (T.unpack x, T.unpack y)) -> stEnv)
                                       stTime
                                       stTimeZone
                                       stReferenceDocx
                                       stReferencePptx
                                       stReferenceODT
                                       stFiles
                                       stUserDataFiles
                                       stCabalDataFiles
  where
    PureState x y z = TP.PureState x y z . fmap (\(a, b) -> (T.pack a, T.pack b))

lookupEnv :: TP.PandocMonad m => String -> m (Maybe String)
lookupEnv = fmap (fmap T.unpack) . TP.lookupEnv . T.pack

openURL :: TP.PandocMonad m => String -> m (B.ByteString, Maybe MimeType)
openURL = fmap go . TP.openURL . T.pack
  where
    go (x, y) = (x, fmap T.unpack y)

trace :: TP.PandocMonad m => String -> m ()
trace = TP.trace . T.pack

setRequestHeader :: PandocMonad m
                 => String
                 -> String
                 -> m ()
setRequestHeader name = TP.setRequestHeader (T.pack name) . T.pack

insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp = TP.insertMedia fp . fmap T.pack

readFileFromDirs :: PandocMonad m => [FilePath] -> FilePath -> m (Maybe String)
readFileFromDirs x = fmap (fmap T.unpack) . TP.readFileFromDirs x

toLang :: PandocMonad m => Maybe String -> m (Maybe Lang)
toLang = TP.toLang . fmap T.pack

translateTerm :: PandocMonad m => Term -> m String
translateTerm = fmap T.unpack . TP.translateTerm

fetchItem :: PandocMonad m
          => String
          -> m (B.ByteString, Maybe MimeType)
fetchItem = fmap go . TP.fetchItem . T.pack
  where
    go (x, y) = (x, fmap T.unpack y)

fetchMediaResource :: PandocMonad m
              => String -> m (FilePath, Maybe MimeType, BL.ByteString)
fetchMediaResource = fmap go . TP.fetchMediaResource . T.pack
  where
    go (x, y, z) = (x, fmap T.unpack y, z)

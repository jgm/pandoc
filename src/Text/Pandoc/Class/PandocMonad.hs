{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Text.Pandoc.Class.PandocMonad
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

This module defines a type class, 'PandocMonad', for pandoc readers
and writers.
-}

module Text.Pandoc.Class.PandocMonad
  ( PandocMonad(..)
  , getPOSIXTime
  , getZonedTime
  , readFileFromDirs
  , report
  , setTrace
  , setRequestHeader
  , setNoCheckCertificate
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
  , readDefaultDataFile
  , readDataFile
  , fillMediaBag
  , toLang
  , setTranslations
  , translateTerm
  , makeCanonical
  , getTimestamp
  ) where

import Codec.Archive.Zip
import Control.Monad.Except (MonadError (catchError, throwError),
                             MonadTrans, lift, when)
import Data.List (foldl')
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds,
                             posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, utcToZonedTime)
import Network.URI ( escapeURIString, nonStrictRelativeTo,
                     unEscapeString, parseURIReference, isAllowedInURI,
                     parseURI, URI(..) )
import System.FilePath ((</>), takeExtension, dropExtension,
                        isRelative, splitDirectories, makeRelative)
import System.Random (StdGen)
import Text.Collate.Lang (Lang(..), parseLang, renderLang)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, getMimeType)
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, MediaItem(..))
import Text.Pandoc.Shared (uriPathToPath, safeRead)
import Text.Pandoc.Translations (Term(..), Translations, lookupTerm,
                                 readTranslations)
import Text.Pandoc.Walk (walkM)
import Text.Parsec (ParsecT, getPosition, sourceLine, sourceName)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Debug.Trace
import qualified System.FilePath.Posix as Posix
import qualified Text.Pandoc.MediaBag as MB
import qualified Text.Pandoc.UTF8 as UTF8
#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#endif

-- | The PandocMonad typeclass contains all the potentially
-- IO-related functions used in pandoc's readers and writers.
-- Instances of this typeclass may implement these functions
-- in IO (as in 'PandocIO') or using an internal state that
-- represents a file system, time, and so on (as in 'PandocPure').
class (Functor m, Applicative m, Monad m, MonadError PandocError m)
      => PandocMonad m where
  -- | Lookup an environment variable.
  lookupEnv :: T.Text -> m (Maybe T.Text)
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
  openURL :: T.Text -> m (B.ByteString, Maybe MimeType)
  -- | Read the lazy ByteString contents from a file path,
  -- raising an error on failure.
  readFileLazy :: FilePath -> m BL.ByteString
  -- | Read the strict ByteString contents from a file path,
  -- raising an error on failure.
  readFileStrict :: FilePath -> m B.ByteString
  -- | Return a list of paths that match a glob, relative to
  -- the working directory.  See 'System.FilePath.Glob' for
  -- the glob syntax.
  glob :: String -> m [FilePath]
  -- | Returns True if file exists.
  fileExists :: FilePath -> m Bool
  -- | Returns the path of data file.
  getDataFileName :: FilePath -> m FilePath
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
  -- | Output a log message.
  logOutput :: LogMessage -> m ()
  -- | Output a debug message to sterr, using 'Debug.Trace.trace',
  -- if tracing is enabled.  Note: this writes to stderr even in
  -- pure instances.
  trace :: T.Text -> m ()
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ Debug.Trace.trace ("[trace] " ++ T.unpack msg) (return ())

-- * Functions defined for all PandocMonad instances

-- | Set the verbosity level.
setVerbosity :: PandocMonad m => Verbosity -> m ()
setVerbosity verbosity =
  modifyCommonState $ \st -> st{ stVerbosity = verbosity }

-- | Get the verbosity level.
getVerbosity :: PandocMonad m => m Verbosity
getVerbosity = getsCommonState stVerbosity

-- | Get the accumulated log messages (in temporal order).
getLog :: PandocMonad m => m [LogMessage]
getLog = reverse <$> getsCommonState stLog

-- | Log a message using 'logOutput'.  Note that 'logOutput' is
-- called only if the verbosity level exceeds the level of the
-- message, but the message is added to the list of log messages
-- that will be retrieved by 'getLog' regardless of its verbosity level.
report :: PandocMonad m => LogMessage -> m ()
report msg = do
  verbosity <- getsCommonState stVerbosity
  let level = messageVerbosity msg
  when (level <= verbosity) $ logOutput msg
  modifyCommonState $ \st -> st{ stLog = msg : stLog st }

-- | Get the time from the @SOURCE_DATE_EPOCH@
-- environment variable. The variable should contain a
-- unix time stamp, the number of seconds since midnight Jan 01
-- 1970 UTC.  If the variable is not set or cannot be
-- parsed as a unix time stamp, the current time is returned.
-- This function is designed to make possible reproducible
-- builds in formats that include a creation timestamp.
getTimestamp :: PandocMonad m => m UTCTime
getTimestamp = do
  mbSourceDateEpoch <- lookupEnv "SOURCE_DATE_EPOCH"
  case mbSourceDateEpoch >>= safeRead of
    Just (epoch :: Integer) ->
      return $ posixSecondsToUTCTime $ fromIntegral epoch
    Nothing -> getCurrentTime

-- | Determine whether tracing is enabled.  This affects
-- the behavior of 'trace'.  If tracing is not enabled,
-- 'trace' does nothing.
setTrace :: PandocMonad m => Bool -> m ()
setTrace useTracing = modifyCommonState $ \st -> st{stTrace = useTracing}

-- | Set request header to use in HTTP requests.
setRequestHeader :: PandocMonad m
                 => T.Text  -- ^ Header name
                 -> T.Text  -- ^ Value
                 -> m ()
setRequestHeader name val = modifyCommonState $ \st ->
  st{ stRequestHeaders =
       (name, val) : filter (\(n,_) -> n /= name) (stRequestHeaders st)  }

-- | Determine whether certificate validation is disabled
setNoCheckCertificate :: PandocMonad m => Bool -> m ()
setNoCheckCertificate noCheckCertificate = modifyCommonState $ \st -> st{stNoCheckCertificate = noCheckCertificate}

-- | Initialize the media bag.
setMediaBag :: PandocMonad m => MediaBag -> m ()
setMediaBag mb = modifyCommonState $ \st -> st{stMediaBag = mb}

-- | Retrieve the media bag.
getMediaBag :: PandocMonad m => m MediaBag
getMediaBag = getsCommonState stMediaBag

-- | Insert an item into the media bag.
insertMedia :: PandocMonad m => FilePath -> Maybe MimeType -> BL.ByteString -> m ()
insertMedia fp mime bs = do
  mb <- getMediaBag
  let mb' = MB.insertMedia fp mime bs mb
  setMediaBag mb'

-- | Retrieve the input filenames.
getInputFiles :: PandocMonad m => m [FilePath]
getInputFiles = getsCommonState stInputFiles

-- | Set the input filenames.
setInputFiles :: PandocMonad m => [FilePath] -> m ()
setInputFiles fs = do
  let sourceURL = case fs of
                    []    -> Nothing
                    (x:_) -> case parseURI x of
                                Just u
                                  | uriScheme u `elem` ["http:","https:"] ->
                                      Just $ show u{ uriQuery = "",
                                                     uriFragment = "" }
                                _ -> Nothing

  modifyCommonState $ \st -> st{ stInputFiles = fs
                               , stSourceURL = T.pack <$> sourceURL }

-- | Retrieve the output filename.
getOutputFile :: PandocMonad m => m (Maybe FilePath)
getOutputFile = getsCommonState stOutputFile

-- | Set the output filename.
setOutputFile :: PandocMonad m => Maybe FilePath -> m ()
setOutputFile mbf = modifyCommonState $ \st -> st{ stOutputFile = mbf }

-- | Retrieve the resource path searched by 'fetchItem'.
getResourcePath :: PandocMonad m => m [FilePath]
getResourcePath = getsCommonState stResourcePath

-- | Set the resource path searched by 'fetchItem'.
setResourcePath :: PandocMonad m => [FilePath] -> m ()
setResourcePath ps = modifyCommonState $ \st -> st{stResourcePath = ps}

-- | Get the POSIX time.
getPOSIXTime :: PandocMonad m => m POSIXTime
getPOSIXTime = utcTimeToPOSIXSeconds <$> getCurrentTime

-- | Get the zoned time.
getZonedTime :: PandocMonad m => m ZonedTime
getZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

-- | Read file, checking in any number of directories.
readFileFromDirs :: PandocMonad m => [FilePath] -> FilePath -> m (Maybe T.Text)
readFileFromDirs [] _ = return Nothing
readFileFromDirs (d:ds) f = catchError
    (Just . T.pack . UTF8.toStringLazy <$> readFileLazy (d </> f))
    (\_ -> readFileFromDirs ds f)

-- | Convert BCP47 string to a Lang, issuing warning
-- if there are problems.
toLang :: PandocMonad m => Maybe T.Text -> m (Maybe Lang)
toLang Nothing = return Nothing
toLang (Just s) =
  case parseLang s of
       Left _ -> do
         report $ InvalidLang s
         return Nothing
       Right l -> return (Just l)

-- | Select the language to use with 'translateTerm'.
-- Note that this does not read a translation file;
-- that is only done the first time 'translateTerm' is
-- used.
setTranslations :: PandocMonad m => Lang -> m ()
setTranslations lang =
  modifyCommonState $ \st -> st{ stTranslations = Just (lang, Nothing) }

-- | Load term map.
getTranslations :: PandocMonad m => m Translations
getTranslations = do
  mbtrans <- getsCommonState stTranslations
  case mbtrans of
       Nothing -> return mempty  -- no language defined
       Just (_, Just t) -> return t
       Just (lang, Nothing) -> do  -- read from file
         let translationFile = "translations/" <> renderLang lang <> ".yaml"
         let fallbackFile = "translations/" <> langLanguage lang <> ".yaml"
         let getTrans fp = do
               bs <- readDataFile fp
               case readTranslations (UTF8.toText bs) of
                    Left e   -> do
                      report $ CouldNotLoadTranslations (renderLang lang)
                        (T.pack fp <> ": " <> e)
                      -- make sure we don't try again...
                      modifyCommonState $ \st ->
                        st{ stTranslations = Nothing }
                      return mempty
                    Right t -> do
                      modifyCommonState $ \st ->
                                  st{ stTranslations = Just (lang, Just t) }
                      return t
         catchError (getTrans $ T.unpack translationFile)
           (\_ ->
             catchError (getTrans $ T.unpack fallbackFile)
               (\e -> do
                 report $ CouldNotLoadTranslations (renderLang lang)
                          $ case e of
                               PandocCouldNotFindDataFileError _ ->
                                 "data file " <> fallbackFile <> " not found"
                               _ -> ""
                 -- make sure we don't try again...
                 modifyCommonState $ \st -> st{ stTranslations = Nothing }
                 return mempty))

-- | Get a translation from the current term map.
-- Issue a warning if the term is not defined.
translateTerm :: PandocMonad m => Term -> m T.Text
translateTerm term = do
  translations <- getTranslations
  case lookupTerm term translations of
       Just s -> return s
       Nothing -> do
         report $ NoTranslation $ T.pack $ show term
         return ""

-- | Specialized version of parseURIReference that disallows
-- single-letter schemes.  Reason:  these are usually windows absolute
-- paths.
parseURIReference' :: T.Text -> Maybe URI
parseURIReference' s = do
  u <- parseURIReference (T.unpack s)
  case uriScheme u of
       [_] -> Nothing
       _   -> Just u

-- | Set the user data directory in common state.
setUserDataDir :: PandocMonad m
               => Maybe FilePath
               -> m ()
setUserDataDir mbfp = modifyCommonState $ \st -> st{ stUserDataDir = mbfp }

-- | Get the user data directory from common state.
getUserDataDir :: PandocMonad m
               => m (Maybe FilePath)
getUserDataDir = getsCommonState stUserDataDir

-- | Fetch an image or other item from the local filesystem or the net.
-- Returns raw content and maybe mime type.
fetchItem :: PandocMonad m
          => T.Text
          -> m (B.ByteString, Maybe MimeType)
fetchItem s = do
  mediabag <- getMediaBag
  case lookupMedia (T.unpack s) mediabag of
    Just item -> return (BL.toStrict (mediaContents item),
                         Just (mediaMimeType item))
    Nothing -> downloadOrRead s

-- | Returns the content and, if available, the MIME type of a resource.
-- If the given resource location is a valid URI, then download the
-- resource from that URI. Otherwise, treat the resource identifier as a
-- local file name.
--
-- Note that resources are treated relative to the URL of the first
-- input source, if any.
downloadOrRead :: PandocMonad m
               => T.Text
               -> m (B.ByteString, Maybe MimeType)
downloadOrRead s = do
  sourceURL <- getsCommonState stSourceURL
  case (sourceURL >>= parseURIReference' .
                       ensureEscaped, ensureEscaped s) of
    (Just u, s') -> -- try fetching from relative path at source
       case parseURIReference' s' of
            Just u' -> openURL $ T.pack $ show $ u' `nonStrictRelativeTo` u
            Nothing -> openURL s' -- will throw error
    (Nothing, s'@(T.unpack -> ('/':'/':c:_))) | c /= '?' ->  -- protocol-relative URI
                -- we exclude //? because of //?UNC/ on Windows
       case parseURIReference' s' of
            Just u' -> openURL $ T.pack $ show $ u' `nonStrictRelativeTo` httpcolon
            Nothing -> openURL s' -- will throw error
    (Nothing, s') ->
       case parseURI (T.unpack s') of  -- requires absolute URI
            Just u' | uriScheme u' == "file:" ->
                 readLocalFile $ uriPathToPath (T.pack $ uriPath u')
            -- We don't want to treat C:/ as a scheme:
            Just u' | length (uriScheme u') > 2 -> openURL (T.pack $ show u')
            _ -> readLocalFile fp -- get from local file system
   where readLocalFile f = do
             resourcePath <- getResourcePath
             (fp', cont) <- if isRelative f
                               then withPaths resourcePath readFileStrict f
                               else (f,) <$> readFileStrict f
             report $ LoadedResource f (makeRelative "." fp')
             return (cont, mime)
         httpcolon = URI{ uriScheme = "http:",
                          uriAuthority = Nothing,
                          uriPath = "",
                          uriQuery = "",
                          uriFragment = "" }
         dropFragmentAndQuery = T.takeWhile (\c -> c /= '?' && c /= '#')
         fp = unEscapeString $ T.unpack $ dropFragmentAndQuery s
         mime = getMimeType $ case takeExtension fp of
                     ".gz" -> dropExtension fp
                     ".svgz" -> dropExtension fp ++ ".svg"
                     x     -> x
         ensureEscaped = T.pack . escapeURIString isAllowedInURI . T.unpack . T.map convertSlash
         convertSlash '\\' = '/'
         convertSlash x    = x

-- | Retrieve default reference.docx.
getDefaultReferenceDocx :: PandocMonad m => m Archive
getDefaultReferenceDocx = do
  let paths = ["[Content_Types].xml",
               "_rels/.rels",
               "docProps/app.xml",
               "docProps/core.xml",
               "docProps/custom.xml",
               "word/document.xml",
               "word/fontTable.xml",
               "word/footnotes.xml",
               "word/comments.xml",
               "word/numbering.xml",
               "word/settings.xml",
               "word/webSettings.xml",
               "word/styles.xml",
               "word/_rels/document.xml.rels",
               "word/_rels/footnotes.xml.rels",
               "word/theme/theme1.xml"]
  let toLazy = BL.fromChunks . (:[])
  let pathToEntry path = do
        epochtime <- floor . utcTimeToPOSIXSeconds <$> getCurrentTime
        contents <- toLazy <$> readDataFile ("docx/" ++ path)
        return $ toEntry path epochtime contents
  datadir <- getUserDataDir
  mbArchive <- case datadir of
                    Nothing   -> return Nothing
                    Just d    -> do
                       exists <- fileExists (d </> "reference.docx")
                       if exists
                          then return (Just (d </> "reference.docx"))
                          else return Nothing
  case mbArchive of
     Just arch -> toArchive <$> readFileLazy arch
     Nothing   -> foldr addEntryToArchive emptyArchive <$>
                     mapM pathToEntry paths

-- | Retrieve default reference.odt.
getDefaultReferenceODT :: PandocMonad m => m Archive
getDefaultReferenceODT = do
  let paths = ["mimetype",
               "manifest.rdf",
               "styles.xml",
               "content.xml",
               "meta.xml",
               "settings.xml",
               "Configurations2/accelerator/current.xml",
               "Thumbnails/thumbnail.png",
               "META-INF/manifest.xml"]
  let pathToEntry path = do epochtime <- floor `fmap` getPOSIXTime
                            contents <- (BL.fromChunks . (:[])) `fmap`
                                          readDataFile ("odt/" ++ path)
                            return $ toEntry path epochtime contents
  datadir <- getUserDataDir
  mbArchive <- case datadir of
                    Nothing   -> return Nothing
                    Just d    -> do
                       exists <- fileExists (d </> "reference.odt")
                       if exists
                          then return (Just (d </> "reference.odt"))
                          else return Nothing
  case mbArchive of
     Just arch -> toArchive <$> readFileLazy arch
     Nothing   -> foldr addEntryToArchive emptyArchive <$>
                     mapM pathToEntry paths

-- | Retrieve default reference.pptx.
getDefaultReferencePptx :: PandocMonad m => m Archive
getDefaultReferencePptx = do
  -- We're going to narrow this down substantially once we get it
  -- working.
  let paths = [ "[Content_Types].xml"
              , "_rels/.rels"
              , "docProps/app.xml"
              , "docProps/core.xml"
              , "ppt/_rels/presentation.xml.rels"
              , "ppt/presProps.xml"
              , "ppt/presentation.xml"
              , "ppt/slideLayouts/_rels/slideLayout1.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout2.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout3.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout4.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout5.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout6.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout7.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout8.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout9.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout10.xml.rels"
              , "ppt/slideLayouts/_rels/slideLayout11.xml.rels"
              , "ppt/slideLayouts/slideLayout1.xml"
              , "ppt/slideLayouts/slideLayout10.xml"
              , "ppt/slideLayouts/slideLayout11.xml"
              , "ppt/slideLayouts/slideLayout2.xml"
              , "ppt/slideLayouts/slideLayout3.xml"
              , "ppt/slideLayouts/slideLayout4.xml"
              , "ppt/slideLayouts/slideLayout5.xml"
              , "ppt/slideLayouts/slideLayout6.xml"
              , "ppt/slideLayouts/slideLayout7.xml"
              , "ppt/slideLayouts/slideLayout8.xml"
              , "ppt/slideLayouts/slideLayout9.xml"
              , "ppt/slideMasters/_rels/slideMaster1.xml.rels"
              , "ppt/slideMasters/slideMaster1.xml"
              , "ppt/slides/_rels/slide1.xml.rels"
              , "ppt/slides/slide1.xml"
              , "ppt/slides/_rels/slide2.xml.rels"
              , "ppt/slides/slide2.xml"
              , "ppt/slides/_rels/slide3.xml.rels"
              , "ppt/slides/slide3.xml"
              , "ppt/slides/_rels/slide4.xml.rels"
              , "ppt/slides/slide4.xml"
              , "ppt/tableStyles.xml"
              , "ppt/theme/theme1.xml"
              , "ppt/viewProps.xml"
              -- These relate to notes slides.
              , "ppt/notesMasters/notesMaster1.xml"
              , "ppt/notesMasters/_rels/notesMaster1.xml.rels"
              , "ppt/notesSlides/notesSlide1.xml"
              , "ppt/notesSlides/_rels/notesSlide1.xml.rels"
              , "ppt/notesSlides/notesSlide2.xml"
              , "ppt/notesSlides/_rels/notesSlide2.xml.rels"
              , "ppt/theme/theme2.xml"
              ]
  let toLazy = BL.fromChunks . (:[])
  let pathToEntry path = do
        epochtime <- floor . utcTimeToPOSIXSeconds <$> getCurrentTime
        contents <- toLazy <$> readDataFile ("pptx/" ++ path)
        return $ toEntry path epochtime contents
  datadir <- getUserDataDir
  mbArchive <- case datadir of
                    Nothing   -> return Nothing
                    Just d    -> do
                       exists <- fileExists (d </> "reference.pptx")
                       if exists
                          then return (Just (d </> "reference.pptx"))
                          else return Nothing
  case mbArchive of
     Just arch -> toArchive <$> readFileLazy arch
     Nothing   -> foldr addEntryToArchive emptyArchive <$>
                     mapM pathToEntry paths

-- | Read file from user data directory or,
-- if not found there, from the default data files.
readDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDataFile fname = do
  datadir <- getUserDataDir
  case datadir of
       Nothing -> readDefaultDataFile fname
       Just userDir -> do
         exists <- fileExists (userDir </> fname)
         if exists
            then readFileStrict (userDir </> fname)
            else readDefaultDataFile fname

-- | Read file from from the default data files.
readDefaultDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDefaultDataFile "reference.docx" =
  B.concat . BL.toChunks . fromArchive <$> getDefaultReferenceDocx
readDefaultDataFile "reference.pptx" =
  B.concat . BL.toChunks . fromArchive <$> getDefaultReferencePptx
readDefaultDataFile "reference.odt" =
  B.concat . BL.toChunks . fromArchive <$> getDefaultReferenceODT
readDefaultDataFile fname =
#ifdef EMBED_DATA_FILES
  case lookup (makeCanonical fname) dataFiles of
    Nothing       -> throwError $ PandocCouldNotFindDataFileError $ T.pack fname
    Just contents -> return contents
#else
  getDataFileName fname' >>= checkExistence >>= readFileStrict
    where fname' = if fname == "MANUAL.txt" then fname else "data" </> fname

-- | Returns the input filename unchanged if the file exits, and throws
-- a `PandocCouldNotFindDataFileError` if it doesn't.
checkExistence :: PandocMonad m => FilePath -> m FilePath
checkExistence fn = do
  exists <- fileExists fn
  if exists
     then return fn
     else throwError $ PandocCouldNotFindDataFileError $ T.pack fn
#endif

-- | Canonicalizes a file path by removing redundant @.@ and @..@.
makeCanonical :: FilePath -> FilePath
makeCanonical = Posix.joinPath . transformPathParts . splitDirectories
 where  transformPathParts = reverse . foldl' go []
        go as     "."  = as
        go (_:as) ".." = as
        go as     x    = x : as

-- | Tries to run an action on a file: for each directory given, a
-- filepath is created from the given filename, and the action is run on
-- that filepath. Returns the result of the first successful execution
-- of the action, or throws a @PandocResourceNotFound@ exception if the
-- action errors for all filepaths.
withPaths :: PandocMonad m
          => [FilePath] -> (FilePath -> m a) -> FilePath -> m (FilePath, a)
withPaths [] _ fp = throwError $ PandocResourceNotFound $ T.pack fp
withPaths (p:ps) action fp =
  catchError ((p </> fp,) <$> action (p </> fp))
             (\_ -> withPaths ps action fp)

-- | Traverse tree, filling media bag for any images that
-- aren't already in the media bag.
fillMediaBag :: PandocMonad m => Pandoc -> m Pandoc
fillMediaBag d = walkM handleImage d
  where handleImage :: PandocMonad m => Inline -> m Inline
        handleImage (Image attr lab (src, tit)) = catchError
          (do mediabag <- getMediaBag
              let fp = T.unpack src
              case lookupMedia fp mediabag of
                Just _ -> return ()
                Nothing -> do
                  (bs, mt) <- fetchItem src
                  insertMedia fp mt (BL.fromStrict bs)
              return $ Image attr lab (src, tit))
          (\e ->
              case e of
                PandocResourceNotFound _ -> do
                  report $ CouldNotFetchResource src
                            "replacing image with description"
                  -- emit alt text
                  return $ Span ("",["image"],[]) lab
                PandocHttpError u er -> do
                  report $ CouldNotFetchResource u
                            (T.pack $ show er ++ "\rReplacing image with description.")
                  -- emit alt text
                  return $ Span ("",["image"],[]) lab
                _ -> throwError e)
        handleImage x = return x

-- This requires UndecidableInstances.  We could avoid that
-- by repeating the definitions below for every monad transformer
-- we use: ReaderT, WriterT, StateT, RWST.  But this seems to
-- be harmless.
instance (MonadTrans t, PandocMonad m, Functor (t m),
          MonadError PandocError (t m), Monad (t m),
          Applicative (t m)) => PandocMonad (t m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  glob = lift . glob
  fileExists = lift . fileExists
  getDataFileName = lift . getDataFileName
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState
  logOutput = lift . logOutput

instance {-# OVERLAPS #-} PandocMonad m => PandocMonad (ParsecT s st m) where
  lookupEnv = lift . lookupEnv
  getCurrentTime = lift getCurrentTime
  getCurrentTimeZone = lift getCurrentTimeZone
  newStdGen = lift newStdGen
  newUniqueHash = lift newUniqueHash
  openURL = lift . openURL
  readFileLazy = lift . readFileLazy
  readFileStrict = lift . readFileStrict
  glob = lift . glob
  fileExists = lift . fileExists
  getDataFileName = lift . getDataFileName
  getModificationTime = lift . getModificationTime
  getCommonState = lift getCommonState
  putCommonState = lift . putCommonState
  trace msg = do
    tracing <- getsCommonState stTrace
    when tracing $ do
      pos <- getPosition
      Debug.Trace.trace
        ("[trace] Parsed " ++ T.unpack msg ++ " at line " ++
            show (sourceLine pos) ++
            if sourceName pos == "chunk"
               then " of chunk"
               else "")
        (return ())
  logOutput = lift . logOutput

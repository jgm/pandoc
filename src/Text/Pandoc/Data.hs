{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef EMBED_DATA_FILES
{-# LANGUAGE TemplateHaskell #-}
#endif
{- |
Module      : Text.Pandoc.Data
Copyright   : Copyright (C) 2013-2023 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Access to pandoc's data files.
-}
module Text.Pandoc.Data ( readDefaultDataFile
                        , readDataFile
                        , getDataFileNames
                        , defaultUserDataDir
                        ) where
import Text.Pandoc.Class (PandocMonad(..), checkUserDataDir, getTimestamp,
                          getUserDataDir, getPOSIXTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Codec.Archive.Zip
import qualified Data.Text as T
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(..))
import System.FilePath
import System.Directory
import qualified Control.Exception as E
#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data.BakedIn (dataFiles)
import Text.Pandoc.Shared (makeCanonical)
#else
import Paths_pandoc (getDataDir)
#endif

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

--- | Read file from user data directory or,
--- if not found there, from the default data files.
readDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDataFile fname = do
  datadir <- checkUserDataDir fname
  case datadir of
       Nothing -> readDefaultDataFile fname
       Just userDir -> do
         exists <- fileExists (userDir </> fname)
         if exists
            then readFileStrict (userDir </> fname)
            else readDefaultDataFile fname

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
        epochtime <- floor . utcTimeToPOSIXSeconds <$> getTimestamp
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
        epochtime <- floor <$> getPOSIXTime
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

getDataFileNames :: IO [FilePath]
getDataFileNames = do
#ifdef EMBED_DATA_FILES
  let allDataFiles = map fst dataFiles
#else
  allDataFiles <- filter (\x -> x /= "." && x /= "..") <$>
                      (getDataDir >>= getDirectoryContents)
#endif
  return $ "reference.docx" : "reference.odt" : "reference.pptx" : allDataFiles

-- | Return appropriate user data directory for platform.  We use
-- XDG_DATA_HOME (or its default value), but for backwards compatibility,
-- we fall back to the legacy user data directory ($HOME/.pandoc on *nix)
-- if the XDG_DATA_HOME is missing and this exists.  If neither directory
-- is present, we return the XDG data directory.  If the XDG data directory
-- is not defined (e.g. because we are in an environment where $HOME is
-- not defined), we return the empty string.
defaultUserDataDir :: IO FilePath
defaultUserDataDir = do
  xdgDir <- E.catch (getXdgDirectory XdgData "pandoc")
               (\(_ :: E.SomeException) -> return mempty)
  legacyDir <- getAppUserDataDirectory "pandoc"
  xdgExists <- doesDirectoryExist xdgDir
  legacyDirExists <- doesDirectoryExist legacyDir
  if not xdgExists && legacyDirExists
     then return legacyDir
     else return xdgDir

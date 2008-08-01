{-# LANGUAGE TemplateHaskell #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.ODT
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for producing an ODT file from OpenDocument XML.
-}
module Text.Pandoc.ODT ( saveOpenDocumentAsODT ) where
import Text.Pandoc.Shared ( contentsOf )
import Data.Maybe ( fromJust, isJust )
import Data.List ( partition, intersperse )
import Prelude hiding ( writeFile, readFile )
import System.IO.UTF8
import System.IO ( stderr )
import System.Directory
import System.FilePath ( (</>), takeDirectory, takeFileName, splitDirectories )
import System.Process ( runCommand, waitForProcess )
import System.Exit
import Text.XML.Light
import Text.XML.Light.Cursor
import Text.Pandoc.Shared ( withTempDir )
import Network.URI ( isURI )
import qualified Data.ByteString.Char8 as B ( writeFile, pack )
import Control.Monad ( unless )

-- | Produce an ODT file from OpenDocument XML.
saveOpenDocumentAsODT :: FilePath    -- ^ Pathname of ODT file to be produced.
                      -> FilePath    -- ^ Relative directory of source file.
                      -> String      -- ^ OpenDocument XML contents.
                      -> IO ()
saveOpenDocumentAsODT destinationODTPath sourceDirRelative xml = do
  let zipCmd = "zip"
  -- check for zip in path:
  findExecutable zipCmd >>= \v -> unless (isJust v) $ error $ "The '" ++ zipCmd ++
                                      "' command, which is needed to build an ODT file, was not found.\n" ++
                                      "It can be obtained from http://www.info-zip.org/Zip.html\n" ++
                                      "Debian (and Debian-based) linux: apt-get install zip\n" ++
                                      "Windows: See http://gnuwin32.sourceforge.net/packages/zip.htm"
  withTempDir "pandoc-odt" $ \tempDir -> do
    let tempODT = tempDir </> "reference.odt"
    copyFile "odt-styles/reference.odt" tempODT 
    B.writeFile tempODT $ B.pack $(contentsOf $ "odt-styles" </> "reference.odt")
    createDirectory $ tempDir </> "Pictures"
    xml' <- handlePictures tempODT sourceDirRelative xml
    writeFile (tempDir </> "content.xml") xml'
    oldDir <- getCurrentDirectory
    setCurrentDirectory tempDir
    let zipCmdLine = "zip -9 -q -r " ++ tempODT ++ " " ++ "content.xml Pictures"
    ec <- runCommand zipCmdLine >>= waitForProcess   -- this requires compilation with -threaded
    setCurrentDirectory oldDir
    case ec of
         ExitSuccess -> copyFile tempODT destinationODTPath
         _           -> error "Error creating ODT." >> exitWith ec


-- | Find <draw:image ... /> elements and copy the file (xlink:href attribute) into Pictures/ in
-- the zip file.  If filename is a URL, attempt to download it.  Modify xlink:href attributes
-- to point to the new locations in Pictures/.  Return modified XML.
handlePictures :: FilePath     -- ^ Path of ODT file in temp directory
               -> FilePath     -- ^ Directory (relative) containing source file
               -> String       -- ^ OpenDocument XML string
               -> IO String    -- ^ Modified XML
handlePictures tempODT sourceDirRelative xml = do
  let parsed = case parseXMLDoc xml of
                Nothing -> error "Could not parse OpenDocument XML."
                Just x  -> x
  let cursor = case (fromForest $ elContent parsed) of
                   Nothing -> error "ODT appears empty"
                   Just x  -> x
  cursor' <- scanPictures tempODT sourceDirRelative cursor
  let modified = parsed { elContent = toForest $ root cursor' }
  return $ showTopElement modified

scanPictures :: FilePath -> FilePath -> Cursor -> IO Cursor
scanPictures tempODT sourceDirRelative cursor = do
  cursor' <- handleTree tempODT sourceDirRelative cursor
  case right cursor' of
       Just n  -> scanPictures tempODT sourceDirRelative n
       Nothing -> return cursor'

handleTree :: FilePath -> FilePath -> Cursor -> IO Cursor
handleTree tempODT sourceDirRelative cursor = do
  case firstChild cursor of
       Nothing -> modifyContentM (handleContent tempODT sourceDirRelative) cursor
       Just n  -> scanPictures tempODT sourceDirRelative n >>= return . fromJust . parent

-- | If content is an image link, handle it appropriately.
-- Otherwise, handle children if any.
handleContent :: FilePath -> FilePath -> Content -> IO Content
handleContent tempODT sourceDirRelative content@(Elem el) = do
  if qName (elName el) == "image"
     then do
        let (hrefs, rest) = partition (\a -> qName (attrKey a) == "href") $ elAttribs el
        let href = case hrefs of
                          []     -> error $ "No href found in " ++ show el
                          [x]    -> x
                          _      -> error $ "Multiple hrefs found in " ++ show el
        if isURI $ attrVal href
           then return content
           else do  -- treat as filename
             let oldLoc = sourceDirRelative </> attrVal href
             fileExists <- doesFileExist oldLoc
             if fileExists
                then do
                   let pref = take 230 $ concat $ intersperse "_" $
                                splitDirectories $ takeDirectory $ attrVal href
                   let newLoc = "Pictures" </> pref ++ "_" ++ (takeFileName $ attrVal href)
                   let tempDir = takeDirectory tempODT
                   copyFile oldLoc $ tempDir </> newLoc
                   let newAttrs = (href { attrVal = newLoc }) : rest
                   return $ Elem (el { elAttribs = newAttrs })
                else do
                   hPutStrLn stderr $ "Warning:  Unable to find image at " ++ oldLoc ++ " - ignoring."
                   return content
     else return content
 
handleContent _ _ c = return c  -- not Element


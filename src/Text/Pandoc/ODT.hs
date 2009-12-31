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
import Data.List ( find )
import System.FilePath ( (</>), takeFileName )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )
import Prelude hiding ( writeFile, readFile )
import Codec.Archive.Zip
import Control.Applicative ( (<$>) )
import Text.ParserCombinators.Parsec
import System.Time
import Text.Pandoc.Shared ( inDirectory )
import Paths_pandoc ( getDataFileName )

-- | Produce an ODT file from OpenDocument XML.
saveOpenDocumentAsODT :: FilePath    -- ^ Pathname of ODT file to be produced.
                      -> FilePath    -- ^ Relative directory of source file.
                      -> String      -- ^ OpenDocument XML contents.
                      -> IO ()
saveOpenDocumentAsODT destinationODTPath sourceDirRelative xml = do
  refArchivePath <- getDataFileName $ "data" </> "odt-styles"
  refArchive <- inDirectory refArchivePath $
                  addFilesToArchive [OptRecursive] emptyArchive ["."]
  -- handle pictures
  let (newContents, pics) = 
        case runParser pPictures [] "OpenDocument XML contents" xml of
          Left err          -> error $ show err
          Right x           -> x
  picEntries <- mapM (makePictureEntry sourceDirRelative) pics 
  (TOD epochTime _) <- getClockTime
  let contentEntry = toEntry "content.xml" epochTime $ fromString newContents
  let archive = foldr addEntryToArchive refArchive (contentEntry : picEntries)
  B.writeFile destinationODTPath $ fromArchive archive

makePictureEntry :: FilePath            -- ^ Relative directory of source file
                 -> (FilePath, String)  -- ^ Path and new path of picture
                 -> IO Entry
makePictureEntry sourceDirRelative (path, newPath) = do
  entry <- readEntry [] $ sourceDirRelative </> path
  return (entry { eRelativePath = newPath })

pPictures :: GenParser Char [(FilePath, String)] ([Char], [(FilePath, String)])
pPictures = do
  contents <- concat <$> many (pPicture <|> many1 (noneOf "<") <|> string "<")
  pics <- getState
  return (contents, pics)

pPicture :: GenParser Char [(FilePath, String)] [Char]
pPicture = try $ do
  string "<draw:image xlink:href=\""
  path <- manyTill anyChar (char '"')
  let filename =  takeFileName path
  pics <- getState
  newPath <- case find (\(o, _) -> o == path) pics of
             Just (_, new) -> return new
             Nothing -> do 
                        -- get a unique name
                        let dups = length $ (filter (\(o, _) -> takeFileName o == filename)) pics 
                        let new = "Pictures/" ++ replicate dups '0' ++ filename
                        updateState ((path, new) :)
                        return new
  return $ "<draw:image xlink:href=\"" ++ newPath ++ "\"" 

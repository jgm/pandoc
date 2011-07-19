{-
Copyright (C) 2008-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.ODT
   Copyright   : Copyright (C) 2008-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ODT.
-}
module Text.Pandoc.Writers.ODT ( writeODT ) where
import Data.IORef
import Data.List ( isPrefixOf )
import System.FilePath ( (</>), takeExtension )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )
import Codec.Archive.Zip
import System.Time
import Paths_pandoc ( getDataFileName )
import Text.Pandoc.Shared ( WriterOptions(..) )
import Text.Pandoc.MIME ( getMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Writers.OpenDocument ( writeOpenDocument )
import System.Directory
import Control.Monad (liftM)
import Network.URI ( unEscapeString )
import Text.Pandoc.XML
import Text.Pandoc.Pretty

-- | Produce an ODT file from a Pandoc document.
writeODT :: Maybe FilePath -- ^ Path specified by --reference-odt
         -> WriterOptions  -- ^ Writer options
         -> Pandoc         -- ^ Document to convert
         -> IO B.ByteString
writeODT mbRefOdt opts doc = do
  let datadir = writerUserDataDir opts
  refArchive <- liftM toArchive $
       case mbRefOdt of
             Just f -> B.readFile f
             Nothing -> do
               let defaultODT = getDataFileName "reference.odt" >>= B.readFile
               case datadir of
                     Nothing  -> defaultODT
                     Just d   -> do
                        exists <- doesFileExist (d </> "reference.odt")
                        if exists
                           then B.readFile (d </> "reference.odt")
                           else defaultODT
  -- handle pictures
  picEntriesRef <- newIORef ([] :: [Entry])
  let sourceDir = writerSourceDirectory opts
  doc' <- bottomUpM (transformPic sourceDir picEntriesRef) doc
  let newContents = writeOpenDocument opts{writerWrapText = False} doc'
  (TOD epochtime _) <- getClockTime
  let contentEntry = toEntry "content.xml" epochtime $ fromString newContents
  picEntries <- readIORef picEntriesRef
  let archive = foldr addEntryToArchive refArchive $ contentEntry : picEntries
  -- construct META-INF/manifest.xml based on archive
  let toFileEntry fp = case getMimeType fp of
                        Nothing  -> empty
                        Just m   -> selfClosingTag "manifest:file-entry"
                                     [("manifest:media-type", m)
                                     ,("manifest:full-path", fp)
                                     ]
  let files = [ ent | ent <- filesInArchive archive, not ("META-INF" `isPrefixOf` ent) ]
  let manifestEntry = toEntry "META-INF/manifest.xml" epochtime
        $ fromString $ show
        $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        $$
         ( inTags True "manifest:manifest"
            [("xmlns:manifest","urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")]
            $ ( selfClosingTag "manifest:file-entry"
                 [("manifest:media-type","application/vnd.oasis.opendocument.text")
                 ,("manifest:version","1.2")
                 ,("manifest:full-path","/")]
                $$ vcat ( map toFileEntry $ files )
              )
         )
  let archive' = addEntryToArchive manifestEntry archive
  return $ fromArchive archive'

transformPic :: FilePath -> IORef [Entry] -> Inline -> IO Inline
transformPic sourceDir entriesRef (Image lab (src,tit)) = do
  let src' = unEscapeString src
  entries <- readIORef entriesRef
  let newsrc = "Pictures/" ++ show (length entries) ++ takeExtension src'
  catch (readEntry [] (sourceDir </> src') >>= \entry ->
           modifyIORef entriesRef (entry{ eRelativePath = newsrc } :) >>
           return (Image lab (newsrc, tit)))
        (\_ -> return (Emph lab))
transformPic _ _ x = return x


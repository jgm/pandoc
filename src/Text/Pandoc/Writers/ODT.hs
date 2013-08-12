{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.UTF8 ( fromStringLazy )
import Codec.Archive.Zip
import Text.Pandoc.Options ( WriterOptions(..) )
import Text.Pandoc.Shared ( stringify, readDataFile, fetchItem, warn )
import Text.Pandoc.ImageSize ( imageSize, sizeInPoints )
import Text.Pandoc.MIME ( getMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Writers.OpenDocument ( writeOpenDocument )
import Control.Monad (liftM)
import Text.Pandoc.XML
import Text.Pandoc.Pretty
import qualified Control.Exception as E
import Data.Time.Clock.POSIX ( getPOSIXTime )
import System.FilePath ( takeExtension )

-- | Produce an ODT file from a Pandoc document.
writeODT :: WriterOptions  -- ^ Writer options
         -> Pandoc         -- ^ Document to convert
         -> IO B.ByteString
writeODT opts doc@(Pandoc meta _) = do
  let datadir = writerUserDataDir opts
  let title = docTitle meta
  refArchive <- liftM toArchive $
       case writerReferenceODT opts of
             Just f -> B.readFile f
             Nothing -> (B.fromChunks . (:[])) `fmap`
                           readDataFile datadir "reference.odt"
  -- handle pictures
  picEntriesRef <- newIORef ([] :: [Entry])
  doc' <- walkM (transformPic opts picEntriesRef) doc
  let newContents = writeOpenDocument opts{writerWrapText = False} doc'
  epochtime <- floor `fmap` getPOSIXTime
  let contentEntry = toEntry "content.xml" epochtime
                     $ fromStringLazy newContents
  picEntries <- readIORef picEntriesRef
  let archive = foldr addEntryToArchive refArchive
                $ contentEntry : picEntries
  -- construct META-INF/manifest.xml based on archive
  let toFileEntry fp = case getMimeType fp of
                        Nothing  -> empty
                        Just m   -> selfClosingTag "manifest:file-entry"
                                     [("manifest:media-type", m)
                                     ,("manifest:full-path", fp)
                                     ,("manifest:version", "1.2")
                                     ]
  let files = [ ent | ent <- filesInArchive archive,
                             not ("META-INF" `isPrefixOf` ent) ]
  let manifestEntry = toEntry "META-INF/manifest.xml" epochtime
        $ fromStringLazy $ render Nothing
        $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        $$
         ( inTags True "manifest:manifest"
            [("xmlns:manifest","urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")
            ,("manifest:version","1.2")]
            $ ( selfClosingTag "manifest:file-entry"
                 [("manifest:media-type","application/vnd.oasis.opendocument.text")
                 ,("manifest:full-path","/")]
                $$ vcat ( map toFileEntry $ files )
              )
         )
  let archive' = addEntryToArchive manifestEntry archive
  let metaEntry = toEntry "meta.xml" epochtime
       $ fromStringLazy $ render Nothing
       $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
       $$
        ( inTags True "office:document-meta"
           [("xmlns:office","urn:oasis:names:tc:opendocument:xmlns:office:1.0")
           ,("xmlns:xlink","http://www.w3.org/1999/xlink")
           ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
           ,("xmlns:meta","urn:oasis:names:tc:opendocument:xmlns:meta:1.0")
           ,("xmlns:ooo","http://openoffice.org/2004/office")
           ,("xmlns:grddl","http://www.w3.org/2003/g/data-view#")
           ,("office:version","1.2")]
           $ ( inTagsSimple "office:meta"
                $ ( inTagsSimple "dc:title" (text $ escapeStringForXML (stringify title))
                  )
             )
        )
  -- make sure mimetype is first
  let mimetypeEntry = toEntry "mimetype" epochtime
                      $ fromStringLazy "application/vnd.oasis.opendocument.text"
  let archive'' = addEntryToArchive mimetypeEntry
                  $ addEntryToArchive metaEntry archive'
  return $ fromArchive archive''

transformPic :: WriterOptions -> IORef [Entry] -> Inline -> IO Inline
transformPic opts entriesRef (Image lab (src,_)) = do
  res <- fetchItem (writerSourceURL opts) src
  case res of
     Left (_ :: E.SomeException) -> do
       warn $ "Could not find image `" ++ src ++ "', skipping..."
       return $ Emph lab
     Right (img, _) -> do
       let size = imageSize img
       let (w,h) = maybe (0,0) id $ sizeInPoints `fmap` size
       let tit' = show w ++ "x" ++ show h
       entries <- readIORef entriesRef
       let newsrc = "Pictures/" ++ show (length entries) ++ takeExtension src
       let toLazy = B.fromChunks . (:[])
       epochtime <- floor `fmap` getPOSIXTime
       let entry = toEntry newsrc epochtime $ toLazy img
       modifyIORef entriesRef (entry:)
       return $ Image lab (newsrc, tit')
transformPic _ _ x = return x


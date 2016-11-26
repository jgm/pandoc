{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2008-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2008-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ODT.
-}
module Text.Pandoc.Writers.ODT ( writeODT ) where
import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe )
import Text.XML.Light.Output
import Text.TeXMath
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.UTF8 ( fromStringLazy )
import Codec.Archive.Zip
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..) )
import Text.Pandoc.Shared ( stringify )
import Text.Pandoc.ImageSize
import Text.Pandoc.MIME ( getMimeType, extensionFromMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared ( fixDisplayMath )
import Text.Pandoc.Writers.OpenDocument ( writeOpenDocument )
import Control.Monad.State
import Text.Pandoc.XML
import Text.Pandoc.Pretty
import qualified Control.Exception as E
import System.FilePath ( takeExtension, takeDirectory, (<.>))
import Text.Pandoc.Class ( PandocMonad )
import qualified Text.Pandoc.Class as P

data ODTState = ODTState { stEntries :: [Entry]
                         }

type O m = StateT ODTState m

-- | Produce an ODT file from a Pandoc document.
writeODT :: PandocMonad m
         => WriterOptions  -- ^ Writer options
         -> Pandoc         -- ^ Document to convert
         -> m B.ByteString
writeODT  opts doc =
  let initState = ODTState{ stEntries = []
                          }
  in
    evalStateT (pandocToODT opts doc) initState

-- | Produce an ODT file from a Pandoc document.
pandocToODT :: PandocMonad m
            => WriterOptions  -- ^ Writer options
            -> Pandoc         -- ^ Document to convert
            -> O m B.ByteString
pandocToODT opts doc@(Pandoc meta _) = do
  let datadir = writerUserDataDir opts
  let title = docTitle meta
  refArchive <-
       case writerReferenceODT opts of
             Just f -> liftM toArchive $ lift $ P.readFileLazy f
             Nothing -> lift $ P.getDefaultReferenceODT datadir
  -- handle formulas and pictures
  -- picEntriesRef <- P.newIORef ([] :: [Entry])
  doc' <- walkM (transformPicMath opts) $ walk fixDisplayMath doc
  newContents <- lift $ writeOpenDocument opts{writerWrapText = WrapNone} doc'
  epochtime <- floor `fmap` (lift P.getPOSIXTime)
  let contentEntry = toEntry "content.xml" epochtime
                     $ fromStringLazy newContents
  picEntries <- gets stEntries
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
  let formulas = [ takeDirectory ent ++ "/" | ent <- filesInArchive archive,
                      "Formula-" `isPrefixOf` ent, takeExtension ent == ".xml" ]
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
                $$ vcat ( map toFileEntry $ formulas )
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

-- | transform both Image and Math elements
transformPicMath :: PandocMonad m => WriterOptions ->Inline -> O m Inline
transformPicMath opts (Image attr@(id', cls, _) lab (src,t)) = do
  res <- lift $ P.fetchItem' (writerMediaBag opts) (writerSourceURL opts) src
  case res of
     Left (_ :: E.SomeException) -> do
       lift $ P.warn $ "Could not find image `" ++ src ++ "', skipping..."
       return $ Emph lab
     Right (img, mbMimeType) -> do
       (ptX, ptY) <- case imageSize img of
                       Right s  -> return $ sizeInPoints s
                       Left msg -> do
                         lift $ P.warn $ "Could not determine image size in `" ++
                           src ++ "': " ++ msg
                         return (100, 100)
       let dims =
             case (getDim Width, getDim Height) of
               (Just w, Just h)              -> [("width", show w), ("height", show h)]
               (Just w@(Percent _), Nothing) -> [("width", show w), ("style:rel-height", "scale")]
               (Nothing, Just h@(Percent _)) -> [("style:rel-width", "scale"), ("height", show h)]
               (Just w@(Inch i), Nothing)    -> [("width", show w), ("height", show (i / ratio) ++ "in")]
               (Nothing, Just h@(Inch i))    -> [("width", show (i * ratio) ++ "in"), ("height", show h)]
               _                             -> [("width", show ptX ++ "pt"), ("height", show ptY ++ "pt")]
             where
               ratio = ptX / ptY
               getDim dir = case (dimension dir attr) of
                              Just (Percent i) -> Just $ Percent i
                              Just dim         -> Just $ Inch $ inInch opts dim
                              Nothing          -> Nothing
       let  newattr = (id', cls, dims)
       entries <- gets stEntries
       let extension = fromMaybe (takeExtension $ takeWhile (/='?') src)
                           (mbMimeType >>= extensionFromMimeType)
       let newsrc = "Pictures/" ++ show (length entries) <.> extension
       let toLazy = B.fromChunks . (:[])
       epochtime <- floor `fmap` (lift P.getPOSIXTime)
       let entry = toEntry newsrc epochtime $ toLazy img
       modify $ \st -> st{ stEntries = entry : entries }
       return $ Image newattr lab (newsrc, t)
transformPicMath _ (Math t math) = do
  entries <- gets stEntries
  let dt = if t == InlineMath then DisplayInline else DisplayBlock
  case writeMathML dt <$> readTeX math of
       Left  _ -> return $ Math t math
       Right r -> do
         let conf = useShortEmptyTags (const False) defaultConfigPP
         let mathml = ppcTopElement conf r
         epochtime <- floor `fmap` (lift $ P.getPOSIXTime)
         let dirname = "Formula-" ++ show (length entries) ++ "/"
         let fname = dirname ++ "content.xml"
         let entry = toEntry fname epochtime (fromStringLazy mathml)
         modify $ \st -> st{ stEntries = entry : entries }
         return $ RawInline (Format "opendocument") $ render Nothing $
           inTags False "draw:frame" [("text:anchor-type",
                                       if t == DisplayMath
                                          then "paragraph"
                                          else "as-char")
                                     ,("style:vertical-pos", "middle")
                                     ,("style:vertical-rel", "text")] $
             selfClosingTag "draw:object" [("xlink:href", dirname)
                                        , ("xlink:type", "simple")
                                        , ("xlink:show", "embed")
                                        , ("xlink:actuate", "onLoad")]

transformPicMath _ x = return x

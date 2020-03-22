{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.ODT
   Copyright   : Copyright (C) 2008-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ODT.
-}
module Text.Pandoc.Writers.ODT ( writeODT ) where
import Codec.Archive.Zip
import Control.Monad.Except (catchError)
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as B
import Data.Generics (everywhere', mkT)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import System.FilePath (takeDirectory, takeExtension, (<.>))
import Text.Pandoc.BCP47 (Lang (..), getLang, renderLang)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, toLang)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (extensionFromMimeType, getMimeType)
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..))
import Text.DocLayout
import Text.Pandoc.Shared (stringify, pandocVersion, tshow)
import Text.Pandoc.Writers.Shared (lookupMetaString, lookupMetaBlocks,
                                   fixDisplayMath)
import Text.Pandoc.UTF8 (fromStringLazy, fromTextLazy, toStringLazy)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.OpenDocument (writeOpenDocument)
import Text.Pandoc.XML
import Text.TeXMath
import Text.XML.Light

newtype ODTState = ODTState { stEntries :: [Entry]
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
  let title = docTitle meta
  let authors = docAuthors meta
  utctime <- P.getCurrentTime
  lang <- toLang (getLang opts meta)
  refArchive <-
       case writerReferenceDoc opts of
             Just f -> liftM toArchive $ lift $ P.readFileLazy f
             Nothing -> lift $ (toArchive . B.fromStrict) <$>
                                P.readDataFile "reference.odt"
  -- handle formulas and pictures
  -- picEntriesRef <- P.newIORef ([] :: [Entry])
  doc' <- walkM (transformPicMath opts) $ walk fixDisplayMath doc
  newContents <- lift $ writeOpenDocument opts{writerWrapText = WrapNone} doc'
  epochtime <- floor `fmap` lift P.getPOSIXTime
  let contentEntry = toEntry "content.xml" epochtime
                     $ fromTextLazy $ TL.fromStrict newContents
  picEntries <- gets stEntries
  let archive = foldr addEntryToArchive refArchive
                $ contentEntry : picEntries
  -- construct META-INF/manifest.xml based on archive
  let toFileEntry fp = case getMimeType fp of
                        Nothing  -> empty
                        Just m   -> selfClosingTag "manifest:file-entry"
                                     [("manifest:media-type", m)
                                     ,("manifest:full-path", T.pack fp)
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
         inTags True "manifest:manifest"
            [("xmlns:manifest","urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")
            ,("manifest:version","1.2")] ( selfClosingTag "manifest:file-entry"
                 [("manifest:media-type","application/vnd.oasis.opendocument.text")
                 ,("manifest:full-path","/")]
                $$ vcat ( map toFileEntry files )
                $$ vcat ( map toFileEntry formulas )
              )
  let archive' = addEntryToArchive manifestEntry archive
  -- create meta.xml
  let userDefinedMetaFields = [k | k <- Map.keys (unMeta meta)
                              , k `notElem` ["title", "lang", "author"
                                           , "description", "subject", "keywords"]]
  let escapedText = text . T.unpack . escapeStringForXML
  let keywords = case lookupMeta "keywords" meta of
                      Just (MetaList xs) -> map stringify xs
                      _                  -> []
  let userDefinedMeta =
        map (\k -> inTags False "meta:user-defined"
              [ ("meta:name", escapeStringForXML k)
              ,("meta:value-type", "string")
              ] (escapedText $ lookupMetaString k meta)) userDefinedMetaFields
  let metaTag metafield = inTagsSimple metafield . escapedText
  let metaEntry = toEntry "meta.xml" epochtime
       $ fromStringLazy $ render Nothing
       $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
       $$
        inTags True "office:document-meta"
           [("xmlns:office","urn:oasis:names:tc:opendocument:xmlns:office:1.0")
           ,("xmlns:xlink","http://www.w3.org/1999/xlink")
           ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
           ,("xmlns:meta","urn:oasis:names:tc:opendocument:xmlns:meta:1.0")
           ,("xmlns:ooo","http://openoffice.org/2004/office")
           ,("xmlns:grddl","http://www.w3.org/2003/g/data-view#")
           ,("office:version","1.2")] ( inTags True "office:meta" []
                 ( metaTag "meta:generator" ("Pandoc/" <> pandocVersion)
                   $$
                   metaTag "dc:title" (stringify title)
                   $$
                   metaTag "dc:description"
                          (T.intercalate "\n" (map stringify $
                                         lookupMetaBlocks "description" meta))
                   $$
                   metaTag "dc:subject" (lookupMetaString "subject" meta)
                   $$
                   metaTag "meta:keyword" (T.intercalate ", " keywords)
                   $$
                   case lang of
                        Just l  -> metaTag "dc:language" (renderLang l)
                        Nothing -> empty
                   $$
                   (\d a -> metaTag "meta:initial-creator" a
                         $$ metaTag "dc:creator" a
                         $$ metaTag "meta:creation-date" d
                         $$ metaTag "dc:date" d
                   ) (T.pack $ formatTime defaultTimeLocale "%FT%XZ" utctime)
                     (T.intercalate "; " (map stringify authors))
                   $$
                   vcat userDefinedMeta
                 )
             )
  -- make sure mimetype is first
  let mimetypeEntry = toEntry "mimetype" epochtime
                      $ fromStringLazy "application/vnd.oasis.opendocument.text"
  archive'' <- updateStyleWithLang lang
                  $ addEntryToArchive mimetypeEntry
                  $ addEntryToArchive metaEntry archive'
  return $ fromArchive archive''

updateStyleWithLang :: PandocMonad m => Maybe Lang -> Archive -> O m Archive
updateStyleWithLang Nothing arch = return arch
updateStyleWithLang (Just lang) arch = do
  epochtime <- floor `fmap` lift P.getPOSIXTime
  return arch{ zEntries = [if eRelativePath e == "styles.xml"
                              then case parseXMLDoc
                                      (toStringLazy (fromEntry e)) of
                                      Nothing -> e
                                      Just d ->
                                        toEntry "styles.xml" epochtime
                                        ( fromStringLazy
                                        . ppTopElement
                                        . addLang lang $ d )
                              else e
                            | e <- zEntries arch] }

addLang :: Lang -> Element -> Element
addLang lang = everywhere' (mkT updateLangAttr)
    where updateLangAttr (Attr n@(QName "language" _ (Just "fo")) _)
                           = Attr n (T.unpack $ langLanguage lang)
          updateLangAttr (Attr n@(QName "country" _ (Just "fo")) _)
                           = Attr n (T.unpack $ langRegion lang)
          updateLangAttr x = x

-- | transform both Image and Math elements
transformPicMath :: PandocMonad m => WriterOptions ->Inline -> O m Inline
transformPicMath opts (Image attr@(id', cls, _) lab (src,t)) = catchError
   (do (img, mbMimeType) <- P.fetchItem src
       (ptX, ptY) <- case imageSize opts img of
                       Right s  -> return $ sizeInPoints s
                       Left msg -> do
                         report $ CouldNotDetermineImageSize src msg
                         return (100, 100)
       let dims =
             case (getDim Width, getDim Height) of
               (Just w, Just h)              -> [("width", tshow w), ("height", tshow h)]
               (Just w@(Percent _), Nothing) -> [("rel-width", tshow w),("rel-height", "scale"),("width", tshow ptX <> "pt"),("height", tshow ptY <> "pt")]
               (Nothing, Just h@(Percent _)) -> [("rel-width", "scale"),("rel-height", tshow h),("width", tshow ptX <> "pt"),("height", tshow ptY <> "pt")]
               (Just w@(Inch i), Nothing)    -> [("width", tshow w), ("height", tshow (i / ratio) <> "in")]
               (Nothing, Just h@(Inch i))    -> [("width", tshow (i * ratio) <> "in"), ("height", tshow h)]
               _                             -> [("width", tshow ptX <> "pt"), ("height", tshow ptY <> "pt")]
             where
               ratio = ptX / ptY
               getDim dir = case dimension dir attr of
                              Just (Percent i) -> Just $ Percent i
                              Just dim         -> Just $ Inch $ inInch opts dim
                              Nothing          -> Nothing
       let  newattr = (id', cls, dims)
       entries <- gets stEntries
       let extension = maybe (takeExtension $ takeWhile (/='?') $ T.unpack src) T.unpack
                           (mbMimeType >>= extensionFromMimeType)
       let newsrc = "Pictures/" ++ show (length entries) <.> extension
       let toLazy = B.fromChunks . (:[])
       epochtime <- floor `fmap` lift P.getPOSIXTime
       let entry = toEntry newsrc epochtime $ toLazy img
       modify $ \st -> st{ stEntries = entry : entries }
       return $ Image newattr lab (T.pack newsrc, t))
   (\e -> do
       report $ CouldNotFetchResource src $ T.pack (show e)
       return $ Emph lab)

transformPicMath _ (Math t math) = do
  entries <- gets stEntries
  let dt = if t == InlineMath then DisplayInline else DisplayBlock
  case writeMathML dt <$> readTeX math of
       Left  _ -> return $ Math t math
       Right r -> do
         let conf = useShortEmptyTags (const False) defaultConfigPP
         let mathml = ppcTopElement conf r
         epochtime <- floor `fmap` lift P.getPOSIXTime
         let dirname = "Formula-" ++ show (length entries) ++ "/"
         let fname = dirname ++ "content.xml"
         let entry = toEntry fname epochtime (fromStringLazy mathml)
         let fname' = dirname ++ "settings.xml"
         let entry' = toEntry fname' epochtime $ documentSettings (t == InlineMath)
         modify $ \st -> st{ stEntries = entry' : (entry : entries) }
         return $ RawInline (Format "opendocument") $ render Nothing $
           inTags False "draw:frame" (if t == DisplayMath
                                      then [("draw:style-name","fr2")
                                           -- `draw:frame` does not support either
                                           -- `style:vertical-pos` or `style:vertical-rel`,
                                           -- therefore those attributes must go into the
                                           -- `style:style` element
                                           ,("text:anchor-type","paragraph")]
                                      else [("draw:style-name","fr1")
                                           ,("text:anchor-type","as-char")]) $
             selfClosingTag "draw:object" [("xlink:href", T.pack dirname)
                                        , ("xlink:type", "simple")
                                        , ("xlink:show", "embed")
                                        , ("xlink:actuate", "onLoad")]

transformPicMath _ x = return x

documentSettings :: Bool -> B.ByteString
documentSettings isTextMode = fromStringLazy $ render Nothing
    $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
    $$
    inTags True "office:document-settings"
      [("xmlns:office","urn:oasis:names:tc:opendocument:xmlns:office:1.0")
      ,("xmlns:xlink","http://www.w3.org/1999/xlink")
      ,("xmlns:config","urn:oasis:names:tc:opendocument:xmlns:config:1.0")
      ,("xmlns:ooo","http://openoffice.org/2004/office")
      ,("office:version","1.2")] (
       inTagsSimple "office:settings" $
         inTags False "config:config-item-set"
           [("config:name", "ooo:configuration-settings")] $
           inTags False "config:config-item" [("config:name", "IsTextMode")
                                             ,("config:type", "boolean")] $
                                              text $ if isTextMode then "true" else "false")

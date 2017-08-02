{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-
Copyright (C) 2012-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Codec.Archive.Zip
import Control.Applicative ((<|>))
import Control.Monad.Except (catchError)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (isSpace, ord, toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Skylighting
import System.Random (randomR)
import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Compat.Time
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Highlighting (highlight)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType,
                         getMimeTypeDef)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Docx.StyleMap
import Text.Pandoc.Shared hiding (Element)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared (fixDisplayMath)
import Text.Pandoc.BCP47 (getLang, renderLang, toLang)
import Text.Printf (printf)
import Text.TeXMath
import Text.XML.Light as XML
import Text.XML.Light.Cursor as XMLC

data ListMarker = NoMarker
                | BulletMarker
                | NumberMarker ListNumberStyle ListNumberDelim Int
                deriving (Show, Read, Eq, Ord)

listMarkerToId :: ListMarker -> String
listMarkerToId NoMarker = "990"
listMarkerToId BulletMarker = "991"
listMarkerToId (NumberMarker sty delim n) =
  '9' : '9' : styNum : delimNum : show n
  where styNum = case sty of
                      DefaultStyle -> '2'
                      Example      -> '3'
                      Decimal      -> '4'
                      LowerRoman   -> '5'
                      UpperRoman   -> '6'
                      LowerAlpha   -> '7'
                      UpperAlpha   -> '8'
        delimNum = case delim of
                      DefaultDelim -> '0'
                      Period       -> '1'
                      OneParen     -> '2'
                      TwoParens    -> '3'

data WriterEnv = WriterEnv{ envTextProperties :: [Element]
                          , envParaProperties :: [Element]
                          , envRTL            :: Bool
                          , envListLevel      :: Int
                          , envListNumId      :: Int
                          , envInDel          :: Bool
                          , envChangesAuthor  :: String
                          , envChangesDate    :: String
                          , envPrintWidth     :: Integer
                          }

defaultWriterEnv :: WriterEnv
defaultWriterEnv = WriterEnv{ envTextProperties = []
                            , envParaProperties = []
                            , envRTL = False
                            , envListLevel = -1
                            , envListNumId = 1
                            , envInDel = False
                            , envChangesAuthor  = "unknown"
                            , envChangesDate    = "1969-12-31T19:00:00Z"
                            , envPrintWidth     = 1
                            }

data WriterState = WriterState{
         stFootnotes      :: [Element]
       , stSectionIds     :: Set.Set String
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, String, Maybe MimeType, Element, B.ByteString)
       , stLists          :: [ListMarker]
       , stInsId          :: Int
       , stDelId          :: Int
       , stStyleMaps      :: StyleMaps
       , stFirstPara      :: Bool
       , stTocTitle       :: [Inline]
       , stDynamicParaProps :: Set.Set String
       , stDynamicTextProps :: Set.Set String
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stFootnotes      = defaultFootnotes
      , stSectionIds     = Set.empty
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stLists          = [NoMarker]
      , stInsId          = 1
      , stDelId          = 1
      , stStyleMaps      = defaultStyleMaps
      , stFirstPara      = False
      , stTocTitle       = [Str "Table of Contents"]
      , stDynamicParaProps = Set.empty
      , stDynamicTextProps = Set.empty
      }

type WS m = ReaderT WriterEnv (StateT WriterState m)

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (nodename k) v) attrs) .  node (nodename s)

nodename :: String -> QName
nodename s = QName{ qName = name, qURI = Nothing, qPrefix = prefix }
 where (name, prefix) = case break (==':') s of
                             (xs,[])    -> (xs, Nothing)
                             (ys, _:zs) -> (zs, Just ys)

toLazy :: B.ByteString -> BL.ByteString
toLazy = BL.fromChunks . (:[])

renderXml :: Element -> BL.ByteString
renderXml elt = BL8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
  UTF8.fromStringLazy (showElement elt)

renumIdMap :: Int -> [Element] -> M.Map String String
renumIdMap _ [] = M.empty
renumIdMap n (e:es)
  | Just oldId <- findAttr (QName "Id" Nothing Nothing) e =
      M.insert oldId ("rId" ++ (show n)) (renumIdMap (n+1) es)
  | otherwise = renumIdMap n es

replaceAttr :: (QName -> Bool) -> String -> [XML.Attr] -> [XML.Attr]
replaceAttr _ _ [] = []
replaceAttr f val (a:as) | f (attrKey a) =
                             (XML.Attr (attrKey a) val) : (replaceAttr f val as)
                         | otherwise = a : (replaceAttr f val as)

renumId :: (QName -> Bool) -> (M.Map String String) -> Element -> Element
renumId f renumMap e
  | Just oldId <- findAttrBy f e
  , Just newId <- M.lookup oldId renumMap =
    let attrs' = replaceAttr f newId (elAttribs e)
    in
     e { elAttribs = attrs' }
  | otherwise = e

renumIds :: (QName -> Bool) -> (M.Map String String) -> [Element] -> [Element]
renumIds f renumMap = map (renumId f renumMap)

-- | Certain characters are invalid in XML even if escaped.
-- See #1992
stripInvalidChars :: String -> String
stripInvalidChars = filter isValidChar

-- | See XML reference
isValidChar :: Char -> Bool
isValidChar (ord -> c)
  | c == 0x9                      = True
  | c == 0xA                      = True
  | c == 0xD                      = True
  | 0x20 <= c &&  c <= 0xD7FF     = True
  | 0xE000 <= c && c <= 0xFFFD    = True
  | 0x10000 <= c && c <= 0x10FFFF = True
  | otherwise                     = False

metaValueToInlines :: MetaValue -> [Inline]
metaValueToInlines (MetaString s)    = [Str s]
metaValueToInlines (MetaInlines ils) = ils
metaValueToInlines (MetaBlocks bs)   = query return bs
metaValueToInlines (MetaBool b)      = [Str $ show b]
metaValueToInlines _                 = []



writeDocx :: (PandocMonad m)
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m BL.ByteString
writeDocx opts doc@(Pandoc meta _) = do
  let datadir = writerUserDataDir opts
  let doc' = walk fixDisplayMath $ doc
  username <- P.lookupEnv "USERNAME"
  utctime <- P.getCurrentTime
  distArchive <- (toArchive . BL.fromStrict) <$>
                      P.readDataFile Nothing "reference.docx"
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive <$> P.readFileLazy f
                     Nothing -> (toArchive . BL.fromStrict) <$>
                        P.readDataFile datadir "reference.docx"

  parsedDoc <- parseXml refArchive distArchive "word/document.xml"
  let wname f qn = qPrefix qn == Just "w" && f (qName qn)
  let mbsectpr = filterElementName (wname (=="sectPr")) parsedDoc

  -- Gets the template size
  let mbpgsz = mbsectpr >>= (filterElementName (wname (=="pgSz")))
  let mbAttrSzWidth = (elAttribs <$> mbpgsz) >>= (lookupAttrBy ((=="w") . qName))

  let mbpgmar = mbsectpr >>= (filterElementName (wname (=="pgMar")))
  let mbAttrMarLeft = (elAttribs <$> mbpgmar) >>= (lookupAttrBy ((=="left") . qName))
  let mbAttrMarRight = (elAttribs <$> mbpgmar) >>= (lookupAttrBy ((=="right") . qName))

  -- Get the avaible area (converting the size and the margins to int and
  -- doing the difference
  let pgContentWidth = (-) <$> (read <$> mbAttrSzWidth ::Maybe Integer)
                       <*> (
                         (+) <$> (read <$> mbAttrMarRight ::Maybe Integer)
                         <*> (read <$> mbAttrMarLeft ::Maybe Integer)
                       )

  -- styles
  mblang <- toLang $ getLang opts meta
  let addLang :: Element -> Element
      addLang e = case mblang >>= \l ->
                         (return . XMLC.toTree . go (renderLang l)
                                 . XMLC.fromElement) e of
                    Just (Elem e') -> e'
                    _              -> e -- return original
        where go :: String -> Cursor -> Cursor
              go l cursor = case XMLC.findRec (isLangElt . current) cursor of
                              Nothing -> cursor
                              Just t  -> XMLC.modifyContent (setval l) t
              setval :: String -> Content -> Content
              setval l (Elem e') = Elem $ e'{ elAttribs = map (setvalattr l) $
                                               elAttribs e' }
              setval _ x         = x
              setvalattr :: String -> XML.Attr -> XML.Attr
              setvalattr l (XML.Attr qn@(QName "val" _ _) _) = XML.Attr qn l
              setvalattr _ x = x
              isLangElt (Elem e') = qName (elName e') == "lang"
              isLangElt _ = False

  let stylepath = "word/styles.xml"
  styledoc <- addLang <$> parseXml refArchive distArchive stylepath

  -- parse styledoc for heading styles
  let styleMaps = getStyleMaps styledoc

  let tocTitle = fromMaybe (stTocTitle defaultWriterState) $
                    metaValueToInlines <$> lookupMeta "toc-title" meta

  let initialSt = defaultWriterState {
          stStyleMaps  = styleMaps
        , stTocTitle   = tocTitle
        }

  let isRTLmeta = case lookupMeta "dir" meta of
        Just (MetaString "rtl")        -> True
        Just (MetaInlines [Str "rtl"]) -> True
        _                              -> False

  let env = defaultWriterEnv {
          envRTL = isRTLmeta
        , envChangesAuthor = fromMaybe "unknown" username
        , envChangesDate   = formatTime defaultTimeLocale "%FT%XZ" utctime
        , envPrintWidth = (maybe 420 (\x -> quot x 20) pgContentWidth)
        }


  ((contents, footnotes), st) <- runStateT
                                 (runReaderT
                                  (writeOpenXML opts{writerWrapText = WrapNone} doc')
                                  env)
                                 initialSt
  let epochtime = floor $ utcTimeToPOSIXSeconds utctime
  let imgs = M.elems $ stImages st

  -- create entries for images in word/media/...
  let toImageEntry (_,path,_,_,img) = toEntry ("word/" ++ path) epochtime $ toLazy img
  let imageEntries = map toImageEntry imgs

  let stdAttributes =
            [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
            ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
            ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
            ,("xmlns:o","urn:schemas-microsoft-com:office:office")
            ,("xmlns:v","urn:schemas-microsoft-com:vml")
            ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
            ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
            ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
            ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")]


  parsedRels <- parseXml refArchive distArchive "word/_rels/document.xml.rels"
  let isHeaderNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
  let isFooterNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
  let headers = filterElements isHeaderNode parsedRels
  let footers = filterElements isFooterNode parsedRels

  let extractTarget = findAttr (QName "Target" Nothing Nothing)

  -- we create [Content_Types].xml and word/_rels/document.xml.rels
  -- from scratch rather than reading from reference.docx,
  -- because Word sometimes changes these files when a reference.docx is modified,
  -- e.g. deleting the reference to footnotes.xml or removing default entries
  -- for image content types.

  -- [Content_Types].xml
  let mkOverrideNode (part', contentType') = mknode "Override"
               [("PartName",part'),("ContentType",contentType')] ()
  let mkImageOverride (_, imgpath, mbMimeType, _, _) =
          mkOverrideNode ("/word/" ++ imgpath,
                          fromMaybe "application/octet-stream" mbMimeType)
  let mkMediaOverride imgpath =
          mkOverrideNode ('/':imgpath, getMimeTypeDef imgpath)
  let overrides = map mkOverrideNode (
                  [("/word/webSettings.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml")
                  ,("/word/numbering.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml")
                  ,("/word/settings.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.settings+xml")
                  ,("/word/theme/theme1.xml",
                    "application/vnd.openxmlformats-officedocument.theme+xml")
                  ,("/word/fontTable.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml")
                  ,("/docProps/app.xml",
                    "application/vnd.openxmlformats-officedocument.extended-properties+xml")
                  ,("/docProps/core.xml",
                    "application/vnd.openxmlformats-package.core-properties+xml")
                  ,("/word/styles.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml")
                  ,("/word/document.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml")
                  ,("/word/footnotes.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml")
                  ] ++
                  map (\x -> (maybe "" ("/word/" ++) $ extractTarget x,
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml")) headers ++
                  map (\x -> (maybe "" ("/word/" ++) $ extractTarget x,
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml")) footers) ++
                    map mkImageOverride imgs ++
                    map mkMediaOverride [ eRelativePath e | e <- zEntries refArchive
                                        , "word/media/" `isPrefixOf` eRelativePath e ]

  let defaultnodes = [mknode "Default"
              [("Extension","xml"),("ContentType","application/xml")] (),
             mknode "Default"
              [("Extension","rels"),("ContentType","application/vnd.openxmlformats-package.relationships+xml")] ()]
  let contentTypesDoc = mknode "Types" [("xmlns","http://schemas.openxmlformats.org/package/2006/content-types")] $ defaultnodes ++ overrides
  let contentTypesEntry = toEntry "[Content_Types].xml" epochtime
        $ renderXml contentTypesDoc

  -- word/_rels/document.xml.rels
  let toBaseRel (url', id', target') = mknode "Relationship"
                                          [("Type",url')
                                          ,("Id",id')
                                          ,("Target",target')] ()
  let baserels' = map toBaseRel
                    [("http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering",
                      "rId1",
                      "numbering.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles",
                      "rId2",
                      "styles.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings",
                      "rId3",
                      "settings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings",
                      "rId4",
                      "webSettings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable",
                      "rId5",
                      "fontTable.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme",
                      "rId6",
                      "theme/theme1.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes",
                      "rId7",
                      "footnotes.xml")
                    ]

  let idMap = renumIdMap (length baserels' + 1) (headers ++ footers)
  let renumHeaders = renumIds (\q -> qName q == "Id") idMap headers
  let renumFooters = renumIds (\q -> qName q == "Id") idMap footers
  let baserels = baserels' ++ renumHeaders ++ renumFooters
  let toImgRel (ident,path,_,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",path)] ()
  let imgrels = map toImgRel imgs
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let linkrels = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc = mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")] $ baserels ++ imgrels ++ linkrels
  let relEntry = toEntry "word/_rels/document.xml.rels" epochtime
        $ renderXml reldoc


  -- adjust contents to add sectPr from reference.docx
  let sectpr = case mbsectpr of
        Just sectpr' -> let cs = renumIds
                                 (\q -> qName q == "id" && qPrefix q == Just "r")
                                 idMap
                                 (elChildren sectpr')
                        in
                         add_attrs (elAttribs sectpr') $ mknode "w:sectPr" [] cs
        Nothing      -> (mknode "w:sectPr" [] ())

  -- let sectpr = fromMaybe (mknode "w:sectPr" [] ()) mbsectpr'
  let contents' = contents ++ [sectpr]
  let docContents = mknode "w:document" stdAttributes
                    $ mknode "w:body" [] contents'



  -- word/document.xml
  let contentEntry = toEntry "word/document.xml" epochtime
                     $ renderXml docContents

  -- footnotes
  let notes = mknode "w:footnotes" stdAttributes footnotes
  let footnotesEntry = toEntry "word/footnotes.xml" epochtime $ renderXml notes

  -- footnote rels
  let footnoteRelEntry = toEntry "word/_rels/footnotes.xml.rels" epochtime
        $ renderXml $ mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")]
        linkrels

  -- styles

  -- We only want to inject paragraph and text properties that
  -- are not already in the style map. Note that keys in the stylemap
  -- are normalized as lowercase.
  let newDynamicParaProps = filter
        (\sty -> isNothing $ M.lookup (toLower <$> sty) $ getMap $ sParaStyleMap styleMaps)
        (Set.toList $ stDynamicParaProps st)

      newDynamicTextProps = filter
        (\sty -> isNothing $ M.lookup (toLower <$> sty) $ getMap $ sCharStyleMap styleMaps)
        (Set.toList $ stDynamicTextProps st)

  let newstyles = map newParaPropToOpenXml newDynamicParaProps ++
                  map newTextPropToOpenXml newDynamicTextProps ++
                  (case writerHighlightStyle opts of
                        Nothing  -> []
                        Just sty -> (styleToOpenXml styleMaps sty))
  let styledoc' = styledoc{ elContent = elContent styledoc ++
                                           map Elem newstyles }
  let styleEntry = toEntry stylepath epochtime $ renderXml styledoc'

  -- construct word/numbering.xml
  let numpath = "word/numbering.xml"
  numbering <- parseXml refArchive distArchive numpath
  newNumElts <- mkNumbering (stLists st)
  let allElts = onlyElems (elContent numbering) ++ newNumElts
  let numEntry = toEntry numpath epochtime $ renderXml numbering{ elContent =
                       -- we want all the abstractNums first, then the nums,
                       -- otherwise things break:
                       [Elem e | e <- allElts
                               , qName (elName e) == "abstractNum" ] ++
                       [Elem e | e <- allElts
                               , qName (elName e) == "num" ] }

  let keywords = case lookupMeta "keywords" meta of
                       Just (MetaList xs)           -> map stringify xs
                       _                            -> []

  let docPropsPath = "docProps/core.xml"
  let docProps = mknode "cp:coreProperties"
          [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
          ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
          ,("xmlns:dcterms","http://purl.org/dc/terms/")
          ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
          ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
          $ mknode "dc:title" [] (stringify $ docTitle meta)
          : mknode "dc:creator" [] (intercalate "; " (map stringify $ docAuthors meta))
          : mknode "cp:keywords" [] (intercalate ", " keywords)
          : (\x -> [ mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")] x
                   , mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] x
                   ]) (formatTime defaultTimeLocale "%FT%XZ" utctime)
  let docPropsEntry = toEntry docPropsPath epochtime $ renderXml docProps

  let relsPath = "_rels/.rels"
  let rels = mknode "Relationships" [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
        $ map (\attrs -> mknode "Relationship" attrs ())
        [ [("Id","rId1")
          ,("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument")
          ,("Target","word/document.xml")]
        , [("Id","rId4")
          ,("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties")
          ,("Target","docProps/app.xml")]
        , [("Id","rId3")
          ,("Type","http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties")
          ,("Target","docProps/core.xml")]
        ]
  let relsEntry = toEntry relsPath epochtime $ renderXml rels

  -- we use dist archive for settings.xml, because Word sometimes
  -- adds references to footnotes or endnotes we don't have...
  -- we do, however, copy some settings over from reference
  let settingsPath = "word/settings.xml"
      settingsList = [ "w:autoHyphenation"
                     , "w:consecutiveHyphenLimit"
                     , "w:hyphenationZone"
                     , "w:doNotHyphenateCap"
                     ]
  settingsEntry <- copyChildren refArchive distArchive settingsPath epochtime settingsList

  let entryFromArchive arch path =
         maybe (fail $ path ++ " missing in reference docx")
               return
               (findEntryByPath path arch `mplus` findEntryByPath path distArchive)
  docPropsAppEntry <- entryFromArchive refArchive "docProps/app.xml"
  themeEntry <- entryFromArchive refArchive "word/theme/theme1.xml"
  fontTableEntry <- entryFromArchive refArchive "word/fontTable.xml"
  webSettingsEntry <- entryFromArchive refArchive "word/webSettings.xml"
  headerFooterEntries <- mapM (entryFromArchive refArchive) $
                     mapMaybe (fmap ("word/" ++) . extractTarget)
                     (headers ++ footers)
  let miscRelEntries = [ e | e <- zEntries refArchive
                       , "word/_rels/" `isPrefixOf` (eRelativePath e)
                       , ".xml.rels" `isSuffixOf` (eRelativePath e)
                       , eRelativePath e /= "word/_rels/document.xml.rels"
                       , eRelativePath e /= "word/_rels/footnotes.xml.rels" ]
  let otherMediaEntries = [ e | e <- zEntries refArchive
                          , "word/media/" `isPrefixOf` eRelativePath e ]

  -- Create archive
  let archive = foldr addEntryToArchive emptyArchive $
                  contentTypesEntry : relsEntry : contentEntry : relEntry :
                  footnoteRelEntry : numEntry : styleEntry : footnotesEntry :
                  docPropsEntry : docPropsAppEntry : themeEntry :
                  fontTableEntry : settingsEntry : webSettingsEntry :
                  imageEntries ++ headerFooterEntries ++
                  miscRelEntries ++ otherMediaEntries
  return $ fromArchive archive


newParaPropToOpenXml :: String -> Element
newParaPropToOpenXml s =
  let styleId = filter (not . isSpace) s
  in mknode "w:style" [ ("w:type", "paragraph")
                      , ("w:customStyle", "1")
                      , ("w:styleId", styleId)]
     [ mknode "w:name" [("w:val", s)] ()
     , mknode "w:basedOn" [("w:val","BodyText")] ()
     , mknode "w:qFormat" [] ()
     ]

newTextPropToOpenXml :: String -> Element
newTextPropToOpenXml s =
  let styleId = filter (not . isSpace) s
  in mknode "w:style" [ ("w:type", "character")
                      , ("w:customStyle", "1")
                      , ("w:styleId", styleId)]
     [ mknode "w:name" [("w:val", s)] ()
     , mknode "w:basedOn" [("w:val","BodyTextChar")] ()
     ]

styleToOpenXml :: StyleMaps -> Style -> [Element]
styleToOpenXml sm style =
  maybeToList parStyle ++ mapMaybe toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype | hasStyleName (show toktype) (sCharStyleMap sm) = Nothing
                        | otherwise = Just $
                          mknode "w:style" [("w:type","character"),
                           ("w:customStyle","1"),("w:styleId",show toktype)]
                             [ mknode "w:name" [("w:val",show toktype)] ()
                             , mknode "w:basedOn" [("w:val","VerbatimChar")] ()
                             , mknode "w:rPr" [] $
                               [ mknode "w:color" [("w:val",tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:shd" [("w:val","clear"),("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ] ++
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ]
                             ]
        tokStyles = tokenStyles style
        tokFeature f toktype = maybe False f $ lookup toktype tokStyles
        tokCol toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenColor =<< lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenBackground =<< lookup toktype tokStyles)
                           `mplus` backgroundColor style
        parStyle | hasStyleName "Source Code" (sParaStyleMap sm) = Nothing
                 | otherwise = Just $
                   mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               $ mknode "w:wordWrap" [("w:val","off")] ()
                               : ( maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill",drop 1 $ fromColor col)] ()])
                                 $ backgroundColor style )
                             ]

copyChildren :: (PandocMonad m) => Archive -> Archive -> String -> Integer -> [String] -> m Entry
copyChildren refArchive distArchive path timestamp elNames = do
  ref  <- parseXml refArchive distArchive path
  dist <- parseXml distArchive distArchive path
  return $ toEntry path timestamp $ renderXml dist{
      elContent = elContent dist ++ copyContent ref
    }
  where
    strName QName{qName=name, qPrefix=prefix}
      | Just p <- prefix = p++":"++name
      | otherwise        = name
    shouldCopy = (`elem` elNames) . strName
    cleanElem el@Element{elName=name} = Elem el{elName=name{qURI=Nothing}}
    copyContent = map cleanElem . filterChildrenName shouldCopy

-- this is the lowest number used for a list numId
baseListId :: Int
baseListId = 1000

mkNumbering :: (PandocMonad m) => [ListMarker] -> m [Element]
mkNumbering lists = do
  elts <- mapM mkAbstractNum (ordNub lists)
  return $ elts ++ zipWith mkNum lists [baseListId..(baseListId + length lists - 1)]

maxListLevel :: Int
maxListLevel = 8

mkNum :: ListMarker -> Int -> Element
mkNum marker numid =
  mknode "w:num" [("w:numId",show numid)]
   $ mknode "w:abstractNumId" [("w:val",listMarkerToId marker)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",show (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",show start)] ())
                [0..maxListLevel]

mkAbstractNum :: (PandocMonad m) => ListMarker -> m Element
mkAbstractNum marker = do
  gen <- P.newStdGen
  let (nsid, _) = randomR (0x10000000 :: Integer, 0xFFFFFFFF :: Integer) gen
  return $ mknode "w:abstractNum" [("w:abstractNumId",listMarkerToId marker)]
    $ mknode "w:nsid" [("w:val", printf "%8x" nsid)] ()
    : mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker)
      [0..maxListLevel]

mkLvl :: ListMarker -> Int -> Element
mkLvl marker lvl =
  mknode "w:lvl" [("w:ilvl",show lvl)] $
    [ mknode "w:start" [("w:val",start)] ()
      | marker /= NoMarker && marker /= BulletMarker ] ++
    [ mknode "w:numFmt" [("w:val",fmt)] ()
    , mknode "w:lvlText" [("w:val",lvltxt)] ()
    , mknode "w:lvlJc" [("w:val","left")] ()
    , mknode "w:pPr" []
      [ mknode "w:tabs" []
        $ mknode "w:tab" [("w:val","num"),("w:pos",show $ lvl * step)] ()
      , mknode "w:ind" [("w:left",show $ lvl * step + hang),("w:hanging",show hang)] ()
      ]
    ]
    where (fmt, lvltxt, start) =
            case marker of
                 NoMarker             -> ("bullet"," ","1")
                 BulletMarker         -> ("bullet",bulletFor lvl,"1")
                 NumberMarker st de n -> (styleFor st lvl
                                         ,patternFor de ("%" ++ show (lvl + 1))
                                         ,show n)
          step = 720
          hang = 480
          bulletFor 0 = "\x2022"  -- filled circle
          bulletFor 1 = "\x2013"  -- en dash
          bulletFor 2 = "\x2022"  -- hyphen bullet
          bulletFor 3 = "\x2013"
          bulletFor 4 = "\x2022"
          bulletFor 5 = "\x2013"
          bulletFor x = bulletFor (x `mod` 6)
          styleFor UpperAlpha _   = "upperLetter"
          styleFor LowerAlpha _   = "lowerLetter"
          styleFor UpperRoman _   = "upperRoman"
          styleFor LowerRoman _   = "lowerRoman"
          styleFor Decimal _      = "decimal"
          styleFor DefaultStyle 1 = "decimal"
          styleFor DefaultStyle 2 = "lowerLetter"
          styleFor DefaultStyle 3 = "lowerRoman"
          styleFor DefaultStyle 4 = "decimal"
          styleFor DefaultStyle 5 = "lowerLetter"
          styleFor DefaultStyle 6 = "lowerRoman"
          styleFor DefaultStyle x = styleFor DefaultStyle (x `mod` 7)
          styleFor _ _            = "decimal"
          patternFor OneParen s  = s ++ ")"
          patternFor TwoParens s = "(" ++ s ++ ")"
          patternFor _ s         = s ++ "."

getNumId :: (PandocMonad m) => WS m Int
getNumId = (((baseListId - 1) +) . length) `fmap` gets stLists


makeTOC :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeTOC opts | writerTableOfContents opts = do
  let depth = "1-"++(show (writerTOCDepth opts))
  let tocCmd = "TOC \\o \""++depth++"\" \\h \\z \\u"
  tocTitle <- gets stTocTitle
  title <- withParaPropM (pStyleM "TOC Heading") (blocksToOpenXML opts [Para tocTitle])
  return $
    [mknode "w:sdt" [] ([
      mknode "w:sdtPr" [] (
        mknode "w:docPartObj" [] (
          [mknode "w:docPartGallery" [("w:val","Table of Contents")] (),
          mknode "w:docPartUnique" [] ()]
        ) -- w:docPartObj
      ), -- w:sdtPr
      mknode "w:sdtContent" [] (title++[
        mknode "w:p" [] (
          mknode "w:r" [] ([
            mknode "w:fldChar" [("w:fldCharType","begin"),("w:dirty","true")] (),
            mknode "w:instrText" [("xml:space","preserve")] tocCmd,
            mknode "w:fldChar" [("w:fldCharType","separate")] (),
            mknode "w:fldChar" [("w:fldCharType","end")] ()
          ]) -- w:r
        ) -- w:p
      ])
    ])] -- w:sdt
makeTOC _ = return []


-- | Convert Pandoc document to two lists of
-- OpenXML elements (the main document and footnotes).
writeOpenXML :: (PandocMonad m) => WriterOptions -> Pandoc -> WS m ([Element], [Element])
writeOpenXML opts (Pandoc meta blocks) = do
  let tit = docTitle meta ++ case lookupMeta "subtitle" meta of
                                  Just (MetaBlocks [Plain xs]) -> LineBreak : xs
                                  _ -> []
  let auths = docAuthors meta
  let dat = docDate meta
  let abstract' = case lookupMeta "abstract" meta of
                       Just (MetaBlocks bs)   -> bs
                       Just (MetaInlines ils) -> [Plain ils]
                       _                      -> []
  let subtitle' = case lookupMeta "subtitle" meta of
                       Just (MetaBlocks [Plain xs]) -> xs
                       Just (MetaBlocks [Para  xs]) -> xs
                       Just (MetaInlines xs)        -> xs
                       _                            -> []
  title <- withParaPropM (pStyleM "Title") $ blocksToOpenXML opts [Para tit | not (null tit)]
  subtitle <- withParaPropM (pStyleM "Subtitle") $ blocksToOpenXML opts [Para subtitle' | not (null subtitle')]
  authors <- withParaProp (pCustomStyle "Author") $ blocksToOpenXML opts $
       map Para auths
  date <- withParaPropM (pStyleM "Date") $ blocksToOpenXML opts [Para dat | not (null dat)]
  abstract <- if null abstract'
                 then return []
                 else withParaProp (pCustomStyle "Abstract") $ blocksToOpenXML opts abstract'
  let convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs)         = Str (x ++ y) : xs
      convertSpace xs                           = xs
  let blocks' = bottomUp convertSpace blocks
  doc' <- (setFirstPara >> blocksToOpenXML opts blocks')
  notes' <- reverse `fmap` gets stFootnotes
  toc <- makeTOC opts
  let meta' = title ++ subtitle ++ authors ++ date ++ abstract ++ toc
  return (meta' ++ doc', notes')

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: (PandocMonad m) => WriterOptions -> [Block] -> WS m [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

pCustomStyle :: String -> Element
pCustomStyle sty = mknode "w:pStyle" [("w:val",sty)] ()

pStyleM :: (PandocMonad m) => String -> WS m XML.Element
pStyleM styleName = do
  styleMaps <- gets stStyleMaps
  let sty' = getStyleId styleName $ sParaStyleMap styleMaps
  return $ mknode "w:pStyle" [("w:val",sty')] ()

rCustomStyle :: String -> Element
rCustomStyle sty = mknode "w:rStyle" [("w:val",sty)] ()

rStyleM :: (PandocMonad m) => String -> WS m XML.Element
rStyleM styleName = do
  styleMaps <- gets stStyleMaps
  let sty' = getStyleId styleName $ sCharStyleMap styleMaps
  return $ mknode "w:rStyle" [("w:val",sty')] ()

getUniqueId :: (PandocMonad m) => m String
-- the + 20 is to ensure that there are no clashes with the rIds
-- already in word/document.xml.rel
getUniqueId = (show . (+ 20)) <$> P.newUniqueHash

-- | Key for specifying user-defined docx styles.
dynamicStyleKey :: String
dynamicStyleKey = "custom-style"

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: (PandocMonad m) => WriterOptions -> Block -> WS m [Element]
blockToOpenXML opts blk = withDirection $ blockToOpenXML' opts blk

blockToOpenXML' :: (PandocMonad m) => WriterOptions -> Block -> WS m [Element]
blockToOpenXML' _ Null = return []
blockToOpenXML' opts (Div (ident,classes,kvs) bs) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just sty -> do
                      modify $ \s ->
                        s{stDynamicParaProps = Set.insert sty
                             (stDynamicParaProps s)}
                      return $ withParaPropM (pStyleM sty)
                   _ -> return id
  dirmod <- case lookup "dir" kvs of
                 Just "rtl" -> return $ local (\env -> env { envRTL = True })
                 Just "ltr" -> return $ local (\env -> env { envRTL = False })
                 _ -> return id
  let (hs, bs') = if "references" `elem` classes
                     then span isHeaderBlock bs
                     else ([], bs)
  let bibmod = if "references" `elem` classes
                  then withParaPropM (pStyleM "Bibliography")
                  else id
  header <- dirmod $ stylemod $ blocksToOpenXML opts hs
  contents <- dirmod $ bibmod $ stylemod $ blocksToOpenXML opts bs'
  if null ident
     then return $ header ++ contents
     else do
       id' <- getUniqueId
       let bookmarkStart = mknode "w:bookmarkStart" [("w:id", id')
                                                    ,("w:name",ident)] ()
       let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
       return $ bookmarkStart : header ++ contents ++ [bookmarkEnd]
blockToOpenXML' opts (Header lev (ident,_,_) lst) = do
  setFirstPara
  paraProps <- withParaPropM (pStyleM ("Heading "++show lev)) $
                    getParaProps False
  contents <- inlinesToOpenXML opts lst
  if null ident
     then return [mknode "w:p" [] (paraProps ++contents)]
     else do
       let bookmarkName = ident
       modify $ \s -> s{ stSectionIds = Set.insert bookmarkName
                                      $ stSectionIds s }
       id' <- getUniqueId
       let bookmarkStart = mknode "w:bookmarkStart" [("w:id", id')
                                               ,("w:name",bookmarkName)] ()
       let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
       return [mknode "w:p" [] (paraProps ++
               [bookmarkStart] ++ contents ++ [bookmarkEnd])]
blockToOpenXML' opts (Plain lst) = withParaProp (pCustomStyle "Compact")
  $ blockToOpenXML opts (Para lst)
-- title beginning with fig: indicates that the image is a figure
blockToOpenXML' opts (Para [Image attr alt (src,'f':'i':'g':':':tit)]) = do
  setFirstPara
  let prop = pCustomStyle $
        if null alt
        then "Figure"
        else "CaptionedFigure"
  paraProps <- local (\env -> env { envParaProperties = prop : envParaProperties env }) (getParaProps False)
  contents <- inlinesToOpenXML opts [Image attr alt (src,tit)]
  captionNode <- withParaProp (pCustomStyle "ImageCaption")
                 $ blockToOpenXML opts (Para alt)
  return $ mknode "w:p" [] (paraProps ++ contents) : captionNode
-- fixDisplayMath sometimes produces a Para [] as artifact
blockToOpenXML' _ (Para []) = return []
blockToOpenXML' opts (Para lst) = do
  isFirstPara <- gets stFirstPara
  paraProps <- getParaProps $ case lst of
                               [Math DisplayMath _] -> True
                               _                    -> False
  bodyTextStyle <- pStyleM "Body Text"
  let paraProps' = case paraProps of
        [] | isFirstPara -> [mknode "w:pPr" [] [pCustomStyle "FirstParagraph"]]
        []               -> [mknode "w:pPr" [] [bodyTextStyle]]
        ps               -> ps
  modify $ \s -> s { stFirstPara = False }
  contents <- inlinesToOpenXML opts lst
  return [mknode "w:p" [] (paraProps' ++ contents)]
blockToOpenXML' opts (LineBlock lns) = blockToOpenXML opts $ linesToPara lns
blockToOpenXML' _ b@(RawBlock format str)
  | format == Format "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise                  = do
      report $ BlockNotRendered b
      return []
blockToOpenXML' opts (BlockQuote blocks) = do
  p <- withParaPropM (pStyleM "Block Text") $ blocksToOpenXML opts blocks
  setFirstPara
  return p
blockToOpenXML' opts (CodeBlock attrs str) = do
  p <- withParaProp (pCustomStyle "SourceCode") (blockToOpenXML opts $ Para [Code attrs str])
  setFirstPara
  return p
blockToOpenXML' _ HorizontalRule = do
  setFirstPara
  return [
    mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML' opts (Table caption aligns widths headers rows) = do
  setFirstPara
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaProp (pCustomStyle "TableCaption")
                      $ blockToOpenXML opts (Para caption)
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  let cellToOpenXML (al, cell) = withParaProp (alignmentFor al)
                                    $ blocksToOpenXML opts cell
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (mapM cellToOpenXML . zip aligns) rows
  let borderProps = mknode "w:tcPr" []
                    [ mknode "w:tcBorders" []
                      $ mknode "w:bottom" [("w:val","single")] ()
                    , mknode "w:vAlign" [("w:val","bottom")] () ]
  let emptyCell = [mknode "w:p" [] [mknode "w:pPr" [] [pCustomStyle "Compact"]]]
  let mkcell border contents = mknode "w:tc" []
                            $ [ borderProps | border ] ++
                            if null contents
                               then emptyCell
                               else contents
  let mkrow border cells = mknode "w:tr" [] $
                        [mknode "w:trPr" [] [
                          mknode "w:cnfStyle" [("w:firstRow","1")] ()] | border]
                        ++ map (mkcell border) cells
  let textwidth = 7920  -- 5.5 in in twips, 1/20 pt
  let fullrow = 5000 -- 100% specified in pct
  let rowwidth = fullrow * sum widths
  let mkgridcol w = mknode "w:gridCol"
                       [("w:w", show (floor (textwidth * w) :: Integer))] ()
  let hasHeader = not (all null headers)
  return $
    caption' ++
    [mknode "w:tbl" []
      ( mknode "w:tblPr" []
        (   mknode "w:tblStyle" [("w:val","Table")] () :
            mknode "w:tblW" [("w:type", "pct"), ("w:w", show rowwidth)] () :
            mknode "w:tblLook" [("w:firstRow","1") | hasHeader ] () :
          [ mknode "w:tblCaption" [("w:val", captionStr)] ()
          | not (null caption) ] )
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow True headers' | hasHeader ] ++
      map (mkrow False) rows'
      )]
blockToOpenXML' opts (BulletList lst) = do
  let marker = BulletMarker
  addList marker
  numid  <- getNumId
  l <- asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
  setFirstPara
  return l
blockToOpenXML' opts (OrderedList (start, numstyle, numdelim) lst) = do
  let marker = NumberMarker numstyle numdelim start
  addList marker
  numid  <- getNumId
  l <- asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
  setFirstPara
  return l
blockToOpenXML' opts (DefinitionList items) = do
  l <- concat `fmap` mapM (definitionListItemToOpenXML opts) items
  setFirstPara
  return l

definitionListItemToOpenXML  :: (PandocMonad m) => WriterOptions -> ([Inline],[[Block]]) -> WS m [Element]
definitionListItemToOpenXML opts (term,defs) = do
  term' <- withParaProp (pCustomStyle "DefinitionTerm")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaProp (pCustomStyle "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

addList :: (PandocMonad m) => ListMarker -> WS m ()
addList marker = do
  lists <- gets stLists
  modify $ \st -> st{ stLists = lists ++ [marker] }

listItemToOpenXML :: (PandocMonad m) => WriterOptions -> Int -> [Block] -> WS m [Element]
listItemToOpenXML _ _ []                   = return []
listItemToOpenXML opts numid (first:rest) = do
  first' <- withNumId numid $ blockToOpenXML opts first
  -- baseListId is the code for no list marker:
  rest'  <- withNumId baseListId $ blocksToOpenXML opts rest
  return $ first' ++ rest'

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: (PandocMonad m) => WriterOptions -> [Inline] -> WS m [Element]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

withNumId :: (PandocMonad m) => Int -> WS m a -> WS m a
withNumId numid = local $ \env -> env{ envListNumId = numid }

asList :: (PandocMonad m) => WS m a -> WS m a
asList = local $ \env -> env{ envListLevel = envListLevel env + 1 }

getTextProps :: (PandocMonad m) => WS m [Element]
getTextProps = do
  props <- asks envTextProperties
  return $ if null props
              then []
              else [mknode "w:rPr" [] props]

withTextProp :: PandocMonad m => Element -> WS m a -> WS m a
withTextProp d p =
  local (\env -> env {envTextProperties = d : envTextProperties env}) p

withTextPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withTextPropM = (. flip withTextProp) . (>>=)

getParaProps :: PandocMonad m => Bool -> WS m [Element]
getParaProps displayMathPara = do
  props <- asks envParaProperties
  listLevel <- asks envListLevel
  numid <- asks envListNumId
  let listPr = if listLevel >= 0 && not displayMathPara
                  then [ mknode "w:numPr" []
                         [ mknode "w:numId" [("w:val",show numid)] ()
                         , mknode "w:ilvl" [("w:val",show listLevel)] () ]
                       ]
                  else []
  return $ case props ++ listPr of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

withParaProp :: PandocMonad m => Element -> WS m a -> WS m a
withParaProp d p =
  local (\env -> env {envParaProperties = d : envParaProperties env}) p

withParaPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withParaPropM = (. flip withParaProp) . (>>=)

formattedString :: PandocMonad m => String -> WS m [Element]
formattedString str =
  -- properly handle soft hyphens
  case splitBy (=='\173') str of
      [w] -> formattedString' w
      ws  -> do
         sh <- formattedRun [mknode "w:softHyphen" [] ()]
         (intercalate sh) <$> mapM formattedString' ws

formattedString' :: PandocMonad m => String -> WS m [Element]
formattedString' str = do
  inDel <- asks envInDel
  formattedRun [ mknode (if inDel then "w:delText" else "w:t")
                 [("xml:space","preserve")] (stripInvalidChars str) ]

formattedRun :: PandocMonad m => [Element] -> WS m [Element]
formattedRun els = do
  props <- getTextProps
  return [ mknode "w:r" [] $ props ++ els ]

setFirstPara :: PandocMonad m => WS m ()
setFirstPara =  modify $ \s -> s { stFirstPara = True }

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: PandocMonad m => WriterOptions -> Inline -> WS m [Element]
inlineToOpenXML opts il = withDirection $ inlineToOpenXML' opts il

inlineToOpenXML' :: PandocMonad m => WriterOptions -> Inline -> WS m [Element]
inlineToOpenXML' _ (Str str) =
  formattedString str
inlineToOpenXML' opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts SoftBreak = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts (Span (ident,classes,kvs) ils) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just sty -> do
                      modify $ \s ->
                        s{stDynamicTextProps = Set.insert sty
                              (stDynamicTextProps s)}
                      return $ withTextProp (rCustomStyle sty)
                   _ -> return id
  let dirmod = case lookup "dir" kvs of
                 Just "rtl" -> local (\env -> env { envRTL = True })
                 Just "ltr" -> local (\env -> env { envRTL = False })
                 _          -> id
  let off x = withTextProp (mknode x [("w:val","0")] ())
  let pmod =  (if "csl-no-emph" `elem` classes then off "w:i" else id) .
              (if "csl-no-strong" `elem` classes then off "w:b" else id) .
              (if "csl-no-smallcaps" `elem` classes
                  then off "w:smallCaps"
                  else id)
  insmod <- if "insertion" `elem` classes
               then do
                 defaultAuthor <- asks envChangesAuthor
                 defaultDate <- asks envChangesDate
                 let author = fromMaybe defaultAuthor (lookup "author" kvs)
                     date   = fromMaybe defaultDate (lookup "date" kvs)
                 insId <- gets stInsId
                 modify $ \s -> s{stInsId = (insId + 1)}
                 return $ \f -> do
                   x <- f
                   return $ [ mknode "w:ins"
                              [("w:id", (show insId)),
                              ("w:author", author),
                              ("w:date", date)] x]
               else return id
  delmod <- if "insertion" `elem` classes
               then do
                 defaultAuthor <- asks envChangesAuthor
                 defaultDate <- asks envChangesDate
                 let author = fromMaybe defaultAuthor (lookup "author" kvs)
                     date   = fromMaybe defaultDate (lookup "date" kvs)
                 insId <- gets stInsId
                 modify $ \s -> s{stInsId = (insId + 1)}
                 return $ \f -> do
                   x <- f
                   return [mknode "w:ins"
                           [("w:id", (show insId)),
                           ("w:author", author),
                           ("w:date", date)] x]
               else return id
  contents <- insmod $ delmod $ dirmod $ stylemod $ pmod
                     $ inlinesToOpenXML opts ils
  if null ident
     then return contents
     else do
       id' <- getUniqueId
       let bookmarkStart = mknode "w:bookmarkStart" [("w:id", id')
                                                    ,("w:name",ident)] ()
       let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
       return $ bookmarkStart : contents ++ [bookmarkEnd]
inlineToOpenXML' opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML' _ LineBreak = return [br]
inlineToOpenXML' _ il@(RawInline f str)
  | f == Format "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise             = do
      report $ InlineNotRendered il
      return []
inlineToOpenXML' opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML' opts (Math mathType str) = do
  when (mathType == DisplayMath) setFirstPara
  res <- (lift . lift) (convertMath writeOMML mathType str)
  case res of
       Right r -> return [r]
       Left il -> inlineToOpenXML' opts il
inlineToOpenXML' opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML' opts (Code attrs str) = do
  let unhighlighted = intercalate [br] `fmap`
                       (mapM formattedString $ lines str)
      formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
      toHlTok (toktype,tok) = mknode "w:r" []
                               [ mknode "w:rPr" []
                                 [ rCustomStyle (show toktype) ]
                               , mknode "w:t" [("xml:space","preserve")] (T.unpack tok) ]
  withTextProp (rCustomStyle "VerbatimChar")
    $ if isNothing (writerHighlightStyle opts)
          then unhighlighted
          else case highlight (writerSyntaxMap opts)
                      formatOpenXML attrs str of
                    Right h  -> return h
                    Left msg -> do
                      unless (null msg) $ report $ CouldNotHighlight msg
                      unhighlighted
inlineToOpenXML' opts (Note bs) = do
  notes <- gets stFootnotes
  notenum <- (lift . lift) getUniqueId
  footnoteStyle <- rStyleM "Footnote Reference"
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] footnoteStyle
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline (Format "openxml") $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : Space : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : Space : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs

  contents <- local (\env -> env{ envListLevel = -1
                                , envParaProperties = []
                                , envTextProperties = [] })
              (withParaPropM (pStyleM "Footnote Text") $ blocksToOpenXML opts
                $ insertNoteRef bs)
  let newnote = mknode "w:footnote" [("w:id", notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] footnoteStyle
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML' opts (Link _ txt ('#':xs,_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  return [ mknode "w:hyperlink" [("w:anchor",xs)] contents ]
-- external link:
inlineToOpenXML' opts (Link _ txt (src,_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId"++) `fmap` ((lift . lift) getUniqueId)
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",id')] contents ]
inlineToOpenXML' opts (Image attr alt (src, title)) = do
  -- first, check to see if we've already done this image
  pageWidth <- asks envPrintWidth
  imgs <- gets stImages
  case M.lookup src imgs of
    Just (_,_,_,elt,_) -> return [elt]
    Nothing -> do
      catchError
        (do (img, mt) <- P.fetchItem (writerSourceURL opts) src
            ident <- ("rId"++) `fmap` ((lift . lift) getUniqueId)
            let (xpt,ypt) = desiredSizeInPoints opts attr
                   (either (const def) id (imageSize opts img))
            -- 12700 emu = 1 pt
            let (xemu,yemu) = fitToPage (xpt * 12700, ypt * 12700)
                                    (pageWidth * 12700)
            let cNvPicPr = mknode "pic:cNvPicPr" [] $
                             mknode "a:picLocks" [("noChangeArrowheads","1")
                                                 ,("noChangeAspect","1")] ()
            let nvPicPr  = mknode "pic:nvPicPr" []
                            [ mknode "pic:cNvPr"
                                [("descr",src),("id","0"),("name","Picture")] ()
                            , cNvPicPr ]
            let blipFill = mknode "pic:blipFill" []
                             [ mknode "a:blip" [("r:embed",ident)] ()
                             , mknode "a:stretch" [] $
                               mknode "a:fillRect" [] () ]
            let xfrm =    mknode "a:xfrm" []
                            [ mknode "a:off" [("x","0"),("y","0")] ()
                            , mknode "a:ext" [("cx",show xemu)
                                             ,("cy",show yemu)] () ]
            let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                             mknode "a:avLst" [] ()
            let ln =      mknode "a:ln" [("w","9525")]
                            [ mknode "a:noFill" [] ()
                            , mknode "a:headEnd" [] ()
                            , mknode "a:tailEnd" [] () ]
            let spPr =    mknode "pic:spPr" [("bwMode","auto")]
                            [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
            let graphic = mknode "a:graphic" [] $
                            mknode "a:graphicData"
                              [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
                              [ mknode "pic:pic" []
                                [ nvPicPr
                                , blipFill
                                , spPr ] ]
            let imgElt = mknode "w:r" [] $
                 mknode "w:drawing" [] $
                   mknode "wp:inline" []
                    [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
                    , mknode "wp:effectExtent"
                         [("b","0"),("l","0"),("r","0"),("t","0")] ()
                    , mknode "wp:docPr" [("descr",stringify alt)
                                        ,("title", title)
                                        ,("id","1")
                                        ,("name","Picture")] ()
                    , graphic ]
            let imgext = case mt >>= extensionFromMimeType of
                              Just x    -> '.':x
                              Nothing   -> case imageType img of
                                                Just Png  -> ".png"
                                                Just Jpeg -> ".jpeg"
                                                Just Gif  -> ".gif"
                                                Just Pdf  -> ".pdf"
                                                Just Eps  -> ".eps"
                                                Just Svg  -> ".svg"
                                                Nothing   -> ""
            if null imgext
               then -- without an extension there is no rule for content type
                 inlinesToOpenXML opts alt -- return alt to avoid corrupted docx
               else do
                 let imgpath = "media/" ++ ident ++ imgext
                 let mbMimeType = mt <|> getMimeType imgpath
                 -- insert mime type to use in constructing [Content_Types].xml
                 modify $ \st -> st{ stImages =
                     M.insert src (ident, imgpath, mbMimeType, imgElt, img)
                             $ stImages st }
                 return [imgElt])
        (\e -> do
           report $ CouldNotFetchResource src (show e)
           -- emit alt text
           inlinesToOpenXML opts alt)

br :: Element
br = breakElement "textWrapping"

breakElement :: String -> Element
breakElement kind = mknode "w:r" [] [mknode "w:br" [("w:type",kind)] () ]

-- Word will insert these footnotes into the settings.xml file
-- (whether or not they're visible in the document). If they're in the
-- file, but not in the footnotes.xml file, it will produce
-- problems. So we want to make sure we insert them into our document.
defaultFootnotes :: [Element]
defaultFootnotes = [ mknode "w:footnote"
                     [("w:type", "separator"), ("w:id", "-1")] $
                     [ mknode "w:p" [] $
                       [mknode "w:r" [] $
                        [ mknode "w:separator" [] ()]]]
                   , mknode "w:footnote"
                     [("w:type", "continuationSeparator"), ("w:id", "0")] $
                     [ mknode "w:p" [] $
                       [ mknode "w:r" [] $
                         [ mknode "w:continuationSeparator" [] ()]]]]

parseXml :: (PandocMonad m) => Archive -> Archive -> String -> m Element
parseXml refArchive distArchive relpath =
  case findEntryByPath relpath refArchive `mplus`
         findEntryByPath relpath distArchive of
            Nothing -> fail $ relpath ++ " missing in reference docx"
            Just e  -> case parseXMLDoc . UTF8.toStringLazy . fromEntry $ e of
                       Nothing -> fail $ relpath ++ " corrupt in reference docx"
                       Just d  -> return d

-- | Scales the image to fit the page
-- sizes are passed in emu
fitToPage :: (Double, Double) -> Integer -> (Integer, Integer)
fitToPage (x, y) pageWidth
  -- Fixes width to the page width and scales the height
  | x > fromIntegral pageWidth =
    (pageWidth, floor $ ((fromIntegral pageWidth) / x) * y)
  | otherwise = (floor x, floor y)

withDirection :: PandocMonad m => WS m a -> WS m a
withDirection x = do
  isRTL <- asks envRTL
  paraProps <- asks envParaProperties
  textProps <- asks envTextProperties
  -- We want to clean all bidirection (bidi) and right-to-left (rtl)
  -- properties from the props first. This is because we don't want
  -- them to stack up.
  let paraProps' = filter (\e -> (qName . elName) e /= "bidi") paraProps
      textProps' = filter (\e -> (qName . elName) e /= "rtl") textProps
  if isRTL
    -- if we are going right-to-left, we (re?)add the properties.
    then flip local x $
         \env -> env { envParaProperties = (mknode "w:bidi" [] ()) : paraProps'
                     , envTextProperties = (mknode "w:rtl" [] ()) : textProps'
                     }
    else flip local x $ \env -> env { envParaProperties = paraProps'
                                    , envTextProperties = textProps'
                                    }

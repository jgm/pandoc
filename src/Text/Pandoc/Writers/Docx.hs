{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2012-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Data.Maybe (fromMaybe)
import Data.List ( intercalate, isPrefixOf, isSuffixOf )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Compat.Monoid ((<>))
import Codec.Archive.Zip
import Data.Time.Clock.POSIX
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared hiding (Element)
import Text.Pandoc.Writers.Shared (fixDisplayMath)
import Text.Pandoc.Options
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Highlighting ( highlight )
import Text.Pandoc.Walk
import Text.Highlighting.Kate.Types ()
import Text.XML.Light
import Text.TeXMath
import Control.Monad.State
import Text.Highlighting.Kate
import Data.Unique (hashUnique, newUnique)
import System.Random (randomRIO)
import Text.Printf (printf)
import qualified Control.Exception as E
import Text.Pandoc.MIME (getMimeType, extensionFromMimeType)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Control.Applicative ((<$>))

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
                      DefaultStyle   -> '2'
                      Example        -> '3'
                      Decimal        -> '4'
                      LowerRoman     -> '5'
                      UpperRoman     -> '6'
                      LowerAlpha     -> '7'
                      UpperAlpha     -> '8'
        delimNum = case delim of
                      DefaultDelim   -> '0'
                      Period         -> '1'
                      OneParen       -> '2'
                      TwoParens      -> '3'

data WriterState = WriterState{
         stTextProperties :: [Element]
       , stParaProperties :: [Element]
       , stFootnotes      :: [Element]
       , stSectionIds     :: [String]
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, String, Maybe String, Element, B.ByteString)
       , stListLevel      :: Int
       , stListNumId      :: Int
       , stLists          :: [ListMarker]
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      , stSectionIds     = []
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stListLevel      = -1
      , stListNumId      = 1
      , stLists          = [NoMarker]
      }

type WS a = StateT WriterState IO a

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) . node (unqual s)

toLazy :: B.ByteString -> BL.ByteString
toLazy = BL.fromChunks . (:[])

renderXml :: Element -> BL.ByteString
renderXml elt = BL8.pack "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
  UTF8.fromStringLazy (showElement elt)

-- | Produce an Docx file from a Pandoc document.
writeDocx :: WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO BL.ByteString
writeDocx opts doc@(Pandoc meta _) = do
  let datadir = writerUserDataDir opts
  let doc' = walk fixDisplayMath doc
  refArchive <- liftM (toArchive . toLazy) $
       case writerReferenceDocx opts of
             Just f  -> B.readFile f
             Nothing -> readDataFile datadir "reference.docx"
  distArchive <- liftM (toArchive . toLazy) $ readDataFile Nothing "reference.docx"

  ((contents, footnotes), st) <- runStateT (writeOpenXML opts{writerWrapText = False} doc')
                       defaultWriterState
  epochtime <- floor `fmap` getPOSIXTime
  let imgs = M.elems $ stImages st

  -- create entries for images in word/media/...
  let toImageEntry (_,path,_,_,img) = toEntry ("word/" ++ path) epochtime $ toLazy img
  let imageEntries = map toImageEntry imgs

  -- adjust contents to add sectPr from reference.docx
  parsedDoc <- parseXml refArchive distArchive "word/document.xml"
  let wname f qn = qPrefix qn == Just "w" && f (qName qn)
  let mbsectpr = filterElementName (wname (=="sectPr")) parsedDoc

  let sectpr = maybe (mknode "w:sectPr" [] $ ()) id mbsectpr

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

  let contents' = contents ++ [sectpr]
  let docContents = mknode "w:document" stdAttributes
                    $ mknode "w:body" [] $ contents'

  parsedRels <- parseXml refArchive distArchive "word/_rels/document.xml.rels"
  let isHeaderNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
  let isFooterNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
  let headers = filterElements isHeaderNode parsedRels
  let footers = filterElements isFooterNode parsedRels

  let extractTarget e = findAttr (QName "Target" Nothing Nothing) e

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
  let mkMediaOverride imgpath = mkOverrideNode ('/':imgpath,
                                 fromMaybe "application/octet-stream"
                                   $ getMimeType imgpath)
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
  let baserels = map toBaseRel
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
                    ] ++
                    headers ++ footers
  let toImgRel (ident,path,_,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",path)] ()
  let imgrels = map toImgRel imgs
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let linkrels = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc = mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")] $ baserels ++ imgrels ++ linkrels
  let relEntry = toEntry "word/_rels/document.xml.rels" epochtime
        $ renderXml reldoc


  -- word/document.xml
  let contentEntry = toEntry "word/document.xml" epochtime
                     $ renderXml docContents

  -- footnotes
  let notes = mknode "w:footnotes" stdAttributes footnotes
  let footnotesEntry = toEntry "word/footnotes.xml" epochtime $ renderXml notes

  -- footnote rels
  let footnoteRelEntry = toEntry "word/_rels/footnotes.xml.rels" epochtime
        $ renderXml $ mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")]
        $ linkrels

  -- styles
  let newstyles = styleToOpenXml $ writerHighlightStyle opts
  let stylepath = "word/styles.xml"
  styledoc <- parseXml refArchive distArchive stylepath
  let styledoc' = styledoc{ elContent = elContent styledoc ++
                  [Elem x | x <- newstyles, writerHighlight opts] }
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
                       [Elem e | e <- allElts, qName (elName e) == "num" ] }
  let docPropsPath = "docProps/core.xml"
  let docProps = mknode "cp:coreProperties"
          [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
          ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
          ,("xmlns:dcterms","http://purl.org/dc/terms/")
          ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
          ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
          $ mknode "dc:title" [] (stringify $ docTitle meta)
          : mknode "dc:creator" [] (intercalate "; " (map stringify $ docAuthors meta))
          : maybe []
             (\x -> [ mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")] $ x
                    , mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] $ x
                    ]) (normalizeDate $ stringify $ docDate meta)
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

  let entryFromArchive arch path =
         maybe (fail $ path ++ " corrupt or missing in reference docx")
               return
               (findEntryByPath path arch `mplus` findEntryByPath path distArchive)
  docPropsAppEntry <- entryFromArchive refArchive "docProps/app.xml"
  themeEntry <- entryFromArchive refArchive "word/theme/theme1.xml"
  fontTableEntry <- entryFromArchive refArchive "word/fontTable.xml"
  -- we use dist archive for settings.xml, because Word sometimes
  -- adds references to footnotes or endnotes we don't have...
  settingsEntry <- entryFromArchive distArchive "word/settings.xml"
  webSettingsEntry <- entryFromArchive refArchive "word/webSettings.xml"
  headerFooterEntries <- mapM (entryFromArchive refArchive) $
                     mapMaybe (\e -> fmap ("word/" ++) $ extractTarget e)
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

styleToOpenXml :: Style -> [Element]
styleToOpenXml style = parStyle : map toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype = mknode "w:style" [("w:type","character"),
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
        parStyle = mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               $ mknode "w:wordWrap" [("w:val","off")] ()
                               : ( maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill",drop 1 $ fromColor col)] ()])
                                 $ backgroundColor style )
                             ]

-- this is the lowest number used for a list numId
baseListId :: Int
baseListId = 1000

mkNumbering :: [ListMarker] -> IO [Element]
mkNumbering lists = do
  elts <- mapM mkAbstractNum (ordNub lists)
  return $ elts ++ zipWith mkNum lists [baseListId..(baseListId + length lists - 1)]

mkNum :: ListMarker -> Int -> Element
mkNum marker numid =
  mknode "w:num" [("w:numId",show numid)]
   $ mknode "w:abstractNumId" [("w:val",listMarkerToId marker)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",show (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",show start)] ()) [0..6]

mkAbstractNum :: ListMarker -> IO Element
mkAbstractNum marker = do
  nsid <- randomRIO (0x10000000 :: Integer, 0xFFFFFFFF :: Integer)
  return $ mknode "w:abstractNum" [("w:abstractNumId",listMarkerToId marker)]
    $ mknode "w:nsid" [("w:val", printf "%8x" nsid)] ()
    : mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker) [0..6]

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
          bulletFor _ = "\x2022"
          styleFor UpperAlpha _ = "upperLetter"
          styleFor LowerAlpha _ = "lowerLetter"
          styleFor UpperRoman _ = "upperRoman"
          styleFor LowerRoman _ = "lowerRoman"
          styleFor Decimal _ = "decimal"
          styleFor DefaultStyle 1 = "decimal"
          styleFor DefaultStyle 2 = "lowerLetter"
          styleFor DefaultStyle 3 = "lowerRoman"
          styleFor DefaultStyle 4 = "decimal"
          styleFor DefaultStyle 5 = "lowerLetter"
          styleFor DefaultStyle 6 = "lowerRoman"
          styleFor _ _ = "decimal"
          patternFor OneParen s = s ++ ")"
          patternFor TwoParens s = "(" ++ s ++ ")"
          patternFor _ s = s ++ "."

getNumId :: WS Int
getNumId = ((999 +) . length) `fmap` gets stLists

-- | Convert Pandoc document to two lists of
-- OpenXML elements (the main document and footnotes).
writeOpenXML :: WriterOptions -> Pandoc -> WS ([Element], [Element])
writeOpenXML opts (Pandoc meta blocks) = do
  let tit = docTitle meta ++ case lookupMeta "subtitle" meta of
                                  Just (MetaBlocks [Plain xs]) -> LineBreak : xs
                                  _ -> []
  let auths = docAuthors meta
  let dat = docDate meta
  let abstract' = case lookupMeta "abstract" meta of
                       Just (MetaBlocks bs) -> bs
                       Just (MetaInlines ils) -> [Plain ils]
                       _ -> []
  let subtitle' = case lookupMeta "subtitle" meta of
                       Just (MetaBlocks [Plain xs]) -> xs
                       Just (MetaBlocks [Para  xs]) -> xs
                       Just (MetaInlines xs)        -> xs
                       _ -> []
  title <- withParaProp (pStyle "Title") $ blocksToOpenXML opts [Para tit | not (null tit)]
  subtitle <- withParaProp (pStyle "Subtitle") $ blocksToOpenXML opts [Para subtitle' | not (null subtitle')]
  authors <- withParaProp (pStyle "Author") $ blocksToOpenXML opts $
       map Para auths
  date <- withParaProp (pStyle "Date") $ blocksToOpenXML opts [Para dat | not (null dat)]
  abstract <- if null abstract'
                 then return []
                 else withParaProp (pStyle "Abstract") $ blocksToOpenXML opts abstract'
  let convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs) = Str (x ++ y) : xs
      convertSpace xs = xs
  let blocks' = bottomUp convertSpace $ blocks
  doc' <- blocksToOpenXML opts blocks'
  notes' <- reverse `fmap` gets stFootnotes
  let meta' = title ++ subtitle ++ authors ++ date ++ abstract
  return (meta' ++ doc', notes')

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: WriterOptions -> [Block] -> WS [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

pStyle :: String -> Element
pStyle sty = mknode "w:pStyle" [("w:val",sty)] ()

rStyle :: String -> Element
rStyle sty = mknode "w:rStyle" [("w:val",sty)] ()

getUniqueId :: MonadIO m => m String
-- the + 20 is to ensure that there are no clashes with the rIds
-- already in word/document.xml.rel
getUniqueId = liftIO $ (show . (+ 20) . hashUnique) `fmap` newUnique

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: WriterOptions -> Block -> WS [Element]
blockToOpenXML _ Null = return []
blockToOpenXML opts (Div _ bs) = blocksToOpenXML opts bs
blockToOpenXML opts (Header lev (ident,_,_) lst) = do

  paraProps <- withParaProp (pStyle $ "Heading" ++ show lev) $
               getParaProps False
  contents <- inlinesToOpenXML opts lst

  usedIdents <- gets stSectionIds
  let bookmarkName = if null ident
                        then uniqueIdent lst usedIdents
                        else ident
  modify $ \s -> s{ stSectionIds = bookmarkName : stSectionIds s }
  id' <- getUniqueId
  let bookmarkStart = mknode "w:bookmarkStart" [("w:id", id')
                                               ,("w:name",bookmarkName)] ()
  let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
  return [mknode "w:p" [] (paraProps ++ [bookmarkStart, bookmarkEnd] ++ contents)]
blockToOpenXML opts (Plain lst) = withParaProp (pStyle "Compact")
  $ blockToOpenXML opts (Para lst)
-- title beginning with fig: indicates that the image is a figure
blockToOpenXML opts (Para [Image alt (src,'f':'i':'g':':':tit)]) = do
  paraProps <- getParaProps False
  contents <- inlinesToOpenXML opts [Image alt (src,tit)]
  captionNode <- withParaProp (pStyle "ImageCaption")
                 $ blockToOpenXML opts (Para alt)
  return $ mknode "w:p" [] (paraProps ++ contents) : captionNode
-- fixDisplayMath sometimes produces a Para [] as artifact
blockToOpenXML _ (Para []) = return []
blockToOpenXML opts (Para lst) = do
    paraProps <- getParaProps $ case lst of
                                     [Math DisplayMath _] -> True
                                     _                    -> False
    contents <- inlinesToOpenXML opts lst
    return [mknode "w:p" [] (paraProps ++ contents)]
blockToOpenXML _ (RawBlock format str)
  | format == Format "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise                  = return []
blockToOpenXML opts (BlockQuote blocks) =
  withParaProp (pStyle "BlockQuote") $ blocksToOpenXML opts blocks
blockToOpenXML opts (CodeBlock attrs str) =
  withParaProp (pStyle "SourceCode") $ blockToOpenXML opts $ Para [Code attrs str]
blockToOpenXML _ HorizontalRule = return [
  mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML opts (Table caption aligns widths headers rows) = do
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaProp (pStyle "TableCaption")
                      $ blockToOpenXML opts (Para caption)
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  let cellToOpenXML (al, cell) = withParaProp (alignmentFor al)
                                    $ blocksToOpenXML opts cell
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (\cells -> mapM cellToOpenXML $ zip aligns cells)
           $ rows
  let borderProps = mknode "w:tcPr" []
                    [ mknode "w:tcBorders" []
                      $ mknode "w:bottom" [("w:val","single")] ()
                    , mknode "w:vAlign" [("w:val","bottom")] () ]
  let emptyCell = [mknode "w:p" [] [mknode "w:pPr" [] $
                    [mknode "w:pStyle" [("w:val","Compact")] ()]]]
  let mkcell border contents = mknode "w:tc" []
                            $ [ borderProps | border ] ++
                            if null contents
                               then emptyCell
                               else contents
  let mkrow border cells = mknode "w:tr" [] $ map (mkcell border) cells
  let textwidth = 7920  -- 5.5 in in twips, 1/20 pt
  let mkgridcol w = mknode "w:gridCol"
                       [("w:w", show $ (floor (textwidth * w) :: Integer))] ()
  return $
    [ mknode "w:tbl" []
      ( mknode "w:tblPr" []
        ( [ mknode "w:tblStyle" [("w:val","TableNormal")] () ] ++
          [ mknode "w:tblCaption" [("w:val", captionStr)] ()
          | not (null caption) ] )
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow True headers' | not (all null headers) ] ++
      map (mkrow False) rows'
      )
    ] ++ caption'
blockToOpenXML opts (BulletList lst) = do
  let marker = BulletMarker
  addList marker
  numid  <- getNumId
  asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
blockToOpenXML opts (OrderedList (start, numstyle, numdelim) lst) = do
  let marker = NumberMarker numstyle numdelim start
  addList marker
  numid  <- getNumId
  asList $ concat `fmap` mapM (listItemToOpenXML opts numid) lst
blockToOpenXML opts (DefinitionList items) =
  concat `fmap` mapM (definitionListItemToOpenXML opts) items

definitionListItemToOpenXML  :: WriterOptions -> ([Inline],[[Block]]) -> WS [Element]
definitionListItemToOpenXML opts (term,defs) = do
  term' <- withParaProp (pStyle "DefinitionTerm")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaProp (pStyle "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

addList :: ListMarker -> WS ()
addList marker = do
  lists <- gets stLists
  modify $ \st -> st{ stLists = lists ++ [marker] }

listItemToOpenXML :: WriterOptions -> Int -> [Block] -> WS [Element]
listItemToOpenXML _ _ []                   = return []
listItemToOpenXML opts numid (first:rest) = do
  first' <- withNumId numid $ blockToOpenXML opts first
  -- baseListId is the code for no list marker:
  rest'  <- withNumId baseListId $ blocksToOpenXML opts rest
  return $ first' ++ rest'

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: WriterOptions -> [Inline] -> WS [Element]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

withNumId :: Int -> WS a -> WS a
withNumId numid p = do
  origNumId <- gets stListNumId
  modify $ \st -> st{ stListNumId = numid }
  result <- p
  modify $ \st -> st{ stListNumId = origNumId }
  return result

asList :: WS a -> WS a
asList p = do
  origListLevel <- gets stListLevel
  modify $ \st -> st{ stListLevel = stListLevel st + 1 }
  result <- p
  modify $ \st -> st{ stListLevel = origListLevel }
  return result

getTextProps :: WS [Element]
getTextProps = do
  props <- gets stTextProperties
  return $ if null props
              then []
              else [mknode "w:rPr" [] $ props]

pushTextProp :: Element -> WS ()
pushTextProp d = modify $ \s -> s{ stTextProperties = d : stTextProperties s }

popTextProp :: WS ()
popTextProp = modify $ \s -> s{ stTextProperties = drop 1 $ stTextProperties s }

withTextProp :: Element -> WS a -> WS a
withTextProp d p = do
  pushTextProp d
  res <- p
  popTextProp
  return res

getParaProps :: Bool -> WS [Element]
getParaProps displayMathPara = do
  props <- gets stParaProperties
  listLevel <- gets stListLevel
  numid <- gets stListNumId
  let listPr = if listLevel >= 0 && not displayMathPara
                  then [ mknode "w:numPr" []
                         [ mknode "w:numId" [("w:val",show numid)] ()
                         , mknode "w:ilvl" [("w:val",show listLevel)] () ]
                       ]
                  else []
  return $ case props ++ listPr of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

pushParaProp :: Element -> WS ()
pushParaProp d = modify $ \s -> s{ stParaProperties = d : stParaProperties s }

popParaProp :: WS ()
popParaProp = modify $ \s -> s{ stParaProperties = drop 1 $ stParaProperties s }

withParaProp :: Element -> WS a -> WS a
withParaProp d p = do
  pushParaProp d
  res <- p
  popParaProp
  return res

formattedString :: String -> WS [Element]
formattedString str = do
  props <- getTextProps
  return [ mknode "w:r" [] $
             props ++
             [ mknode "w:t" [("xml:space","preserve")] str ] ]

texMathToOMML :: DisplayType -> String -> Either String Element
texMathToOMML dt inp = writeOMML dt <$> readTeX inp

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: WriterOptions -> Inline -> WS [Element]
inlineToOpenXML _ (Str str) = formattedString str
inlineToOpenXML opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML opts (Span (_,classes,_) ils) = do
  let off x = withTextProp (mknode x [("w:val","0")] ())
  ((if "csl-no-emph" `elem` classes then off "w:i" else id) .
   (if "csl-no-strong" `elem` classes then off "w:b" else id) .
   (if "csl-no-smallcaps" `elem` classes then off "w:smallCaps" else id))
   $ inlinesToOpenXML opts ils
inlineToOpenXML opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML _ LineBreak = return [br]
inlineToOpenXML _ (RawInline f str)
  | f == Format "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise            = return []
inlineToOpenXML opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML opts (Math mathType str) = do
  let displayType = if mathType == DisplayMath
                       then DisplayBlock
                       else DisplayInline
  case texMathToOMML displayType str of
        Right r -> return [r]
        Left  _ -> inlinesToOpenXML opts (texMathToInlines mathType str)
inlineToOpenXML opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML opts (Code attrs str) =
  withTextProp (rStyle "VerbatimChar")
  $ if writerHighlight opts
       then case highlight formatOpenXML attrs str of
             Nothing  -> unhighlighted
             Just h   -> return h
       else unhighlighted
     where unhighlighted = intercalate [br] `fmap`
                             (mapM formattedString $ lines str)
           formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
           toHlTok (toktype,tok) = mknode "w:r" []
                                     [ mknode "w:rPr" []
                                       [ rStyle $ show toktype ]
                                     , mknode "w:t" [("xml:space","preserve")] tok ]
inlineToOpenXML opts (Note bs) = do
  notes <- gets stFootnotes
  notenum <- getUniqueId
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] (rStyle "FootnoteRef")
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline (Format "openxml") $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs
  oldListLevel <- gets stListLevel
  oldParaProperties <- gets stParaProperties
  oldTextProperties <- gets stTextProperties
  modify $ \st -> st{ stListLevel = -1, stParaProperties = [], stTextProperties = [] }
  contents <- withParaProp (pStyle "FootnoteText") $ blocksToOpenXML opts
                $ insertNoteRef bs
  modify $ \st -> st{ stListLevel = oldListLevel, stParaProperties = oldParaProperties,
                      stTextProperties = oldTextProperties }
  let newnote = mknode "w:footnote" [("w:id", notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] (rStyle "FootnoteRef")
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML opts (Link txt ('#':xs,_)) = do
  contents <- withTextProp (rStyle "Link") $ inlinesToOpenXML opts txt
  return [ mknode "w:hyperlink" [("w:anchor",xs)] contents ]
-- external link:
inlineToOpenXML opts (Link txt (src,_)) = do
  contents <- withTextProp (rStyle "Link") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId"++) `fmap` getUniqueId
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",id')] contents ]
inlineToOpenXML opts (Image alt (src, tit)) = do
  -- first, check to see if we've already done this image
  imgs <- gets stImages
  case M.lookup src imgs of
    Just (_,_,_,elt,_) -> return [elt]
    Nothing -> do
      res <- liftIO $
               fetchItem' (writerMediaBag opts) (writerSourceURL opts) src
      case res of
        Left (_ :: E.SomeException) -> do
          liftIO $ warn $ "Could not find image `" ++ src ++ "', skipping..."
          -- emit alt text
          inlinesToOpenXML opts alt
        Right (img, mt) -> do
          ident <- ("rId"++) `fmap` getUniqueId
          let size = imageSize img
          let (xpt,ypt) = maybe (120,120) sizeInPoints size
          -- 12700 emu = 1 pt
          let (xemu,yemu) = (xpt * 12700, ypt * 12700)
          let cNvPicPr = mknode "pic:cNvPicPr" [] $
                           mknode "a:picLocks" [("noChangeArrowheads","1"),("noChangeAspect","1")] ()
          let nvPicPr  = mknode "pic:nvPicPr" []
                          [ mknode "pic:cNvPr"
                              [("descr",src),("id","0"),("name","Picture")] ()
                          , cNvPicPr ]
          let blipFill = mknode "pic:blipFill" []
                           [ mknode "a:blip" [("r:embed",ident)] ()
                           , mknode "a:stretch" [] $ mknode "a:fillRect" [] () ]
          let xfrm =    mknode "a:xfrm" []
                          [ mknode "a:off" [("x","0"),("y","0")] ()
                          , mknode "a:ext" [("cx",show xemu),("cy",show yemu)] () ]
          let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                           mknode "a:avLst" [] ()
          let ln =      mknode "a:ln" [("w","9525")]
                          [ mknode "a:noFill" [] ()
                          , mknode "a:headEnd" [] ()
                          , mknode "a:tailEnd" [] () ]
          let spPr =    mknode "pic:spPr" [("bwMode","auto")]
                          [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
          let graphic = mknode "a:graphic" [] $
                          mknode "a:graphicData" [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
                            [ mknode "pic:pic" []
                              [ nvPicPr
                              , blipFill
                              , spPr ] ]
          let imgElt = mknode "w:r" [] $
               mknode "w:drawing" [] $
                 mknode "wp:inline" []
                  [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
                  , mknode "wp:effectExtent" [("b","0"),("l","0"),("r","0"),("t","0")] ()
                  , mknode "wp:docPr" [("descr",tit),("id","1"),("name","Picture")] ()
                  , graphic ]
          let imgext = case mt >>= extensionFromMimeType of
                            Just x    -> '.':x
                            Nothing   -> case imageType img of
                                              Just Png  -> ".png"
                                              Just Jpeg -> ".jpeg"
                                              Just Gif  -> ".gif"
                                              Just Pdf  -> ".pdf"
                                              Just Eps  -> ".eps"
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
               return [imgElt]

br :: Element
br = mknode "w:r" [] [mknode "w:br" [("w:type","textWrapping")] () ]

parseXml :: Archive -> Archive -> String -> IO Element
parseXml refArchive distArchive relpath =
  case ((findEntryByPath relpath refArchive `mplus`
         findEntryByPath relpath distArchive)
         >>= parseXMLDoc . UTF8.toStringLazy . fromEntry) of
            Just d  -> return d
            Nothing -> fail $ relpath ++ " corrupt or missing in reference docx"

{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Codec.Archive.Zip
import Control.Applicative ((<|>))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, isLetter)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.String (fromString)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA (sha1, showDigest)
import Skylighting
import System.Random (randomRs, mkStdGen)
import Text.Pandoc.BCP47 (getLang, renderLang)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, toLang)
import qualified Text.Pandoc.Class.PandocMonad as P
import Data.Time
import Text.Pandoc.UTF8 (fromTextLazy)
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Highlighting (highlight)
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType,
                         getMimeTypeDef)
import Text.Pandoc.Options
import Text.Pandoc.Writers.Docx.StyleMap
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)
import Text.TeXMath
import Text.XML.Light as XML
import Text.XML.Light.Cursor as XMLC
import Text.Pandoc.Writers.OOXML

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

data EnvProps = EnvProps{ styleElement  :: Maybe Element
                        , otherElements :: [Element]
                        }

instance Semigroup EnvProps where
  EnvProps s es <> EnvProps s' es' = EnvProps (s <|> s') (es ++ es')

instance Monoid EnvProps where
  mempty = EnvProps Nothing []
  mappend = (<>)

squashProps :: EnvProps -> [Element]
squashProps (EnvProps Nothing es) = es
squashProps (EnvProps (Just e) es) = e : es

data WriterEnv = WriterEnv{ envTextProperties :: EnvProps
                          , envParaProperties :: EnvProps
                          , envRTL            :: Bool
                          , envListLevel      :: Int
                          , envListNumId      :: Int
                          , envInDel          :: Bool
                          , envChangesAuthor  :: T.Text
                          , envChangesDate    :: T.Text
                          , envPrintWidth     :: Integer
                          }

defaultWriterEnv :: WriterEnv
defaultWriterEnv = WriterEnv{ envTextProperties = mempty
                            , envParaProperties = mempty
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
       , stComments       :: [([(T.Text, T.Text)], [Inline])]
       , stSectionIds     :: Set.Set T.Text
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, String, Maybe MimeType, B.ByteString)
       , stLists          :: [ListMarker]
       , stInsId          :: Int
       , stDelId          :: Int
       , stStyleMaps      :: StyleMaps
       , stFirstPara      :: Bool
       , stInTable        :: Bool
       , stInList         :: Bool
       , stTocTitle       :: [Inline]
       , stDynamicParaProps :: Set.Set ParaStyleName
       , stDynamicTextProps :: Set.Set CharStyleName
       , stCurId          :: Int
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stFootnotes      = defaultFootnotes
      , stComments       = []
      , stSectionIds     = Set.empty
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      , stLists          = [NoMarker]
      , stInsId          = 1
      , stDelId          = 1
      , stStyleMaps      = StyleMaps M.empty M.empty
      , stFirstPara      = False
      , stInTable        = False
      , stInList         = False
      , stTocTitle       = [Str "Table of Contents"]
      , stDynamicParaProps = Set.empty
      , stDynamicTextProps = Set.empty
      , stCurId          = 20
      }

type WS m = ReaderT WriterEnv (StateT WriterState m)

renumIdMap :: Int -> [Element] -> M.Map String String
renumIdMap _ [] = M.empty
renumIdMap n (e:es)
  | Just oldId <- findAttr (QName "Id" Nothing Nothing) e =
      M.insert oldId ("rId" ++ show n) (renumIdMap (n+1) es)
  | otherwise = renumIdMap n es

replaceAttr :: (QName -> Bool) -> String -> [XML.Attr] -> [XML.Attr]
replaceAttr f val = map $
    \a -> if f (attrKey a) then XML.Attr (attrKey a) val else a

renumId :: (QName -> Bool) -> M.Map String String -> Element -> Element
renumId f renumMap e
  | Just oldId <- findAttrBy f e
  , Just newId <- M.lookup oldId renumMap =
    let attrs' = replaceAttr f newId (elAttribs e)
    in
     e { elAttribs = attrs' }
  | otherwise = e

renumIds :: (QName -> Bool) -> M.Map String String -> [Element] -> [Element]
renumIds f renumMap = map (renumId f renumMap)

findAttrTextBy :: (QName -> Bool) -> Element -> Maybe T.Text
findAttrTextBy x = fmap T.pack . findAttrBy x

lookupAttrTextBy :: (QName -> Bool) -> [XML.Attr] -> Maybe T.Text
lookupAttrTextBy x = fmap T.pack . lookupAttrBy x

-- | Certain characters are invalid in XML even if escaped.
-- See #1992
stripInvalidChars :: T.Text -> T.Text
stripInvalidChars = T.filter isValidChar

-- | See XML reference
isValidChar :: Char -> Bool
isValidChar '\t' = True
isValidChar '\n' = True
isValidChar '\r' = True
isValidChar '\xFFFE' = False
isValidChar '\xFFFF' = False
isValidChar c = (' ' <= c && c <= '\xD7FF') || ('\xE000' <= c)

writeDocx :: (PandocMonad m)
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m BL.ByteString
writeDocx opts doc = do
  let Pandoc meta blocks = walk fixDisplayMath doc
  let blocks' = makeSections True Nothing blocks
  let doc' = Pandoc meta blocks'

  username <- P.lookupEnv "USERNAME"
  utctime <- P.getCurrentTime
  oldUserDataDir <- P.getUserDataDir
  P.setUserDataDir Nothing
  res <- P.readDefaultDataFile "reference.docx"
  P.setUserDataDir oldUserDataDir
  let distArchive = toArchive $ BL.fromStrict res
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive <$> P.readFileLazy f
                     Nothing -> toArchive . BL.fromStrict <$>
                        P.readDataFile "reference.docx"

  parsedDoc <- parseXml refArchive distArchive "word/document.xml"
  let wname f qn = qPrefix qn == Just "w" && f (qName qn)
  let mbsectpr = filterElementName (wname (=="sectPr")) parsedDoc

  -- Gets the template size
  let mbpgsz = mbsectpr >>= filterElementName (wname (=="pgSz"))
  let mbAttrSzWidth = mbpgsz >>= lookupAttrTextBy ((=="w") . qName) . elAttribs

  let mbpgmar = mbsectpr >>= filterElementName (wname (=="pgMar"))
  let mbAttrMarLeft = mbpgmar >>= lookupAttrTextBy ((=="left") . qName) . elAttribs
  let mbAttrMarRight = mbpgmar >>= lookupAttrTextBy ((=="right") . qName) . elAttribs

  -- Get the available area (converting the size and the margins to int and
  -- doing the difference
  let pgContentWidth = do
                         w <- mbAttrSzWidth >>= safeRead
                         r <- mbAttrMarRight >>= safeRead
                         l <- mbAttrMarLeft >>= safeRead
                         pure $ w - r - l

  -- styles
  mblang <- toLang $ getLang opts meta
  let addLang :: Element -> Element
      addLang e = case (\l -> XMLC.toTree . go (T.unpack $ renderLang l) $
                                 XMLC.fromElement e) <$> mblang of
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
              setvalattr _ x                                 = x
              isLangElt (Elem e') = qName (elName e') == "lang"
              isLangElt _         = False

  let stylepath = "word/styles.xml"
  styledoc <- addLang <$> parseXml refArchive distArchive stylepath

  -- parse styledoc for heading styles
  let styleMaps = getStyleMaps refArchive

  let tocTitle = case lookupMetaInlines "toc-title" meta of
                   [] -> stTocTitle defaultWriterState
                   ls -> ls

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
        , envChangesDate   = T.pack $ formatTime defaultTimeLocale "%FT%XZ" utctime
        , envPrintWidth = maybe 420 (`quot` 20) pgContentWidth
        }


  ((contents, footnotes, comments), st) <- runStateT
                         (runReaderT
                          (writeOpenXML opts{writerWrapText = WrapNone} doc')
                          env)
                         initialSt
  let epochtime = floor $ utcTimeToPOSIXSeconds utctime
  let imgs = M.elems $ stImages st

  -- create entries for images in word/media/...
  let toImageEntry (_,path,_,img) = toEntry ("word/" ++ path) epochtime $ toLazy img
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
  let mkImageOverride (_, imgpath, mbMimeType, _) =
          mkOverrideNode ("/word/" ++ imgpath,
                          maybe "application/octet-stream" T.unpack mbMimeType)
  let mkMediaOverride imgpath =
          mkOverrideNode ('/':imgpath, T.unpack $ getMimeTypeDef imgpath)
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
                  ,("/docProps/custom.xml",
                    "application/vnd.openxmlformats-officedocument.custom-properties+xml")
                  ,("/word/styles.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml")
                  ,("/word/document.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml")
                  ,("/word/comments.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml")
                  ,("/word/footnotes.xml",
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml")
                  ] ++
                  map (\x -> (maybe "" ("/word/" ++) $ extractTarget x,
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml")) headers ++
                  map (\x -> (maybe "" ("/word/" ++) $ extractTarget x,
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml")) footers) ++
                    map mkImageOverride imgs ++
                    [ mkMediaOverride (eRelativePath e) | e <- zEntries refArchive
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
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments",
                      "rId8",
                      "comments.xml")
                    ]

  let idMap = renumIdMap (length baserels' + 1) (headers ++ footers)
  let renumHeaders = renumIds (\q -> qName q == "Id") idMap headers
  let renumFooters = renumIds (\q -> qName q == "Id") idMap footers
  let baserels = baserels' ++ renumHeaders ++ renumFooters
  let toImgRel (ident,path,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",path)] ()
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
        Nothing      -> mknode "w:sectPr" [] ()

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

  -- comments
  let commentsEntry = toEntry "word/comments.xml" epochtime
        $ renderXml $ mknode "w:comments" stdAttributes comments

  -- styles

  -- We only want to inject paragraph and text properties that
  -- are not already in the style map. Note that keys in the stylemap
  -- are normalized as lowercase.
  let newDynamicParaProps = filter
        (\sty -> not $ hasStyleName sty $ smParaStyle styleMaps)
        (Set.toList $ stDynamicParaProps st)

      newDynamicTextProps = filter
        (\sty -> not $ hasStyleName sty $ smCharStyle styleMaps)
        (Set.toList $ stDynamicTextProps st)

  let newstyles = map newParaPropToOpenXml newDynamicParaProps ++
                  map newTextPropToOpenXml newDynamicTextProps ++
                  maybe [] (styleToOpenXml styleMaps) (writerHighlightStyle opts)
  let styledoc' = styledoc{ elContent = elContent styledoc ++
                                           map Elem newstyles }
  let styleEntry = toEntry stylepath epochtime $ renderXml styledoc'

  -- construct word/numbering.xml
  let numpath = "word/numbering.xml"
  numbering <- parseXml refArchive distArchive numpath
  let newNumElts = mkNumbering (stLists st)
  let pandocAdded e =
       case findAttrTextBy ((== "abstractNumId") . qName) e >>= safeRead of
         Just numid -> numid >= (990 :: Int)
         Nothing    ->
           case findAttrTextBy ((== "numId") . qName) e >>= safeRead of
             Just numid -> numid >= (1000 :: Int)
             Nothing    -> False
  let oldElts = filter (not . pandocAdded) $ onlyElems (elContent numbering)
  let allElts = oldElts ++ newNumElts
  let numEntry = toEntry numpath epochtime $ renderXml numbering{ elContent =
                       -- we want all the abstractNums first, then the nums,
                       -- otherwise things break:
                       [Elem e | e <- allElts
                               , qName (elName e) == "abstractNum" ] ++
                       [Elem e | e <- allElts
                               , qName (elName e) == "num" ] }

  let keywords = case lookupMeta "keywords" meta of
                       Just (MetaList xs) -> map stringify xs
                       _                  -> []

  -- docProps/core.xml
  let docPropsPath = "docProps/core.xml"
  let extraCoreProps = ["subject","lang","category","description"]
  let extraCorePropsMap = M.fromList $ zip extraCoreProps
                       ["dc:subject","dc:language","cp:category","dc:description"]
  let lookupMetaString' :: T.Text -> Meta -> T.Text
      lookupMetaString' key' meta' =
        case key' of
             "description"    -> T.intercalate "_x000d_\n" (map stringify $ lookupMetaBlocks "description" meta')
             key''            -> lookupMetaString key'' meta'

  let docProps = mknode "cp:coreProperties"
          [("xmlns:cp","http://schemas.openxmlformats.org/package/2006/metadata/core-properties")
          ,("xmlns:dc","http://purl.org/dc/elements/1.1/")
          ,("xmlns:dcterms","http://purl.org/dc/terms/")
          ,("xmlns:dcmitype","http://purl.org/dc/dcmitype/")
          ,("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance")]
          $ mktnode "dc:title" [] (stringify $ docTitle meta)
          : mktnode "dc:creator" [] (T.intercalate "; " (map stringify $ docAuthors meta))
          : [ mktnode (M.findWithDefault "" k extraCorePropsMap) [] (lookupMetaString' k meta)
            | k <- M.keys (unMeta meta), k `elem` extraCoreProps]
          ++ mknode "cp:keywords" [] (T.unpack $ T.intercalate ", " keywords)
          : (\x -> [ mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")] x
                   , mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] x
                   ]) (formatTime defaultTimeLocale "%FT%XZ" utctime)
  let docPropsEntry = toEntry docPropsPath epochtime $ renderXml docProps

  -- docProps/custom.xml
  let customProperties :: [(String, String)]
      customProperties = [(T.unpack k, T.unpack $ lookupMetaString k meta) | k <- M.keys (unMeta meta)
                         , k `notElem` (["title", "author", "keywords"]
                                       ++ extraCoreProps)]
  let mkCustomProp (k, v) pid = mknode "property"
         [("fmtid","{D5CDD505-2E9C-101B-9397-08002B2CF9AE}")
         ,("pid", show pid)
         ,("name", k)] $ mknode "vt:lpwstr" [] v
  let customPropsPath = "docProps/custom.xml"
  let customProps = mknode "Properties"
          [("xmlns","http://schemas.openxmlformats.org/officeDocument/2006/custom-properties")
          ,("xmlns:vt","http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes")
          ] $ zipWith mkCustomProp customProperties [(2 :: Int)..]
  let customPropsEntry = toEntry customPropsPath epochtime $ renderXml customProps

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
        , [("Id","rId5")
          ,("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/custom-properties")
          ,("Target","docProps/custom.xml")]
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
                     , "w:evenAndOddHeaders"
                     , "w:proofState"
                     ]
  settingsEntry <- copyChildren refArchive distArchive settingsPath epochtime settingsList

  let entryFromArchive arch path =
         maybe (throwError $ PandocSomeError
                           $ T.pack $ path ++ " missing in reference docx")
               return
               (findEntryByPath path arch `mplus` findEntryByPath path distArchive)
  docPropsAppEntry <- entryFromArchive refArchive "docProps/app.xml"
  themeEntry <- entryFromArchive refArchive "word/theme/theme1.xml"
  fontTableEntry <- entryFromArchive refArchive "word/fontTable.xml"
  webSettingsEntry <- entryFromArchive refArchive "word/webSettings.xml"
  headerFooterEntries <- mapM (entryFromArchive refArchive . ("word/" ++)) $
                     mapMaybe extractTarget (headers ++ footers)
  let miscRelEntries = [ e | e <- zEntries refArchive
                       , "word/_rels/" `isPrefixOf` eRelativePath e
                       , ".xml.rels" `isSuffixOf` eRelativePath e
                       , eRelativePath e /= "word/_rels/document.xml.rels"
                       , eRelativePath e /= "word/_rels/footnotes.xml.rels" ]
  let otherMediaEntries = [ e | e <- zEntries refArchive
                          , "word/media/" `isPrefixOf` eRelativePath e ]

  -- Create archive
  let archive = foldr addEntryToArchive emptyArchive $
                  contentTypesEntry : relsEntry : contentEntry : relEntry :
                  footnoteRelEntry : numEntry : styleEntry : footnotesEntry :
                  commentsEntry :
                  docPropsEntry : docPropsAppEntry : customPropsEntry :
                  themeEntry :
                  fontTableEntry : settingsEntry : webSettingsEntry :
                  imageEntries ++ headerFooterEntries ++
                  miscRelEntries ++ otherMediaEntries
  return $ fromArchive archive

newParaPropToOpenXml :: ParaStyleName -> Element
newParaPropToOpenXml (fromStyleName -> s) =
  let styleId = T.filter (not . isSpace) s
  in mknode "w:style" [ ("w:type", "paragraph")
                      , ("w:customStyle", "1")
                      , ("w:styleId", T.unpack styleId)]
     [ mknode "w:name" [("w:val", T.unpack s)] ()
     , mknode "w:basedOn" [("w:val","BodyText")] ()
     , mknode "w:qFormat" [] ()
     ]

newTextPropToOpenXml :: CharStyleName -> Element
newTextPropToOpenXml (fromStyleName -> s) =
  let styleId = T.filter (not . isSpace) s
  in mknode "w:style" [ ("w:type", "character")
                      , ("w:customStyle", "1")
                      , ("w:styleId", T.unpack styleId)]
     [ mknode "w:name" [("w:val", T.unpack s)] ()
     , mknode "w:basedOn" [("w:val","BodyTextChar")] ()
     ]

styleToOpenXml :: StyleMaps -> Style -> [Element]
styleToOpenXml sm style =
  maybeToList parStyle ++ mapMaybe toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype | hasStyleName (fromString $ show toktype) (smCharStyle sm) = Nothing
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
        tokFeature f toktype = maybe False f $ M.lookup toktype tokStyles
        tokCol toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenColor =<< M.lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenBackground =<< M.lookup toktype tokStyles)
                           `mplus` backgroundColor style
        parStyle | hasStyleName "Source Code" (smParaStyle sm) = Nothing
                 | otherwise = Just $
                   mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               $ mknode "w:wordWrap" [("w:val","off")] ()
                               :
                         maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill",drop 1 $ fromColor col)] ()]) (backgroundColor style)
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

mkNumbering :: [ListMarker] -> [Element]
mkNumbering lists =
  elts ++ zipWith mkNum lists [baseListId..(baseListId + length lists - 1)]
    where elts = zipWith mkAbstractNum (ordNub lists) $
                     randomRs (0x10000000, 0xFFFFFFFF) $ mkStdGen 1848

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

mkAbstractNum :: ListMarker -> Integer -> Element
mkAbstractNum marker nsid =
  mknode "w:abstractNum" [("w:abstractNumId",listMarkerToId marker)]
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
          styleFor DefaultStyle 0 = "decimal"
          styleFor DefaultStyle 1 = "lowerLetter"
          styleFor DefaultStyle 2 = "lowerRoman"
          styleFor DefaultStyle 3 = "decimal"
          styleFor DefaultStyle 4 = "lowerLetter"
          styleFor DefaultStyle 5 = "lowerRoman"
          styleFor DefaultStyle x = styleFor DefaultStyle (x `mod` 6)
          styleFor _ _            = "decimal"
          patternFor OneParen s  = s ++ ")"
          patternFor TwoParens s = "(" ++ s ++ ")"
          patternFor _ s         = s ++ "."

getNumId :: (PandocMonad m) => WS m Int
getNumId = (((baseListId - 1) +) . length) `fmap` gets stLists


makeTOC :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeTOC opts = do
  let depth = "1-"++show (writerTOCDepth opts)
  let tocCmd = "TOC \\o \""++depth++"\" \\h \\z \\u"
  tocTitle <- gets stTocTitle
  title <- withParaPropM (pStyleM "TOC Heading") (blocksToOpenXML opts [Para tocTitle])
  return
    [mknode "w:sdt" [] [
      mknode "w:sdtPr" [] (
        mknode "w:docPartObj" []
          [mknode "w:docPartGallery" [("w:val","Table of Contents")] (),
          mknode "w:docPartUnique" [] ()]
         -- w:docPartObj
      ), -- w:sdtPr
      mknode "w:sdtContent" [] (title++[
        mknode "w:p" [] (
          mknode "w:r" [] [
            mknode "w:fldChar" [("w:fldCharType","begin"),("w:dirty","true")] (),
            mknode "w:instrText" [("xml:space","preserve")] tocCmd,
            mknode "w:fldChar" [("w:fldCharType","separate")] (),
            mknode "w:fldChar" [("w:fldCharType","end")] ()
          ] -- w:r
        ) -- w:p
      ])
    ]] -- w:sdt

-- | Convert Pandoc document to two lists of
-- OpenXML elements (the main document and footnotes).
writeOpenXML :: (PandocMonad m) => WriterOptions -> Pandoc -> WS m ([Element], [Element],[Element])
writeOpenXML opts (Pandoc meta blocks) = do
  let tit = docTitle meta
  let auths = docAuthors meta
  let dat = docDate meta
  let abstract' = lookupMetaBlocks "abstract" meta
  let subtitle' = lookupMetaInlines "subtitle" meta
  let includeTOC = writerTableOfContents opts || lookupMetaBool "toc" meta
  title <- withParaPropM (pStyleM "Title") $ blocksToOpenXML opts [Para tit | not (null tit)]
  subtitle <- withParaPropM (pStyleM "Subtitle") $ blocksToOpenXML opts [Para subtitle' | not (null subtitle')]
  authors <- withParaPropM (pStyleM "Author") $ blocksToOpenXML opts $
       map Para auths
  date <- withParaPropM (pStyleM "Date") $ blocksToOpenXML opts [Para dat | not (null dat)]
  abstract <- if null abstract'
                 then return []
                 else withParaPropM (pStyleM "Abstract") $ blocksToOpenXML opts abstract'
  let convertSpace (Str x : Space : Str y : xs) = Str (x <> " " <> y) : xs
      convertSpace (Str x : Str y : xs)         = Str (x <> y) : xs
      convertSpace xs                           = xs
  let blocks' = bottomUp convertSpace blocks
  doc' <- setFirstPara >> blocksToOpenXML opts blocks'
  notes' <- reverse <$> gets stFootnotes
  comments <- reverse <$> gets stComments
  let toComment (kvs, ils) = do
        annotation <- inlinesToOpenXML opts ils
        return $
          mknode "w:comment" [('w':':':T.unpack k,T.unpack v) | (k,v) <- kvs]
            [ mknode "w:p" [] $
              [ mknode "w:pPr" []
                [ mknode "w:pStyle" [("w:val", "CommentText")] () ]
              , mknode "w:r" []
                [ mknode "w:rPr" []
                  [ mknode "w:rStyle" [("w:val", "CommentReference")] ()
                  , mknode "w:annotationRef" [] ()
                  ]
                ]
              ] ++ annotation
            ]
  comments' <- mapM toComment comments
  toc <- if includeTOC
            then makeTOC opts
            else return []
  let meta' = title ++ subtitle ++ authors ++ date ++ abstract ++ toc
  return (meta' ++ doc', notes', comments')

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: (PandocMonad m) => WriterOptions -> [Block] -> WS m [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

pStyleM :: (PandocMonad m) => ParaStyleName -> WS m XML.Element
pStyleM styleName = do
  pStyleMap <- gets (smParaStyle . stStyleMaps)
  let sty' = getStyleIdFromName styleName pStyleMap
  return $ mknode "w:pStyle" [("w:val", T.unpack $ fromStyleId sty')] ()

rStyleM :: (PandocMonad m) => CharStyleName -> WS m XML.Element
rStyleM styleName = do
  cStyleMap <- gets (smCharStyle . stStyleMaps)
  let sty' = getStyleIdFromName styleName cStyleMap
  return $ mknode "w:rStyle" [("w:val", T.unpack $ fromStyleId sty')] ()

getUniqueId :: (PandocMonad m) => WS m String
-- the + 20 is to ensure that there are no clashes with the rIds
-- already in word/document.xml.rel
getUniqueId = do
  n <- gets stCurId
  modify $ \st -> st{stCurId = n + 1}
  return $ show n

-- | Key for specifying user-defined docx styles.
dynamicStyleKey :: T.Text
dynamicStyleKey = "custom-style"

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: (PandocMonad m) => WriterOptions -> Block -> WS m [Element]
blockToOpenXML opts blk = withDirection $ blockToOpenXML' opts blk

blockToOpenXML' :: (PandocMonad m) => WriterOptions -> Block -> WS m [Element]
blockToOpenXML' _ Null = return []
blockToOpenXML' opts (Div (ident,_classes,kvs) bs) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just (fromString . T.unpack -> sty) -> do
                      modify $ \s ->
                        s{stDynamicParaProps = Set.insert sty
                             (stDynamicParaProps s)}
                      return $ withParaPropM (pStyleM sty)
                   _ -> return id
  dirmod <- case lookup "dir" kvs of
                 Just "rtl" -> return $ local (\env -> env { envRTL = True })
                 Just "ltr" -> return $ local (\env -> env { envRTL = False })
                 _ -> return id
  let (hs, bs') = if ident == "refs"
                     then span isHeaderBlock bs
                     else ([], bs)
  let bibmod = if ident == "refs"
                  then withParaPropM (pStyleM "Bibliography")
                  else id
  header <- dirmod $ stylemod $ blocksToOpenXML opts hs
  contents <- dirmod $ bibmod $ stylemod $ blocksToOpenXML opts bs'
  wrapBookmark ident $ header <> contents
blockToOpenXML' opts (Header lev (ident,_,kvs) lst) = do
  setFirstPara
  paraProps <- withParaPropM (pStyleM (fromString $ "Heading "++show lev)) $
                    getParaProps False
  number <-
        if writerNumberSections opts
           then
             case lookup "number" kvs of
                Just n -> do
                   num <- withTextPropM (rStyleM "SectionNumber")
                            (inlineToOpenXML opts (Str n))
                   return $ num ++ [mknode "w:r" [] [mknode "w:tab" [] ()]]
                Nothing -> return []
           else return []
  contents <- (number ++) <$> inlinesToOpenXML opts lst
  if T.null ident
     then return [mknode "w:p" [] (paraProps ++ contents)]
     else do
       let bookmarkName = ident
       modify $ \s -> s{ stSectionIds = Set.insert bookmarkName
                                      $ stSectionIds s }
       bookmarkedContents <- wrapBookmark bookmarkName contents
       return [mknode "w:p" [] (paraProps ++ bookmarkedContents)]
blockToOpenXML' opts (Plain lst) = do
  isInTable <- gets stInTable
  isInList <- gets stInList
  let block = blockToOpenXML opts (Para lst)
  prop <- pStyleM "Compact"
  if isInTable || isInList
     then withParaProp prop block
     else block
-- title beginning with fig: indicates that the image is a figure
blockToOpenXML' opts (Para [Image attr alt (src,T.stripPrefix "fig:" -> Just tit)]) = do
  setFirstPara
  prop <- pStyleM $
        if null alt
        then "Figure"
        else "Captioned Figure"
  paraProps <- local (\env -> env { envParaProperties = EnvProps (Just prop) [] <> envParaProperties env }) (getParaProps False)
  contents <- inlinesToOpenXML opts [Image attr alt (src,tit)]
  captionNode <- withParaPropM (pStyleM "Image Caption")
                 $ blockToOpenXML opts (Para alt)
  return $ mknode "w:p" [] (paraProps ++ contents) : captionNode
blockToOpenXML' opts (Para lst)
  | null lst && not (isEnabled Ext_empty_paragraphs opts) = return []
  | otherwise = do
      isFirstPara <- gets stFirstPara
      let displayMathPara = case lst of
                                 [x] -> isDisplayMath x
                                 _   -> False
      paraProps <- getParaProps displayMathPara
      bodyTextStyle <- pStyleM $ if isFirstPara
                       then "First Paragraph"
                       else "Body Text"
      let paraProps' = case paraProps of
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
  p <- withParaPropM (pStyleM "Block Text")
       $ blocksToOpenXML opts blocks
  setFirstPara
  return p
blockToOpenXML' opts (CodeBlock attrs@(ident, _, _) str) = do
  p <- withParaPropM (pStyleM "Source Code") (blockToOpenXML opts $ Para [Code attrs str])
  setFirstPara
  wrapBookmark ident p
blockToOpenXML' _ HorizontalRule = do
  setFirstPara
  return [
    mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML' opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  setFirstPara
  modify $ \s -> s { stInTable = True }
  let captionStr = stringify caption
  caption' <- if null caption
                 then return []
                 else withParaPropM (pStyleM "Table Caption")
                      $ blockToOpenXML opts (Para caption)
  let alignmentFor al = mknode "w:jc" [("w:val",alignmentToString al)] ()
  -- Table cells require a <w:p> element, even an empty one!
  -- Not in the spec but in Word 2007, 2010. See #4953.
  let cellToOpenXML (al, cell) = do
        es <- withParaProp (alignmentFor al) $ blocksToOpenXML opts cell
        return $ if any (\e -> qName (elName e) == "p") es
           then es
           else es ++ [mknode "w:p" [] ()]
  headers' <- mapM cellToOpenXML $ zip aligns headers
  rows' <- mapM (mapM cellToOpenXML . zip aligns) rows
  let borderProps = mknode "w:tcPr" []
                    [ mknode "w:tcBorders" []
                      $ mknode "w:bottom" [("w:val","single")] ()
                    , mknode "w:vAlign" [("w:val","bottom")] () ]
  compactStyle <- pStyleM "Compact"
  let emptyCell' = [mknode "w:p" [] [mknode "w:pPr" [] [compactStyle]]]
  let mkcell border contents = mknode "w:tc" []
                            $ [ borderProps | border ] ++
                            if null contents
                               then emptyCell'
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
  let hasHeader = any (not . null) headers
  modify $ \s -> s { stInTable = False }
  return $
    caption' ++
    [mknode "w:tbl" []
      ( mknode "w:tblPr" []
        (   mknode "w:tblStyle" [("w:val","Table")] () :
            mknode "w:tblW" [("w:type", "pct"), ("w:w", show rowwidth)] () :
            mknode "w:tblLook" [("w:firstRow",if hasHeader then "1" else "0")
                               ,("w:lastRow","0")
                               ,("w:firstColumn","0")
                               ,("w:lastColumn","0")
                               ,("w:noHBand","0")
                               ,("w:noVBand","0")] () :
          [ mknode "w:tblCaption" [("w:val", T.unpack captionStr)] ()
          | not (null caption) ] )
      : mknode "w:tblGrid" []
        (if all (==0) widths
            then []
            else map mkgridcol widths)
      : [ mkrow True headers' | hasHeader ] ++
      map (mkrow False) rows'
      )]
blockToOpenXML' opts el
  | BulletList lst <- el = addOpenXMLList BulletMarker lst
  | OrderedList (start, numstyle, numdelim) lst <- el
  = addOpenXMLList (NumberMarker numstyle numdelim start) lst
  where
    addOpenXMLList marker lst = do
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
  term' <- withParaPropM (pStyleM "Definition Term")
           $ blockToOpenXML opts (Para term)
  defs' <- withParaPropM (pStyleM "Definition")
           $ concat `fmap` mapM (blocksToOpenXML opts) defs
  return $ term' ++ defs'

addList :: (PandocMonad m) => ListMarker -> WS m ()
addList marker = do
  lists <- gets stLists
  modify $ \st -> st{ stLists = lists ++ [marker] }

listItemToOpenXML :: (PandocMonad m) => WriterOptions -> Int -> [Block] -> WS m [Element]
listItemToOpenXML _ _ []                   = return []
listItemToOpenXML opts numid (first:rest) = do
  oldInList <- gets stInList
  modify $ \st -> st{ stInList = True }
  first' <- withNumId numid $ blockToOpenXML opts first
  -- baseListId is the code for no list marker:
  rest'  <- withNumId baseListId $ blocksToOpenXML opts rest
  modify $ \st -> st{ stInList = oldInList }
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

isStyle :: Element -> Bool
isStyle e = isElem [] "w" "rStyle" e ||
            isElem [] "w" "pStyle" e

getTextProps :: (PandocMonad m) => WS m [Element]
getTextProps = do
  props <- asks envTextProperties
  let squashed = squashProps props
  return [mknode "w:rPr" [] squashed | (not . null) squashed]

withTextProp :: PandocMonad m => Element -> WS m a -> WS m a
withTextProp d p =
  local (\env -> env {envTextProperties = ep <> envTextProperties env}) p
  where ep = if isStyle d then EnvProps (Just d) [] else EnvProps Nothing [d]

withTextPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withTextPropM md p = do
  d <- md
  withTextProp d p

getParaProps :: PandocMonad m => Bool -> WS m [Element]
getParaProps displayMathPara = do
  props <- asks envParaProperties
  listLevel <- asks envListLevel
  numid <- asks envListNumId
  let listPr = [mknode "w:numPr" []
                [ mknode "w:ilvl" [("w:val",show listLevel)] ()
                , mknode "w:numId" [("w:val",show numid)] () ] | listLevel >= 0 && not displayMathPara]
  return $ case listPr ++ squashProps props of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

withParaProp :: PandocMonad m => Element -> WS m a -> WS m a
withParaProp d p =
  local (\env -> env {envParaProperties = ep <> envParaProperties env}) p
  where ep = if isStyle d then EnvProps (Just d) [] else EnvProps Nothing [d]

withParaPropM :: PandocMonad m => WS m Element -> WS m a -> WS m a
withParaPropM md p = do
  d <- md
  withParaProp d p

formattedString :: PandocMonad m => T.Text -> WS m [Element]
formattedString str =
  -- properly handle soft hyphens
  case splitTextBy (=='\173') str of
      [w] -> formattedString' w
      ws  -> do
         sh <- formattedRun [mknode "w:softHyphen" [] ()]
         intercalate sh <$> mapM formattedString' ws

formattedString' :: PandocMonad m => T.Text -> WS m [Element]
formattedString' str = do
  inDel <- asks envInDel
  formattedRun [ mktnode (if inDel then "w:delText" else "w:t")
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
inlineToOpenXML' _ (Span (ident,["comment-start"],kvs) ils) = do
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
      kvs' = filter (("id" /=) . fst) kvs
  modify $ \st -> st{ stComments = (("id",ident'):kvs', ils) : stComments st }
  return [ mknode "w:commentRangeStart" [("w:id", T.unpack ident')] () ]
inlineToOpenXML' _ (Span (ident,["comment-end"],kvs) _) =
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
  in
    return [ mknode "w:commentRangeEnd" [("w:id", T.unpack ident')] ()
           , mknode "w:r" []
             [ mknode "w:rPr" []
               [ mknode "w:rStyle" [("w:val", "CommentReference")] () ]
             , mknode "w:commentReference" [("w:id", T.unpack ident')] () ]
           ]
inlineToOpenXML' opts (Span (ident,classes,kvs) ils) = do
  stylemod <- case lookup dynamicStyleKey kvs of
                   Just (fromString . T.unpack -> sty) -> do
                      modify $ \s ->
                        s{stDynamicTextProps = Set.insert sty
                              (stDynamicTextProps s)}
                      return $ withTextPropM (rStyleM sty)
                   _ -> return id
  let dirmod = case lookup "dir" kvs of
                 Just "rtl" -> local (\env -> env { envRTL = True })
                 Just "ltr" -> local (\env -> env { envRTL = False })
                 _          -> id
      off x = withTextProp (mknode x [("w:val","0")] ())
      pmod =  (if "csl-no-emph" `elem` classes then off "w:i" else id) .
              (if "csl-no-strong" `elem` classes then off "w:b" else id) .
              (if "csl-no-smallcaps" `elem` classes
                  then off "w:smallCaps"
                  else id)
      getChangeAuthorDate = do
        defaultAuthor <- asks envChangesAuthor
        defaultDate <- asks envChangesDate
        let author = fromMaybe defaultAuthor (lookup "author" kvs)
            date   = fromMaybe defaultDate (lookup "date" kvs)
        return (author, date)
  insmod <- if "insertion" `elem` classes
               then do
                 (author, date) <- getChangeAuthorDate
                 insId <- gets stInsId
                 modify $ \s -> s{stInsId = insId + 1}
                 return $ \f -> do
                   x <- f
                   return [ mknode "w:ins"
                              [("w:id", show insId),
                              ("w:author", T.unpack author),
                              ("w:date", T.unpack date)] x ]
               else return id
  delmod <- if "deletion" `elem` classes
               then do
                 (author, date) <- getChangeAuthorDate
                 delId <- gets stDelId
                 modify $ \s -> s{stDelId = delId + 1}
                 return $ \f -> local (\env->env{envInDel=True}) $ do
                   x <- f
                   return [mknode "w:del"
                           [("w:id", show delId),
                           ("w:author", T.unpack author),
                           ("w:date", T.unpack date)] x]
               else return id
  contents <- insmod $ delmod $ dirmod $ stylemod $ pmod
                     $ inlinesToOpenXML opts ils
  wrapBookmark ident contents
inlineToOpenXML' opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML' opts (Underline lst) =
  withTextProp (mknode "w:u" [("w:val","single")] ()) $
    inlinesToOpenXML opts lst
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
  let alltoktypes = [KeywordTok ..]
  tokTypesMap <- mapM (\tt -> (,) tt <$> rStyleM (fromString $ show tt)) alltoktypes
  let unhighlighted = intercalate [br] `fmap`
                       mapM formattedString (T.lines str)
      formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
      toHlTok (toktype,tok) =
        mknode "w:r" []
          [ mknode "w:rPr" [] $
            maybeToList (lookup toktype tokTypesMap)
            , mknode "w:t" [("xml:space","preserve")] (T.unpack tok) ]
  withTextPropM (rStyleM "Verbatim Char")
    $ if isNothing (writerHighlightStyle opts)
          then unhighlighted
          else case highlight (writerSyntaxMap opts)
                      formatOpenXML attrs str of
                    Right h  -> return h
                    Left msg -> do
                      unless (T.null msg) $ report $ CouldNotHighlight msg
                      unhighlighted
inlineToOpenXML' opts (Note bs) = do
  notes <- gets stFootnotes
  notenum <- getUniqueId
  footnoteStyle <- rStyleM "Footnote Reference"
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] footnoteStyle
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline (Format "openxml") $ T.pack $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : Space : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : Space : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs

  contents <- local (\env -> env{ envListLevel = -1
                                , envParaProperties = mempty
                                , envTextProperties = mempty })
              (withParaPropM (pStyleM "Footnote Text") $ blocksToOpenXML opts
                $ insertNoteRef bs)
  let newnote = mknode "w:footnote" [("w:id", notenum)] contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] footnoteStyle
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML' opts (Link _ txt (T.uncons -> Just ('#', xs),_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  return
    [ mknode "w:hyperlink" [("w:anchor", T.unpack $ toBookmarkName xs)] contents ]
-- external link:
inlineToOpenXML' opts (Link _ txt (src,_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup (T.unpack src) extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId"++) `fmap` getUniqueId
              modify $ \st -> st{ stExternalLinks =
                        M.insert (T.unpack src) i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",id')] contents ]
inlineToOpenXML' opts (Image attr@(imgident, _, _) alt (src, title)) = do
  pageWidth <- asks envPrintWidth
  imgs <- gets stImages
  let
    stImage = M.lookup (T.unpack src) imgs
    generateImgElt (ident, _, _, img) =
      let
        (xpt,ypt) = desiredSizeInPoints opts attr
               (either (const def) id (imageSize opts img))
        -- 12700 emu = 1 pt
        (xemu,yemu) = fitToPage (xpt * 12700, ypt * 12700)
                                (pageWidth * 12700)
        cNvPicPr = mknode "pic:cNvPicPr" [] $
                         mknode "a:picLocks" [("noChangeArrowheads","1")
                                             ,("noChangeAspect","1")] ()
        nvPicPr  = mknode "pic:nvPicPr" []
                        [ mknode "pic:cNvPr"
                            [("descr",T.unpack src),("id","0"),("name","Picture")] ()
                        , cNvPicPr ]
        blipFill = mknode "pic:blipFill" []
          [ mknode "a:blip" [("r:embed",ident)] ()
          , mknode "a:stretch" [] $
              mknode "a:fillRect" [] ()
          ]
        xfrm =    mknode "a:xfrm" []
                        [ mknode "a:off" [("x","0"),("y","0")] ()
                        , mknode "a:ext" [("cx",show xemu)
                                         ,("cy",show yemu)] () ]
        prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                         mknode "a:avLst" [] ()
        ln =      mknode "a:ln" [("w","9525")]
                        [ mknode "a:noFill" [] ()
                        , mknode "a:headEnd" [] ()
                        , mknode "a:tailEnd" [] () ]
        spPr =    mknode "pic:spPr" [("bwMode","auto")]
                        [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
        graphic = mknode "a:graphic" [] $
          mknode "a:graphicData"
            [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
            [ mknode "pic:pic" []
              [ nvPicPr
              , blipFill
              , spPr
              ]
            ]
        imgElt = mknode "w:r" [] $
          mknode "w:drawing" [] $
            mknode "wp:inline" []
              [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
              , mknode "wp:effectExtent"
                [("b","0"),("l","0"),("r","0"),("t","0")] ()
              , mknode "wp:docPr"
                [ ("descr", T.unpack $ stringify alt)
                , ("title", T.unpack title)
                , ("id","1")
                , ("name","Picture")
                ] ()
              , graphic
              ]
      in
        imgElt

  wrapBookmark imgident =<< case stImage of
    Just imgData -> return [generateImgElt imgData]
    Nothing -> ( do --try
      (img, mt) <- P.fetchItem src
      ident <- ("rId"++) `fmap` getUniqueId

      let
        imgext = case mt >>= extensionFromMimeType of
          Just x    -> "." <> x
          Nothing   -> case imageType img of
            Just Png  -> ".png"
            Just Jpeg -> ".jpeg"
            Just Gif  -> ".gif"
            Just Pdf  -> ".pdf"
            Just Eps  -> ".eps"
            Just Svg  -> ".svg"
            Just Emf  -> ".emf"
            Nothing   -> ""
        imgpath = "media/" <> ident <> T.unpack imgext
        mbMimeType = mt <|> getMimeType imgpath

        imgData = (ident, imgpath, mbMimeType, img)

      if T.null imgext
         then -- without an extension there is no rule for content type
           inlinesToOpenXML opts alt -- return alt to avoid corrupted docx
         else do
           -- insert mime type to use in constructing [Content_Types].xml
           modify $ \st -> st { stImages = M.insert (T.unpack src) imgData $ stImages st }
           return [generateImgElt imgData]
      )
      `catchError` ( \e -> do
        report $ CouldNotFetchResource src $ T.pack (show e)
        -- emit alt text
        inlinesToOpenXML opts alt
      )

br :: Element
br = mknode "w:r" [] [mknode "w:br" [] ()]

-- Word will insert these footnotes into the settings.xml file
-- (whether or not they're visible in the document). If they're in the
-- file, but not in the footnotes.xml file, it will produce
-- problems. So we want to make sure we insert them into our document.
defaultFootnotes :: [Element]
defaultFootnotes = [ mknode "w:footnote"
                     [("w:type", "separator"), ("w:id", "-1")]
                     [ mknode "w:p" []
                       [mknode "w:r" []
                        [ mknode "w:separator" [] ()]]]
                   , mknode "w:footnote"
                     [("w:type", "continuationSeparator"), ("w:id", "0")]
                     [ mknode "w:p" []
                       [ mknode "w:r" []
                         [ mknode "w:continuationSeparator" [] ()]]]]


withDirection :: PandocMonad m => WS m a -> WS m a
withDirection x = do
  isRTL <- asks envRTL
  paraProps <- asks envParaProperties
  textProps <- asks envTextProperties
  -- We want to clean all bidirection (bidi) and right-to-left (rtl)
  -- properties from the props first. This is because we don't want
  -- them to stack up.
  let paraProps' = filter (\e -> (qName . elName) e /= "bidi") (otherElements paraProps)
      textProps' = filter (\e -> (qName . elName) e /= "rtl") (otherElements textProps)
      paraStyle = styleElement paraProps
      textStyle = styleElement textProps
  if isRTL
    -- if we are going right-to-left, we (re?)add the properties.
    then flip local x $
         \env -> env { envParaProperties = EnvProps paraStyle $ mknode "w:bidi" [] () : paraProps'
                     , envTextProperties = EnvProps textStyle $ mknode "w:rtl" [] () : textProps'
                     }
    else flip local x $ \env -> env { envParaProperties = EnvProps paraStyle paraProps'
                                    , envTextProperties = EnvProps textStyle textProps'
                                    }

wrapBookmark :: (PandocMonad m) => T.Text -> [Element] -> WS m [Element]
wrapBookmark "" contents = return contents
wrapBookmark ident contents = do
  id' <- getUniqueId
  let bookmarkStart = mknode "w:bookmarkStart"
                       [("w:id", id')
                       ,("w:name", T.unpack $ toBookmarkName ident)] ()
      bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
  return $ bookmarkStart : contents ++ [bookmarkEnd]

-- Word imposes a 40 character limit on bookmark names and requires
-- that they begin with a letter.  So we just use a hash of the
-- identifier when otherwise we'd have an illegal bookmark name.
toBookmarkName :: T.Text -> T.Text
toBookmarkName s
  | Just (c, _) <- T.uncons s
  , isLetter c
  , T.length s <= 40 = s
  | otherwise = T.pack $ 'X' : drop 1 (showDigest (sha1 (fromTextLazy $ TL.fromStrict s)))

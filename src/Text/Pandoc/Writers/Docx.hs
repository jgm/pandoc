{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012-2021 John MacFarlane
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
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace, isLetter)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Data.String (fromString)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA (sha1, showDigest)
import Skylighting
import Text.Collate.Lang (renderLang)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, toLang, translateTerm)
import qualified Text.Pandoc.Translations as Term
import qualified Text.Pandoc.Class.PandocMonad as P
import Data.Time
import Text.Pandoc.UTF8 (fromTextLazy)
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Highlighting (highlight)
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (extensionFromMimeType, getMimeType, getMimeTypeDef)
import Text.Pandoc.Options
import Text.Pandoc.Writers.Docx.StyleMap
import Text.Pandoc.Writers.Docx.Table
import Text.Pandoc.Writers.Docx.Types
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import qualified Text.Pandoc.Writers.GridTable as Grid
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.TeXMath
import Text.Pandoc.Writers.OOXML
import Text.Pandoc.XML.Light as XML
import Data.Generics (mkT, everywhere)

squashProps :: EnvProps -> [Element]
squashProps (EnvProps Nothing es) = es
squashProps (EnvProps (Just e) es) = e : es

renumIdMap :: Int -> [Element] -> M.Map Text Text
renumIdMap _ [] = M.empty
renumIdMap n (e:es)
  | Just oldId <- findAttr (QName "Id" Nothing Nothing) e =
      M.insert oldId ("rId" <> tshow n) (renumIdMap (n+1) es)
  | otherwise = renumIdMap n es

replaceAttr :: (QName -> Bool) -> Text -> [XML.Attr] -> [XML.Attr]
replaceAttr f val = map $
    \a -> if f (attrKey a) then XML.Attr (attrKey a) val else a

renumId :: (QName -> Bool) -> M.Map Text Text -> Element -> Element
renumId f renumMap e
  | Just oldId <- findAttrBy f e
  , Just newId <- M.lookup oldId renumMap =
    let attrs' = replaceAttr f newId (elAttribs e)
    in
     e { elAttribs = attrs' }
  | otherwise = e

renumIds :: (QName -> Bool) -> M.Map Text Text -> [Element] -> [Element]
renumIds f renumMap = map (renumId f renumMap)

-- | Certain characters are invalid in XML even if escaped.
-- See #1992
stripInvalidChars :: Text -> Text
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
  utctime <- P.getTimestamp
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
  let mbAttrSzWidth = mbpgsz >>= lookupAttrBy ((=="w") . qName) . elAttribs

  let mbpgmar = mbsectpr >>= filterElementName (wname (=="pgMar"))
  let mbAttrMarLeft = mbpgmar >>= lookupAttrBy ((=="left") . qName) . elAttribs
  let mbAttrMarRight = mbpgmar >>= lookupAttrBy ((=="right") . qName) . elAttribs

  -- Get the available area (converting the size and the margins to int and
  -- doing the difference
  let pgContentWidth = do
                         w <- mbAttrSzWidth >>= safeRead
                         r <- mbAttrMarRight >>= safeRead
                         l <- mbAttrMarLeft >>= safeRead
                         pure $ w - r - l

  -- styles
  mblang <- toLang $ getLang opts meta
  -- TODO FIXME avoid this generic traversal!
  -- lang is in w:docDefaults /  w:rPr  /  w:lang
  let addLang :: Element -> Element
      addLang = case mblang of
                  Nothing -> id
                  Just l  -> everywhere (mkT (go (renderLang l)))
        where
          go :: Text -> Element -> Element
          go l e'
            | qName (elName e') == "lang"
                = e'{ elAttribs = map (setvalattr l) $ elAttribs e' }
            | otherwise = e'

          setvalattr l (XML.Attr qn@(QName "val" _ _) _) = XML.Attr qn l
          setvalattr _ x                                 = x

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
               [("PartName", T.pack part')
               ,("ContentType", contentType')] ()
  let mkImageOverride (_, imgpath, mbMimeType, _) =
          mkOverrideNode ("/word/" <> imgpath,
                          fromMaybe "application/octet-stream" mbMimeType)
  let mkMediaOverride imgpath =
          mkOverrideNode ("/" <> imgpath, getMimeTypeDef imgpath)
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
                  map (\x -> (maybe "" (T.unpack . ("/word/" <>)) (extractTarget x),
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml")) headers ++
                  map (\x -> (maybe "" (T.unpack . ("/word/" <>)) (extractTarget x),
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml")) footers) ++
                    map mkImageOverride imgs ++
                    [ mkMediaOverride (eRelativePath e)
                        | e <- zEntries refArchive
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
  let toImgRel (ident,path,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",T.pack ident),("Target",T.pack path)] ()
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
  let contents' = contents ++ [Elem sectpr]
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
       case findAttrBy ((== "abstractNumId") . qName) e >>= safeRead of
         Just numid -> numid >= (990 :: Int)
         Nothing    ->
           case findAttrBy ((== "numId") . qName) e >>= safeRead of
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
  let lookupMetaString' :: Text -> Meta -> Text
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
          ++ mknode "cp:keywords" [] (T.intercalate ", " keywords)
          : (\x -> [ mknode "dcterms:created" [("xsi:type","dcterms:W3CDTF")] x
                   , mknode "dcterms:modified" [("xsi:type","dcterms:W3CDTF")] x
                   ]) (T.pack $ formatTime defaultTimeLocale "%FT%XZ" utctime)
  let docPropsEntry = toEntry docPropsPath epochtime $ renderXml docProps

  -- docProps/custom.xml
  let customProperties :: [(Text, Text)]
      customProperties = [ (k, lookupMetaString k meta)
                         | k <- M.keys (unMeta meta)
                         , k `notElem` (["title", "author", "keywords"]
                                       ++ extraCoreProps)]
  let mkCustomProp (k, v) pid = mknode "property"
         [("fmtid","{D5CDD505-2E9C-101B-9397-08002B2CF9AE}")
         ,("pid", tshow pid)
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
      settingsList = [ "zoom"
                     , "embedSystemFonts"
                     , "doNotTrackMoves"
                     , "defaultTabStop"
                     , "drawingGridHorizontalSpacing"
                     , "drawingGridVerticalSpacing"
                     , "displayHorizontalDrawingGridEvery"
                     , "displayVerticalDrawingGridEvery"
                     , "characterSpacingControl"
                     , "savePreviewPicture"
                     , "mathPr"
                     , "themeFontLang"
                     , "decimalSymbol"
                     , "listSeparator"
                     , "autoHyphenation"
                     , "consecutiveHyphenLimit"
                     , "hyphenationZone"
                     , "doNotHyphenateCap"
                     , "evenAndOddHeaders"
                     , "proofState"
                     , "compat"
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
                         mapMaybe (fmap T.unpack . extractTarget)
                         (headers ++ footers)
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
                      , ("w:styleId", styleId)]
     [ mknode "w:name" [("w:val", s)] ()
     , mknode "w:basedOn" [("w:val","BodyText")] ()
     , mknode "w:qFormat" [] ()
     ]

newTextPropToOpenXml :: CharStyleName -> Element
newTextPropToOpenXml (fromStyleName -> s) =
  let styleId = T.filter (not . isSpace) s
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
        toStyle toktype | hasStyleName (fromString $ show toktype) (smCharStyle sm) = Nothing
                        | otherwise = Just $
                          mknode "w:style" [("w:type","character"),
                           ("w:customStyle","1"),("w:styleId", tshow toktype)]
                             [ mknode "w:name" [("w:val", tshow toktype)] ()
                             , mknode "w:basedOn" [("w:val","VerbatimChar")] ()
                             , mknode "w:rPr" [] $
                               [ mknode "w:color" [("w:val", tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:shd" [("w:val","clear")
                                                ,("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ] ++
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ]
                             ]
        tokStyles = tokenStyles style
        tokFeature f toktype = maybe False f $ M.lookup toktype tokStyles
        tokCol toktype = maybe "auto" (T.pack . drop 1 . fromColor)
                         $ (tokenColor =<< M.lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (T.pack . drop 1 . fromColor)
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
                         maybe [] (\col -> [mknode "w:shd" [("w:val","clear"),("w:fill", T.pack $ drop 1 $ fromColor col)] ()]) (backgroundColor style)
                             ]

copyChildren :: (PandocMonad m)
             => Archive -> Archive -> String -> Integer -> [Text] -> m Entry
copyChildren refArchive distArchive path timestamp elNames = do
  ref  <- parseXml refArchive distArchive path
  dist <- parseXml distArchive distArchive path
  let elsToCopy =
        map cleanElem $ filterChildrenName (\e -> qName e `elem` elNames) ref
  let elsToKeep =
        [e | Elem e <- elContent dist, not (any (hasSameNameAs e) elsToCopy)]
  return $ toEntry path timestamp $ renderXml dist{
      elContent = map Elem elsToKeep ++ map Elem elsToCopy
    }
  where
    hasSameNameAs (Element {elName = n1}) (Element {elName = n2}) =
      qName n1 == qName n2
    cleanElem el@Element{elName=name} = el{elName=name{qURI=Nothing}}

-- this is the lowest number used for a list numId
baseListId :: Int
baseListId = 1000

mkNumbering :: [ListMarker] -> [Element]
mkNumbering lists =
  elts ++ zipWith mkNum lists [baseListId..(baseListId + length lists - 1)]
    where elts = map mkAbstractNum (ordNub lists)

maxListLevel :: Int
maxListLevel = 8

mkNum :: ListMarker -> Int -> Element
mkNum marker numid =
  mknode "w:num" [("w:numId",tshow numid)]
   $ mknode "w:abstractNumId" [("w:val",listMarkerToId marker)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",tshow (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",tshow start)] ())
                [0..maxListLevel]

mkAbstractNum :: ListMarker -> Element
mkAbstractNum marker =
  mknode "w:abstractNum" [("w:abstractNumId",listMarkerToId marker)]
    $ mknode "w:nsid" [("w:val", "A" <> listMarkerToId marker)] ()
    : mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker)
      [0..maxListLevel]

mkLvl :: ListMarker -> Int -> Element
mkLvl marker lvl =
  mknode "w:lvl" [("w:ilvl",tshow lvl)] $
    [ mknode "w:start" [("w:val",start)] ()
      | marker /= NoMarker && marker /= BulletMarker ] ++
    [ mknode "w:numFmt" [("w:val",fmt)] ()
    , mknode "w:lvlText" [("w:val", lvltxt)] ()
    , mknode "w:lvlJc" [("w:val","left")] ()
    , mknode "w:pPr" []
      [ mknode "w:ind" [ ("w:left",tshow $ lvl * step + step)
                       , ("w:hanging",tshow (hang :: Int))
                       ] ()
      ]
    ]
    where (fmt, lvltxt, start) =
            case marker of
                 NoMarker             -> ("bullet"," ","1")
                 BulletMarker         -> ("bullet",bulletFor lvl,"1")
                 NumberMarker st de n -> (styleFor st lvl
                                         ,patternFor de ("%" <> tshow (lvl + 1))
                                         ,tshow n)
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
          patternFor OneParen s  = s <> ")"
          patternFor TwoParens s = "(" <> s <> ")"
          patternFor _ s         = s <> "."

getNumId :: (PandocMonad m) => WS m Int
getNumId = (((baseListId - 1) +) . length) `fmap` gets stLists


makeTOC :: (PandocMonad m) => WriterOptions -> WS m [Element]
makeTOC opts = do
  let depth = "1-" <> tshow (writerTOCDepth opts)
  let tocCmd = "TOC \\o \"" <> depth <> "\" \\h \\z \\u"
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
      mknode "w:sdtContent" [] (title ++ [ Elem $
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
writeOpenXML :: (PandocMonad m)
             => WriterOptions -> Pandoc
             -> WS m ([Content], [Element], [Element])
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
          mknode "w:comment" [("w:" <> k, v) | (k,v) <- kvs]
            [ mknode "w:p" [] $
              map Elem
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
  let meta' = title ++ subtitle ++ authors ++ date ++ abstract ++ map Elem toc
  return (meta' ++ doc', notes', comments')

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: (PandocMonad m) => WriterOptions -> [Block] -> WS m [Content]
blocksToOpenXML opts = fmap concat . mapM (blockToOpenXML opts) . separateTables

-- Word combines adjacent tables unless you put an empty paragraph between
-- them.  See #4315.
separateTables :: [Block] -> [Block]
separateTables [] = []
separateTables (x@Table{}:xs@(Table{}:_)) =
  x : RawBlock (Format "openxml") "<w:p />" : separateTables xs
separateTables (x:xs) = x : separateTables xs

rStyleM :: (PandocMonad m) => CharStyleName -> WS m XML.Element
rStyleM styleName = do
  cStyleMap <- gets (smCharStyle . stStyleMaps)
  let sty' = getStyleIdFromName styleName cStyleMap
  return $ mknode "w:rStyle" [("w:val", fromStyleId sty')] ()

getUniqueId :: (PandocMonad m) => WS m Text
-- the + 20 is to ensure that there are no clashes with the rIds
-- already in word/document.xml.rel
getUniqueId = do
  n <- gets stCurId
  modify $ \st -> st{stCurId = n + 1}
  return $ tshow n

-- | Key for specifying user-defined docx styles.
dynamicStyleKey :: Text
dynamicStyleKey = "custom-style"

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: (PandocMonad m) => WriterOptions -> Block -> WS m [Content]
blockToOpenXML opts blk = withDirection $ blockToOpenXML' opts blk

blockToOpenXML' :: (PandocMonad m) => WriterOptions -> Block -> WS m [Content]
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
                   return $ num ++ [Elem $ mknode "w:r" [] [mknode "w:tab" [] ()]]
                Nothing -> return []
           else return []
  contents <- (number ++) <$> inlinesToOpenXML opts lst
  if T.null ident
     then return [Elem $ mknode "w:p" [] (map Elem paraProps ++ contents)]
     else do
       let bookmarkName = ident
       modify $ \s -> s{ stSectionIds = Set.insert bookmarkName
                                      $ stSectionIds s }
       bookmarkedContents <- wrapBookmark bookmarkName contents
       return [Elem $ mknode "w:p" [] (map Elem paraProps ++ bookmarkedContents)]
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
  fignum <- gets stNextFigureNum
  unless (null alt) $ modify $ \st -> st{ stNextFigureNum = fignum + 1 }
  let figid = "fig" <> tshow fignum
  figname <- translateTerm Term.Figure
  prop <- pStyleM $
        if null alt
        then "Figure"
        else "Captioned Figure"
  paraProps <- local (\env -> env { envParaProperties = EnvProps (Just prop) [] <> envParaProperties env }) (getParaProps False)
  contents <- inlinesToOpenXML opts [Image attr alt (src,tit)]
  captionNode <- if null alt
                    then return []
                    else withParaPropM (pStyleM "Image Caption")
                         $ blockToOpenXML opts
                            (Para $ Span (figid,[],[])
                               [Str (figname <> "\160"),
                                RawInline (Format "openxml")
                                ("<w:fldSimple w:instr=\"SEQ Figure"
                                <> " \\* ARABIC \"><w:r><w:t>"
                                <> tshow fignum
                                <> "</w:t></w:r></w:fldSimple>"),
                                Str ":", Space] : alt)
  return $
    Elem (mknode "w:p" [] (map Elem paraProps ++ contents))
    : captionNode
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
      return [Elem $ mknode "w:p" [] (map Elem paraProps' ++ contents)]
blockToOpenXML' opts (LineBlock lns) = blockToOpenXML opts $ linesToPara lns
blockToOpenXML' _ b@(RawBlock format str)
  | format == Format "openxml" = return [
        Text (CData CDataRaw str Nothing)
      ]
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
  return [ Elem $
    mknode "w:p" [] $ mknode "w:r" [] $ mknode "w:pict" []
    $ mknode "v:rect" [("style","width:0;height:1.5pt"),
                       ("o:hralign","center"),
                       ("o:hrstd","t"),("o:hr","t")] () ]
blockToOpenXML' opts (Table attr caption colspecs thead tbodies tfoot) =
  tableToOpenXML (blocksToOpenXML opts)
                 (Grid.toTable attr caption colspecs thead tbodies tfoot)
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

definitionListItemToOpenXML  :: (PandocMonad m)
                             => WriterOptions -> ([Inline],[[Block]])
                             -> WS m [Content]
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

listItemToOpenXML :: (PandocMonad m)
                  => WriterOptions
                  -> Int -> [Block]
                  -> WS m [Content]
listItemToOpenXML _ _ []                  = return []
listItemToOpenXML opts numid (first:rest) = do
  oldInList <- gets stInList
  modify $ \st -> st{ stInList = True }
  let isListBlock = \case
        BulletList{}  -> True
        OrderedList{} -> True
        _             -> False
  -- Prepend an empty string if the first entry is another
  -- list. Otherwise the outer bullet will disappear.
  let (first', rest') = if isListBlock first
                           then (Plain [Str ""] , first:rest)
                           else (first, rest)
  first'' <- withNumId numid $ blockToOpenXML opts first'
  -- baseListId is the code for no list marker:
  rest''  <- withNumId baseListId $ blocksToOpenXML opts rest'
  modify $ \st -> st{ stInList = oldInList }
  return $ first'' ++ rest''

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: PandocMonad m => WriterOptions -> [Inline] -> WS m [Content]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

withNumId :: (PandocMonad m) => Int -> WS m a -> WS m a
withNumId numid = local $ \env -> env{ envListNumId = numid }

asList :: (PandocMonad m) => WS m a -> WS m a
asList = local $ \env -> env{ envListLevel = envListLevel env + 1 }

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
                [ mknode "w:ilvl" [("w:val",tshow listLevel)] ()
                , mknode "w:numId" [("w:val",tshow numid)] () ] | listLevel >= 0 && not displayMathPara]
  return $ case listPr ++ squashProps props of
                [] -> []
                ps -> [mknode "w:pPr" [] ps]

formattedString :: PandocMonad m => Text -> WS m [Element]
formattedString str =
  -- properly handle soft hyphens
  case splitTextBy (=='\173') str of
      [w] -> formattedString' w
      ws  -> do
         sh <- formattedRun [mknode "w:softHyphen" [] ()]
         intercalate sh <$> mapM formattedString' ws

formattedString' :: PandocMonad m => Text -> WS m [Element]
formattedString' str = do
  inDel <- asks envInDel
  formattedRun [ mktnode (if inDel then "w:delText" else "w:t")
                 [("xml:space","preserve")] (stripInvalidChars str) ]

formattedRun :: PandocMonad m => [Element] -> WS m [Element]
formattedRun els = do
  props <- getTextProps
  return [ mknode "w:r" [] $ props ++ els ]

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: PandocMonad m => WriterOptions -> Inline -> WS m [Content]
inlineToOpenXML opts il = withDirection $ inlineToOpenXML' opts il

inlineToOpenXML' :: PandocMonad m => WriterOptions -> Inline -> WS m [Content]
inlineToOpenXML' _ (Str str) =
  map Elem <$> formattedString str
inlineToOpenXML' opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts SoftBreak = inlineToOpenXML opts (Str " ")
inlineToOpenXML' opts (Span ("",["csl-block"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-left-margin"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-right-inline"],[]) ils) =
   ([Elem $
     mknode "w:r" []
     (mknode "w:t"
       [("xml:space","preserve")]
       ("\t" :: Text))] ++)
    <$> inlinesToOpenXML opts ils
inlineToOpenXML' opts (Span ("",["csl-indent"],[]) ils) =
  inlinesToOpenXML opts ils
inlineToOpenXML' _ (Span (ident,["comment-start"],kvs) ils) = do
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
      kvs' = filter (("id" /=) . fst) kvs
  modify $ \st -> st{ stComments = (("id",ident'):kvs', ils) : stComments st }
  return [ Elem $ mknode "w:commentRangeStart" [("w:id", ident')] () ]
inlineToOpenXML' _ (Span (ident,["comment-end"],kvs) _) =
  -- prefer the "id" in kvs, since that is the one produced by the docx
  -- reader.
  let ident' = fromMaybe ident (lookup "id" kvs)
  in return . map Elem $
     [ mknode "w:commentRangeEnd" [("w:id", ident')] ()
     , mknode "w:r" []
       [ mknode "w:rPr" []
         [ mknode "w:rStyle" [("w:val", "CommentReference")] () ]
       , mknode "w:commentReference" [("w:id", ident')] () ]
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
        let author = fromMaybe defaultAuthor (lookup "author" kvs)
        let mdate = lookup "date" kvs
        return $ ("w:author", author) :
                   maybe [] (\date -> [("w:date", date)]) mdate
  insmod <- if "insertion" `elem` classes
               then do
                 changeAuthorDate <- getChangeAuthorDate
                 insId <- gets stInsId
                 modify $ \s -> s{stInsId = insId + 1}
                 return $ \f -> do
                   x <- f
                   return [Elem $
                           mknode "w:ins"
                             (("w:id", tshow insId) : changeAuthorDate) x]
               else return id
  delmod <- if "deletion" `elem` classes
               then do
                 changeAuthorDate <- getChangeAuthorDate
                 delId <- gets stDelId
                 modify $ \s -> s{stDelId = delId + 1}
                 return $ \f -> local (\env->env{envInDel=True}) $ do
                   x <- f
                   return [Elem $ mknode "w:del"
                             (("w:id", tshow delId) : changeAuthorDate) x]
               else return id
  contents <- insmod $ delmod $ dirmod $ stylemod $ pmod
                     $ inlinesToOpenXML opts ils
  wrapBookmark ident contents
inlineToOpenXML' opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $
  withTextProp (mknode "w:bCs" [] ()) $ -- needed for LTR, #6911
  inlinesToOpenXML opts lst
inlineToOpenXML' opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $
  withTextProp (mknode "w:iCs" [] ()) $  -- needed for LTR, #6911
  inlinesToOpenXML opts lst
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
inlineToOpenXML' _ LineBreak = return [Elem br]
inlineToOpenXML' _ il@(RawInline f str)
  | f == Format "openxml" = return
                            [Text (CData CDataRaw str Nothing)]
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
       Right r -> return [Elem $ fromXLElement r]
       Left il -> inlineToOpenXML' opts il
inlineToOpenXML' opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML' opts (Code attrs str) = do
  let alltoktypes = [KeywordTok ..]
  tokTypesMap <- mapM (\tt -> (,) tt <$> rStyleM (fromString $ show tt)) alltoktypes
  let unhighlighted = (map Elem . intercalate [br]) `fmap`
                       mapM formattedString (T.lines str)
      formatOpenXML _fmtOpts = intercalate [br] . map (map toHlTok)
      toHlTok (toktype,tok) =
        mknode "w:r" []
          [ mknode "w:rPr" [] $
            maybeToList (lookup toktype tokTypesMap)
            , mknode "w:t" [("xml:space","preserve")] tok ]
  withTextPropM (rStyleM "Verbatim Char")
    $ if isNothing (writerHighlightStyle opts)
          then unhighlighted
          else case highlight (writerSyntaxMap opts)
                      formatOpenXML attrs str of
                    Right h  -> return (map Elem h)
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
  let notemarkerXml = RawInline (Format "openxml") $ ppElement notemarker
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
  return [ Elem $ mknode "w:r" []
           [ mknode "w:rPr" [] footnoteStyle
           , mknode "w:footnoteReference" [("w:id", notenum)] () ] ]
-- internal link:
inlineToOpenXML' opts (Link _ txt (T.uncons -> Just ('#', xs),_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  return
    [ Elem $ mknode "w:hyperlink" [("w:anchor", toBookmarkName xs)] contents ]
-- external link:
inlineToOpenXML' opts (Link _ txt (src,_)) = do
  contents <- withTextPropM (rStyleM "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  id' <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              i <- ("rId" <>) <$> getUniqueId
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ Elem $ mknode "w:hyperlink" [("r:id",id')] contents ]
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
                            [("descr",src),("id","0"),("name","Picture")] ()
                        , cNvPicPr ]
        blipFill = mknode "pic:blipFill" []
          [ mknode "a:blip" [("r:embed",T.pack ident)] ()
          , mknode "a:stretch" [] $
              mknode "a:fillRect" [] ()
          ]
        xfrm =    mknode "a:xfrm" []
                        [ mknode "a:off" [("x","0"),("y","0")] ()
                        , mknode "a:ext" [("cx",tshow xemu)
                                         ,("cy",tshow yemu)] () ]
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
              [ mknode "wp:extent" [("cx",tshow xemu),("cy",tshow yemu)] ()
              , mknode "wp:effectExtent"
                [("b","0"),("l","0"),("r","0"),("t","0")] ()
              , mknode "wp:docPr"
                [ ("descr", stringify alt)
                , ("title", title)
                , ("id","1")
                , ("name","Picture")
                ] ()
              , graphic
              ]
      in
        imgElt

  wrapBookmark imgident =<< case stImage of
    Just imgData -> return [Elem $ generateImgElt imgData]
    Nothing -> ( do --try
      (img, mt) <- P.fetchItem src
      ident <- ("rId" <>) <$> getUniqueId

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
            Just Tiff -> ".tiff"
            Nothing   -> ""
        imgpath = "media/" <> ident <> imgext
        mbMimeType = mt <|> getMimeType (T.unpack imgpath)

        imgData = (T.unpack ident, T.unpack imgpath, mbMimeType, img)

      if T.null imgext
         then -- without an extension there is no rule for content type
           inlinesToOpenXML opts alt -- return alt to avoid corrupted docx
         else do
           -- insert mime type to use in constructing [Content_Types].xml
           modify $ \st -> st { stImages = M.insert (T.unpack src) imgData $ stImages st }
           return [Elem $ generateImgElt imgData]
      )
      `catchError` ( \e -> do
        report $ CouldNotFetchResource src $ T.pack (show e)
        -- emit alt text
        inlinesToOpenXML opts alt
      )

br :: Element
br = mknode "w:r" [] [mknode "w:br" [] ()]


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

wrapBookmark :: (PandocMonad m) => Text -> [Content] -> WS m [Content]
wrapBookmark "" contents = return contents
wrapBookmark ident contents = do
  id' <- getUniqueId
  let bookmarkStart = mknode "w:bookmarkStart"
                       [("w:id", id')
                       ,("w:name", toBookmarkName ident)] ()
      bookmarkEnd = mknode "w:bookmarkEnd" [("w:id", id')] ()
  return $ Elem bookmarkStart : contents ++ [Elem bookmarkEnd]

-- Word imposes a 40 character limit on bookmark names and requires
-- that they begin with a letter.  So we just use a hash of the
-- identifier when otherwise we'd have an illegal bookmark name.
toBookmarkName :: Text -> Text
toBookmarkName s
  | Just (c, _) <- T.uncons s
  , isLetter c
  , T.length s <= 40 = s
  | otherwise = T.pack $ 'X' : drop 1 (showDigest (sha1 (fromTextLazy $ TL.fromStrict s)))

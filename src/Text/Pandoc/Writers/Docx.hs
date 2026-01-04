{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012-2025 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Codec.Archive.Zip
    ( Archive(zEntries),
      addEntryToArchive,
      emptyArchive,
      findEntryByPath,
      fromArchive,
      toArchive,
      toEntry,
      Entry(eRelativePath) )
import Control.Monad (MonadPlus(mplus), foldM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State.Strict ( StateT(runStateT) )
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrd)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String (fromString)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Skylighting
import Text.Pandoc.Class (PandocMonad, toLang)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Data (readDataFile, readDefaultDataFile)
import Data.Time
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Highlighting (defaultStyle)
import Text.Pandoc.MIME (getMimeTypeDef)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Docx.Parse (extractTarget)
import Text.Pandoc.Writers.Docx.StyleMap
import Text.Pandoc.Writers.Docx.Types
import Text.Pandoc.Writers.Docx.OpenXML (writeOpenXML, maxListLevel)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Writers.OOXML
import Text.Pandoc.XML.Light as XML
import Data.Generics (mkT, everywhere)
import Text.Collate.Lang (renderLang, Lang(..))

writeDocx :: (PandocMonad m)
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m BL.ByteString
writeDocx opts doc = do
  let Pandoc meta blocks = walk fixDisplayMath doc
  setupTranslations meta
  let blocks' = makeSectionsWithOffsets (writerNumberOffset opts)
                   True Nothing blocks
  let doc' = Pandoc meta blocks'

  username <- P.lookupEnv "USERNAME"
  utctime <- P.getTimestamp
  oldUserDataDir <- P.getUserDataDir
  P.setUserDataDir Nothing
  res <- readDefaultDataFile "reference.docx"
  P.setUserDataDir oldUserDataDir
  let distArchive = toArchive $ BL.fromStrict res
  refArchive <- case writerReferenceDoc opts of
                     Just f  -> toArchive . BL.fromStrict . fst
                                   <$> P.fetchItem (T.pack f)
                     Nothing -> toArchive . BL.fromStrict <$>
                          readDataFile "reference.docx"

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
                  Just l  -> everywhere (mkT (go l))
        where
          go :: Lang -> Element -> Element
          go lang e'
           | qName (elName e') == "lang"
             = if isEastAsianLang lang
                  then e'{ elAttribs =
                             map (setattr "eastAsia" (renderLang lang)) $
                             elAttribs e' }
                  else
                    if isBidiLang lang
                       then e'{ elAttribs =
                                 map (setattr "bidi" (renderLang lang)) $
                                 elAttribs e' }
                       else e'{ elAttribs =
                                 map (setattr "val" (renderLang lang)) $
                                 elAttribs e' }
           | otherwise = e'

          setattr attrname l (XML.Attr qn@(QName s _ _) _)
            | s == attrname  = XML.Attr qn l
          setattr _ _ x      = x

          isEastAsianLang Lang{ langLanguage = lang } =
             lang == "zh" || lang == "ja" || lang == "ko"
          isBidiLang Lang{ langLanguage = lang } =
             lang == "he" || lang == "ar"

  let stylepath = "word/styles.xml"
  styledoc <- addLang <$> parseXml refArchive distArchive stylepath

  -- parse styledoc for heading styles
  let styleMaps = getStyleMaps refArchive

  let tocTitle = case lookupMetaInlines "toc-title" meta of
                   [] -> stTocTitle defaultWriterState
                   ls -> ls

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

  let isImageNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"
  let isHeaderNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/header"
  let isFooterNode e = findAttr (QName "Type" Nothing Nothing) e == Just "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
  parsedRels <- filterElements
                  (\e -> isImageNode e || isHeaderNode e || isFooterNode e)
              <$> parseXml refArchive distArchive "word/_rels/document.xml.rels"
  let getRelId e =
        case findAttr (QName "Id" Nothing Nothing) e of
          Just ident -> T.stripPrefix "rId" ident >>= safeRead
          Nothing -> Nothing
  let relIds = mapMaybe getRelId parsedRels
  let maxRelId = if null relIds then 0 else maximum relIds

  let headers = filter isHeaderNode parsedRels
  let footers = filter isFooterNode parsedRels
  -- word/_rels/document.xml.rels
  let addBaseRel (url', target') (maxId, rels) =
        case [e | e <- rels
                , findAttr (QName "Target" Nothing Nothing) e ==
                   Just target'] of
          [] -> (maxId + 1, mknode "Relationship"
                            [("Type",url')
                            ,("Id","rId" <> tshow (maxId + 1))
                            ,("Target",target')] () : rels)
          _ -> (maxId, rels)

  let (newMaxRelId, baserels) = foldr addBaseRel (maxRelId, parsedRels)
                    [("http://schemas.openxmlformats.org/officeDocument/2006/relationships/numbering",
                      "numbering.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles",
                      "styles.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/settings",
                      "settings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/webSettings",
                      "webSettings.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/fontTable",
                      "fontTable.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme",
                      "theme/theme1.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/footnotes",
                      "footnotes.xml")
                    ,("http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments",
                      "comments.xml")
                    ]

  let initialSt = defaultWriterState {
          stStyleMaps  = styleMaps
        , stTocTitle   = tocTitle
        , stCurId      = newMaxRelId + 1
        }

  -- adjust contents to add sectPr from reference.docx
  let sectpr = case mbsectpr of
        Just sectpr' -> add_attrs (elAttribs sectpr') $ mknode "w:sectPr" []
                             (elChildren sectpr')
        Nothing      -> mknode "w:sectPr" []
                          [ mknode "w:footnotePr" []
                            [ mknode "w:numRestart" [("w:val","eachSect")] () ]
                          ]

  ((contents, footnotes, comments), st) <- runStateT
                         (runReaderT
                          (writeOpenXML opts{ writerWrapText = WrapNone }
                                        doc')
                          env{ envSectPr = Just sectpr })
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
                        , "word/media/" `isPrefixOf` eRelativePath e
                        , not ("/" `isSuffixOf` eRelativePath e) ]

  let mkDefaultNode (ext, mt) =
        mknode "Default" [("Extension",ext),("ContentType",mt)] ()
  let defaultnodes = map mkDefaultNode
        [("xml", "application/xml"),
         ("rels", "application/vnd.openxmlformats-package.relationships+xml"),
         ("odttf",
           "application/vnd.openxmlformats-officedocument.obfuscatedFont")]
  let contentTypesDoc = mknode "Types" [("xmlns","http://schemas.openxmlformats.org/package/2006/content-types")] $ defaultnodes ++ overrides
  let contentTypesEntry = toEntry "[Content_Types].xml" epochtime
        $ renderXml contentTypesDoc

  let toImgRel (ident,path,_,_) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",T.pack ident),("Target",T.pack path)] ()
  let imgrels = map toImgRel imgs
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let linkrels = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc = mknode "Relationships" [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")] $ baserels ++ imgrels ++ linkrels
  let relEntry = toEntry "word/_rels/document.xml.rels" epochtime
        $ renderXml reldoc

  let contents' = BL.fromStrict $ UTF8.fromText contents

  -- word/document.xml
  let contentEntry = toEntry "word/document.xml" epochtime contents'

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
                  (case writerHighlightMethod opts of
                     Skylighting sty -> styleToOpenXml styleMaps sty
                     DefaultHighlighting -> styleToOpenXml styleMaps
                                              defaultStyle
                     _ -> [])
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

  settingsEntry <- copyChildren refArchive distArchive settingsPath epochtime
                      -- note: these must go in the following order:
                     [ "writeProtection"
                     , "view"
                     , "zoom"
                     , "removePersonalInformation"
                     , "removeDateAndTime"
                     , "doNotDisplayPageBoundaries"
                     , "displayBackgroundShape"
                     , "printPostScriptOverText"
                     , "printFractionalCharacterWidth"
                     , "printFormsData"
                     , "embedTrueTypeFonts"
                     , "embedSystemFonts"
                     , "saveSubsetFonts"
                     , "saveFormsData"
                     , "mirrorMargins"
                     , "alignBordersAndEdges"
                     , "bordersDoNotSurroundHeader"
                     , "bordersDoNotSurroundFooter"
                     , "gutterAtTop"
                     , "hideSpellingErrors"
                     , "hideGrammaticalErrors"
                     , "activeWritingStyle"
                     , "proofState"
                     , "formsDesign"
                     , "attachedTemplate"
                     , "linkStyles"
                     , "stylePaneFormatFilter"
                     , "stylePaneSortMethod"
                     , "documentType"
                     , "mailMerge"
                     , "revisionView"
                     , "trackRevisions"
                     , "doNotTrackMoves"
                     , "doNotTrackFormatting"
                     , "documentProtection"
                     , "autoFormatOverride"
                     , "styleLockTheme"
                     , "styleLockQFSet"
                     , "defaultTabStop"
                     , "autoHyphenation"
                     , "consecutiveHyphenLimit"
                     , "hyphenationZone"
                     , "doNotHyphenateCaps"
                     , "showEnvelope"
                     , "summaryLength"
                     , "clickAndTypeStyle"
                     , "defaultTableStyle"
                     , "evenAndOddHeaders"
                     , "bookFoldRevPrinting"
                     , "bookFoldPrinting"
                     , "bookFoldPrintingSheets"
                     , "drawingGridHorizontalSpacing"
                     , "drawingGridVerticalSpacing"
                     , "displayHorizontalDrawingGridEvery"
                     , "displayVerticalDrawingGridEvery"
                     , "doNotUseMarginsForDrawingGridOrigin"
                     , "drawingGridHorizontalOrigin"
                     , "drawingGridVerticalOrigin"
                     , "doNotShadeFormData"
                     , "noPunctuationKerning"
                     , "characterSpacingControl"
                     , "printTwoOnOne"
                     , "strictFirstAndLastChars"
                     , "noLineBreaksAfter"
                     , "noLineBreaksBefore"
                     , "savePreviewPicture"
                     , "doNotValidateAgainstSchema"
                     , "saveInvalidXml"
                     , "ignoreMixedContent"
                     , "alwaysShowPlaceholderText"
                     , "doNotDemarcateInvalidXml"
                     , "saveXmlDataOnly"
                     , "useXSLTWhenSaving"
                     , "saveThroughXslt"
                     , "showXMLTags"
                     , "alwaysMergeEmptyNamespace"
                     , "updateFields"
                     , "hdrShapeDefaults"
                     -- , "footnotePr" -- this can cause problems, see #9522
                     -- , "endnotePr"
                     , "compat"
                     , "docVars"
                     , "rsids"
                     , "attachedSchema"
                     , "themeFontLang"
                     , "clrSchemeMapping"
                     , "doNotIncludeSubdocsInStats"
                     , "doNotAutoCompressPictures"
                     , "forceUpgrade"
                     , "captions"
                     , "readModeInkLockDown"
                     , "smartTagType"
                     , "shapeDefaults"
                     , "doNotEmbedSmartTags"
                     , "decimalSymbol"
                     , "listSeparator" ]

  let entryFromArchive arch path =
         maybe (throwError $ PandocSomeError
                           $ T.pack $ path ++ " missing in reference docx")
               return
               (findEntryByPath path arch `mplus` findEntryByPath path distArchive)
  docPropsAppEntry <- entryFromArchive refArchive "docProps/app.xml"
  themeEntry <- entryFromArchive refArchive "word/theme/theme1.xml"
  fontTableEntry <- entryFromArchive refArchive "word/fontTable.xml"
  let fontTableRelsEntries = maybeToList $
       findEntryByPath "word/_rels/fontTable.xml.rels" refArchive
  let fontEntries = [entry | entry <- zEntries refArchive
                           , "word/fonts/" `isPrefixOf` (eRelativePath entry)]
                        -- or parse fontTable.xml.rels?
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
                  settingsEntry : webSettingsEntry :
                  fontTableEntry :
                  fontTableRelsEntries ++ fontEntries ++
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
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:color" [("w:val", tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ] ++
                               [ mknode "w:shd" [("w:val","clear")
                                                ,("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ]
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
  els <- foldM (addEl ref dist) [] (reverse elNames)
  return $ toEntry path timestamp
         $ renderXml dist{ elContent = map cleanElem els }
  where
    addEl ref dist els name =
      case filterChildName (hasName name) ref `mplus`
             filterChildName (hasName name) dist of
        Just el -> pure (el : els)
        Nothing -> pure els
    hasName name = (== name) . qName
    cleanElem el@Element{elName=name} = Elem el{elName=name{qURI=Nothing}}

-- this is the lowest number used for a list numId
baseListId :: Int
baseListId = 1000

mkNumbering :: [ListMarker] -> [Element]
mkNumbering lists =
  elts ++ zipWith mkNum lists [baseListId..(baseListId + length lists - 1)]
    where elts = map mkAbstractNum (nubOrd lists)

mkNum :: ListMarker -> Int -> Element
mkNum marker numid =
  mknode "w:num" [("w:numId",tshow numid)]
   $ mknode "w:abstractNumId" [("w:val",listMarkerToId marker)] ()
   : case marker of
       NoMarker     -> []
       BulletMarker -> []
       CheckboxMarker _ -> []
       NumberMarker _ _ start ->
          map (\lvl -> mknode "w:lvlOverride" [("w:ilvl",tshow (lvl :: Int))]
              $ mknode "w:startOverride" [("w:val",tshow start)] ())
                [0..maxListLevel]

mkAbstractNum :: ListMarker -> Element
mkAbstractNum marker =
  mknode "w:abstractNum" [("w:abstractNumId",listMarkerToId marker)]
    $ mknode "w:nsid" [("w:val", T.justifyRight 8 '0' ("A" <> listMarkerToId marker))] ()
    : mknode "w:multiLevelType" [("w:val","multilevel")] ()
    : map (mkLvl marker)
      [0..maxListLevel]

mkLvl :: ListMarker -> Int -> Element
mkLvl marker lvl =
  mknode "w:lvl" [("w:ilvl",tshow lvl)] $
    (case marker of
        NumberMarker{} -> [mknode "w:start" [("w:val",start)] ()]
        _ -> []) ++
    [ mknode "w:numFmt" [("w:val",fmt)] ()
    , mknode "w:lvlText" [("w:val", lvltxt)] ()
    , mknode "w:lvlJc" [("w:val","left")] ()
    , mknode "w:pPr" [] $
        mknode "w:ind" [ ("w:left",tshow $ lvl * step + step)
                       , ("w:hanging",tshow hang)
                       ] ()
    ] ++
    maybe [] (\font ->
                [ mknode "w:rPr" []
                  [ mknode "w:rFonts" [ ("w:ascii", font)
                                      , ("w:hAnsi", font)
                                      , ("w:cs", font)
                                      , ("w:hint", "default") ] () ]]) mbfont
    where (fmt, lvltxt, mbfont, start) =
            case marker of
                 NoMarker             -> ("bullet"," ", Nothing, "1")
                 BulletMarker         -> bulletFor lvl
                 CheckboxMarker False -> ("bullet","\9744", Nothing, "1")
                 CheckboxMarker True  -> ("bullet","\9746", Nothing, "1")
                 NumberMarker st de n -> (styleFor st lvl
                                         ,patternFor de ("%" <> tshow (lvl + 1))
                                         ,Nothing
                                         ,tshow n)
          step = 720
          hang :: Int
          hang = 360
          bulletFor 0 = ("bullet", "\xf0b7", Just "Symbol", "1") -- filled circle
          bulletFor 1 = ("bullet", "o", Just "Courier New", "1") -- open o
          bulletFor 2 = ("bullet", "\xf0a7", Just "Wingdings", "1")  -- closed box
          bulletFor x = bulletFor (x `mod` 3)
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

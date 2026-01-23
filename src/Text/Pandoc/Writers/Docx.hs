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
import qualified Data.ByteString as B
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
import Text.Pandoc.MIME (MimeType, getMimeTypeDef)
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
import Text.Collate.Lang (renderLang, Lang(..))

writeDocx :: (PandocMonad m)
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m BL.ByteString
writeDocx opts doc = do
  -- Phase 1: Document preprocessing
  let Pandoc meta blocks = walk fixDisplayMath doc
  setupTranslations meta
  let blocks' = makeSectionsWithOffsets (writerNumberOffset opts)
                   True Nothing blocks
  let doc' = Pandoc meta blocks'

  -- Phase 2: Archive loading
  (refArchive, distArchive, username, utctime) <- loadArchives opts
  let epochtime = floor $ utcTimeToPOSIXSeconds utctime

  -- Phase 3: Page layout extraction
  (mbsectpr, pgContentWidth) <- extractPageLayout refArchive distArchive

  -- Phase 4: Language & style setup
  mblang <- toLang $ getLang opts meta
  let addLang = mkLangTransformer mblang
  styledoc <- addLang <$> parseXml refArchive distArchive "word/styles.xml"
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

  -- Phase 5: Relationship extraction
  (baserels, headers, footers, newMaxRelId) <- extractRelationships refArchive distArchive

  let initialSt = defaultWriterState {
          stStyleMaps  = styleMaps
        , stTocTitle   = tocTitle
        , stCurId      = newMaxRelId + 1
        }

  -- Phase 6: Core content generation
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
  let imgs = M.elems $ stImages st

  -- Phase 7: XML document construction
  -- We create [Content_Types].xml and word/_rels/document.xml.rels
  -- from scratch rather than reading from reference.docx,
  -- because Word sometimes changes these files when a reference.docx is modified,
  -- e.g. deleting the reference to footnotes.xml or removing default entries
  -- for image content types.
  let contentTypesEntry = mkContentTypesEntry epochtime imgs headers footers refArchive
  let relEntry = mkDocumentRelsEntry epochtime baserels imgs (stExternalLinks st)
  let contentEntry = toEntry "word/document.xml" epochtime
                       (BL.fromStrict $ UTF8.fromText contents)
  let footnotesEntry = mkFootnotesEntry epochtime footnotes
  let footnoteRelEntry = mkFootnoteRelsEntry epochtime (stExternalLinks st)
  let commentsEntry = mkCommentsEntry epochtime comments
  let styleEntry = mkStylesEntry epochtime styledoc styleMaps st opts
  numEntry <- mkNumberingEntry refArchive distArchive epochtime (stLists st)
  let docPropsEntry = mkCorePropsEntry epochtime utctime meta
  let customPropsEntry = mkCustomPropsEntry epochtime meta
  let relsEntry = mkPackageRelsEntry epochtime

  -- we use dist archive for settings.xml, because Word sometimes
  -- adds references to footnotes or endnotes we don't have...
  -- we do, however, copy some settings over from reference
  settingsEntry <- copyChildren refArchive distArchive "word/settings.xml"
                     epochtime settingsElementNames

  -- Phase 8: Archive assembly
  let toImageEntry (_, path, _, img) = toEntry ("word/" ++ path) epochtime $ toLazy img
  let imageEntries = map toImageEntry imgs

  refEntries <- collectReferenceEntries refArchive distArchive headers footers

  let archive = foldr addEntryToArchive emptyArchive $
                  contentTypesEntry : relsEntry : contentEntry : relEntry :
                  footnoteRelEntry : numEntry : styleEntry : footnotesEntry :
                  commentsEntry :
                  docPropsEntry : customPropsEntry :
                  settingsEntry :
                  imageEntries ++ refEntries
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

-- | Standard XML namespace attributes for docx elements
stdAttributes :: [(Text, Text)]
stdAttributes =
  [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
  ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
  ,("xmlns:o","urn:schemas-microsoft-com:office:office")
  ,("xmlns:v","urn:schemas-microsoft-com:vml")
  ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
  ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
  ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
  ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")]

-- | Settings elements to copy from reference.docx (order matters)
settingsElementNames :: [Text]
settingsElementNames =
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

-- | Build language transformer function for modifying XML elements.
-- Navigates directly to w:docDefaults/w:rPr/w:lang instead of generic traversal.
mkLangTransformer :: Maybe Lang -> (Element -> Element)
mkLangTransformer Nothing  = id
mkLangTransformer (Just lang) = modifyAtPath path updateLangAttrs
  where
    -- Path is: w:docDefaults / w:rPrDefault / w:rPr / w:lang
    path = [named "docDefaults", named "rPrDefault", named "rPr", named "lang"]
    named n = (== n) . qName

    updateLangAttrs e
      | isEastAsianLang lang = e{ elAttribs = map (setattr "eastAsia") $ elAttribs e }
      | isBidiLang lang      = e{ elAttribs = map (setattr "bidi") $ elAttribs e }
      | otherwise            = e{ elAttribs = map (setattr "val") $ elAttribs e }

    setattr attrname (XML.Attr qn@(QName s _ _) _)
      | s == attrname  = XML.Attr qn (renderLang lang)
    setattr _ x        = x

    isEastAsianLang Lang{ langLanguage = l } = l == "zh" || l == "ja" || l == "ko"
    isBidiLang Lang{ langLanguage = l } = l == "he" || l == "ar"

-- | Modify an element at a specific path in the XML tree.
-- The path is a list of predicates that match element names at each level.
modifyAtPath :: [(QName -> Bool)] -> (Element -> Element) -> Element -> Element
modifyAtPath [] f e = f e
modifyAtPath (p:ps) f e = e{ elContent = map go (elContent e) }
  where
    go (Elem el) | p (elName el) = Elem (modifyAtPath ps f el)
    go c = c

-- | Load reference and distribution archives
loadArchives :: PandocMonad m
             => WriterOptions
             -> m (Archive, Archive, Maybe Text, UTCTime)
loadArchives opts = do
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
  return (refArchive, distArchive, username, utctime)

-- | Extract page dimensions from template
extractPageLayout :: PandocMonad m
                  => Archive -> Archive -> m (Maybe Element, Maybe Integer)
extractPageLayout refArchive distArchive = do
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

  return (mbsectpr, pgContentWidth)

-- | Parse and augment relationships from reference.docx
extractRelationships :: PandocMonad m
                     => Archive -> Archive
                     -> m ([Element], [Element], [Element], Int)
extractRelationships refArchive distArchive = do
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

  return (baserels, headers, footers, newMaxRelId)

-- | Create footnotes XML entry
mkFootnotesEntry :: Integer -> [Element] -> Entry
mkFootnotesEntry epochtime footnotes =
  let notes = mknode "w:footnotes" stdAttributes footnotes
  in toEntry "word/footnotes.xml" epochtime $ renderXml notes

-- | Create footnote relationships entry
mkFootnoteRelsEntry :: Integer -> M.Map Text Text -> Entry
mkFootnoteRelsEntry epochtime externalLinks =
  let linkrels = map toLinkRel $ M.toList externalLinks
      toLinkRel (src, ident) = mknode "Relationship"
        [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
        ,("Id",ident)
        ,("Target",src)
        ,("TargetMode","External")] ()
  in toEntry "word/_rels/footnotes.xml.rels" epochtime
       $ renderXml $ mknode "Relationships"
           [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")]
           linkrels

-- | Create comments XML entry
mkCommentsEntry :: Integer -> [Element] -> Entry
mkCommentsEntry epochtime comments =
  toEntry "word/comments.xml" epochtime
    $ renderXml $ mknode "w:comments" stdAttributes comments

-- | Create package-level relationships entry
mkPackageRelsEntry :: Integer -> Entry
mkPackageRelsEntry epochtime =
  let rels = mknode "Relationships"
        [("xmlns", "http://schemas.openxmlformats.org/package/2006/relationships")]
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
  in toEntry "_rels/.rels" epochtime $ renderXml rels

-- | Create content types manifest entry
mkContentTypesEntry :: Integer
                    -> [(String, String, Maybe MimeType, B.ByteString)]  -- imgs
                    -> [Element]  -- headers
                    -> [Element]  -- footers
                    -> Archive    -- refArchive
                    -> Entry
mkContentTypesEntry epochtime imgs headers footers refArchive =
  let mkOverrideNode (part', contentType') = mknode "Override"
           [("PartName", T.pack part')
           ,("ContentType", contentType')] ()
      mkImageOverride (_, imgpath, mbMimeType, _) =
          mkOverrideNode ("/word/" <> imgpath,
                          fromMaybe "application/octet-stream" mbMimeType)
      mkMediaOverride imgpath =
          mkOverrideNode ("/" <> imgpath, getMimeTypeDef imgpath)
      overrides = map mkOverrideNode (
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
      mkDefaultNode (ext, mt) =
        mknode "Default" [("Extension",ext),("ContentType",mt)] ()
      defaultnodes = map mkDefaultNode
        [("xml", "application/xml"),
         ("rels", "application/vnd.openxmlformats-package.relationships+xml"),
         ("odttf",
           "application/vnd.openxmlformats-officedocument.obfuscatedFont")]
      contentTypesDoc = mknode "Types"
        [("xmlns","http://schemas.openxmlformats.org/package/2006/content-types")]
        $ defaultnodes ++ overrides
  in toEntry "[Content_Types].xml" epochtime $ renderXml contentTypesDoc

-- | Create document relationships entry
mkDocumentRelsEntry :: Integer
                    -> [Element]  -- baserels
                    -> [(String, String, Maybe MimeType, B.ByteString)]  -- imgs
                    -> M.Map Text Text  -- externalLinks
                    -> Entry
mkDocumentRelsEntry epochtime baserels imgs externalLinks =
  let toImgRel (ident, path, _, _) = mknode "Relationship"
        [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image")
        ,("Id",T.pack ident)
        ,("Target",T.pack path)] ()
      imgrels = map toImgRel imgs
      toLinkRel (src, ident) = mknode "Relationship"
        [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink")
        ,("Id",ident)
        ,("Target",src)
        ,("TargetMode","External")] ()
      linkrels = map toLinkRel $ M.toList externalLinks
      reldoc = mknode "Relationships"
        [("xmlns","http://schemas.openxmlformats.org/package/2006/relationships")]
        $ baserels ++ imgrels ++ linkrels
  in toEntry "word/_rels/document.xml.rels" epochtime $ renderXml reldoc

-- | Create styles entry with dynamic additions
mkStylesEntry :: Integer -> Element -> StyleMaps -> WriterState -> WriterOptions -> Entry
mkStylesEntry epochtime styledoc styleMaps st opts =
  let stylepath = "word/styles.xml"
      -- We only want to inject paragraph and text properties that
      -- are not already in the style map. Note that keys in the stylemap
      -- are normalized as lowercase.
      newDynamicParaProps = filter
        (\sty -> not $ hasStyleName sty $ smParaStyle styleMaps)
        (Set.toList $ stDynamicParaProps st)

      newDynamicTextProps = filter
        (\sty -> not $ hasStyleName sty $ smCharStyle styleMaps)
        (Set.toList $ stDynamicTextProps st)

      newstyles = map newParaPropToOpenXml newDynamicParaProps ++
                  map newTextPropToOpenXml newDynamicTextProps ++
                  (case writerHighlightMethod opts of
                     Skylighting sty -> styleToOpenXml styleMaps sty
                     DefaultHighlighting -> styleToOpenXml styleMaps
                                              defaultStyle
                     _ -> [])
      styledoc' = styledoc{ elContent = elContent styledoc ++
                                           map Elem newstyles }
  in toEntry stylepath epochtime $ renderXml styledoc'

-- | Create core document properties entry
mkCorePropsEntry :: Integer -> UTCTime -> Meta -> Entry
mkCorePropsEntry epochtime utctime meta =
  let keywords = case lookupMeta "keywords" meta of
                       Just (MetaList xs) -> map stringify xs
                       _                  -> []
      docPropsPath = "docProps/core.xml"
      extraCoreProps = ["subject","lang","category","description"]
      extraCorePropsMap = M.fromList $ zip extraCoreProps
                       ["dc:subject","dc:language","cp:category","dc:description"]
      lookupMetaString' :: Text -> Meta -> Text
      lookupMetaString' key' meta' =
        case key' of
             "description" -> T.intercalate "_x000d_\n"
                                (map stringify $ lookupMetaBlocks "description" meta')
             key''         -> lookupMetaString key'' meta'

      docProps = mknode "cp:coreProperties"
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
  in toEntry docPropsPath epochtime $ renderXml docProps

-- | Create custom document properties entry
mkCustomPropsEntry :: Integer -> Meta -> Entry
mkCustomPropsEntry epochtime meta =
  let extraCoreProps = ["subject","lang","category","description"]
      customProperties :: [(Text, Text)]
      customProperties = [ (k, lookupMetaString k meta)
                         | k <- M.keys (unMeta meta)
                         , k `notElem` (["title", "author", "keywords"]
                                       ++ extraCoreProps)]
      mkCustomProp (k, v) pid = mknode "property"
         [("fmtid","{D5CDD505-2E9C-101B-9397-08002B2CF9AE}")
         ,("pid", tshow pid)
         ,("name", k)] $ mknode "vt:lpwstr" [] v
      customPropsPath = "docProps/custom.xml"
      customProps = mknode "Properties"
          [("xmlns","http://schemas.openxmlformats.org/officeDocument/2006/custom-properties")
          ,("xmlns:vt","http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes")
          ] $ zipWith mkCustomProp customProperties [(2 :: Int)..]
  in toEntry customPropsPath epochtime $ renderXml customProps

-- | Create numbering entry
mkNumberingEntry :: PandocMonad m
                 => Archive -> Archive -> Integer -> [ListMarker] -> m Entry
mkNumberingEntry refArchive distArchive epochtime lists = do
  let numpath = "word/numbering.xml"
  numbering <- parseXml refArchive distArchive numpath
  let newNumElts = mkNumbering lists
  let pandocAdded e =
       case findAttrBy ((== "abstractNumId") . qName) e >>= safeRead of
         Just numid -> numid >= (990 :: Int)
         Nothing    ->
           case findAttrBy ((== "numId") . qName) e >>= safeRead of
             Just numid -> numid >= (1000 :: Int)
             Nothing    -> False
  let oldElts = filter (not . pandocAdded) $ onlyElems (elContent numbering)
  let allElts = oldElts ++ newNumElts
  return $ toEntry numpath epochtime $ renderXml numbering{ elContent =
                       -- we want all the abstractNums first, then the nums,
                       -- otherwise things break:
                       [Elem e | e <- allElts
                               , qName (elName e) == "abstractNum" ] ++
                       [Elem e | e <- allElts
                               , qName (elName e) == "num" ] }

-- | Collect auxiliary entries from reference archive
collectReferenceEntries :: PandocMonad m
                        => Archive -> Archive -> [Element] -> [Element]
                        -> m [Entry]
collectReferenceEntries refArchive distArchive headers footers = do
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
  return $ docPropsAppEntry : themeEntry : fontTableEntry : webSettingsEntry
         : fontTableRelsEntries ++ fontEntries ++ headerFooterEntries
         ++ miscRelEntries ++ otherMediaEntries

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.EPUB
   Copyright   : Copyright (C) 2010-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to EPUB.
-}
module Text.Pandoc.Writers.EPUB ( writeEPUB2, writeEPUB3 ) where
import Codec.Archive.Zip (Entry, addEntryToArchive, eRelativePath, emptyArchive,
                          fromArchive, fromEntry, toEntry)
import Control.Applicative ( (<|>) )
import Control.Monad (mplus, unless, when, zipWithM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Strict (StateT, evalState, evalStateT, get,
                                   gets, lift, modify)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Char (isAlphaNum, isAscii, isDigit, toLower)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Network.HTTP (urlEncode)
import System.FilePath (takeExtension, takeFileName, makeRelative)
import Text.HTML.TagSoup (Tag (TagOpen), fromAttrib, parseTags)
import Text.Pandoc.Builder (fromList, setMeta)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import qualified Text.Pandoc.Class.PandocPure as P
import qualified Text.Pandoc.Class.PandocMonad as P
import Data.Time
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType)
import Text.Pandoc.Options (EPUBVersion (..), HTMLMathMethod (..),
                            ObfuscationMethod (NoObfuscation), WrapOption (..),
                            WriterOptions (..))
import Text.Pandoc.Shared (makeSections, normalizeDate, renderTags',
                           safeRead, stringify, trim, uniqueIdent, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.UUID (getRandomUUID)
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Writers.HTML (writeHtmlStringForEPUB)
import Text.Printf (printf)
import Text.XML.Light (Attr (..), Element (..), Node (..), QName (..),
                       add_attrs, lookupAttr, node, onlyElems, parseXML,
                       ppElement, showElement, strContent, unode, unqual)
import Text.Pandoc.XML (escapeStringForXML)
import Text.DocTemplates (FromContext(lookupContext), Context(..),
                          ToContext(toVal), Val(..))

-- A Chapter includes a list of blocks.
data Chapter = Chapter [Block]
  deriving (Show)

data EPUBState = EPUBState {
        stMediaPaths  :: [(FilePath, (FilePath, Maybe Entry))]
      , stMediaNextId :: Int
      , stEpubSubdir  :: String
      }

type E m = StateT EPUBState m

data EPUBMetadata = EPUBMetadata{
    epubIdentifier    :: [Identifier]
  , epubTitle         :: [Title]
  , epubDate          :: [Date]
  , epubLanguage      :: String
  , epubCreator       :: [Creator]
  , epubContributor   :: [Creator]
  , epubSubject       :: [String]
  , epubDescription   :: Maybe String
  , epubType          :: Maybe String
  , epubFormat        :: Maybe String
  , epubPublisher     :: Maybe String
  , epubSource        :: Maybe String
  , epubRelation      :: Maybe String
  , epubCoverage      :: Maybe String
  , epubRights        :: Maybe String
  , epubCoverImage    :: Maybe String
  , epubStylesheets   :: [FilePath]
  , epubPageDirection :: Maybe ProgressionDirection
  , epubIbooksFields  :: [(String, String)]
  , epubCalibreFields :: [(String, String)]
  } deriving Show

data Date = Date{
    dateText  :: String
  , dateEvent :: Maybe String
  } deriving Show

data Creator = Creator{
    creatorText   :: String
  , creatorRole   :: Maybe String
  , creatorFileAs :: Maybe String
  } deriving Show

data Identifier = Identifier{
    identifierText   :: String
  , identifierScheme :: Maybe String
  } deriving Show

data Title = Title{
    titleText   :: String
  , titleFileAs :: Maybe String
  , titleType   :: Maybe String
  } deriving Show

data ProgressionDirection = LTR | RTL deriving Show

dcName :: String -> QName
dcName n = QName n Nothing (Just "dc")

dcNode :: Node t => String -> t -> Element
dcNode = node . dcName

opfName :: String -> QName
opfName n = QName n Nothing (Just "opf")

toId :: FilePath -> String
toId = map (\x -> if isAlphaNum x || x == '-' || x == '_'
                     then x
                     else '_') . takeFileName

removeNote :: Inline -> Inline
removeNote (Note _) = Str ""
removeNote x        = x

toVal' :: String -> Val TS.Text
toVal' = toVal . TS.pack

mkEntry :: PandocMonad m => FilePath -> B.ByteString -> E m Entry
mkEntry path content = do
  epubSubdir <- gets stEpubSubdir
  let addEpubSubdir :: Entry -> Entry
      addEpubSubdir e = e{ eRelativePath =
          (if null epubSubdir
              then ""
              else epubSubdir ++ "/") ++ eRelativePath e }
  epochtime <- floor <$> lift P.getPOSIXTime
  return $
       (if path == "mimetype" || "META-INF" `isPrefixOf` path
           then id
           else addEpubSubdir) $ toEntry path epochtime content

getEPUBMetadata :: PandocMonad m => WriterOptions -> Meta -> E m EPUBMetadata
getEPUBMetadata opts meta = do
  let md = metadataFromMeta opts meta
  let elts = maybe [] (onlyElems . parseXML) $ writerEpubMetadata opts
  let md' = foldr addMetadataFromXML md elts
  let addIdentifier m =
       if null (epubIdentifier m)
          then do
            randomId <- getRandomUUID
            return $ m{ epubIdentifier = [Identifier (show randomId) Nothing] }
          else return m
  let addLanguage m =
       if null (epubLanguage m)
          then case lookupContext "lang" (writerVariables opts) of
                     Just x  -> return m{ epubLanguage = TS.unpack x }
                     Nothing -> do
                       mLang <- lift $ P.lookupEnv "LANG"
                       let localeLang =
                             case mLang of
                               Just lang ->
                                 TS.map (\c -> if c == '_' then '-' else c) $
                                 TS.takeWhile (/='.') lang
                               Nothing -> "en-US"
                       return m{ epubLanguage = TS.unpack localeLang }
          else return m
  let fixDate m =
       if null (epubDate m)
          then do
            currentTime <- lift P.getCurrentTime
            return $ m{ epubDate = [ Date{
                             dateText = showDateTimeISO8601 currentTime
                           , dateEvent = Nothing } ] }
          else return m
  let addAuthor m =
       if any (\c -> creatorRole c == Just "aut") $ epubCreator m
          then return m
          else do
            let authors' = map stringify $ docAuthors meta
            let toAuthor name = Creator{ creatorText = TS.unpack name
                                       , creatorRole = Just "aut"
                                       , creatorFileAs = Nothing }
            return $ m{ epubCreator = map toAuthor authors' ++ epubCreator m }
  addIdentifier md' >>= fixDate >>= addAuthor >>= addLanguage

addMetadataFromXML :: Element -> EPUBMetadata -> EPUBMetadata
addMetadataFromXML e@(Element (QName name _ (Just "dc")) attrs _ _) md
  | name == "identifier" = md{ epubIdentifier =
             Identifier{ identifierText = strContent e
                       , identifierScheme = lookupAttr (opfName "scheme") attrs
                       } : epubIdentifier md }
  | name == "title" = md{ epubTitle =
            Title{ titleText = strContent e
                 , titleFileAs = getAttr "file-as"
                 , titleType = getAttr "type"
                 } : epubTitle md }
  | name == "date" = md{ epubDate =
             Date{ dateText = fromMaybe "" $ normalizeDate' $ strContent e
                 , dateEvent = getAttr "event"
                 } : epubDate md }
  | name == "language" = md{ epubLanguage = strContent e }
  | name == "creator" = md{ epubCreator =
              Creator{ creatorText = strContent e
                     , creatorRole = getAttr "role"
                     , creatorFileAs = getAttr "file-as"
                     } : epubCreator md }
  | name == "contributor" = md{ epubContributor =
              Creator  { creatorText = strContent e
                       , creatorRole = getAttr "role"
                       , creatorFileAs = getAttr "file-as"
                       } : epubContributor md }
  | name == "subject" = md{ epubSubject = strContent e : epubSubject md }
  | name == "description" = md { epubDescription = Just $ strContent e }
  | name == "type" = md { epubType = Just $ strContent e }
  | name == "format" = md { epubFormat = Just $ strContent e }
  | name == "type" = md { epubType = Just $ strContent e }
  | name == "publisher" = md { epubPublisher = Just $ strContent e }
  | name == "source" = md { epubSource = Just $ strContent e }
  | name == "relation" = md { epubRelation = Just $ strContent e }
  | name == "coverage" = md { epubCoverage = Just $ strContent e }
  | name == "rights" = md { epubRights = Just $ strContent e }
  | otherwise = md
  where getAttr n = lookupAttr (opfName n) attrs
addMetadataFromXML e@(Element (QName "meta" _ _) attrs _ _) md =
  case getAttr "property" of
       Just s | "ibooks:" `isPrefixOf` s ->
                md{ epubIbooksFields = (drop 7 s, strContent e) :
                       epubIbooksFields md }
       _ -> case getAttr "name" of
                 Just s | "calibre:" `isPrefixOf` s ->
                   md{ epubCalibreFields =
                         (drop 8 s, fromMaybe "" $ getAttr "content") :
                          epubCalibreFields md }
                 _ -> md
  where getAttr n = lookupAttr (unqual n) attrs
addMetadataFromXML _ md = md

metaValueToString :: MetaValue -> String
metaValueToString (MetaString s)    = TS.unpack s
metaValueToString (MetaInlines ils) = TS.unpack $ stringify ils
metaValueToString (MetaBlocks bs)   = TS.unpack $ stringify bs
metaValueToString (MetaBool True)   = "true"
metaValueToString (MetaBool False)  = "false"
metaValueToString _                 = ""

metaValueToPaths :: MetaValue -> [FilePath]
metaValueToPaths (MetaList xs) = map metaValueToString xs
metaValueToPaths x             = [metaValueToString x]

getList :: TS.Text -> Meta -> (MetaValue -> a) -> [a]
getList s meta handleMetaValue =
  case lookupMeta s meta of
       Just (MetaList xs) -> map handleMetaValue xs
       Just mv            -> [handleMetaValue mv]
       Nothing            -> []

getIdentifier :: Meta -> [Identifier]
getIdentifier meta = getList "identifier" meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Identifier{ identifierText = maybe "" metaValueToString
                                        $ M.lookup "text" m
                     , identifierScheme = metaValueToString <$>
                                          M.lookup "scheme" m }
        handleMetaValue mv = Identifier (metaValueToString mv) Nothing

getTitle :: Meta -> [Title]
getTitle meta = getList "title" meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Title{ titleText = maybe "" metaValueToString $ M.lookup "text" m
                , titleFileAs = metaValueToString <$> M.lookup "file-as" m
                , titleType = metaValueToString <$> M.lookup "type" m }
        handleMetaValue mv = Title (metaValueToString mv) Nothing Nothing

getCreator :: TS.Text -> Meta -> [Creator]
getCreator s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Creator{ creatorText = maybe "" metaValueToString $ M.lookup "text" m
                  , creatorFileAs = metaValueToString <$> M.lookup "file-as" m
                  , creatorRole = metaValueToString <$> M.lookup "role" m }
        handleMetaValue mv = Creator (metaValueToString mv) Nothing Nothing

getDate :: TS.Text -> Meta -> [Date]
getDate s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Date{ dateText = fromMaybe "" $
                   M.lookup "text" m >>= normalizeDate' . metaValueToString
               , dateEvent = metaValueToString <$> M.lookup "event" m }
        handleMetaValue mv = Date { dateText = fromMaybe "" $ normalizeDate' $ metaValueToString mv
                                  , dateEvent = Nothing }

simpleList :: TS.Text -> Meta -> [String]
simpleList s meta =
  case lookupMeta s meta of
       Just (MetaList xs) -> map metaValueToString xs
       Just x             -> [metaValueToString x]
       Nothing            -> []

metadataFromMeta :: WriterOptions -> Meta -> EPUBMetadata
metadataFromMeta opts meta = EPUBMetadata{
      epubIdentifier         = identifiers
    , epubTitle              = titles
    , epubDate               = date
    , epubLanguage           = language
    , epubCreator            = creators
    , epubContributor        = contributors
    , epubSubject            = subjects
    , epubDescription        = description
    , epubType               = epubtype
    , epubFormat             = format
    , epubPublisher          = publisher
    , epubSource             = source
    , epubRelation           = relation
    , epubCoverage           = coverage
    , epubRights             = rights
    , epubCoverImage         = coverImage
    , epubStylesheets        = stylesheets
    , epubPageDirection      = pageDirection
    , epubIbooksFields       = ibooksFields
    , epubCalibreFields      = calibreFields
    }
  where identifiers = getIdentifier meta
        titles = getTitle meta
        date = getDate "date" meta
        language = maybe "" metaValueToString $
           lookupMeta "language" meta `mplus` lookupMeta "lang" meta
        creators = getCreator "creator" meta
        contributors = getCreator "contributor" meta
        subjects = simpleList "subject" meta
        description = metaValueToString <$> lookupMeta "description" meta
        epubtype = metaValueToString <$> lookupMeta "type" meta
        format = metaValueToString <$> lookupMeta "format" meta
        publisher = metaValueToString <$> lookupMeta "publisher" meta
        source = metaValueToString <$> lookupMeta "source" meta
        relation = metaValueToString <$> lookupMeta "relation" meta
        coverage = metaValueToString <$> lookupMeta "coverage" meta
        rights = metaValueToString <$> lookupMeta "rights" meta
        coverImage =
            (TS.unpack <$> lookupContext "epub-cover-image"
                              (writerVariables opts))
            `mplus` (metaValueToString <$> lookupMeta "cover-image" meta)
        mCss = lookupMeta "css" meta <|> lookupMeta "stylesheet" meta
        stylesheets = maybe [] metaValueToPaths mCss ++
                      case lookupContext "css" (writerVariables opts) of
                         Just xs -> map TS.unpack xs
                         Nothing ->
                           case lookupContext "css" (writerVariables opts) of
                             Just x  -> [TS.unpack x]
                             Nothing -> []
        pageDirection = case map toLower . metaValueToString <$>
                             lookupMeta "page-progression-direction" meta of
                              Just "ltr" -> Just LTR
                              Just "rtl" -> Just RTL
                              _          -> Nothing
        ibooksFields = case lookupMeta "ibooks" meta of
                            Just (MetaMap mp)
                               -> M.toList $ M.mapKeys TS.unpack $ M.map metaValueToString mp
                            _  -> []
        calibreFields = case lookupMeta "calibre" meta of
                            Just (MetaMap mp)
                               -> M.toList $ M.mapKeys TS.unpack $ M.map metaValueToString mp
                            _  -> []

-- | Produce an EPUB2 file from a Pandoc document.
writeEPUB2 :: PandocMonad m
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m B.ByteString
writeEPUB2 = writeEPUB EPUB2

-- | Produce an EPUB3 file from a Pandoc document.
writeEPUB3 :: PandocMonad m
          => WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m B.ByteString
writeEPUB3 = writeEPUB EPUB3

-- | Produce an EPUB file from a Pandoc document.
writeEPUB :: PandocMonad m
          => EPUBVersion
          -> WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> m B.ByteString
writeEPUB epubVersion opts doc = do
  let epubSubdir = writerEpubSubdirectory opts
  -- sanity check on epubSubdir
  unless (TS.all (\c -> isAscii c && isAlphaNum c) epubSubdir) $
    throwError $ PandocEpubSubdirectoryError epubSubdir
  let initState = EPUBState { stMediaPaths = [], stMediaNextId = 0, stEpubSubdir = TS.unpack epubSubdir }
  evalStateT (pandocToEPUB epubVersion opts doc) initState

pandocToEPUB :: PandocMonad m
             => EPUBVersion
             -> WriterOptions
             -> Pandoc
             -> E m B.ByteString
pandocToEPUB version opts doc = do
  -- handle pictures
  Pandoc meta blocks <- walkM (transformInline opts) doc >>=
                        walkM transformBlock
  picEntries <- mapMaybe (snd . snd) <$> gets stMediaPaths

  epubSubdir <- gets stEpubSubdir
  let epub3 = version == EPUB3
  let writeHtml o = fmap (UTF8.fromTextLazy . TL.fromStrict) .
                      writeHtmlStringForEPUB version o
  metadata <- getEPUBMetadata opts meta

  let plainTitle = case docTitle' meta of
                        [] -> case epubTitle metadata of
                                   []    -> "UNTITLED"
                                   (x:_) -> titleText x
                        x  -> TS.unpack $ stringify x

  -- stylesheet
  stylesheets <- case epubStylesheets metadata of
                      [] -> (\x -> [B.fromChunks [x]]) <$>
                             P.readDataFile "epub.css"
                      fs -> mapM P.readFileLazy fs
  stylesheetEntries <- zipWithM
        (\bs n -> mkEntry ("styles/stylesheet" ++ show n ++ ".css") bs)
        stylesheets [(1 :: Int)..]

  let vars = Context $
               M.delete "css" .
               M.insert "epub3"
                 (toVal' $ if epub3 then "true" else "false") .
               M.insert "lang" (toVal' $ epubLanguage metadata)
             $ unContext $ writerVariables opts

  let cssvars useprefix = Context $ M.insert "css"
                           (ListVal $ map
                             (\e -> toVal' $
                                (if useprefix then "../" else "") <>
                                makeRelative epubSubdir (eRelativePath e))
                             stylesheetEntries)
                             mempty

  let opts' = opts{ writerEmailObfuscation = NoObfuscation
                  , writerSectionDivs = True
                  , writerVariables = vars
                  , writerHTMLMathMethod =
                       if epub3
                          then MathML
                          else writerHTMLMathMethod opts
                  , writerWrapText = WrapAuto }

  -- cover page
  (cpgEntry, cpicEntry) <-
                case epubCoverImage metadata of
                     Nothing   -> return ([],[])
                     Just img  -> do
                       let coverImage = takeFileName img
                       imgContent <- lift $ P.readFileLazy img
                       (coverImageWidth, coverImageHeight) <-
                             case imageSize opts' (B.toStrict imgContent) of
                               Right sz  -> return $ sizeInPixels sz
                               Left err' -> (0, 0) <$ report
                                 (CouldNotDetermineImageSize (TS.pack img) err')
                       cpContent <- lift $ writeHtml
                            opts'{ writerVariables =
                                   Context (M.fromList [
                                    ("coverpage", toVal' "true"),
                                    ("pagetitle", toVal $
                                      escapeStringForXML $ TS.pack plainTitle),
                                    ("cover-image", toVal' coverImage),
                                    ("cover-image-width", toVal' $
                                       show coverImageWidth),
                                    ("cover-image-height", toVal' $
                                       show coverImageHeight)]) <>
                                     cssvars True <> vars }
                            (Pandoc meta [])
                       coverEntry <- mkEntry "text/cover.xhtml" cpContent
                       coverImageEntry <- mkEntry ("media/" ++ coverImage)
                                             imgContent
                       return ( [ coverEntry ]
                              , [ coverImageEntry ] )

  -- title page
  tpContent <- lift $ writeHtml opts'{
                                  writerVariables =
                                      Context (M.fromList [
                                        ("titlepage", toVal' "true"),
                                        ("body-type",  toVal' "frontmatter"),
                                        ("pagetitle", toVal $
                                            escapeStringForXML $ TS.pack plainTitle)])
                                      <> cssvars True <> vars }
                               (Pandoc meta [])
  tpEntry <- mkEntry "text/title_page.xhtml" tpContent

  -- handle fonts
  let matchingGlob f = do
        xs <- lift $ P.glob f
        when (null xs) $
          report $ CouldNotFetchResource (TS.pack f) "glob did not match any font files"
        return xs
  let mkFontEntry f = mkEntry ("fonts/" ++ takeFileName f) =<<
                        lift (P.readFileLazy f)
  fontFiles <- concat <$> mapM matchingGlob (writerEpubFonts opts')
  fontEntries <- mapM mkFontEntry fontFiles

  -- set page progression direction attribution
  let progressionDirection = case epubPageDirection metadata of
                                  Just LTR | epub3 ->
                                    [("page-progression-direction", "ltr")]
                                  Just RTL | epub3 ->
                                    [("page-progression-direction", "rtl")]
                                  _  -> []

  -- body pages

  let chapterHeaderLevel = writerEpubChapterLevel opts

  let isChapterHeader (Div _ (Header n _ _:_)) = n <= chapterHeaderLevel
      isChapterHeader _ = False

  let secsToChapters :: [Block] -> [Chapter]
      secsToChapters [] = []
      secsToChapters (d@(Div attr (h@(Header lvl _ _) : bs)) : rest)
        | chapterHeaderLevel == lvl =
           Chapter [d] : secsToChapters rest
        | chapterHeaderLevel > lvl =
           Chapter [Div attr (h:xs)] :
           secsToChapters ys ++ secsToChapters rest
             where (xs, ys) = break isChapterHeader bs
      secsToChapters bs =
          (if null xs then id else (Chapter xs :)) $ secsToChapters ys
            where (xs, ys) = break isChapterHeader bs

  -- add level 1 header to beginning if none there
  let secs = makeSections True Nothing
              $ addIdentifiers opts
              $ case blocks of
                  (Div _
                    (Header{}:_) : _) -> blocks
                  (Header 1 _ _ : _)  -> blocks
                  _                   -> Header 1 ("",["unnumbered"],[])
                                             (docTitle' meta) : blocks

  let chapters' = secsToChapters secs

  let extractLinkURL' :: Int -> Inline -> [(TS.Text, TS.Text)]
      extractLinkURL' num (Span (ident, _, _) _)
        | not (TS.null ident) = [(ident, TS.pack (showChapter num) <> "#" <> ident)]
      extractLinkURL' _ _ = []

  let extractLinkURL :: Int -> Block -> [(TS.Text, TS.Text)]
      extractLinkURL num (Div (ident, _, _) _)
        | not (TS.null ident) = [(ident, TS.pack (showChapter num) <> "#" <> ident)]
      extractLinkURL num (Header _ (ident, _, _) _)
        | not (TS.null ident) = [(ident, TS.pack (showChapter num) <> "#" <> ident)]
      extractLinkURL num b = query (extractLinkURL' num) b

  let reftable = concat $ zipWith (\(Chapter bs) num ->
                                    query (extractLinkURL num) bs)
                          chapters' [1..]

  let fixInternalReferences :: Inline -> Inline
      fixInternalReferences (Link attr lab (src, tit))
        | Just ('#', xs) <- TS.uncons src = case lookup xs reftable of
             Just ys -> Link attr lab (ys, tit)
             Nothing -> Link attr lab (src, tit)
      fixInternalReferences x = x

  -- internal reference IDs change when we chunk the file,
  -- so that '#my-header-1' might turn into 'chap004.xhtml#my-header'.
  -- this fixes that:
  let chapters = map (\(Chapter bs) ->
                         Chapter $ walk fixInternalReferences bs)
                 chapters'

  let chapToEntry num (Chapter bs) =
        mkEntry ("text/" ++ showChapter num) =<<
        writeHtml opts'{ writerVariables =
                            Context (M.fromList
                                     [("body-type", toVal' bodyType),
                                      ("pagetitle", toVal' $
                                           showChapter num)])
                            <> cssvars True <> vars } pdoc
         where (pdoc, bodyType) =
                 case bs of
                     (Div (_,"section":_,kvs)
                       (Header _ _ xs : _) : _) ->
                       -- remove notes or we get doubled footnotes
                       (Pandoc (setMeta "title"
                           (walk removeNote $ fromList xs) nullMeta) bs,
                        case lookup "epub:type" kvs of
                             Nothing -> "bodymatter"
                             Just x
                               | x `elem` frontMatterTypes -> "frontmatter"
                               | x `elem` backMatterTypes  -> "backmatter"
                               | otherwise                 -> "bodymatter")
                     _                   -> (Pandoc nullMeta bs, "bodymatter")
               frontMatterTypes = ["prologue", "abstract", "acknowledgments",
                                   "copyright-page", "dedication",
                                   "credits", "keywords", "imprint",
                                   "contributors", "other-credits",
                                   "errata", "revision-history",
                                   "titlepage", "halftitlepage", "seriespage",
                                   "foreword", "preface",
                                   "seriespage", "titlepage"]
               backMatterTypes = ["appendix", "colophon", "bibliography",
                                  "index"]

  chapterEntries <- zipWithM chapToEntry [1..] chapters

  -- incredibly inefficient (TODO):
  let containsMathML ent = epub3 &&
                           "<math" `isInfixOf`
        B8.unpack (fromEntry ent)
  let containsSVG ent    = epub3 &&
                           "<svg" `isInfixOf`
        B8.unpack (fromEntry ent)
  let props ent = ["mathml" | containsMathML ent] ++ ["svg" | containsSVG ent]

  -- contents.opf
  let chapterNode ent = unode "item" !
                           ([("id", toId $ makeRelative epubSubdir
                                         $ eRelativePath ent),
                             ("href", makeRelative epubSubdir
                                      $ eRelativePath ent),
                             ("media-type", "application/xhtml+xml")]
                            ++ case props ent of
                                    [] -> []
                                    xs -> [("properties", unwords xs)])
                        $ ()

  let chapterRefNode ent = unode "itemref" !
                             [("idref", toId $ makeRelative epubSubdir
                                             $ eRelativePath ent)] $ ()
  let pictureNode ent = unode "item" !
                           [("id", toId $ makeRelative epubSubdir
                                        $ eRelativePath ent),
                            ("href", makeRelative epubSubdir
                                     $ eRelativePath ent),
                            ("media-type",
                               maybe "application/octet-stream" TS.unpack
                               $ mediaTypeOf $ eRelativePath ent)] $ ()
  let fontNode ent = unode "item" !
                           [("id", toId $ makeRelative epubSubdir
                                        $ eRelativePath ent),
                            ("href", makeRelative epubSubdir
                                     $ eRelativePath ent),
                            ("media-type", maybe "" TS.unpack $
                                  getMimeType $ eRelativePath ent)] $ ()

  let tocTitle = maybe plainTitle
                   metaValueToString $ lookupMeta "toc-title" meta
  uuid <- case epubIdentifier metadata of
            (x:_) -> return $ identifierText x  -- use first identifier as UUID
            []    -> throwError $ PandocShouldNeverHappenError "epubIdentifier is null"  -- shouldn't happen
  currentTime <- lift P.getCurrentTime
  let contentsData = UTF8.fromStringLazy $ ppTopElement $
        unode "package" !
          ([("version", case version of
                             EPUB2 -> "2.0"
                             EPUB3 -> "3.0")
           ,("xmlns","http://www.idpf.org/2007/opf")
           ,("unique-identifier","epub-id-1")
           ] ++
           [("prefix","ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/") | version == EPUB3]) $
          [ metadataElement version metadata currentTime
          , unode "manifest" $
             [ unode "item" ! [("id","ncx"), ("href","toc.ncx")
                              ,("media-type","application/x-dtbncx+xml")] $ ()
             , unode "item" ! ([("id","nav")
                               ,("href","nav.xhtml")
                               ,("media-type","application/xhtml+xml")] ++
                               [("properties","nav") | epub3 ]) $ ()
             ] ++
             [ unode "item" ! [("id","stylesheet" ++ show n), ("href",fp)
                              ,("media-type","text/css")] $ () |
                             (n :: Int, fp) <- zip [1..] (map
                               (makeRelative epubSubdir . eRelativePath)
                               stylesheetEntries) ] ++
             map chapterNode (cpgEntry ++ (tpEntry : chapterEntries)) ++
             (case cpicEntry of
                    []    -> []
                    (x:_) -> [add_attrs
                              [Attr (unqual "properties") "cover-image" | epub3]
                              (pictureNode x)]) ++
             map pictureNode picEntries ++
             map fontNode fontEntries
          , unode "spine" ! (
             ("toc","ncx") : progressionDirection) $
              case epubCoverImage metadata of
                    Nothing -> []
                    Just _ -> [ unode "itemref" !
                                [("idref", "cover_xhtml")] $ () ]
              ++ ((unode "itemref" ! [("idref", "title_page_xhtml")
                                     ,("linear",
                                         case lookupMeta "title" meta of
                                               Just _  -> "yes"
                                               Nothing -> "no")] $ ()) :
                  [unode "itemref" ! [("idref", "nav")] $ ()
                         | writerTableOfContents opts ] ++
                  map chapterRefNode chapterEntries)
          , unode "guide" $
             [ unode "reference" !
                   [("type","toc"),("title", tocTitle),
                    ("href","nav.xhtml")] $ ()
             ] ++
             [ unode "reference" !
                   [("type","cover")
                   ,("title","Cover")
                   ,("href","text/cover.xhtml")] $ ()
               | isJust (epubCoverImage metadata)
             ]
          ]
  contentsEntry <- mkEntry "content.opf" contentsData

  -- toc.ncx
  let tocLevel = writerTOCDepth opts

  let navPointNode :: PandocMonad m
                   => (Int -> [Inline] -> TS.Text -> [Element] -> Element)
                   -> Block -> StateT Int m [Element]
      navPointNode formatter (Div (ident,_,_)
                                (Header lvl (_,_,kvs) ils : children)) =
        if lvl > tocLevel
           then return []
           else do
             n <- get
             modify (+1)
             let num = fromMaybe "" $ lookup "number" kvs
             let tit = if writerNumberSections opts && not (TS.null num)
                          then Span ("", ["section-header-number"], [])
                                [Str num] : Space : ils
                          else ils
             src <- case lookup ident reftable of
                      Just x  -> return x
                      Nothing -> throwError $ PandocSomeError $
                                    ident <> " not found in reftable"
             subs <- concat <$> mapM (navPointNode formatter) children
             return [formatter n tit src subs]
      navPointNode formatter (Div _ bs) =
        concat <$> mapM (navPointNode formatter) bs
      navPointNode _ _ = return []

  let navMapFormatter :: Int -> [Inline] -> TS.Text -> [Element] -> Element
      navMapFormatter n tit src subs = unode "navPoint" !
               [("id", "navPoint-" ++ show n)] $
                  [ unode "navLabel" $ unode "text" $ TS.unpack $ stringify tit
                  , unode "content" ! [("src", "text/" <> TS.unpack src)] $ ()
                  ] ++ subs

  let tpNode = unode "navPoint" !  [("id", "navPoint-0")] $
                  [ unode "navLabel" $ unode "text" (TS.unpack $ stringify $ docTitle' meta)
                  , unode "content" ! [("src", "text/title_page.xhtml")]
                  $ () ]

  navMap <- lift $ evalStateT
             (concat <$> mapM (navPointNode navMapFormatter) secs) 1
  let tocData = UTF8.fromStringLazy $ ppTopElement $
        unode "ncx" ! [("version","2005-1")
                       ,("xmlns","http://www.daisy.org/z3986/2005/ncx/")] $
          [ unode "head" $
             [ unode "meta" ! [("name","dtb:uid")
                              ,("content", uuid)] $ ()
             , unode "meta" ! [("name","dtb:depth")
                              ,("content", "1")] $ ()
             , unode "meta" ! [("name","dtb:totalPageCount")
                              ,("content", "0")] $ ()
             , unode "meta" ! [("name","dtb:maxPageNumber")
                              ,("content", "0")] $ ()
             ] ++ case epubCoverImage metadata of
                        Nothing  -> []
                        Just img -> [unode "meta" ! [("name","cover"),
                                            ("content", toId img)] $ ()]
          , unode "docTitle" $ unode "text" plainTitle
          , unode "navMap" $
              tpNode : navMap
          ]
  tocEntry <- mkEntry "toc.ncx" tocData

  let navXhtmlFormatter :: Int -> [Inline] -> TS.Text -> [Element] -> Element
      navXhtmlFormatter n tit src subs = unode "li" !
                                       [("id", "toc-li-" ++ show n)] $
                                            (unode "a" !
                                                [("href", "text/" <> TS.unpack src)]
                                             $ titElements)
                                            : case subs of
                                                 []    -> []
                                                 (_:_) -> [unode "ol" ! [("class","toc")] $ subs]
          where titElements = parseXML titRendered
                titRendered = case P.runPure
                               (writeHtmlStringForEPUB version
                                 opts{ writerTemplate = Nothing
                                     , writerVariables =
                                         Context (M.fromList
                                           [("pagetitle", toVal $
                                             escapeStringForXML $ TS.pack plainTitle)])
                                       <> writerVariables opts}
                                 (Pandoc nullMeta
                                   [Plain $ walk clean tit])) of
                                Left _  -> stringify tit
                                Right x -> x
                -- can't have <a> elements inside generated links...
                clean (Link _ ils _) = Span ("", [], []) ils
                clean (Note _)       = Str ""
                clean x              = x

  let navtag = if epub3 then "nav" else "div"
  tocBlocks <- lift $ evalStateT
                 (concat <$> mapM (navPointNode navXhtmlFormatter) secs) 1
  let navBlocks = [RawBlock (Format "html")
                  $ TS.pack $ showElement $ -- prettyprinting introduces bad spaces
                   unode navtag ! ([("epub:type","toc") | epub3] ++
                                   [("id","toc")]) $
                    [ unode "h1" ! [("id","toc-title")] $ tocTitle
                    , unode "ol" ! [("class","toc")] $ tocBlocks ]]
  let landmarkItems = if epub3
                         then [ unode "li"
                                [ unode "a" ! [("href", "text/cover.xhtml")
                                              ,("epub:type", "cover")] $
                                  ("Cover" :: String)] |
                                  isJust (epubCoverImage metadata)
                              ] ++
                              [ unode "li"
                                [ unode "a" ! [("href", "#toc")
                                              ,("epub:type", "toc")] $
                                    ("Table of contents" :: String)
                                ] | writerTableOfContents opts
                              ]
                         else []
  let landmarks = if null landmarkItems
                     then []
                     else [RawBlock (Format "html") $ TS.pack $ ppElement $
                            unode "nav" ! [("epub:type","landmarks")
                                          ,("id","landmarks")
                                          ,("hidden","hidden")] $
                            [ unode "ol" landmarkItems ]
                          ]
  navData <- lift $ writeHtml opts'{ writerVariables =
                     Context (M.fromList [("navpage", toVal' "true")])
                     <> cssvars False <> vars }
            (Pandoc (setMeta "title"
                     (walk removeNote $ fromList $ docTitle' meta) nullMeta)
               (navBlocks ++ landmarks))
  navEntry <- mkEntry "nav.xhtml" navData

  -- mimetype
  mimetypeEntry <- mkEntry "mimetype" $
                        UTF8.fromStringLazy "application/epub+zip"

  -- container.xml
  let containerData = UTF8.fromStringLazy $ ppTopElement $
       unode "container" ! [("version","1.0")
              ,("xmlns","urn:oasis:names:tc:opendocument:xmlns:container")] $
         unode "rootfiles" $
           unode "rootfile" ! [("full-path",
                    (if null epubSubdir
                        then ""
                        else epubSubdir ++ "/") ++ "content.opf")
               ,("media-type","application/oebps-package+xml")] $ ()
  containerEntry <- mkEntry "META-INF/container.xml" containerData

  -- com.apple.ibooks.display-options.xml
  let apple = UTF8.fromStringLazy $ ppTopElement $
        unode "display_options" $
          unode "platform" ! [("name","*")] $
            unode "option" ! [("name","specified-fonts")] $ ("true" :: String)
  appleEntry <- mkEntry "META-INF/com.apple.ibooks.display-options.xml" apple

  -- construct archive
  let archive = foldr addEntryToArchive emptyArchive $
                 [mimetypeEntry, containerEntry, appleEntry,
                  contentsEntry, tocEntry, navEntry, tpEntry] ++
                  stylesheetEntries ++ picEntries ++ cpicEntry ++
                  cpgEntry ++ chapterEntries ++ fontEntries
  return $ fromArchive archive

metadataElement :: EPUBVersion -> EPUBMetadata -> UTCTime -> Element
metadataElement version md currentTime =
  unode "metadata" ! [("xmlns:dc","http://purl.org/dc/elements/1.1/")
                     ,("xmlns:opf","http://www.idpf.org/2007/opf")] $ mdNodes
  where mdNodes = identifierNodes ++ titleNodes ++ dateNodes
                  ++ languageNodes ++ ibooksNodes ++ calibreNodes
                  ++ creatorNodes ++ contributorNodes ++ subjectNodes
                  ++ descriptionNodes ++ typeNodes ++ formatNodes
                  ++ publisherNodes ++ sourceNodes ++ relationNodes
                  ++ coverageNodes ++ rightsNodes ++ coverImageNodes
                  ++ modifiedNodes
        withIds base f = concat . zipWith f (map (\x -> base ++ ('-' : show x))
                         ([1..] :: [Int]))
        identifierNodes = withIds "epub-id" toIdentifierNode $
                          epubIdentifier md
        titleNodes = withIds "epub-title" toTitleNode $ epubTitle md
        dateNodes = if version == EPUB2
                       then withIds "epub-date" toDateNode $ epubDate md
                       else -- epub3 allows only one dc:date
                            -- http://www.idpf.org/epub/30/spec/epub30-publications.html#sec-opf-dcdate
                            case epubDate md of
                                 [] -> []
                                 (x:_) -> [dcNode "date" ! [("id","epub-date")]
                                            $ dateText x]
        ibooksNodes = map ibooksNode (epubIbooksFields md)
        ibooksNode (k, v) = unode "meta" ! [("property", "ibooks:" ++ k)] $ v
        calibreNodes = map calibreNode (epubCalibreFields md)
        calibreNode (k, v) = unode "meta" ! [("name", "calibre:" ++ k),
                                             ("content", v)] $ ()
        languageNodes = [dcTag "language" $ epubLanguage md]
        creatorNodes = withIds "epub-creator" (toCreatorNode "creator") $
                       epubCreator md
        contributorNodes = withIds "epub-contributor"
                           (toCreatorNode "contributor") $ epubContributor md
        subjectNodes = map (dcTag "subject") $ epubSubject md
        descriptionNodes = maybe [] (dcTag' "description") $ epubDescription md
        typeNodes = maybe [] (dcTag' "type") $ epubType md
        formatNodes = maybe [] (dcTag' "format") $ epubFormat md
        publisherNodes = maybe [] (dcTag' "publisher") $ epubPublisher md
        sourceNodes = maybe [] (dcTag' "source") $ epubSource md
        relationNodes = maybe [] (dcTag' "relation") $ epubRelation md
        coverageNodes = maybe [] (dcTag' "coverage") $ epubCoverage md
        rightsNodes = maybe [] (dcTag' "rights") $ epubRights md
        coverImageNodes = maybe []
            (\img -> [unode "meta" !  [("name","cover"),
                                       ("content",toId img)] $ ()])
            $ epubCoverImage md
        modifiedNodes = [ unode "meta" ! [("property", "dcterms:modified")] $
               showDateTimeISO8601 currentTime | version == EPUB3 ]
        dcTag n s = unode ("dc:" ++ n) s
        dcTag' n s = [dcTag n s]
        toIdentifierNode id' (Identifier txt scheme)
          | version == EPUB2 = [dcNode "identifier" !
              (("id",id') : maybe [] (\x -> [("opf:scheme", x)]) scheme) $
              txt]
          | otherwise = [dcNode "identifier" ! [("id",id')] $ txt] ++
              maybe [] ((\x -> [unode "meta" !
                                [ ("refines",'#':id')
                                , ("property","identifier-type")
                                , ("scheme","onix:codelist5")
                                ]
                                $ x
                               ])
                        . schemeToOnix)
                    scheme
        toCreatorNode s id' creator
          | version == EPUB2 = [dcNode s !
             (("id",id') :
              maybe [] (\x -> [("opf:file-as",x)]) (creatorFileAs creator) ++
              maybe [] (\x -> [("opf:role",x)])
               (creatorRole creator >>= toRelator)) $ creatorText creator]
          | otherwise = [dcNode s ! [("id",id')] $ creatorText creator] ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","file-as")] $ x])
                   (creatorFileAs creator) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","role"),
                     ("scheme","marc:relators")] $ x])
                   (creatorRole creator >>= toRelator)
        toTitleNode id' title
          | version == EPUB2 = [dcNode "title" !
             (("id",id') :
              -- note: EPUB2 doesn't accept opf:title-type
              maybe [] (\x -> [("opf:file-as",x)]) (titleFileAs title)) $
              titleText title]
          | otherwise = [dcNode "title" ! [("id",id')] $ titleText title]
              ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","file-as")] $ x])
                   (titleFileAs title) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines",'#':id'),("property","title-type")] $ x])
                   (titleType title)
        toDateNode id' date = [dcNode "date" !
             (("id",id') :
                maybe [] (\x -> [("opf:event",x)]) (dateEvent date)) $
                 dateText date]
        schemeToOnix :: String -> String
        schemeToOnix "ISBN-10"              = "02"
        schemeToOnix "GTIN-13"              = "03"
        schemeToOnix "UPC"                  = "04"
        schemeToOnix "ISMN-10"              = "05"
        schemeToOnix "DOI"                  = "06"
        schemeToOnix "LCCN"                 = "13"
        schemeToOnix "GTIN-14"              = "14"
        schemeToOnix "ISBN-13"              = "15"
        schemeToOnix "Legal deposit number" = "17"
        schemeToOnix "URN"                  = "22"
        schemeToOnix "OCLC"                 = "23"
        schemeToOnix "ISMN-13"              = "25"
        schemeToOnix "ISBN-A"               = "26"
        schemeToOnix "JP"                   = "27"
        schemeToOnix "OLCC"                 = "28"
        schemeToOnix _                      = "01"

showDateTimeISO8601 :: UTCTime -> String
showDateTimeISO8601 = formatTime defaultTimeLocale "%FT%TZ"

transformTag :: PandocMonad m
             => Tag TS.Text
             -> E m (Tag TS.Text)
transformTag tag@(TagOpen name attr)
  | name `elem` ["video", "source", "img", "audio"] &&
    isNothing (lookup "data-external" attr) = do
  let src = fromAttrib "src" tag
  let poster = fromAttrib "poster" tag
  newsrc <- modifyMediaRef $ TS.unpack src
  newposter <- modifyMediaRef $ TS.unpack poster
  let attr' = filter (\(x,_) -> x /= "src" && x /= "poster") attr ++
              [("src", "../" <> newsrc) | not (TS.null newsrc)] ++
              [("poster", "../" <> newposter) | not (TS.null newposter)]
  return $ TagOpen name attr'
transformTag tag = return tag

modifyMediaRef :: PandocMonad m
               => FilePath
               -> E m TS.Text
modifyMediaRef "" = return ""
modifyMediaRef oldsrc = do
  media <- gets stMediaPaths
  case lookup oldsrc media of
         Just (n,_) -> return $ TS.pack n
         Nothing    -> catchError
           (do (img, mbMime) <- P.fetchItem $ TS.pack oldsrc
               let ext = maybe (takeExtension (takeWhile (/='?') oldsrc)) TS.unpack
                         (("." <>) <$> (mbMime >>= extensionFromMimeType))
               newName <- getMediaNextNewName ext
               let newPath = "media/" ++ newName
               entry <- mkEntry newPath (B.fromChunks . (:[]) $ img)
               modify $ \st -> st{ stMediaPaths =
                            (oldsrc, (newPath, Just entry)):media}
               return $ TS.pack newPath)
           (\e -> do
                report $ CouldNotFetchResource (TS.pack oldsrc) (tshow e)
                return $ TS.pack oldsrc)

getMediaNextNewName :: PandocMonad m => String -> E m String
getMediaNextNewName ext = do
  nextId <- gets stMediaNextId
  modify $ \st -> st { stMediaNextId = nextId + 1 }
  let nextName = "file" ++ show nextId ++ ext
  (P.fetchItem (TS.pack nextName) >> getMediaNextNewName ext) `catchError` const (return nextName)

transformBlock  :: PandocMonad m
                => Block
                -> E m Block
transformBlock (RawBlock fmt raw)
  | fmt == Format "html" = do
  let tags = parseTags raw
  tags' <- mapM transformTag tags
  return $ RawBlock fmt (renderTags' tags')
transformBlock b = return b

transformInline  :: PandocMonad m
                 => WriterOptions
                 -> Inline
                 -> E m Inline
transformInline _opts (Image attr lab (src,tit)) = do
    newsrc <- modifyMediaRef $ TS.unpack src
    return $ Image attr lab ("../" <> newsrc, tit)
transformInline opts x@(Math t m)
  | WebTeX url <- writerHTMLMathMethod opts = do
    newsrc <- modifyMediaRef (TS.unpack url <> urlEncode (TS.unpack m))
    let mathclass = if t == DisplayMath then "display" else "inline"
    return $ Span ("",["math",mathclass],[])
                [Image nullAttr [x] ("../" <> newsrc, "")]
transformInline _opts (RawInline fmt raw)
  | fmt == Format "html" = do
  let tags = parseTags raw
  tags' <- mapM transformTag tags
  return $ RawInline fmt (renderTags' tags')
transformInline _ x = return x

(!) :: (t -> Element) -> [(String, String)] -> t -> Element
(!) f attrs n = add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) (f n)

-- | Version of 'ppTopElement' that specifies UTF-8 encoding.
ppTopElement :: Element -> String
ppTopElement = ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++) . unEntity . ppElement
  -- unEntity removes numeric  entities introduced by ppElement
  -- (kindlegen seems to choke on these).
  where unEntity [] = ""
        unEntity ('&':'#':xs) =
                   let (ds,ys) = break (==';') xs
                       rest = drop 1 ys
                   in  case safeRead (TS.pack $ "'\\" <> ds <> "'") of
                          Just x  -> x : unEntity rest
                          Nothing -> '&':'#':unEntity xs
        unEntity (x:xs) = x : unEntity xs

mediaTypeOf :: FilePath -> Maybe MimeType
mediaTypeOf x =
  let mediaPrefixes = ["image", "video", "audio"] in
  case getMimeType x of
    Just y | any (`TS.isPrefixOf` y) mediaPrefixes -> Just y
    _      -> Nothing

-- Returns filename for chapter number.
showChapter :: Int -> String
showChapter = printf "ch%03d.xhtml"

-- Add identifiers to any headers without them.
addIdentifiers :: WriterOptions -> [Block] -> [Block]
addIdentifiers opts bs = evalState (mapM go bs) Set.empty
 where go (Header n (ident,classes,kvs) ils) = do
         ids <- get
         let ident' = if TS.null ident
                         then uniqueIdent (writerExtensions opts) ils ids
                         else ident
         modify $ Set.insert ident'
         return $ Header n (ident',classes,kvs) ils
       go x = return x

-- Variant of normalizeDate that allows partial dates: YYYY, YYYY-MM
normalizeDate' :: String -> Maybe String
normalizeDate' = fmap TS.unpack . go . trim . TS.pack
  where
    go xs
      | TS.length xs == 4            -- YYY
      , TS.all isDigit xs = Just xs
      | (y, s) <- TS.splitAt 4 xs    -- YYY-MM
      , Just ('-', m) <- TS.uncons s
      , TS.length m == 2
      , TS.all isDigit y && TS.all isDigit m = Just xs
      | otherwise = normalizeDate xs

toRelator :: String -> Maybe String
toRelator x
  | x `elem` relators = Just x
  | otherwise         = lookup (map toLower x) relatorMap

relators :: [String]
relators = map snd relatorMap

relatorMap :: [(String, String)]
relatorMap =
           [("abridger", "abr")
           ,("actor", "act")
           ,("adapter", "adp")
           ,("addressee", "rcp")
           ,("analyst", "anl")
           ,("animator", "anm")
           ,("annotator", "ann")
           ,("appellant", "apl")
           ,("appellee", "ape")
           ,("applicant", "app")
           ,("architect", "arc")
           ,("arranger", "arr")
           ,("art copyist", "acp")
           ,("art director", "adi")
           ,("artist", "art")
           ,("artistic director", "ard")
           ,("assignee", "asg")
           ,("associated name", "asn")
           ,("attributed name", "att")
           ,("auctioneer", "auc")
           ,("author", "aut")
           ,("author in quotations or text abstracts", "aqt")
           ,("author of afterword, colophon, etc.", "aft")
           ,("author of dialog", "aud")
           ,("author of introduction, etc.", "aui")
           ,("autographer", "ato")
           ,("bibliographic antecedent", "ant")
           ,("binder", "bnd")
           ,("binding designer", "bdd")
           ,("blurb writer", "blw")
           ,("book designer", "bkd")
           ,("book producer", "bkp")
           ,("bookjacket designer", "bjd")
           ,("bookplate designer", "bpd")
           ,("bookseller", "bsl")
           ,("braille embosser", "brl")
           ,("broadcaster", "brd")
           ,("calligrapher", "cll")
           ,("cartographer", "ctg")
           ,("caster", "cas")
           ,("censor", "cns")
           ,("choreographer", "chr")
           ,("cinematographer", "cng")
           ,("client", "cli")
           ,("collection registrar", "cor")
           ,("collector", "col")
           ,("collotyper", "clt")
           ,("colorist", "clr")
           ,("commentator", "cmm")
           ,("commentator for written text", "cwt")
           ,("compiler", "com")
           ,("complainant", "cpl")
           ,("complainant-appellant", "cpt")
           ,("complainant-appellee", "cpe")
           ,("composer", "cmp")
           ,("compositor", "cmt")
           ,("conceptor", "ccp")
           ,("conductor", "cnd")
           ,("conservator", "con")
           ,("consultant", "csl")
           ,("consultant to a project", "csp")
           ,("contestant", "cos")
           ,("contestant-appellant", "cot")
           ,("contestant-appellee", "coe")
           ,("contestee", "cts")
           ,("contestee-appellant", "ctt")
           ,("contestee-appellee", "cte")
           ,("contractor", "ctr")
           ,("contributor", "ctb")
           ,("copyright claimant", "cpc")
           ,("copyright holder", "cph")
           ,("corrector", "crr")
           ,("correspondent", "crp")
           ,("costume designer", "cst")
           ,("court governed", "cou")
           ,("court reporter", "crt")
           ,("cover designer", "cov")
           ,("creator", "cre")
           ,("curator", "cur")
           ,("dancer", "dnc")
           ,("data contributor", "dtc")
           ,("data manager", "dtm")
           ,("dedicatee", "dte")
           ,("dedicator", "dto")
           ,("defendant", "dfd")
           ,("defendant-appellant", "dft")
           ,("defendant-appellee", "dfe")
           ,("degree granting institution", "dgg")
           ,("delineator", "dln")
           ,("depicted", "dpc")
           ,("depositor", "dpt")
           ,("designer", "dsr")
           ,("director", "drt")
           ,("dissertant", "dis")
           ,("distribution place", "dbp")
           ,("distributor", "dst")
           ,("donor", "dnr")
           ,("draftsman", "drm")
           ,("dubious author", "dub")
           ,("editor", "edt")
           ,("editor of compilation", "edc")
           ,("editor of moving image work", "edm")
           ,("electrician", "elg")
           ,("electrotyper", "elt")
           ,("enacting jurisdiction", "enj")
           ,("engineer", "eng")
           ,("engraver", "egr")
           ,("etcher", "etr")
           ,("event place", "evp")
           ,("expert", "exp")
           ,("facsimilist", "fac")
           ,("field director", "fld")
           ,("film director", "fmd")
           ,("film distributor", "fds")
           ,("film editor", "flm")
           ,("film producer", "fmp")
           ,("filmmaker", "fmk")
           ,("first party", "fpy")
           ,("forger", "frg")
           ,("former owner", "fmo")
           ,("funder", "fnd")
           ,("geographic information specialist", "gis")
           ,("honoree", "hnr")
           ,("host", "hst")
           ,("host institution", "his")
           ,("illuminator", "ilu")
           ,("illustrator", "ill")
           ,("inscriber", "ins")
           ,("instrumentalist", "itr")
           ,("interviewee", "ive")
           ,("interviewer", "ivr")
           ,("inventor", "inv")
           ,("issuing body", "isb")
           ,("judge", "jud")
           ,("jurisdiction governed", "jug")
           ,("laboratory", "lbr")
           ,("laboratory director", "ldr")
           ,("landscape architect", "lsa")
           ,("lead", "led")
           ,("lender", "len")
           ,("libelant", "lil")
           ,("libelant-appellant", "lit")
           ,("libelant-appellee", "lie")
           ,("libelee", "lel")
           ,("libelee-appellant", "let")
           ,("libelee-appellee", "lee")
           ,("librettist", "lbt")
           ,("licensee", "lse")
           ,("licensor", "lso")
           ,("lighting designer", "lgd")
           ,("lithographer", "ltg")
           ,("lyricist", "lyr")
           ,("manufacture place", "mfp")
           ,("manufacturer", "mfr")
           ,("marbler", "mrb")
           ,("markup editor", "mrk")
           ,("metadata contact", "mdc")
           ,("metal-engraver", "mte")
           ,("moderator", "mod")
           ,("monitor", "mon")
           ,("music copyist", "mcp")
           ,("musical director", "msd")
           ,("musician", "mus")
           ,("narrator", "nrt")
           ,("onscreen presenter", "osp")
           ,("opponent", "opn")
           ,("organizer of meeting", "orm")
           ,("originator", "org")
           ,("other", "oth")
           ,("owner", "own")
           ,("panelist", "pan")
           ,("papermaker", "ppm")
           ,("patent applicant", "pta")
           ,("patent holder", "pth")
           ,("patron", "pat")
           ,("performer", "prf")
           ,("permitting agency", "pma")
           ,("photographer", "pht")
           ,("plaintiff", "ptf")
           ,("plaintiff-appellant", "ptt")
           ,("plaintiff-appellee", "pte")
           ,("platemaker", "plt")
           ,("praeses", "pra")
           ,("presenter", "pre")
           ,("printer", "prt")
           ,("printer of plates", "pop")
           ,("printmaker", "prm")
           ,("process contact", "prc")
           ,("producer", "pro")
           ,("production company", "prn")
           ,("production designer", "prs")
           ,("production manager", "pmn")
           ,("production personnel", "prd")
           ,("production place", "prp")
           ,("programmer", "prg")
           ,("project director", "pdr")
           ,("proofreader", "pfr")
           ,("provider", "prv")
           ,("publication place", "pup")
           ,("publisher", "pbl")
           ,("publishing director", "pbd")
           ,("puppeteer", "ppt")
           ,("radio director", "rdd")
           ,("radio producer", "rpc")
           ,("recording engineer", "rce")
           ,("recordist", "rcd")
           ,("redaktor", "red")
           ,("renderer", "ren")
           ,("reporter", "rpt")
           ,("repository", "rps")
           ,("research team head", "rth")
           ,("research team member", "rtm")
           ,("researcher", "res")
           ,("respondent", "rsp")
           ,("respondent-appellant", "rst")
           ,("respondent-appellee", "rse")
           ,("responsible party", "rpy")
           ,("restager", "rsg")
           ,("restorationist", "rsr")
           ,("reviewer", "rev")
           ,("rubricator", "rbr")
           ,("scenarist", "sce")
           ,("scientific advisor", "sad")
           ,("screenwriter", "aus")
           ,("scribe", "scr")
           ,("sculptor", "scl")
           ,("second party", "spy")
           ,("secretary", "sec")
           ,("seller", "sll")
           ,("set designer", "std")
           ,("setting", "stg")
           ,("signer", "sgn")
           ,("singer", "sng")
           ,("sound designer", "sds")
           ,("speaker", "spk")
           ,("sponsor", "spn")
           ,("stage director", "sgd")
           ,("stage manager", "stm")
           ,("standards body", "stn")
           ,("stereotyper", "str")
           ,("storyteller", "stl")
           ,("supporting host", "sht")
           ,("surveyor", "srv")
           ,("teacher", "tch")
           ,("technical director", "tcd")
           ,("television director", "tld")
           ,("television producer", "tlp")
           ,("thesis advisor", "ths")
           ,("transcriber", "trc")
           ,("translator", "trl")
           ,("type designer", "tyd")
           ,("typographer", "tyg")
           ,("university place", "uvp")
           ,("videographer", "vdg")
           ,("witness", "wit")
           ,("wood engraver", "wde")
           ,("woodcutter", "wdc")
           ,("writer of accompanying material", "wam")
           ,("writer of added commentary", "wac")
           ,("writer of added lyrics", "wal")
           ,("writer of added text", "wat")
           ]

docTitle' :: Meta -> [Inline]
docTitle' meta = maybe [] go $ lookupMeta "title" meta
  where go (MetaString s) = [Str s]
        go (MetaInlines xs) = xs
        go (MetaBlocks [Para xs]) = xs
        go (MetaBlocks [Plain xs]) = xs
        go (MetaMap m) =
              case M.lookup "type" m of
                   Just x | stringify x == "main" ->
                              maybe [] go $ M.lookup "text" m
                   _ -> []
        go (MetaList xs) = concatMap go xs
        go _ = []

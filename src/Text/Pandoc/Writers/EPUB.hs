{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.EPUB
   Copyright   : Copyright (C) 2010-2023 John MacFarlane
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
import Control.Monad.State.Strict (State, StateT, evalState, evalStateT, get,
                                   gets, lift, modify)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing, mapMaybe, isJust, catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import System.FilePath (takeExtension, takeFileName, makeRelative)
import Text.HTML.TagSoup (Tag (TagOpen), fromAttrib, parseTags)
import Text.Pandoc.Builder (fromList, setMeta)
import Text.Pandoc.Writers.Shared (ensureValidXmlIdentifiers)
import Data.Tree (Tree(..))
import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class.PandocPure as P
import Text.Pandoc.Data (readDataFile)
import qualified Text.Pandoc.Class.PandocMonad as P
import Data.Time
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType)
import Text.Pandoc.URI (urlEncode)
import Text.Pandoc.Options (EPUBVersion (..), HTMLMathMethod (..),
                            ObfuscationMethod (NoObfuscation), WrapOption (..),
                            WriterOptions (..))
import Text.Pandoc.Shared (normalizeDate, renderTags',
                           stringify, uniqueIdent, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.UUID (getRandomUUID)
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Writers.HTML (writeHtmlStringForEPUB)
import Text.Printf (printf)
import Text.Pandoc.XML.Light
import Text.Pandoc.XML (escapeStringForXML)
import Text.DocTemplates (FromContext(lookupContext), Context(..),
                          ToContext(toVal), Val(..))
import Text.Pandoc.Chunks (splitIntoChunks, Chunk(..), ChunkedDoc(..),
                           SecInfo(..))

-- A Chapter includes a list of blocks.
newtype Chapter = Chapter [Block]
  deriving (Show)

data EPUBState = EPUBState {
        stMediaPaths  :: [(FilePath, (FilePath, Maybe Entry))]
      , stMediaNextId :: Int
      , stEpubSubdir  :: FilePath
      }

type E m = StateT EPUBState m

data EPUBMetadata = EPUBMetadata{
    epubIdentifier          :: [Identifier]
  , epubTitle               :: [Title]
  , epubDate                :: [Date]
  , epubLanguage            :: Text
  , epubCreator             :: [Creator]
  , epubContributor         :: [Creator]
  , epubSubject             :: [Subject]
  , epubDescription         :: Maybe Text
  , epubType                :: Maybe Text
  , epubFormat              :: Maybe Text
  , epubPublisher           :: Maybe Text
  , epubSource              :: Maybe Text
  , epubRelation            :: Maybe Text
  , epubCoverage            :: Maybe Text
  , epubRights              :: Maybe Text
  , epubBelongsToCollection :: Maybe Text
  , epubGroupPosition       :: Maybe Text
  , epubCoverImage          :: Maybe FilePath
  , epubStylesheets         :: [FilePath]
  , epubPageDirection       :: Maybe ProgressionDirection
  , epubIbooksFields        :: [(Text, Text)]
  , epubCalibreFields       :: [(Text, Text)]
  } deriving Show

data Date = Date{
    dateText  :: Text
  , dateEvent :: Maybe Text
  } deriving Show

data Creator = Creator{
    creatorText   :: Text
  , creatorRole   :: Maybe Text
  , creatorFileAs :: Maybe Text
  } deriving Show

data Identifier = Identifier{
    identifierText   :: Text
  , identifierScheme :: Maybe Text
  } deriving Show

data Title = Title{
    titleText   :: Text
  , titleFileAs :: Maybe Text
  , titleType   :: Maybe Text
  } deriving Show

data ProgressionDirection = LTR | RTL deriving Show

data Subject = Subject{
    subjectText      :: Text
  , subjectAuthority :: Maybe Text
  , subjectTerm      :: Maybe Text
  } deriving Show

dcName :: Text -> QName
dcName n = QName n Nothing (Just "dc")

dcNode :: Node t => Text -> t -> Element
dcNode = node . dcName

opfName :: Text -> QName
opfName n = QName n Nothing (Just "opf")

toId :: FilePath -> Text
toId = T.pack .
       map (\x -> if isAlphaNum x || x == '-' || x == '_'
                     then x
                     else '_') . takeFileName

removeNote :: Inline -> Inline
removeNote (Note _) = Str ""
removeNote x        = x

toVal' :: Text -> Val T.Text
toVal' = toVal

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
  elts <- case writerEpubMetadata opts of
            Nothing -> return []
            Just t -> case parseXMLContents (TL.fromStrict t) of
                          Left msg -> throwError $
                            PandocXMLError "epub metadata" msg
                          Right ns -> return (onlyElems ns)
  let md' = foldr addMetadataFromXML md elts
  let addIdentifier m =
       if null (epubIdentifier m)
          then do
            randomId <- getRandomUUID
            return $ m{ epubIdentifier = [Identifier (tshow randomId) Nothing] }
          else return m
  let addLanguage m =
       if T.null (epubLanguage m)
          then case lookupContext "lang" (writerVariables opts) of
                     Just x  -> return m{ epubLanguage = x }
                     Nothing -> do
                       mLang <- lift $ P.lookupEnv "LANG"
                       let localeLang =
                             case mLang of
                               Just lang ->
                                 T.map (\c -> if c == '_' then '-' else c) $
                                 T.takeWhile (/='.') lang
                               Nothing -> "en-US"
                       return m{ epubLanguage = localeLang }
          else return m
  let fixDate m =
       if null (epubDate m)
          then do
            currentTime <- lift P.getTimestamp
            return $ m{ epubDate = [ Date{
                             dateText = showDateTimeISO8601 currentTime
                           , dateEvent = Nothing } ] }
          else return m
  let addAuthor m =
       if any (\c -> creatorRole c == Just "aut") $ epubCreator m
          then return m
          else do
            let authors' = map stringify $ docAuthors meta
            let toAuthor name = Creator{ creatorText = name
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
  | name == "subject" = md{ epubSubject =
              Subject  { subjectText = strContent e
                       , subjectAuthority = getAttr "authority"
                       , subjectTerm = getAttr "term"
                       } : epubSubject md }
  | name == "description" = md { epubDescription = Just $ strContent e }
  | name == "type" = md { epubType = Just $ strContent e }
  | name == "format" = md { epubFormat = Just $ strContent e }
  | name == "publisher" = md { epubPublisher = Just $ strContent e }
  | name == "source" = md { epubSource = Just $ strContent e }
  | name == "relation" = md { epubRelation = Just $ strContent e }
  | name == "coverage" = md { epubCoverage = Just $ strContent e }
  | name == "rights" = md { epubRights = Just $ strContent e }
  | name == "belongs-to-collection" = md { epubBelongsToCollection = Just $ strContent e }
  | name == "group-position" = md { epubGroupPosition = Just $ strContent e }
  | otherwise = md
  where getAttr n = lookupAttr (opfName n) attrs
addMetadataFromXML e@(Element (QName "meta" _ _) attrs _ _) md =
  case getAttr "property" of
       Just s | "ibooks:" `T.isPrefixOf` s ->
                md{ epubIbooksFields = (T.drop 7 s, strContent e) :
                       epubIbooksFields md }
       _ -> case getAttr "name" of
                 Just s | "calibre:" `T.isPrefixOf` s ->
                   md{ epubCalibreFields =
                         (T.drop 8 s, fromMaybe "" $ getAttr "content") :
                          epubCalibreFields md }
                 _ -> md
  where getAttr n = lookupAttr (unqual n) attrs
addMetadataFromXML _ md = md

metaValueToString :: MetaValue -> Text
metaValueToString (MetaString s)    = s
metaValueToString (MetaInlines ils) = stringify ils
metaValueToString (MetaBlocks bs)   = stringify bs
metaValueToString (MetaBool True)   = "true"
metaValueToString (MetaBool False)  = "false"
metaValueToString _                 = ""

metaValueToPaths :: MetaValue -> [FilePath]
metaValueToPaths (MetaList xs) = map (T.unpack . metaValueToString) xs
metaValueToPaths x             = [T.unpack $ metaValueToString x]

getList :: T.Text -> Meta -> (MetaValue -> a) -> [a]
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

getCreator :: T.Text -> Meta -> [Creator]
getCreator s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Creator{ creatorText = maybe "" metaValueToString $ M.lookup "text" m
                  , creatorFileAs = metaValueToString <$> M.lookup "file-as" m
                  , creatorRole = metaValueToString <$> M.lookup "role" m }
        handleMetaValue mv = Creator (metaValueToString mv) Nothing Nothing

getDate :: T.Text -> Meta -> [Date]
getDate s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Date{ dateText = fromMaybe "" $
                   M.lookup "text" m >>= normalizeDate' . metaValueToString
               , dateEvent = metaValueToString <$> M.lookup "event" m }
        handleMetaValue mv = Date { dateText = fromMaybe "" $ normalizeDate' $ metaValueToString mv
                                  , dateEvent = Nothing }

getSubject :: T.Text -> Meta -> [Subject]
getSubject s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Subject{ subjectText = maybe "" metaValueToString $ M.lookup "text" m
                  , subjectAuthority = metaValueToString <$> M.lookup "authority" m
                  , subjectTerm = metaValueToString <$> M.lookup "term" m }
        handleMetaValue mv = Subject (metaValueToString mv) Nothing Nothing

metadataFromMeta :: WriterOptions -> Meta -> EPUBMetadata
metadataFromMeta opts meta = EPUBMetadata{
      epubIdentifier           = identifiers
    , epubTitle                = titles
    , epubDate                 = date
    , epubLanguage             = language
    , epubCreator              = creators
    , epubContributor          = contributors
    , epubSubject              = subjects
    , epubDescription          = description
    , epubType                 = epubtype
    , epubFormat               = format
    , epubPublisher            = publisher
    , epubSource               = source
    , epubRelation             = relation
    , epubCoverage             = coverage
    , epubRights               = rights
    , epubBelongsToCollection  = belongsToCollection
    , epubGroupPosition        = groupPosition
    , epubCoverImage           = coverImage
    , epubStylesheets          = stylesheets
    , epubPageDirection        = pageDirection
    , epubIbooksFields         = ibooksFields
    , epubCalibreFields        = calibreFields
    }
  where identifiers = getIdentifier meta
        titles = getTitle meta
        date = getDate "date" meta
        language = maybe "" metaValueToString $
           lookupMeta "language" meta `mplus` lookupMeta "lang" meta
        creators = getCreator "creator" meta
        contributors = getCreator "contributor" meta
        subjects = getSubject "subject" meta
        description = metaValueToString <$> lookupMeta "description" meta
        epubtype = metaValueToString <$> lookupMeta "type" meta
        format = metaValueToString <$> lookupMeta "format" meta
        publisher = metaValueToString <$> lookupMeta "publisher" meta
        source = metaValueToString <$> lookupMeta "source" meta
        relation = metaValueToString <$> lookupMeta "relation" meta
        coverage = metaValueToString <$> lookupMeta "coverage" meta
        rights = metaValueToString <$> lookupMeta "rights" meta
        belongsToCollection = metaValueToString <$> lookupMeta "belongs-to-collection" meta
        groupPosition = metaValueToString <$> lookupMeta "group-position" meta
        coverImage = T.unpack <$>
            lookupContext "epub-cover-image" (writerVariables opts)
            `mplus` (metaValueToString <$> lookupMeta "cover-image" meta)
        mCss = lookupMeta "css" meta <|> lookupMeta "stylesheet" meta
        stylesheets = maybe [] metaValueToPaths mCss ++
                      case lookupContext "css" (writerVariables opts) of
                         Just xs -> map T.unpack xs
                         Nothing ->
                           case lookupContext "css" (writerVariables opts) of
                             Just x  -> [T.unpack x]
                             Nothing -> []
        pageDirection = case T.toLower . metaValueToString <$>
                             lookupMeta "page-progression-direction" meta of
                              Just "ltr" -> Just LTR
                              Just "rtl" -> Just RTL
                              _          -> Nothing
        ibooksFields = case lookupMeta "ibooks" meta of
                            Just (MetaMap mp)
                               -> M.toList $ M.map metaValueToString mp
                            _  -> []
        calibreFields = case lookupMeta "calibre" meta of
                            Just (MetaMap mp)
                               -> M.toList $ M.map metaValueToString mp
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
  unless (T.all (\c -> isAscii c && isAlphaNum c) epubSubdir) $
    throwError $ PandocEpubSubdirectoryError epubSubdir
  let initState = EPUBState { stMediaPaths = []
                            , stMediaNextId = 0
                            , stEpubSubdir = T.unpack epubSubdir }
  evalStateT (pandocToEPUB epubVersion opts doc) initState

pandocToEPUB :: PandocMonad m
             => EPUBVersion
             -> WriterOptions
             -> Pandoc
             -> E m B.ByteString
pandocToEPUB version opts doc = do
  let doc' = ensureValidXmlIdentifiers doc
  -- handle pictures
  Pandoc meta blocks <- walkM (transformInline opts) doc' >>=
                        walkM transformBlock
  picEntries <- mapMaybe (snd . snd) <$> gets stMediaPaths

  epubSubdir <- gets stEpubSubdir
  let epub3 = version == EPUB3

  let writeHtml o = fmap (UTF8.fromTextLazy . TL.fromStrict) .
                      writeHtmlStringForEPUB version o
  metadata <- getEPUBMetadata opts meta

  -- retrieve title of document
  let plainTitle :: Text
      plainTitle = case docTitle' meta of
                        [] -> case epubTitle metadata of
                                   []    -> "UNTITLED"
                                   (x:_) -> titleText x
                        x  -> stringify x

  -- stylesheet
  stylesheets <- case epubStylesheets metadata of
                      [] -> (\x -> [B.fromChunks [x]]) <$>
                               readDataFile "epub.css"
                      fs -> mapM P.readFileLazy fs
  stylesheetEntries <- zipWithM
        (\bs n -> mkEntry ("styles/stylesheet" ++ show n ++ ".css") bs)
        stylesheets [(1 :: Int)..]

  -- writer variables
  let vars :: Context Text
      vars = Context $
               M.delete "css" .
               M.insert "epub3"
                 (toVal' $ if epub3 then "true" else "false") .
               M.insert "lang" (toVal' $ epubLanguage metadata)
             $ unContext $ writerVariables opts

  -- If True create paths relative to parent folder
  let cssvars :: Bool -> Context Text
      cssvars useprefix = Context $ M.insert "css"
                           (ListVal $ map
                             (\e -> toVal' $
                                (if useprefix then "../" else "") <>
                                T.pack
                                 (makeRelative epubSubdir (eRelativePath e)))
                             stylesheetEntries)
                             mempty

  -- Add additional options for the writer
  let opts' :: WriterOptions
      opts' = opts{ writerEmailObfuscation = NoObfuscation
                  , writerSectionDivs = True
                  , writerVariables = vars
                  , writerWrapText = WrapAuto }

  -- cover page
  (cpgEntry, cpicEntry) <- createCoverPage meta metadata opts' vars cssvars writeHtml plainTitle

  -- title page
  tpContent <- lift $ writeHtml opts'{
                                  writerVariables =
                                      Context (M.fromList [
                                        ("titlepage", toVal' "true"),
                                        ("body-type",  toVal' "frontmatter"),
                                        ("pagetitle", toVal $
                                            escapeStringForXML plainTitle)])
                                      <> cssvars True <> vars }
                               (Pandoc meta [])
  tpEntry <- mkEntry "text/title_page.xhtml" tpContent


  -- handle fonts
  let matchingGlob f = do
        xs <- lift $ P.glob f
        when (null xs) $
          report $ CouldNotFetchResource (T.pack f) "glob did not match any font files"
        return xs

  let mkFontEntry :: PandocMonad m => FilePath -> StateT EPUBState m Entry
      mkFontEntry f = mkEntry ("fonts/" ++ takeFileName f) =<<
                        lift (P.readFileLazy f)
  fontFiles <- concat <$> mapM matchingGlob (writerEpubFonts opts')
  fontEntries <- mapM mkFontEntry fontFiles

  -- body pages

  -- add level 1 header to beginning if none there
  let blocks' = addIdentifiers opts
                $ case blocks of
                    (Div _
                      (Header{}:_) : _) -> blocks
                    (Header 1 _ _ : _)  -> blocks
                    _                   -> Header 1 ("",["unnumbered"],[])
                                               (docTitle' meta) : blocks

  -- create the chapters
  let chunkedDoc = splitIntoChunks "ch%n.xhtml"
                     (writerNumberSections opts)
                     Nothing
                     (writerSplitLevel opts)
                     (Pandoc meta blocks')


  -- Create the chapter entries from the chapters.
  -- Also requires access to the extended writer options and context
  -- as well as the css Context and html writer
  chapterEntries <- createChapterEntries opts' vars cssvars writeHtml
                      (chunkedChunks chunkedDoc)



  -- contents.opf

  -- set page progression direction attribution
  let progressionDirection :: [(Text, Text)]
      progressionDirection = case epubPageDirection metadata of
                                  Just LTR | epub3 ->
                                    [("page-progression-direction", "ltr")]
                                  Just RTL | epub3 ->
                                    [("page-progression-direction", "rtl")]
                                  _  -> []

  -- incredibly inefficient (TODO):
  let containsMathML ent = epub3 &&
                           "<math" `isInfixOf`
        B8.unpack (fromEntry ent)
  let containsSVG ent    = epub3 &&
                           "<svg" `isInfixOf`
        B8.unpack (fromEntry ent)
  let props ent = ["mathml" | containsMathML ent] ++ ["svg" | containsSVG ent]

  let chapterNode ent = unode "item" !
                           ([("id", toId $ makeRelative epubSubdir
                                         $ eRelativePath ent),
                             ("href", T.pack $ makeRelative epubSubdir
                                      $ eRelativePath ent),
                             ("media-type", "application/xhtml+xml")]
                            ++ case props ent of
                                    [] -> []
                                    xs -> [("properties", T.unwords xs)])
                        $ ()

  let chapterRefNode ent = unode "itemref" !
                             [("idref", toId $ makeRelative epubSubdir
                                             $ eRelativePath ent)] $ ()
  let pictureNode ent = unode "item" !
                           [("id", toId $ makeRelative epubSubdir
                                        $ eRelativePath ent),
                            ("href", T.pack $ makeRelative epubSubdir
                                     $ eRelativePath ent),
                            ("media-type",
                               fromMaybe "application/octet-stream"
                               $ mediaTypeOf $ eRelativePath ent)] $ ()
  let fontNode ent = unode "item" !
                           [("id", toId $ makeRelative epubSubdir
                                        $ eRelativePath ent),
                            ("href", T.pack $ makeRelative epubSubdir
                                     $ eRelativePath ent),
                            ("media-type", fromMaybe "" $
                                  getMimeType $ eRelativePath ent)] $ ()

  -- The tocTitle is either the normal title or a specially configured title.
  let tocTitle = maybe plainTitle
                   metaValueToString $ lookupMeta "toc-title" meta
  currentTime <- lift P.getTimestamp

  -- Construct the contentsData
  let contentsData = UTF8.fromTextLazy $ TL.fromStrict $ ppTopElement $
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
             [ unode "item" ! [("id","stylesheet" <> tshow n)
                              , ("href", T.pack fp)
                              ,("media-type","text/css")] $ () |
                             (n :: Int, fp) <- zip [1..] (map
                               (makeRelative epubSubdir . eRelativePath)
                               stylesheetEntries) ] ++
             map chapterNode (cpgEntry ++
                               [tpEntry | writerEpubTitlePage opts] ++
                               chapterEntries) ++
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
              ++ ([unode "itemref" ! [("idref", "title_page_xhtml")
                                     ,("linear",
                                         case lookupMeta "title" meta of
                                               Just _  -> "yes"
                                               Nothing -> "no")] $ ()
                     | writerEpubTitlePage opts] ++
                  [unode "itemref" ! [("idref", "nav")] $ ()
                         | writerTableOfContents opts ] ++
                  map chapterRefNode chapterEntries)
          , unode "guide" $
             (unode "reference" !
                 [("type","toc"),("title", tocTitle),
                  ("href","nav.xhtml")] $ ()
             ) :
             [ unode "reference" !
                   [("type","cover")
                   ,("title","Cover")
                   ,("href","text/cover.xhtml")] $ ()
               | isJust (epubCoverImage metadata)
             ]
          ]
  -- Content should be stored in content.opf
  contentsEntry <- mkEntry "content.opf" contentsData

  -- toc.ncx
  -- Create the tocEntry from the metadata together with the sections and title.
  tocEntry <- createTocEntry opts' meta metadata plainTitle
                (chunkedTOC chunkedDoc)

  -- Create the navEntry using the metadata, all of the various writer options,
  -- the CSS and HTML helpers, the document and toc title as well as the epub version and all of the sections
  navEntry <- createNavEntry opts' meta metadata vars cssvars
                writeHtml tocTitle version (chunkedTOC chunkedDoc)

  -- mimetype
  mimetypeEntry <- mkEntry "mimetype" $
                        UTF8.fromStringLazy "application/epub+zip"

  -- container.xml
  let containerData = B.fromStrict $ UTF8.fromText $ ppTopElement $
       unode "container" ! [("version","1.0")
              ,("xmlns","urn:oasis:names:tc:opendocument:xmlns:container")] $
         unode "rootfiles" $
           unode "rootfile" ! [("full-path",
                    (if null epubSubdir
                        then ""
                        else T.pack epubSubdir <> "/") <> "content.opf")
               ,("media-type","application/oebps-package+xml")] $ ()
  containerEntry <- mkEntry "META-INF/container.xml" containerData

  -- com.apple.ibooks.display-options.xml
  let apple = B.fromStrict $ UTF8.fromText $ ppTopElement $
        unode "display_options" $
          unode "platform" ! [("name","*")] $
            unode "option" ! [("name","specified-fonts")] $ ("true" :: Text)
  appleEntry <- mkEntry "META-INF/com.apple.ibooks.display-options.xml" apple

  -- construct archive
  let archive = foldr addEntryToArchive emptyArchive $
                 [mimetypeEntry, containerEntry, appleEntry,
                  contentsEntry, tocEntry, navEntry] ++
                  [tpEntry | writerEpubTitlePage opts] ++
                  stylesheetEntries ++ picEntries ++ cpicEntry ++
                  cpgEntry ++ chapterEntries ++ fontEntries
  return $ fromArchive archive

-- | Function used during conversion from pandoc to EPUB to create the cover page.
-- The first Entry list is for the cover while the second one is for the cover image.
-- If no cover images are specified, empty lists will be returned.
createCoverPage :: PandocMonad m =>
                   Meta
                   -> EPUBMetadata
                   -> WriterOptions
                   -> Context Text
                   -> (Bool -> Context Text)
                   -> (WriterOptions -> Pandoc -> m B8.ByteString)
                   -> Text
                   -> StateT EPUBState m ([Entry], [Entry])
createCoverPage meta metadata opts' vars cssvars writeHtml plainTitle =
    case epubCoverImage metadata of
        Nothing   -> return ([],[])
        Just img  -> do
          let fp = takeFileName img
          -- retrieve cover image file
          mediaPaths <- gets (map (fst . snd) . stMediaPaths)
          coverImageName <-  -- see #4206
                if ("media/" <> fp) `elem` mediaPaths
                  then getMediaNextNewName (takeExtension fp)
                  else return fp
          -- image dimensions
          imgContent <- lift $ P.readFileLazy img
          (coverImageWidth, coverImageHeight) <-
                case imageSize opts' (B.toStrict imgContent) of
                  Right sz  -> return $ sizeInPixels sz
                  Left err' -> (0, 0) <$ report
                    (CouldNotDetermineImageSize (T.pack img) err')
          -- write the HTML. Use the cssvars, vars and additional writer options.
          cpContent <- lift $ writeHtml
                opts'{ writerVariables =
                      Context (M.fromList [
                        ("coverpage", toVal' "true"),
                        ("pagetitle", toVal $
                          escapeStringForXML plainTitle),
                        ("cover-image",
                          toVal' $ T.pack coverImageName),
                        ("cover-image-width", toVal' $
                          tshow coverImageWidth),
                        ("cover-image-height", toVal' $
                          tshow coverImageHeight)]) <>
                        cssvars True <> vars }
                (Pandoc meta [])

          coverEntry <- mkEntry "text/cover.xhtml" cpContent
          coverImageEntry <- mkEntry ("media/" ++ coverImageName)
                                imgContent

          return ( [ coverEntry ], [ coverImageEntry ] )

-- | Converts the given chapters to entries using the writeHtml function
-- and the various provided options
createChapterEntries :: PandocMonad m =>
                            WriterOptions
                            -> Context Text
                            -> (Bool -> Context Text)
                            -> (WriterOptions -> Pandoc -> StateT EPUBState m B8.ByteString)
                            -> [Chunk]
                            -> StateT EPUBState m [Entry]
createChapterEntries opts' vars cssvars writeHtml chapters = do
  -- Create an entry from the chapter with the provided number.
  -- chapToEntry :: Int -> Chapter -> StateT EPUBState m Entry
  let chapToEntry num chunk =
        mkEntry ("text/" ++ chunkPath chunk) =<<
        -- Combine all provided options
        writeHtml opts'{ writerVariables =
                            Context (M.fromList
                                     [("body-type", toVal' bodyType),
                                      ("pagetitle", toVal' $
                                           showChapter num)])
                            <> cssvars True <> vars } pdoc
         where bs = chunkContents chunk
               meta' = setMeta "title" (fromList
                         (walk removeNote
                          (chunkHeading chunk))) nullMeta
               (pdoc, bodyType) =
                 case bs of
                     (Div (_,"section":_,kvs) _ : _) ->
                       -- remove notes or we get doubled footnotes
                       (Pandoc meta' bs,
                        -- Check if the chapters belongs to the frontmatter,
                        -- backmatter of bodymatter defaulting to the body
                        case lookup "epub:type" kvs of
                             Nothing -> "bodymatter"
                             Just x
                               | x `elem` frontMatterTypes -> "frontmatter"
                               | x `elem` backMatterTypes  -> "backmatter"
                               | otherwise                 -> "bodymatter")
                     _                   -> (Pandoc meta' bs, "bodymatter")
               frontMatterTypes = ["prologue", "abstract", "acknowledgments",
                                   "copyright-page", "dedication",
                                   "credits", "keywords", "imprint",
                                   "contributors", "other-credits",
                                   "errata", "revision-history",
                                   "titlepage", "halftitlepage", "seriespage",
                                   "foreword", "preface", "frontispiece",
                                   "seriespage", "titlepage"]
               backMatterTypes = ["appendix", "colophon", "bibliography",
                                  "index"]

  zipWithM chapToEntry [1..] chapters

createTocEntry :: PandocMonad m =>
                  WriterOptions
               -> Meta
               -> EPUBMetadata
               -> Text
               -> Tree SecInfo
               -> StateT EPUBState m Entry
createTocEntry opts meta metadata plainTitle (Node _ secs) = do
  let mkNavPoint :: Tree SecInfo -> State Int (Maybe Element)
      mkNavPoint (Node secinfo subsecs)
        | secLevel secinfo > writerTOCDepth opts = return Nothing
        | otherwise = do
          n <- get
          modify (+ 1)
          subs <- catMaybes <$> mapM mkNavPoint subsecs
          let secnum' = case secNumber secinfo of
                          Just t -> t <> " "
                          Nothing -> ""
          let title' = secnum' <> stringify (secTitle secinfo)
          return $ Just $ unode "navPoint" !
                   [("id", "navPoint-" <> tshow n)] $
                      [ unode "navLabel" $ unode "text" title'
                      , unode "content" !
                          [("src", "text/" <> secPath secinfo)] $ ()
                      ] ++ subs

  let tpNode = unode "navPoint" !  [("id", "navPoint-0")] $
                  [ unode "navLabel" $ unode "text"
                     (stringify $ docTitle' meta)
                  , unode "content" ! [("src", "text/title_page.xhtml")]
                  $ () ]

  let navMap = evalState (catMaybes <$> mapM mkNavPoint secs) 1

  uuid <- case epubIdentifier metadata of
          (x:_) -> return $ identifierText x  -- use first identifier as UUID
          []    -> throwError $ PandocShouldNeverHappenError "epubIdentifier is null"  -- shouldn't happen
  let tocData = B.fromStrict $ UTF8.fromText $ ppTopElement $
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
          , unode "navMap" $ [tpNode | writerEpubTitlePage opts] ++ navMap
          ]
  mkEntry "toc.ncx" tocData


createNavEntry  :: PandocMonad m
                => WriterOptions
                -> Meta
                -> EPUBMetadata
                -> Context Text
                -> (Bool -> Context Text)
                -> (WriterOptions -> Pandoc -> m B8.ByteString)
                -> Text
                -> EPUBVersion
                -> Tree SecInfo
                -> StateT EPUBState m Entry
createNavEntry opts meta metadata
               vars cssvars writeHtml tocTitle version (Node _ secs) = do
  let mkItem :: Tree SecInfo -> State Int (Maybe Element)
      mkItem (Node secinfo subsecs)
        | secLevel secinfo > writerTOCDepth opts = return Nothing
        | otherwise = do
          n <- get
          modify (+ 1)
          subs <- catMaybes <$> mapM mkItem subsecs
          let secnum' = case secNumber secinfo of
                          Just num -> [Span ("", ["section-header-number"], [])
                                       [Str num] , Space]
                          Nothing -> []
          let title' = secnum' <> secTitle secinfo
          -- can't have <a> elements inside generated links...
          let clean (Link _ ils _) = Span ("", [], []) ils
              clean (Note _)       = Str ""
              clean x              = x
          let titRendered = case P.runPure
                                  (writeHtmlStringForEPUB version
                                    opts{ writerTemplate = Nothing }
                                    (Pandoc nullMeta
                                      [Plain $ walk clean title'])) of
                                  Left _  -> stringify title'
                                  Right x -> x
          let titElements = either (const []) id $
                                  parseXMLContents (TL.fromStrict titRendered)

          return $ Just $ unode "li" !
                   [("id", "toc-li-" <> tshow n)] $
                      (unode "a" !
                        [("href", "text/" <> secPath secinfo)]
                        $ titElements)
                       : case subs of
                           [] -> []
                           (_:_) -> [unode "ol" ! [("class","toc")] $ subs]

  let navtag = if version == EPUB3 then "nav" else "div"
  let tocBlocks = evalState (catMaybes <$> mapM mkItem secs) 1
  let navBlocks = [RawBlock (Format "html")
                  $ showElement $ -- prettyprinting introduces bad spaces
                   unode navtag ! ([("epub:type","toc") | version == EPUB3] ++
                                   [("id","toc")]) $
                    [ unode "h1" ! [("id","toc-title")] $ tocTitle
                    , unode "ol" ! [("class","toc")] $ tocBlocks ]]
  let landmarkItems = if version == EPUB3
                         then [ unode "li"
                                [ unode "a" ! [("href",
                                                  "text/title_page.xhtml")
                                               ,("epub:type", "titlepage")] $
                                  ("Title Page" :: Text) ] |
                                  writerEpubTitlePage opts ] ++
                              [ unode "li"
                                [ unode "a" ! [("href", "text/cover.xhtml")
                                              ,("epub:type", "cover")] $
                                  ("Cover" :: Text)] |
                                  isJust (epubCoverImage metadata)
                              ] ++
                              [ unode "li"
                                [ unode "a" ! [("href", "#toc")
                                              ,("epub:type", "toc")] $
                                    ("Table of Contents" :: Text)
                                ] | writerTableOfContents opts
                              ]
                         else []
  let landmarks = [RawBlock (Format "html") $ ppElement $
                    unode "nav" ! [("epub:type","landmarks")
                                  ,("id","landmarks")
                                  ,("hidden","hidden")] $
                    [ unode "ol" landmarkItems ]
                  | not (null landmarkItems)]
  navData <- lift $ writeHtml opts{ writerVariables =
                     Context (M.fromList [("navpage", toVal' "true")
                                         ,("body-type",  toVal' "frontmatter")
                                         ])
                     <> cssvars False <> vars }
            (Pandoc (setMeta "title"
                     (walk removeNote $ fromList $ docTitle' meta) nullMeta)
               (navBlocks ++ landmarks))
  -- Return
  mkEntry "nav.xhtml" navData

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
                  ++ modifiedNodes ++ belongsToCollectionNodes
        withIds base f = concat . zipWith f (map (\x -> base <>
                                                        T.cons '-' (tshow x))
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
        ibooksNode (k, v) = unode "meta" ! [("property", "ibooks:" <> k)] $ v
        calibreNodes = map calibreNode (epubCalibreFields md)
        calibreNode (k, v) = unode "meta" ! [("name", "calibre:" <> k),
                                             ("content", v)] $ ()
        languageNodes = [dcTag "language" $ epubLanguage md]
        creatorNodes = withIds "epub-creator" (toCreatorNode "creator") $
                       epubCreator md
        contributorNodes = withIds "epub-contributor"
                           (toCreatorNode "contributor") $ epubContributor md
        subjectNodes = withIds "subject" toSubjectNode $ epubSubject md
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
        belongsToCollectionNodes =
            maybe []
                (\belongsToCollection -> (unode "meta" !  [("property", "belongs-to-collection"), ("id", "epub-collection-1")] $ belongsToCollection )
                :
                [unode "meta" !  [("refines", "#epub-collection-1"), ("property", "collection-type")] $ ("series" :: Text) ])
                (epubBelongsToCollection md)++
            maybe []
                (\groupPosition -> [unode "meta" !  [("refines", "#epub-collection-1"), ("property", "group-position")] $ groupPosition ])
                (epubGroupPosition md)
        dcTag n s = unode ("dc:" <> n) s
        dcTag' n s = [dcTag n s]
        toIdentifierNode id' (Identifier txt scheme)
          | version == EPUB2 = [dcNode "identifier" !
              (("id",id') : maybe [] (\x -> [("opf:scheme", x)]) scheme) $
              txt]
          | otherwise = (dcNode "identifier" ! [("id",id')] $ txt) :
              maybe [] ((\x -> [unode "meta" !
                                [ ("refines","#" <> id')
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
                   [("refines","#" <> id'),("property","file-as")] $ x])
                   (creatorFileAs creator) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines","#" <> id'),("property","role"),
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
                   [("refines","#" <> id'),("property","file-as")] $ x])
                   (titleFileAs title) ++
              maybe [] (\x -> [unode "meta" !
                   [("refines","#" <> id'),("property","title-type")] $ x])
                   (titleType title)
        toDateNode id' date = [dcNode "date" !
             (("id",id') :
                maybe [] (\x -> [("opf:event",x)]) (dateEvent date)) $
                 dateText date]
        toSubjectNode id' subject
          | version == EPUB2 = [dcNode "subject" !
            [("id",id')] $ subjectText subject]
          | otherwise = (dcNode "subject" ! [("id",id')] $ subjectText subject)
            : maybe [] (\x -> (unode "meta" !
                    [("refines", "#" <> id'),("property","authority")] $ x) :
                    maybe [] (\y -> [unode "meta" !
                         [("refines", "#" <> id'),("property","term")] $ y])
                         (subjectTerm subject))
                    (subjectAuthority subject)
        schemeToOnix :: Text -> Text
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

showDateTimeISO8601 :: UTCTime -> Text
showDateTimeISO8601 = T.pack . formatTime defaultTimeLocale "%FT%TZ"

transformTag :: PandocMonad m
             => Tag T.Text
             -> E m (Tag T.Text)
transformTag tag@(TagOpen name attr)
  | name `elem` ["video", "source", "img", "audio"] &&
    isNothing (lookup "data-external" attr) = do
  let src = fromAttrib "src" tag
  let poster = fromAttrib "poster" tag
  newsrc <- modifyMediaRef $ T.unpack src
  newposter <- modifyMediaRef $ T.unpack poster
  let attr' = filter (\(x,_) -> x /= "src" && x /= "poster") attr ++
              [("src", "../" <> newsrc) | not (T.null newsrc)] ++
              [("poster", "../" <> newposter) | not (T.null newposter)]
  return $ TagOpen name attr'
transformTag tag = return tag

modifyMediaRef :: PandocMonad m
               => FilePath
               -> E m T.Text
modifyMediaRef "" = return ""
modifyMediaRef oldsrc = do
  media <- gets stMediaPaths
  case lookup oldsrc media of
         Just (n,_) -> return $ T.pack n
         Nothing    -> catchError
           (do (img, mbMime) <- P.fetchItem $ T.pack oldsrc
               let ext = maybe
                          (takeExtension (takeWhile (/='?') oldsrc))
                          (T.unpack . ("." <>))
                          (mbMime >>= extensionFromMimeType)
               newName <- getMediaNextNewName ext
               let newPath = "media/" ++ newName
               entry <- mkEntry newPath (B.fromChunks . (:[]) $ img)
               modify $ \st -> st{ stMediaPaths =
                            (oldsrc, (newPath, Just entry)):media}
               return $ T.pack newPath)
           (\e -> do
                report $ CouldNotFetchResource (T.pack oldsrc) (tshow e)
                return $ T.pack oldsrc)

getMediaNextNewName :: PandocMonad m => FilePath -> E m FilePath
getMediaNextNewName ext = do
  nextId <- gets stMediaNextId
  modify $ \st -> st { stMediaNextId = nextId + 1 }
  return $ "file" ++ show nextId ++ ext

isHtmlFormat :: Format -> Bool
isHtmlFormat (Format "html") = True
isHtmlFormat (Format "html4") = True
isHtmlFormat (Format "html5") = True
isHtmlFormat _ = False

transformBlock  :: PandocMonad m
                => Block
                -> E m Block
transformBlock (RawBlock fmt raw)
  | isHtmlFormat fmt = do
  let tags = parseTags raw
  tags' <- mapM transformTag tags
  return $ RawBlock fmt (renderTags' tags')
transformBlock b = return b

transformInline  :: PandocMonad m
                 => WriterOptions
                 -> Inline
                 -> E m Inline
transformInline _opts (Image attr@(_,_,kvs) lab (src,tit))
  | isNothing (lookup "external" kvs) = do
    newsrc <- modifyMediaRef $ T.unpack src
    return $ Image attr lab ("../" <> newsrc, tit)
transformInline opts x@(Math t m)
  | WebTeX url <- writerHTMLMathMethod opts = do
    newsrc <- modifyMediaRef (T.unpack (url <> urlEncode m))
    let mathclass = if t == DisplayMath then "display" else "inline"
    return $ Span ("",["math",mathclass],[])
                [Image nullAttr [x] ("../" <> newsrc, "")]
transformInline _opts (RawInline fmt raw)
  | isHtmlFormat fmt = do
  let tags = parseTags raw
  tags' <- mapM transformTag tags
  return $ RawInline fmt (renderTags' tags')
transformInline _ x = return x

(!) :: (t -> Element) -> [(Text, Text)] -> t -> Element
(!) f attrs n = add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) (f n)

mediaTypeOf :: FilePath -> Maybe MimeType
mediaTypeOf x =
  let mediaPrefixes = ["image", "video", "audio"] in
  case getMimeType x of
    Just y | any (`T.isPrefixOf` y) mediaPrefixes -> Just y
    _      -> Nothing

-- Returns filename for chapter number.
showChapter :: Int -> Text
showChapter = T.pack . printf "ch%03d.xhtml"

-- Add identifiers to any headers without them.
addIdentifiers :: WriterOptions -> [Block] -> [Block]
addIdentifiers opts bs = evalState (mapM go bs) Set.empty
 where go (Header n (ident,classes,kvs) ils) = do
         ids <- get
         let ident' = if T.null ident
                         then uniqueIdent (writerExtensions opts) ils ids
                         else ident
         modify $ Set.insert ident'
         return $ Header n (ident',classes,kvs) ils
       go x = return x

-- Variant of normalizeDate that allows partial dates: YYYY, YYYY-MM
normalizeDate' :: Text -> Maybe Text
normalizeDate' = go . T.strip
  where
    go xs
      | T.length xs == 4            -- YYY
      , T.all isDigit xs = Just xs
      | (y, s) <- T.splitAt 4 xs    -- YYY-MM
      , Just ('-', m) <- T.uncons s
      , T.length m == 2
      , T.all isDigit y && T.all isDigit m = Just xs
      | otherwise = normalizeDate xs

toRelator :: Text -> Maybe Text
toRelator x
  | x `elem` relators = Just x
  | otherwise         = lookup (T.toLower x) relatorMap

relators :: [Text]
relators = map snd relatorMap

relatorMap :: [(Text, Text)]
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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2010-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.EPUB
   Copyright   : Copyright (C) 2010-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to EPUB.
-}
module Text.Pandoc.Writers.EPUB ( writeEPUB2, writeEPUB3 ) where
import Codec.Archive.Zip (Entry, addEntryToArchive, eRelativePath, emptyArchive,
                          fromArchive, fromEntry, toEntry)
import Control.Monad (mplus, when, unless, zipWithM)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Strict (State, StateT, evalState, evalStateT, get, gets,
                            lift, modify, put)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text.Lazy as TL
import Data.Char (isAlphaNum, isDigit, toLower, isAscii)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Network.HTTP (urlEncode)
import System.FilePath (takeExtension, takeFileName)
import Text.HTML.TagSoup (Tag (TagOpen), fromAttrib, parseTags)
import Text.Pandoc.Builder (fromList, setMeta)
import Text.Pandoc.Class (PandocMonad, report)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Compat.Time
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (MimeType, extensionFromMimeType, getMimeType)
import Text.Pandoc.Options (EPUBVersion (..), HTMLMathMethod (..),
                            ObfuscationMethod (NoObfuscation), WrapOption (..),
                            WriterOptions (..))
import Text.Pandoc.Shared (hierarchicalize, normalizeDate, renderTags',
                           safeRead, stringify, trim, uniqueIdent)
import qualified Text.Pandoc.Shared as S (Element (..))
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.UUID (getUUID)
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Writers.HTML (writeHtmlStringForEPUB)
import Text.Printf (printf)
import Text.XML.Light (Attr (..), Element (..), Node (..), QName (..),
                       add_attrs, lookupAttr, node, onlyElems, parseXML,
                       ppElement, strContent, unode, unqual)

-- A Chapter includes a list of blocks and maybe a section
-- number offset.  Note, some chapters are unnumbered. The section
-- number is different from the index number, which will be used
-- in filenames, chapter0003.xhtml.
data Chapter = Chapter (Maybe [Int]) [Block]

data EPUBState = EPUBState {
        stMediaPaths  :: [(FilePath, (FilePath, Maybe Entry))]
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

getEPUBMetadata :: PandocMonad m => WriterOptions -> Meta -> E m EPUBMetadata
getEPUBMetadata opts meta = do
  let md = metadataFromMeta opts meta
  let elts = maybe [] (onlyElems . parseXML) $ writerEpubMetadata opts
  let md' = foldr addMetadataFromXML md elts
  let addIdentifier m =
       if null (epubIdentifier m)
          then do
            randomId <- (show . getUUID) <$> lift P.newStdGen
            return $ m{ epubIdentifier = [Identifier randomId Nothing] }
          else return m
  let addLanguage m =
       if null (epubLanguage m)
          then case lookup "lang" (writerVariables opts) of
                     Just x  -> return m{ epubLanguage = x }
                     Nothing -> do
                       mLang <- lift $ P.lookupEnv "LANG"
                       let localeLang =
                             case mLang of
                               Just lang ->
                                 map (\c -> if c == '_' then '-' else c) $
                                 takeWhile (/='.') lang
                               Nothing -> "en-US"
                       return m{ epubLanguage = localeLang }
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
addMetadataFromXML _ md = md

metaValueToString :: MetaValue -> String
metaValueToString (MetaString s)    = s
metaValueToString (MetaInlines ils) = stringify ils
metaValueToString (MetaBlocks bs)   = stringify bs
metaValueToString (MetaBool True)   = "true"
metaValueToString (MetaBool False)  = "false"
metaValueToString _                 = ""

metaValueToPaths:: MetaValue -> [FilePath]
metaValueToPaths (MetaList xs) = map metaValueToString xs
metaValueToPaths x             = [metaValueToString x]

getList :: String -> Meta -> (MetaValue -> a) -> [a]
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

getCreator :: String -> Meta -> [Creator]
getCreator s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Creator{ creatorText = maybe "" metaValueToString $ M.lookup "text" m
                  , creatorFileAs = metaValueToString <$> M.lookup "file-as" m
                  , creatorRole = metaValueToString <$> M.lookup "role" m }
        handleMetaValue mv = Creator (metaValueToString mv) Nothing Nothing

getDate :: String -> Meta -> [Date]
getDate s meta = getList s meta handleMetaValue
  where handleMetaValue (MetaMap m) =
           Date{ dateText = maybe "" id $
                   M.lookup "text" m >>= normalizeDate' . metaValueToString
               , dateEvent = metaValueToString <$> M.lookup "event" m }
        handleMetaValue mv = Date { dateText = maybe ""
                                      id $ normalizeDate' $ metaValueToString mv
                                  , dateEvent = Nothing }

simpleList :: String -> Meta -> [String]
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
        coverImage = lookup "epub-cover-image" (writerVariables opts) `mplus`
             (metaValueToString <$> lookupMeta "cover-image" meta)
        stylesheets = maybe [] id
                        (metaValueToPaths <$> lookupMeta "stylesheet" meta) ++
                      [f | ("css",f) <- writerVariables opts]
        pageDirection = case map toLower . metaValueToString <$>
                             lookupMeta "page-progression-direction" meta of
                              Just "ltr" -> Just LTR
                              Just "rtl" -> Just RTL
                              _          -> Nothing
        ibooksFields = case lookupMeta "ibooks" meta of
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
writeEPUB epubVersion opts doc =
  let initState = EPUBState { stMediaPaths = [] }
  in
    evalStateT (pandocToEPUB epubVersion opts doc)
      initState

pandocToEPUB :: PandocMonad m
             => EPUBVersion
             -> WriterOptions
             -> Pandoc
             -> E m B.ByteString
pandocToEPUB version opts doc@(Pandoc meta _) = do
  let epubSubdir = writerEpubSubdirectory opts
  -- sanity check on epubSubdir
  unless (all (\c -> isAscii c && isAlphaNum c) epubSubdir) $
    throwError $ PandocEpubSubdirectoryError epubSubdir
  let epub3 = version == EPUB3
  let writeHtml o = fmap (UTF8.fromTextLazy . TL.fromStrict) .
                      writeHtmlStringForEPUB version o
  epochtime <- floor <$> lift P.getPOSIXTime
  metadata <- getEPUBMetadata opts meta
  let mkEntry path content = toEntry path epochtime content

  -- stylesheet
  stylesheets <- case epubStylesheets metadata of
                      [] -> (\x -> [B.fromChunks [x]]) <$>
                             P.readDataFile (writerUserDataDir opts)
                             "epub.css"
                      fs -> mapM P.readFileLazy fs
  let stylesheetEntries = zipWith
        (\bs n -> mkEntry ("styles/stylesheet" ++ show n ++ ".css") bs)
        stylesheets [(1 :: Int)..]

  let vars = ("epub3", if epub3 then "true" else "false")
           : map (\e -> ("css", "../" ++ eRelativePath e)) stylesheetEntries
           ++ [(x,y) | (x,y) <- writerVariables opts, x /= "css"]
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
                       let coverImage = "media/" ++ takeFileName img
                       cpContent <- lift $ writeHtml
                            opts'{ writerVariables = ("coverpage","true"):vars }
                            (Pandoc meta [RawBlock (Format "html") $ "<div id=\"cover-image\">\n<img src=\"" ++ coverImage ++ "\" alt=\"cover image\" />\n</div>"])
                       imgContent <- lift $ P.readFileLazy img
                       return ( [mkEntry "cover.xhtml" cpContent]
                              , [mkEntry coverImage imgContent] )

  -- title page
  tpContent <- lift $ writeHtml opts'{
                                  writerVariables = ("titlepage","true"):vars }
                               (Pandoc meta [])
  let tpEntry = mkEntry "text/title_page.xhtml" tpContent

  -- handle pictures
  -- mediaRef <- P.newIORef []
  Pandoc _ blocks <- walkM (transformInline opts') doc >>=
                     walkM (transformBlock opts')
  picEntries <- (catMaybes . map (snd . snd)) <$> (gets stMediaPaths)
  -- handle fonts
  let matchingGlob f = do
        xs <- lift $ P.glob f
        when (null xs) $
          report $ CouldNotFetchResource f "glob did not match any font files"
        return xs
  let mkFontEntry f = mkEntry ("fonts/" ++ takeFileName f) <$>
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

  -- add level 1 header to beginning if none there
  let blocks' = addIdentifiers
                $ case blocks of
                      (Header 1 _ _ : _) -> blocks
                      _                  -> Header 1 ("",["unnumbered"],[])
                                                 (docTitle' meta) : blocks

  let chapterHeaderLevel = writerEpubChapterLevel opts

  let isChapterHeader (Header n _ _) = n <= chapterHeaderLevel
      isChapterHeader (Div ("",["references"],[]) (Header n _ _:_)) =
        n <= chapterHeaderLevel
      isChapterHeader _ = False

  let toChapters :: [Block] -> State [Int] [Chapter]
      toChapters []     = return []
      toChapters (Div ("",["references"],[]) bs@(Header 1 _ _:_) : rest) =
        toChapters (bs ++ rest)
      toChapters (Header n attr@(_,classes,_) ils : bs) = do
        nums <- get
        mbnum <- if "unnumbered" `elem` classes
                    then return Nothing
                    else case splitAt (n - 1) nums of
                              (ks, (m:_)) -> do
                                let nums' = ks ++ [m+1]
                                put nums'
                                return $ Just (ks ++ [m])
                                -- note, this is the offset not the sec number
                              (ks, []) -> do
                                let nums' = ks ++ [1]
                                put nums'
                                return $ Just ks
        let (xs,ys) = break isChapterHeader bs
        (Chapter mbnum (Header n attr ils : xs) :) `fmap` toChapters ys
      toChapters (b:bs) = do
        let (xs,ys) = break isChapterHeader bs
        (Chapter Nothing (b:xs) :) `fmap` toChapters ys

  let chapters' = evalState (toChapters blocks') []

  let extractLinkURL' :: Int -> Inline -> [(String, String)]
      extractLinkURL' num (Span (ident, _, _) _)
        | not (null ident) = [(ident, showChapter num ++ ('#':ident))]
      extractLinkURL' _ _ = []

  let extractLinkURL :: Int -> Block -> [(String, String)]
      extractLinkURL num (Div (ident, _, _) _)
        | not (null ident) = [(ident, showChapter num ++ ('#':ident))]
      extractLinkURL num (Header _ (ident, _, _) _)
        | not (null ident) = [(ident, showChapter num ++ ('#':ident))]
      extractLinkURL num b = query (extractLinkURL' num) b

  let reftable = concat $ zipWith (\(Chapter _ bs) num ->
                                    query (extractLinkURL num) bs)
                          chapters' [1..]

  let fixInternalReferences :: Inline -> Inline
      fixInternalReferences (Link attr lab ('#':xs, tit)) =
        case lookup xs reftable of
             Just ys -> Link attr lab (ys, tit)
             Nothing -> Link attr lab ('#':xs, tit)
      fixInternalReferences x = x

  -- internal reference IDs change when we chunk the file,
  -- so that '#my-header-1' might turn into 'chap004.xhtml#my-header'.
  -- this fixes that:
  let chapters = map (\(Chapter mbnum bs) ->
                         Chapter mbnum $ walk fixInternalReferences bs)
                 chapters'

  let chapToEntry num (Chapter mbnum bs) =
       mkEntry ("text/" ++ showChapter num) <$>
        (writeHtml opts'{ writerNumberOffset = fromMaybe [] mbnum }
         $ case bs of
             (Header _ _ xs : _) ->
               -- remove notes or we get doubled footnotes
               Pandoc (setMeta "title" (walk removeNote $ fromList xs)
                        nullMeta) bs
             _                   ->
               Pandoc nullMeta bs)

  chapterEntries <- lift $ zipWithM chapToEntry [1..] chapters

  -- incredibly inefficient (TODO):
  let containsMathML ent = epub3 &&
                           "<math" `isInfixOf` (B8.unpack $ fromEntry ent)
  let containsSVG ent    = epub3 &&
                           "<svg" `isInfixOf` (B8.unpack $ fromEntry ent)
  let props ent = ["mathml" | containsMathML ent] ++ ["svg" | containsSVG ent]

  -- contents.opf
  let chapterNode ent = unode "item" !
                           ([("id", toId $ eRelativePath ent),
                             ("href", eRelativePath ent),
                             ("media-type", "application/xhtml+xml")]
                            ++ case props ent of
                                    [] -> []
                                    xs -> [("properties", unwords xs)])
                        $ ()
  let chapterRefNode ent = unode "itemref" !
                             [("idref", toId $ eRelativePath ent)] $ ()
  let pictureNode ent = unode "item" !
                           [("id", toId $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", fromMaybe "application/octet-stream"
                               $ mediaTypeOf $ eRelativePath ent)] $ ()
  let fontNode ent = unode "item" !
                           [("id", toId $ eRelativePath ent),
                            ("href", eRelativePath ent),
                            ("media-type", fromMaybe "" $ getMimeType $ eRelativePath ent)] $ ()
  let plainTitle = case docTitle' meta of
                        [] -> case epubTitle metadata of
                                   []    -> "UNTITLED"
                                   (x:_) -> titleText x
                        x  -> stringify x

  let tocTitle = fromMaybe plainTitle $
                   metaValueToString <$> lookupMeta "toc-title" meta
  uuid <- case epubIdentifier metadata of
            (x:_) -> return $ identifierText x  -- use first identifier as UUID
            []    -> throwError $ PandocShouldNeverHappenError "epubIdentifier is null"  -- shouldn't happen
  currentTime <- lift $ P.getCurrentTime
  let contentsData = UTF8.fromStringLazy $ ppTopElement $
        unode "package" ! [("version", case version of
                                             EPUB2 -> "2.0"
                                             EPUB3 -> "3.0")
                          ,("xmlns","http://www.idpf.org/2007/opf")
                          ,("unique-identifier","epub-id-1")
                          ,("prefix","ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/")] $
          [ metadataElement version metadata currentTime
          , unode "manifest" $
             [ unode "item" ! [("id","ncx"), ("href","toc.ncx")
                              ,("media-type","application/x-dtbncx+xml")] $ ()
             , unode "item" ! ([("id","nav")
                               ,("href","nav.xhtml")
                               ,("media-type","application/xhtml+xml")] ++
                               [("properties","nav") | epub3 ]) $ ()
             ] ++
             [ (unode "item" ! [("id","style"), ("href",fp)
                              ,("media-type","text/css")] $ ()) |
                          fp <- map eRelativePath stylesheetEntries ] ++
             map chapterNode (cpgEntry ++ (tpEntry : chapterEntries)) ++
             (case cpicEntry of
                    []    -> []
                    (x:_) -> [add_attrs
                              [Attr (unqual "properties") "cover-image" | epub3]
                              (pictureNode x)]) ++
             map pictureNode picEntries ++
             map fontNode fontEntries
          , unode "spine" ! ([("toc","ncx")] ++ progressionDirection) $
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
                   [("type","cover"),("title","Cover"),("href","cover.xhtml")] $ () | epubCoverImage metadata /= Nothing
             ]
          ]
  let contentsEntry = mkEntry "content.opf" contentsData

  -- toc.ncx
  let secs = hierarchicalize blocks'

  let tocLevel = writerTOCDepth opts

  let navPointNode :: PandocMonad m
                   => (Int -> String -> String -> [Element] -> Element)
                   -> S.Element -> StateT Int m Element
      navPointNode formatter (S.Sec _ nums (ident,_,_) ils children) = do
        n <- get
        modify (+1)
        let showNums :: [Int] -> String
            showNums = intercalate "." . map show
        let tit' = stringify ils
        let tit = if writerNumberSections opts && not (null nums)
                     then showNums nums ++ " " ++ tit'
                     else tit'
        src <- case lookup ident reftable of
                 Just x  -> return x
                 Nothing -> throwError $ PandocSomeError $ ident ++ " not found in reftable"
        let isSec (S.Sec lev _ _ _ _) = lev <= tocLevel
            isSec _                   = False
        let subsecs = filter isSec children
        subs <- mapM (navPointNode formatter) subsecs
        return $ formatter n tit src subs
      navPointNode _ (S.Blk _) = throwError $ PandocSomeError "navPointNode encountered Blk"

  let navMapFormatter :: Int -> String -> String -> [Element] -> Element
      navMapFormatter n tit src subs = unode "navPoint" !
               [("id", "navPoint-" ++ show n)] $
                  [ unode "navLabel" $ unode "text" tit
                  , unode "content" ! [("src", "text/" ++ src)] $ ()
                  ] ++ subs

  let tpNode = unode "navPoint" !  [("id", "navPoint-0")] $
                  [ unode "navLabel" $ unode "text" (stringify $ docTitle' meta)
                  , unode "content" ! [("src","text/title_page.xhtml")] $ () ]

  navMap <- lift $ evalStateT (mapM (navPointNode navMapFormatter) secs) 1
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
          , unode "docTitle" $ unode "text" $ plainTitle
          , unode "navMap" $
              tpNode : navMap
          ]
  let tocEntry = mkEntry "toc.ncx" tocData

  let navXhtmlFormatter :: Int -> String -> String -> [Element] -> Element
      navXhtmlFormatter n tit src subs = unode "li" !
                                       [("id", "toc-li-" ++ show n)] $
                                            (unode "a" ! [("href", "text/" ++
                                                                   src)]
                                             $ tit)
                                            : case subs of
                                                 []    -> []
                                                 (_:_) -> [unode "ol" ! [("class","toc")] $ subs]

  let navtag = if epub3 then "nav" else "div"
  tocBlocks <- lift $ evalStateT (mapM (navPointNode navXhtmlFormatter) secs) 1
  let navBlocks = [RawBlock (Format "html") $ ppElement $
                   unode navtag ! ([("epub:type","toc") | epub3] ++
                                   [("id","toc")]) $
                    [ unode "h1" ! [("id","toc-title")] $ tocTitle
                    , unode "ol" ! [("class","toc")] $ tocBlocks ]]
  let landmarks = if epub3
                     then [RawBlock (Format "html") $ ppElement $
                            unode "nav" ! [("epub:type","landmarks")
                                          ,("hidden","hidden")] $
                            [ unode "ol" $
                              [ unode "li"
                                [ unode "a" ! [("href", "cover.xhtml")
                                              ,("epub:type", "cover")] $
                                  "Cover"] |
                                  epubCoverImage metadata /= Nothing
                              ] ++
                              [ unode "li"
                                [ unode "a" ! [("href", "#toc")
                                              ,("epub:type", "toc")] $
                                    "Table of contents"
                                ] | writerTableOfContents opts
                              ]
                            ]
                          ]
                     else []
  navData <- lift $ writeHtml opts'{ writerVariables = ("navpage","true"):
                     -- remove the leading ../ from stylesheet paths:
                     map (\(k,v) -> if k == "css"
                                       then (k, drop 3 v)
                                       else (k, v)) vars }
            (Pandoc (setMeta "title"
                     (walk removeNote $ fromList $ docTitle' meta) nullMeta)
               (navBlocks ++ landmarks))
  let navEntry = mkEntry "nav.xhtml" navData

  -- mimetype
  let mimetypeEntry = mkEntry "mimetype" $ UTF8.fromStringLazy "application/epub+zip"

  -- container.xml
  let containerData = UTF8.fromStringLazy $ ppTopElement $
       unode "container" ! [("version","1.0")
              ,("xmlns","urn:oasis:names:tc:opendocument:xmlns:container")] $
         unode "rootfiles" $
           unode "rootfile" ! [("full-path",
               epubSubdir ++ ['/' | not (null epubSubdir)] ++ "content.opf")
               ,("media-type","application/oebps-package+xml")] $ ()
  let containerEntry = mkEntry "META-INF/container.xml" containerData

  -- com.apple.ibooks.display-options.xml
  let apple = UTF8.fromStringLazy $ ppTopElement $
        unode "display_options" $
          unode "platform" ! [("name","*")] $
            unode "option" ! [("name","specified-fonts")] $ "true"
  let appleEntry = mkEntry "META-INF/com.apple.ibooks.display-options.xml" apple

  let addEpubSubdir :: Entry -> Entry
      addEpubSubdir e = e{ eRelativePath =
        epubSubdir ++ ['/' | not (null epubSubdir)] ++ eRelativePath e }
  -- construct archive
  let archive = foldr addEntryToArchive emptyArchive $
                 [mimetypeEntry, containerEntry, appleEntry] ++
                 map addEpubSubdir
                 (tpEntry : contentsEntry : tocEntry : navEntry :
                  (stylesheetEntries ++ picEntries ++ cpicEntry ++
                   cpgEntry ++ chapterEntries ++ fontEntries))
  return $ fromArchive archive

metadataElement :: EPUBVersion -> EPUBMetadata -> UTCTime -> Element
metadataElement version md currentTime =
  unode "metadata" ! [("xmlns:dc","http://purl.org/dc/elements/1.1/")
                     ,("xmlns:opf","http://www.idpf.org/2007/opf")] $ mdNodes
  where mdNodes = identifierNodes ++ titleNodes ++ dateNodes
                  ++ languageNodes ++ ibooksNodes
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
               (showDateTimeISO8601 currentTime) | version == EPUB3 ]
        dcTag n s = unode ("dc:" ++ n) s
        dcTag' n s = [dcTag n s]
        toIdentifierNode id' (Identifier txt scheme)
          | version == EPUB2 = [dcNode "identifier" !
              ([("id",id')] ++ maybe [] (\x -> [("opf:scheme", x)]) scheme) $
              txt]
          | otherwise = [dcNode "identifier" ! [("id",id')] $ txt] ++
              maybe [] (\x -> [unode "meta" !
                  [("refines",'#':id'),("property","identifier-type"),
                   ("scheme","onix:codelist5")] $ x])
                (schemeToOnix `fmap` scheme)
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
             => WriterOptions
             -- -> IORef [(FilePath, (FilePath, Maybe Entry))] -- ^ (oldpath, newpath, entry) media
             -> Tag String
             -> E m (Tag String)
transformTag opts tag@(TagOpen name attr)
  | name `elem` ["video", "source", "img", "audio"] &&
    lookup "data-external" attr == Nothing = do
  let src = fromAttrib "src" tag
  let poster = fromAttrib "poster" tag
  newsrc <- modifyMediaRef opts src
  newposter <- modifyMediaRef opts poster
  let attr' = filter (\(x,_) -> x /= "src" && x /= "poster") attr ++
              [("src", newsrc) | not (null newsrc)] ++
              [("poster", newposter) | not (null newposter)]
  return $ TagOpen name attr'
transformTag _ tag = return tag

modifyMediaRef :: PandocMonad m
               => WriterOptions
               -> FilePath
               -> E m FilePath
modifyMediaRef _ "" = return ""
modifyMediaRef opts oldsrc = do
  media <- gets stMediaPaths
  case lookup oldsrc media of
         Just (n,_) -> return n
         Nothing    -> catchError
           (do (img, mbMime) <- P.fetchItem (writerSourceURL opts) oldsrc
               let new = "media/file" ++ show (length media) ++
                          fromMaybe (takeExtension (takeWhile (/='?') oldsrc))
                          (('.':) <$> (mbMime >>= extensionFromMimeType))
               epochtime <- floor `fmap` lift P.getPOSIXTime
               let entry = toEntry new epochtime (B.fromChunks . (:[]) $ img)
               modify $ \st -> st{ stMediaPaths =
                            (oldsrc, (new, Just entry)):media}
               return new)
           (\e -> do
                report $ CouldNotFetchResource oldsrc (show e)
                return oldsrc)

transformBlock  :: PandocMonad m
                => WriterOptions
                -- -> IORef [(FilePath, (FilePath, Maybe Entry))] -- ^ (oldpath, newpath, entry) media
                -> Block
                -> E m Block
transformBlock opts (RawBlock fmt raw)
  | fmt == Format "html" = do
  let tags = parseTags raw
  tags' <- mapM (transformTag opts)  tags
  return $ RawBlock fmt (renderTags' tags')
transformBlock _ b = return b

transformInline  :: PandocMonad m
                 => WriterOptions
                 -- -> IORef [(FilePath, (FilePath, Maybe Entry))] -- ^ (oldpath, newpath) media
                 -> Inline
                 -> E m Inline
transformInline opts (Image attr lab (src,tit)) = do
    newsrc <- modifyMediaRef opts src
    return $ Image attr lab ("../" ++ newsrc, tit)
transformInline opts (x@(Math t m))
  | WebTeX url <- writerHTMLMathMethod opts = do
    newsrc <- modifyMediaRef opts (url ++ urlEncode m)
    let mathclass = if t == DisplayMath then "display" else "inline"
    return $ Span ("",["math",mathclass],[])
                [Image nullAttr [x] ("../" ++ newsrc, "")]
transformInline opts (RawInline fmt raw)
  | fmt == Format "html" = do
  let tags = parseTags raw
  tags' <- mapM (transformTag opts) tags
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
                   in  case safeRead ('\'':'\\':ds ++ "'") of
                          Just x  -> x : unEntity rest
                          Nothing -> '&':'#':unEntity xs
        unEntity (x:xs) = x : unEntity xs

mediaTypeOf :: FilePath -> Maybe MimeType
mediaTypeOf x =
  let mediaPrefixes = ["image", "video", "audio"] in
  case getMimeType x of
    Just y | any (`isPrefixOf` y) mediaPrefixes -> Just y
    _      -> Nothing

-- Returns filename for chapter number.
showChapter :: Int -> String
showChapter = printf "ch%03d.xhtml"

-- Add identifiers to any headers without them.
addIdentifiers :: [Block] -> [Block]
addIdentifiers bs = evalState (mapM go bs) Set.empty
 where go (Header n (ident,classes,kvs) ils) = do
         ids <- get
         let ident' = if null ident
                         then uniqueIdent ils ids
                         else ident
         modify $ Set.insert ident'
         return $ Header n (ident',classes,kvs) ils
       go x = return x

-- Variant of normalizeDate that allows partial dates: YYYY, YYYY-MM
normalizeDate' :: String -> Maybe String
normalizeDate' xs =
  let xs' = trim xs in
  case xs' of
       [y1,y2,y3,y4] | all isDigit [y1,y2,y3,y4] -> Just xs'     -- YYYY
       [y1,y2,y3,y4,'-',m1,m2] | all isDigit [y1,y2,y3,y4,m1,m2]  -- YYYY-MM
                                                 -> Just xs'
       _                                         -> normalizeDate xs'

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
docTitle' meta = fromMaybe [] $ go <$> lookupMeta "title" meta
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

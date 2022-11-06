{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Citeproc
  ( processCitations,
    getReferences,
  )
where

import Citeproc
import Citeproc.Pandoc ()
import Text.Pandoc.Citeproc.Locator (parseLocator, toLocatorMap,
                                     LocatorInfo(..))
import Text.Pandoc.Citeproc.CslJson (cslJsonToReferences)
import Text.Pandoc.Citeproc.BibTeX (readBibtexString, Variant(..))
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Readers.RIS (readRIS)
import Text.Pandoc.Citeproc.MetaValue (metaValueToReference, metaValueToText)
import Text.Pandoc.Readers.Markdown (yamlToRefs)
import Text.Pandoc.Builder (Inlines, Many(..), deleteMeta, setMeta)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition as Pandoc
import Text.Pandoc.Class (PandocMonad(..), getResourcePath, getUserDataDir,
                          fetchItem, report, setResourcePath)
import Text.Pandoc.Data (readDataFile)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Extensions (pandocExtensions)
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options (ReaderOptions(..))
import Text.Pandoc.Shared (stringify, tshow)
import Data.Containers.ListUtils (nubOrd)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk (query, walk, walkM)
import Control.Applicative ((<|>))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (State, evalState, get, put, runState)
import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Char (isPunctuation, isUpper)
import Data.Default (Default(def))
import qualified Data.Foldable as Foldable
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord ()
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeExtension)
import Safe (lastMay, initSafe)

processCitations  :: PandocMonad m => Pandoc -> m Pandoc
processCitations (Pandoc meta bs) = do
  style <- getStyle (Pandoc meta bs)
  mblang <- getCiteprocLang meta
  let locale = Citeproc.mergeLocales mblang style

  let addQuoteSpan (Quoted _ xs) = Span ("",["csl-quoted"],[]) xs
      addQuoteSpan x = x
  refs <- map (walk addQuoteSpan) <$>
          getReferences (Just locale) (Pandoc meta bs)

  let otherIdsMap = foldr (\ref m ->
                             case T.words . extractText <$>
                                  M.lookup "other-ids"
                                      (referenceVariables ref) of
                                Nothing  -> m
                                Just ids -> foldr
                                  (\id' ->
                                    M.insert id' (referenceId ref)) m ids)
                          M.empty refs
  let meta' = deleteMeta "nocite" meta
  let citations = getCitations locale otherIdsMap $ Pandoc meta' bs


  let linkCites = maybe False truish $ lookupMeta "link-citations" meta
  let linkBib = maybe True truish $ lookupMeta "link-bibliography" meta
  let opts = defaultCiteprocOptions{ linkCitations = linkCites
                                   , linkBibliography = linkBib }
  let result = Citeproc.citeproc opts style mblang refs citations
  mapM_ (report . CiteprocWarning) (resultWarnings result)
  let sopts = styleOptions style
  let classes = "references" : -- TODO remove this or keep for compatibility?
                "csl-bib-body" :
                ["hanging-indent" | styleHangingIndent sopts]
  let refkvs = (case styleEntrySpacing sopts of
                   Just es | es > 0 -> (("entry-spacing",T.pack $ show es):)
                   _ -> id) .
               (case styleLineSpacing sopts of
                   Just ls | ls > 1 -> (("line-spacing",T.pack $ show ls):)
                   _ -> id) $ []
  let bibs = mconcat $ map (\(ident, out) ->
                     B.divWith ("ref-" <> ident,["csl-entry"],[]) . B.para .
                         insertSpace $ out)
                      (resultBibliography result)
  let moveNotes = maybe (styleIsNoteStyle sopts) truish
                   (lookupMeta "notes-after-punctuation" meta)
  let cits = resultCitations result

  let metanocites = lookupMeta "nocite" meta
  let Pandoc meta'' bs' =
         maybe id (setMeta "nocite") metanocites .
         walk (mvPunct moveNotes locale) .
         (if styleIsNoteStyle sopts
             then walk addNote .  walk deNote
             else id) .
         evalState (walkM insertResolvedCitations $ Pandoc meta' bs)
         $ cits
  return $ walk removeQuoteSpan
         $ insertRefs refkvs classes (B.toList bibs)
         $ Pandoc meta'' bs'

removeQuoteSpan :: Inline -> Inline
removeQuoteSpan (Span ("",["csl-quoted"],[]) xs) = Span nullAttr xs
removeQuoteSpan x = x

-- | Retrieve the CSL style specified by the csl or citation-style
-- metadata field in a pandoc document, or the default CSL style
-- if none is specified.  Retrieve the parent style
-- if the style is a dependent style.  Add abbreviations defined
-- in an abbreviation file if one has been specified.
getStyle :: PandocMonad m => Pandoc -> m (Style Inlines)
getStyle (Pandoc meta _) = do
  let cslfile = (lookupMeta "csl" meta <|> lookupMeta "citation-style" meta)
                >>= metaValueToText

  let getFile defaultExtension fp = do
        oldRp <- getResourcePath
        mbUdd <- getUserDataDir
        setResourcePath $ oldRp ++ maybe []
                                   (\u -> [u <> "/csl",
                                           u <> "/csl/dependent"]) mbUdd
        let fp' = if T.any (=='.') fp || "data:" `T.isPrefixOf` fp
                     then fp
                     else fp <> defaultExtension
        (result, _) <- fetchItem fp'
        setResourcePath oldRp
        return result

  let getCslDefault = readDataFile "default.csl"

  cslContents <- UTF8.toText <$> maybe getCslDefault (getFile ".csl") cslfile

  let abbrevFile = lookupMeta "citation-abbreviations" meta >>= metaValueToText

  mbAbbrevs <- case abbrevFile of
                 Nothing -> return Nothing
                 Just fp -> do
                   rawAbbr <- getFile ".json" fp
                   case eitherDecode (L.fromStrict rawAbbr) of
                     Left err -> throwError $ PandocCiteprocError $
                                 CiteprocParseError $
                                 "Could not parse abbreviations file " <> fp
                                 <> "\n" <> T.pack err
                     Right abbr -> return $ Just abbr

  let getParentStyle url = do
        -- first, try to retrieve the style locally, then use HTTP.
        let basename = T.takeWhileEnd (/='/') url
        UTF8.toText <$>
          catchError (getFile ".csl" basename) (\_ -> fst <$> fetchItem url)

  styleRes <- Citeproc.parseStyle getParentStyle cslContents
  case styleRes of
     Left err    -> throwError $ PandocAppError $ prettyCiteprocError err
     Right style -> return style{ styleAbbreviations = mbAbbrevs }


-- Retrieve citeproc lang based on metadata.
getCiteprocLang :: PandocMonad m => Meta -> m (Maybe Lang)
getCiteprocLang meta = maybe (return Nothing) bcp47LangToIETF
  ((lookupMeta "lang" meta <|> lookupMeta "locale" meta) >>= metaValueToText)

-- | Get references defined inline in the metadata and via an external
-- bibliography.  Only references that are actually cited in the document
-- (either with a genuine citation or with `nocite`) are returned.
-- URL variables are converted to links.
getReferences :: PandocMonad m
              => Maybe Locale -> Pandoc -> m [Reference Inlines]
getReferences mblocale (Pandoc meta bs) = do
  locale <- case mblocale of
                Just l  -> return l
                Nothing -> do
                  mblang <- getCiteprocLang meta
                  case mblang of
                    Just lang -> return $ either mempty id $ getLocale lang
                    Nothing   -> return mempty

  let getCiteId (Cite cs _) = Set.fromList $ map B.citationId cs
      getCiteId _ = mempty
  let metanocites = lookupMeta "nocite" meta
  let nocites = maybe mempty (query getCiteId) metanocites
  let citeIds = query getCiteId (Pandoc meta bs)
  let idpred = if "*" `Set.member` nocites
                  then const True
                  else (`Set.member` citeIds)
  let inlineRefs = case lookupMeta "references" meta of
                    Just (MetaList rs) ->
                      filter (idpred . unItemId . referenceId)
                         $  mapMaybe metaValueToReference rs
                    _                  -> []
  externalRefs <- case lookupMeta "bibliography" meta of
                    Just (MetaList xs) ->
                      mconcat <$>
                        mapM (getRefsFromBib locale idpred)
                          (mapMaybe metaValueToText xs)
                    Just x ->
                      case metaValueToText x of
                        Just fp -> getRefsFromBib locale idpred fp
                        Nothing -> return []
                    Nothing -> return []
  return $ map legacyDateRanges (externalRefs ++ inlineRefs)
            -- note that inlineRefs can override externalRefs



-- If we have a span.csl-left-margin followed by span.csl-right-inline,
-- we insert a space. This ensures that they will be separated by a space,
-- even in formats that don't have special handling for the display spans.
insertSpace :: Inlines -> Inlines
insertSpace ils =
  case Seq.viewl (unMany ils) of
    (Span ("",["csl-left-margin"],[]) xs) Seq.:< rest ->
      case Seq.lookup 0 rest of
        Just (Span ("",["csl-right-inline"],[]) _) ->
          Many $
            Span ("",["csl-left-margin"],[]) (xs ++ case lastMay xs of
                                                      Just Space -> []
                                                      _          -> [Space])
            Seq.<| rest
        _ -> ils
    _ -> ils

getRefsFromBib :: PandocMonad m
               => Locale -> (Text -> Bool) -> Text -> m [Reference Inlines]
getRefsFromBib locale idpred fp = do
  (raw, mt) <- fetchItem fp
  case getBibliographyFormat (T.unpack fp) mt of
    Just f -> getRefs locale f idpred (Just fp) raw
    Nothing -> throwError $ PandocAppError $
                 "Could not determine bibliography format for " <> fp

getRefs :: PandocMonad m
        => Locale
        -> BibFormat
        -> (Text -> Bool)
        -> Maybe Text
        -> ByteString
        -> m [Reference Inlines]
getRefs locale format idpred mbfp raw = do
  let err' = throwError .
             PandocBibliographyError (fromMaybe mempty mbfp)
  case format of
    Format_bibtex ->
      either (err' . tshow) return .
        readBibtexString Bibtex locale idpred . UTF8.toText $ raw
    Format_biblatex ->
      either (err' . tshow) return .
        readBibtexString Biblatex locale idpred . UTF8.toText $ raw
    Format_json ->
      either (err' . T.pack)
             (return . filter (idpred . unItemId . referenceId)) .
        cslJsonToReferences $ raw
    Format_yaml -> do
      rs <- yamlToRefs idpred
              def{ readerExtensions = pandocExtensions }
              (T.unpack <$> mbfp)
              raw
      return $ mapMaybe metaValueToReference rs
    Format_ris -> do
      Pandoc meta _ <- readRIS def (UTF8.toText raw)
      case lookupMeta "references" meta of
        Just (MetaList rs) -> return $ mapMaybe metaValueToReference rs
        _ -> return []

-- assumes we walk in same order as query
insertResolvedCitations :: Inline -> State [Inlines] Inline
insertResolvedCitations (Cite cs ils) = do
  resolved <- get
  case resolved of
    [] -> return (Cite cs ils)
    (x:xs) -> do
      put xs
      return $ Cite cs (B.toList x)
insertResolvedCitations x = return x

getCitations :: Locale
             -> M.Map Text ItemId
             -> Pandoc
             -> [Citeproc.Citation Inlines]
getCitations locale otherIdsMap = Foldable.toList . query getCitation
 where
  getCitation (Cite cs _fallback) = Seq.singleton $
    Citeproc.Citation { Citeproc.citationId = Nothing
                      , Citeproc.citationNoteNumber =
                          case cs of
                            []    -> Nothing
                            (Pandoc.Citation{ Pandoc.citationNoteNum = n }:
                               _) | n > 0     -> Just n
                                  | otherwise -> Nothing
                      , Citeproc.citationItems =
                           fromPandocCitations locale otherIdsMap cs
                      }
  getCitation _ = mempty

fromPandocCitations :: Locale
                    -> M.Map Text ItemId
                    -> [Pandoc.Citation]
                    -> [CitationItem Inlines]
fromPandocCitations locale otherIdsMap = concatMap go
 where
  locmap = toLocatorMap locale
  go c =
    let (mblocinfo, suffix) = parseLocator locmap (citationSuffix c)
        cit = CitationItem
               { citationItemId = fromMaybe
                   (ItemId $ Pandoc.citationId c)
                   (M.lookup (Pandoc.citationId c) otherIdsMap)
               , citationItemLabel = locatorLabel <$> mblocinfo
               , citationItemLocator = locatorLoc <$> mblocinfo
               , citationItemType = NormalCite
               , citationItemPrefix = case citationPrefix c of
                                        [] -> Nothing
                                        ils -> Just $ B.fromList ils <>
                                                      B.space
               , citationItemSuffix = case suffix of
                                        [] -> Nothing
                                        ils -> Just $ B.fromList ils
               , citationItemData = Nothing }
     in if Pandoc.citationId c == "*"
           then []
           else
             case citationMode c of
                  AuthorInText   -> [ cit{ citationItemType = AuthorOnly
                                         , citationItemSuffix = Nothing }
                                    , cit{ citationItemType =
                                              Citeproc.SuppressAuthor
                                         , citationItemPrefix = Nothing } ]
                  NormalCitation -> [ cit ]
                  Pandoc.SuppressAuthor
                                 -> [ cit{ citationItemType =
                                              Citeproc.SuppressAuthor } ]



data BibFormat =
    Format_biblatex
  | Format_bibtex
  | Format_json
  | Format_yaml
  | Format_ris
  deriving (Show, Eq, Ord)

getBibliographyFormat :: FilePath -> Maybe MimeType -> Maybe BibFormat
getBibliographyFormat fp mbmime = do
  let ext = takeExtension fp
  case ext of
    ".biblatex" -> pure Format_biblatex
    ".bibtex"   -> pure Format_bibtex
    ".bib"      -> pure Format_biblatex
    ".json"     -> pure Format_json
    ".yaml"     -> pure Format_yaml
    ".yml"      -> pure Format_yaml
    ".ris"      -> pure Format_ris
    _ -> do
      mime <- mbmime
      case T.takeWhile (/= ';') mime of
            "application/x-bibtex" -> pure Format_biblatex
            "application/x-reseach-info-systems" -> pure Format_ris
            "application/vnd.citationstyles.csl+json" -> pure Format_json
            "application/json" -> pure Format_json
            "application/x-yaml" -> pure Format_yaml
            "text/x-yaml" -> pure Format_yaml
            "text/yaml" -> pure Format_yaml
            _ -> Nothing

isNote :: Inline -> Bool
isNote (Cite _ [Note _]) = True
 -- the following allows citation styles that are "in-text" but use superscript
 -- references to be treated as if they are "notes" for the purposes of moving
 -- the citations after trailing punctuation (see <https://github.com/jgm/pandoc-citeproc/issues/382>):
isNote (Cite _ [Superscript _]) = True
isNote _                 = False

isSpacy :: Inline -> Bool
isSpacy Space     = True
isSpacy SoftBreak = True
isSpacy _         = False

movePunctInsideQuotes :: Locale -> [Inline] -> [Inline]
movePunctInsideQuotes locale
  | localePunctuationInQuote locale == Just True
    = B.toList . movePunctuationInsideQuotes . B.fromList
  | otherwise
    = id

mvPunct :: Bool -> Locale -> [Inline] -> [Inline]
mvPunct moveNotes locale (x : xs)
  | isSpacy x = x : mvPunct moveNotes locale xs
-- 'x [^1],' -> 'x,[^1]'
mvPunct moveNotes locale (q : s : x : ys)
  | isSpacy s
  , isNote x
  = let spunct = T.takeWhile isPunctuation $ stringify ys
    in  if moveNotes
           then if T.null spunct
                   then q : x : mvPunct moveNotes locale ys
                   else movePunctInsideQuotes locale
                        [q , Str spunct , x] ++ mvPunct moveNotes locale
                        (B.toList
                          (dropTextWhile isPunctuation (B.fromList ys)))
           else q : x : mvPunct moveNotes locale ys
-- 'x[^1],' -> 'x,[^1]'
mvPunct moveNotes locale (Cite cs ils : ys)
   | not (null ils)
   , isNote (last ils)
   , startWithPunct ys
   , moveNotes
   = let s = stringify ys
         spunct = T.takeWhile isPunctuation s
     in  Cite cs (movePunctInsideQuotes locale $
                    init ils
                    ++ [Str spunct | not (endWithPunct False (init ils))]
                    ++ [last ils]) :
         mvPunct moveNotes locale
           (B.toList (dropTextWhile isPunctuation (B.fromList ys)))
mvPunct moveNotes locale (s : x : ys) | isSpacy s, isNote x =
  x : mvPunct moveNotes locale ys
mvPunct moveNotes locale (s : x@(Cite _ (Superscript _ : _)) : ys)
  | isSpacy s = x : mvPunct moveNotes locale ys
mvPunct moveNotes locale (Cite cs ils : Str "." : ys)
  | "." `T.isSuffixOf` (stringify ils)
  = Cite cs ils : mvPunct moveNotes locale ys
mvPunct moveNotes locale (x:xs) = x : mvPunct moveNotes locale xs
mvPunct _ _ [] = []

endWithPunct :: Bool -> [Inline] -> Bool
endWithPunct _ [] = False
endWithPunct onlyFinal xs@(_:_) =
  case reverse (T.unpack $ stringify xs) of
       []                       -> True
       -- covers .), .", etc.:
       (d:c:_) | isPunctuation d
                 && not onlyFinal
                 && isEndPunct c -> True
       (c:_) | isEndPunct c      -> True
             | otherwise         -> False
  where isEndPunct c = c `elem` (".,;:!?" :: String)



startWithPunct :: [Inline] -> Bool
startWithPunct ils =
  case T.uncons (stringify ils) of
    Just (c,_) -> c `elem` (".,;:!?" :: [Char])
    Nothing -> False

truish :: MetaValue -> Bool
truish (MetaBool t) = t
truish (MetaString s) = isYesValue (T.toLower s)
truish (MetaInlines ils) = isYesValue (T.toLower (stringify ils))
truish (MetaBlocks [Plain ils]) = isYesValue (T.toLower (stringify ils))
truish _ = False

isYesValue :: Text -> Bool
isYesValue "t" = True
isYesValue "true" = True
isYesValue "yes" = True
isYesValue _ = False

-- if document contains a Div with id="refs", insert
-- references as its contents.  Otherwise, insert references
-- at the end of the document in a Div with id="refs"
insertRefs :: [(Text,Text)] -> [Text] -> [Block] -> Pandoc -> Pandoc
insertRefs _ _ [] d = d
insertRefs refkvs refclasses refs (Pandoc meta bs) =
  if isRefRemove meta
     then Pandoc meta bs
     else case runState (walkM go (Pandoc meta bs)) False of
               (d', True) -> d'
               (Pandoc meta' bs', False)
                 -> Pandoc meta' $
                    case refTitle meta of
                      Nothing ->
                        case reverse bs' of
                          Header lev (id',classes,kvs) ys : xs ->
                            reverse xs ++
                            [Header lev (id',addUnNumbered classes,kvs) ys,
                             Div ("refs",refclasses,refkvs) refs]
                          _ -> bs' ++ [refDiv]
                      Just ils -> bs' ++
                        [Header 1 ("bibliography", ["unnumbered"], []) ils,
                         refDiv]
  where
   refDiv = Div ("refs", refclasses, refkvs) refs
   addUnNumbered cs = "unnumbered" : [c | c <- cs, c /= "unnumbered"]
   go :: Block -> State Bool Block
   go (Div ("refs",cs,kvs) xs) = do
     put True
     -- refHeader isn't used if you have an explicit references div
     let cs' = nubOrd $ cs ++ refclasses
     let kvs' = nubOrd $ kvs ++ refkvs
     return $ Div ("refs",cs',kvs') (xs ++ refs)
   go x = return x

refTitle :: Meta -> Maybe [Inline]
refTitle meta =
  case lookupMeta "reference-section-title" meta of
    Just (MetaString s)           -> Just [Str s]
    Just (MetaInlines ils)        -> Just ils
    Just (MetaBlocks [Plain ils]) -> Just ils
    Just (MetaBlocks [Para ils])  -> Just ils
    _                             -> Nothing

isRefRemove :: Meta -> Bool
isRefRemove meta =
  maybe False truish $ lookupMeta "suppress-bibliography" meta

legacyDateRanges :: Reference Inlines -> Reference Inlines
legacyDateRanges ref =
  ref{ referenceVariables = M.map go $ referenceVariables ref }
 where
  go (DateVal d)
    | null (dateParts d)
    , Just lit <- dateLiteral d
    = case T.splitOn "_" lit of
        [x,y] -> case Citeproc.rawDateEDTF (x <> "/" <> y) of
                   Just d' -> DateVal d'
                   Nothing -> DateVal d
        _ -> DateVal d
  go x = x

extractText :: Val Inlines -> Text
extractText (TextVal x)  = x
extractText (FancyVal x) = toText x
extractText (NumVal n)   = T.pack (show n)
extractText _            = mempty

-- Here we take the Spans with class csl-note that are left
-- after deNote has removed nested ones, and convert them
-- into real notes.
addNote :: Inline -> Inline
addNote (Span ("",["csl-note"],[]) ils) =
  Note [Para $
         B.toList . addTextCase Nothing CapitalizeFirst . B.fromList $ ils]
addNote x = x

-- Here we handle citation notes that occur inside footnotes
-- or other citation notes, in a note style.  We don't want
-- notes inside notes, so we convert these to parenthesized
-- or comma-separated citations.
deNote :: Inline -> Inline
deNote (Note bs) =
  case bs of
    [Para (cit@(Cite (c:_) _) : ils)]
       | citationMode c /= AuthorInText ->
         -- if citation is first in note, no need to parenthesize.
         Note [Para (walk removeNotes $ cit : walk addParens ils)]
    _ -> Note (walk removeNotes . walk addParens $ bs)

 where
  addParens [] = []
  addParens (Cite (c:cs) ils : zs)
    | citationMode c == AuthorInText
      = Cite (c:cs) (addCommas (needsPeriod zs) ils) :
        addParens zs
    | otherwise
      = Cite (c:cs) (concatMap noteInParens ils) : addParens zs
  addParens (x:xs) = x : addParens xs

  removeNotes (Span ("",["csl-note"],[]) ils) = Span ("",[],[]) ils
  removeNotes x = x

  needsPeriod [] = True
  needsPeriod (Str t:_) = case T.uncons t of
                            Nothing    -> False
                            Just (c,_) -> isUpper c
  needsPeriod (Space:zs) = needsPeriod zs
  needsPeriod _ = False

  noteInParens (Span ("",["csl-note"],[]) ils)
       = Space : Str "(" :
         removeFinalPeriod ils ++ [Str ")"]
  noteInParens x = [x]

  -- We want to add a comma before a CSL note citation, but not
  -- before the author name, and not before the first citation
  -- if it doesn't begin with an author name.
  addCommas = addCommas' True -- boolean == "at beginning"

  addCommas' _ _ [] = []
  addCommas' atBeginning needsPer
    (Span ("",["csl-note"],[]) ils : rest)
      | not (null ils)
       = (if atBeginning then id else ([Str "," , Space] ++)) $
         (if needsPer then ils else removeFinalPeriod ils) ++
         addCommas' False needsPer rest
  addCommas' _ needsPer (il : rest) = il : addCommas' False needsPer rest

deNote x = x

-- Note: we can't use dropTextWhileEnd indiscriminately,
-- because this would remove the final period on abbreviations like Ibid.
-- But it turns out that when the note citation ends with Ibid.
-- (or Ed. etc.), the last inline will be Str "" as a result of
-- the punctuation-fixing mechanism that removes the double '.'.
removeFinalPeriod :: [Inline] -> [Inline]
removeFinalPeriod ils =
  case lastMay ils of
    Just (Span attr ils')
      -> initSafe ils ++ [Span attr (removeFinalPeriod ils')]
    Just (Emph ils')
      -> initSafe ils ++ [Emph (removeFinalPeriod ils')]
    Just (Strong ils')
      -> initSafe ils ++ [Strong (removeFinalPeriod ils')]
    Just (SmallCaps ils')
      -> initSafe ils ++ [SmallCaps (removeFinalPeriod ils')]
    Just (Str t)
      | T.takeEnd 1 t == "." -> initSafe ils ++ [Str (T.dropEnd 1 t)]
      | isRightQuote (T.takeEnd 1 t)
        -> removeFinalPeriod
             (initSafe ils ++ [Str tInit | not (T.null tInit)]) ++ [Str tEnd]
             where
               tEnd  = T.takeEnd 1 t
               tInit = T.dropEnd 1 t
    _ -> ils
 where
  isRightQuote "\8221" = True
  isRightQuote "\8217" = True
  isRightQuote "\187"  = True
  isRightQuote _       = False

bcp47LangToIETF :: PandocMonad m => Text -> m (Maybe Lang)
bcp47LangToIETF bcplang =
  case parseLang bcplang of
    Left _ -> do
      report $ InvalidLang bcplang
      return Nothing
    Right lang -> return $ Just lang

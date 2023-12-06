{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.JATS
   Copyright   : Copyright (C) 2017-2020 Hamish Mackenzie
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of JATS XML to 'Pandoc' document.
-}

module Text.Pandoc.Readers.JATS ( readJATS ) where
import Control.Monad.State.Strict ( StateT(runStateT), gets, modify )
import Control.Monad (forM_,  when, unless)
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(..))
import Data.Char (isDigit, isSpace)
import Data.Default
import Data.Generics
import Data.List (foldl', intersperse)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Options
import Text.Pandoc.Shared (safeRead, extractSpaces)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.XML.Light
import Text.TeXMath (readMathML, writeTeX)
import qualified Data.Set as S (fromList, member)
import Data.Set ((\\))
import Text.Pandoc.Sources (ToSources(..), sourcesToText)
import Safe (headMay)
import Text.Printf (printf)

type JATS m = StateT JATSState m

data JATSState = JATSState{ jatsSectionLevel :: Int
                          , jatsQuoteType    :: QuoteType
                          , jatsMeta         :: Meta
                          , jatsBook         :: Bool
                          , jatsFootnotes    :: Map.Map Text Blocks
                          , jatsContent      :: [Content]
                          , jatsInFigure     :: Bool
                          } deriving Show

instance Default JATSState where
  def = JATSState{ jatsSectionLevel = 0
                 , jatsQuoteType = DoubleQuote
                 , jatsMeta = mempty
                 , jatsBook = False
                 , jatsFootnotes = mempty
                 , jatsContent = []
                 , jatsInFigure = False }


readJATS :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readJATS _ inp = do
  let sources = toSources inp
  tree <- either (throwError . PandocXMLError "") return $
            parseXMLContents (TL.fromStrict . sourcesToText $ sources)
  (bs, st') <- flip runStateT (def{ jatsContent = tree }) $ mapM parseBlock tree
  let footnotes = jatsFootnotes st'
  let blockList = toList $ mconcat bs
  let linkToFootnotes :: Inline -> Inline
      linkToFootnotes link'@(Link _attr _txt (href, _title)) =
        case T.uncons href of
          Just ('#', rid) -> case Map.lookup rid footnotes of
                               Just footnote -> Note (toList footnote)
                               Nothing       -> link'
          _               -> link'
      linkToFootnotes inline = inline
  return $ Pandoc (jatsMeta st') (walk linkToFootnotes blockList)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: Text -> Element -> Text
attrValue attr =
  fromMaybe "" . maybeAttrValue attr

maybeAttrValue :: Text -> Element -> Maybe Text
maybeAttrValue attr elt =
  lookupAttrBy (\x -> qName x == attr) (elAttribs elt)

-- convenience function
named :: Text -> Element -> Bool
named s e = qName (elName e) == s

--

addMeta :: PandocMonad m => ToMetaValue a => Text -> a -> JATS m ()
addMeta field val = modify (setMeta field val)

instance HasMeta JATSState where
  setMeta field v s =  s {jatsMeta = setMeta field v (jatsMeta s)}
  deleteMeta field s = s {jatsMeta = deleteMeta field (jatsMeta s)}

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = case qName (elName e) of
            "disp-formula" -> if onlyOneChild e
                                  then if hasFormulaChild e
                                          then False
                                          else case filterChild (named "alternatives") e of
                                            Just a -> if hasFormulaChild a then False else True
                                            Nothing -> True
                                  else True
            "alternatives" -> if hasFormulaChild e then False else True
            _ -> qName (elName e) `S.member` blocktags

  where blocktags = S.fromList (paragraphLevel ++ lists ++ formulae ++ other) \\ S.fromList canBeInline
        paragraphLevel = ["address", "answer", "answer-set", "array", "boxed-text", "chem-struct-wrap",
            "code", "explanation", "fig", "fig-group", "graphic", "media", "preformat", "question", "question-wrap", "question-wrap-group",
            "supplementary-material", "table-wrap", "table-wrap-group",
            "alternatives", "disp-formula", "disp-formula-group"]
        lists = ["def-list", "list"]
        formulae = ["tex-math", "mml:math"]
        other = ["p", "related-article", "related-object", "ack", "disp-quote",
            "speech", "statement", "verse-group", "x"]
        canBeInline = ["tex-math", "mml:math", "related-object", "x"]
        onlyOneChild x = length (allChildren x) == 1
        allChildren x = filterChildren (const True) x

isBlockElement _ = False

-- Trim leading and trailing newline characters
trimNl :: Text -> Text
trimNl = T.dropAround (== '\n')

-- function that is used by both graphic (in parseBlock)
-- and inline-graphic (in parseInline)
getGraphic :: PandocMonad m => Element -> JATS m Inlines
getGraphic e = do
  let atVal a = attrValue a e
  let altText = case filterElement (named "alt-text") e of
         Just alt -> textContent alt
         Nothing -> mempty
      (ident, title, altText') = (atVal "id", atVal "title", text altText)
      attr = (ident, T.words $ atVal "role", [])
      imageUrl = atVal "href"
  return $ imageWith attr imageUrl title altText'

getBlocks :: PandocMonad m => Element -> JATS m Blocks
getBlocks e =  mconcat <$>
                 mapM parseBlock (elContent e)


parseBlock :: PandocMonad m => Content -> JATS m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if T.all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text s
parseBlock (CRef x) = return $ plain $ str $ T.toUpper x
parseBlock (Elem e) = do
  sectionLevel <- gets jatsSectionLevel
  let parseBlockWithHeader = wrapWithHeader (sectionLevel+1) (getBlocks e)

  case qName (elName e) of
        "book" -> parseBook
        "book-part-wrapper" -> parseBook
        "p" -> parseMixed para (elContent e)
        "code" -> codeBlockWithLang
        "preformat" -> codeBlockWithLang
        "disp-quote" -> wrapWithHeader (sectionLevel+1) parseBlockquote
        "list" ->  wrapWithHeader (sectionLevel+1) parseList
        "def-list" -> wrapWithHeader (sectionLevel+1) (definitionList <$> deflistitems)
        "sec" -> parseBlockWithHeader
        "abstract" -> parseBlockWithHeader
        "ack" -> parseBlockWithHeader
        "answer" -> parseBlockWithHeader
        "answer-set" -> parseBlockWithHeader
        "app" -> parseBlockWithHeader
        "app-group" -> parseBlockWithHeader
        "author-comment" -> parseBlockWithHeader
        "author-notes" -> parseBlockWithHeader
        "back" -> parseBlockWithHeader
        "bio" -> parseBlockWithHeader
        "explanation" -> parseBlockWithHeader
        "glossary" -> parseBlockWithHeader
        "kwd-group" -> parseBlockWithHeader
        "list-item" -> parseBlockWithHeader
        "notes" -> parseBlockWithHeader
        "option" -> parseBlockWithHeader
        "question" -> parseBlockWithHeader
        "question-preamble" -> parseBlockWithHeader
        "question-wrap-group" -> parseBlockWithHeader
        "statement" -> parseBlockWithHeader
        "supplement" -> parseBlockWithHeader
        "table-wrap-foot" -> parseBlockWithHeader
        "trans-abstract" -> parseBlockWithHeader
        "verse-group" -> parseBlockWithHeader
        "graphic" -> para <$> getGraphic e
        "journal-meta" -> parseMetadata e
        "article-meta" -> parseMetadata e
        "custom-meta" -> parseMetadata e
        "processing-meta" -> return mempty
        "book-meta" -> parseMetadata e
        "title" -> return mempty -- processed by header
        "label" -> return mempty -- processed by header
        "table" -> parseTable
        "fig" -> parseFigure
        "fig-group" -> divWith (attrValue "id" e, ["fig-group"], [])
                          <$> getBlocks e
        "table-wrap" -> divWith (attrValue "id" e, ["table-wrap"], [])
                          <$> getBlocks e
        "caption" -> do
          inFigure <- gets jatsInFigure
          if inFigure -- handled by parseFigure
             then return mempty
             else divWith (attrValue "id" e, ["caption"], []) <$> wrapWithHeader 6 (getBlocks e)
        "fn-group" -> parseFootnoteGroup
        "ref-list" -> parseRefList e
        "alternatives" -> if hasFormulaChild e
                            then blockFormula displayMath e
                            else getBlocks e
        "disp-formula" -> if hasFormulaChild e
                            then blockFormula displayMath e
                            else divWith (attrValue "id" e, ["disp-formula"], [])
                                    <$> getBlocks e
        "index" -> parseBlockWithHeader
        "index-div" -> parseBlockWithHeader
        "index-group" -> parseBlockWithHeader
        "index-title-group" -> return mempty -- handled by index and index-div
        "toc" -> parseBlockWithHeader
        "toc-div" -> parseBlockWithHeader
        "toc-entry" -> parseBlockWithHeader
        "toc-group" -> parseBlockWithHeader
        "toc-title-group" -> return mempty -- handled by toc
        "legend" -> parseBlockWithHeader
        "dedication" -> parseBlockWithHeader
        "foreword" -> parseBlockWithHeader
        "preface" -> parseBlockWithHeader
        "?xml"  -> return mempty
        _       -> getBlocks e
   where parseMixed container conts = do
           let (ils,rest) = break isBlockElement conts
           ils' <- trimInlines . mconcat <$> mapM parseInline ils
           let p = if ils' == mempty then mempty else container ils'
           case rest of
                 []     -> return p
                 (r:rs) -> do
                    b <- parseBlock r
                    x <- parseMixed container rs
                    return $ p <> b <> x
         codeBlockWithLang = do
           let classes' = case attrValue "language" e of
                                "" -> []
                                x  -> [x]
           return $ codeBlockWith (attrValue "id" e, classes', [])
                  $ trimNl $ strContentRecursive e
         parseBlockquote = do
            attrib <- case filterChild (named "attrib") e of
                             Nothing  -> return mempty
                             Just z   -> para . (str "— " <>) . mconcat
                                         <$>
                                              mapM parseInline (elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
         parseList = do
            case attrValue "list-type" e of
              "bullet" -> bulletList <$> listitems
              listType -> do
                let start =
                      fromMaybe 1 $
                        ( filterElement (named "list-item") e
                            >>= filterElement (named "label")
                        )
                          >>= safeRead . textContent
                orderedListWith (start, parseListStyleType listType, DefaultDelim)
                  <$> listitems
         parseListStyleType "roman-lower" = LowerRoman
         parseListStyleType "roman-upper" = UpperRoman
         parseListStyleType "alpha-lower" = LowerAlpha
         parseListStyleType "alpha-upper" = UpperAlpha
         parseListStyleType _             = DefaultStyle
         listitems = mapM getBlocks $ filterChildren (named "list-item") e
         deflistitems = mapM parseVarListEntry $ filterChildren
                     (named "def-item") e
         parseVarListEntry e' = do
                     let terms = filterChildren (named "term") e'
                     let items = filterChildren (named "def") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseFigure = do
           modify $ \st -> st{ jatsInFigure = True }
           capt <- case filterChild (named "caption") e of
                     Just t  -> mconcat . intersperse linebreak <$>
                                mapM getInlines (filterChildren (const True) t)
                     Nothing -> return mempty
           contents <- getBlocks e
           modify $ \st -> st{ jatsInFigure = False }
           return $ figureWith
             (attrValue "id" e, [], [])
             (simpleCaption $ plain capt)
             contents
         parseFootnoteGroup = do
           forM_ (filterChildren (named "fn") e) $ \fn -> do
             let id' = attrValue "id" fn
             contents <- getBlocks fn
             modify $ \st ->
               st { jatsFootnotes = Map.insert id' contents (jatsFootnotes st) }
           return mempty

         parseTable = do
                      let isCaption x = named "title" x || named "caption" x
                      capt <- case filterChild isCaption e of
                                    Just t  -> getInlines t
                                    Nothing -> return mempty
                      let e' = fromMaybe e $ filterChild (named "tgroup") e
                      let isColspec x = named "colspec" x || named "col" x
                      let colspecs = case filterChild (named "colgroup") e' of
                                           Just c -> filterChildren isColspec c
                                           _      -> filterChildren isColspec e'
                      let isRow x = named "row" x || named "tr" x

                      let parseRows elementWithRows =
                            map parseElement $ filterChildren isRow elementWithRows

                      -- list of list of body cell elements
                      let multipleBodyRowElements =
                            map parseRows $ filterChildren (named "tbody") e'

                      -- list of list header cell elements
                      let headRowElements = maybe [] parseRows (filterChild (named "thead") e')

                      -- list of foot cell elements
                      let footRowElements = maybe [] parseRows (filterChild (named "tfoot") e')

                      let toAlignment c = case findAttr (unqual "align") c of
                                                Just "left"   -> AlignLeft
                                                Just "right"  -> AlignRight
                                                Just "center" -> AlignCenter
                                                _             -> AlignDefault
                      let toColSpan element = fromMaybe 1 $
                            findAttr (unqual "colspan") element >>= safeRead
                      let toRowSpan element =  fromMaybe 1 $
                            findAttr (unqual "rowspan") element >>= safeRead
                      let toWidth c = do
                            w <- findAttr (unqual "colwidth") c
                            n <- safeRead $ "0" <> T.filter (\x -> isDigit x || x == '.') w
                            if n > 0 then Just n else Nothing
                      let firstBody = fromMaybe [] (headMay multipleBodyRowElements)
                      let numrows = foldl' max 0 $ map length firstBody
                      let aligns = case colspecs of
                                     [] -> replicate numrows AlignDefault
                                     cs -> map toAlignment cs
                      let widths = case colspecs of
                                     [] -> replicate numrows ColWidthDefault
                                     cs -> let ws = map toWidth cs
                                           in case sequence ws of
                                                Just ws' -> let tot = sum ws'
                                                            in  ColWidth . (/ tot) <$> ws'
                                                Nothing  -> replicate numrows ColWidthDefault
                      let parseCell = parseMixed plain . elContent
                      let elementToCell element = cell
                            (toAlignment element)
                            (RowSpan $ toRowSpan element)
                            (ColSpan $ toColSpan element)
                            <$> (parseCell element)
                      let rowElementsToCells elements = mapM elementToCell elements
                      let toRow = fmap (Row nullAttr) . rowElementsToCells
                          toRows elements = mapM toRow elements

                      headerRows <- toRows headRowElements
                      footerRows <- toRows footRowElements
                      bodyRows <- mapM toRows multipleBodyRowElements

                      return $ table (simpleCaption $ plain capt)
                                     (zip aligns widths)
                                     (TableHead nullAttr headerRows)
                                     (map (TableBody nullAttr 0 []) bodyRows)
                                     (TableFoot nullAttr footerRows)
         isEntry x  = named "entry" x || named "td" x || named "th" x
         parseElement = filterChildren isEntry
         wrapWithHeader n mBlocks = do
                      isBook <- gets jatsBook
                      let n' = case (filterChild (named "title") e >>= maybeAttrValue "display-as") of
                                  Just t -> read $ T.unpack t
                                  Nothing -> if isBook || n == 0 then n + 1 else n
                      headerText <- case filterChild (named "title") e of
                                       Just t  -> case maybeAttrValue "supress" t of
                                                     Just s -> if s == "no"
                                                                 then getInlines t
                                                                 else return mempty
                                                     Nothing -> getInlines t
                                       Nothing -> do
                                           let name = qName (elName e)
                                           if (name == "dedication" || name == "foreword" || name == "preface")
                                             then return $ str $ T.toTitle name
                                             else case filterChild (named "index-title-group") e >>= filterChild (named "title") of
                                                     Just i -> getInlines i
                                                     Nothing -> case filterChild (named "toc-title-group") e >>= filterChild (named "title") of
                                                                   Just t -> getInlines t
                                                                   Nothing -> return mempty
                      oldN <- gets jatsSectionLevel
                      modify $ \st -> st{ jatsSectionLevel = n }
                      blocks <- mBlocks
                      let ident = attrValue "id" e
                      modify $ \st -> st{ jatsSectionLevel = oldN }
                      return $ (if headerText == mempty
                                  then mempty
                                  else headerWith (ident,[],[]) n' headerText) <> blocks
         parseBook = do
           modify $ \st -> st{ jatsBook = True }
           getBlocks e

getInlines :: PandocMonad m => Element -> JATS m Inlines
getInlines e' = trimInlines . mconcat <$>
                 mapM parseInline (elContent e')

parseMetadata :: PandocMonad m => Element -> JATS m Blocks
parseMetadata e = do
  isBook <- gets jatsBook
  if isBook then getBookTitle e else getArticleTitle e
  if isBook then getBookAuthors e else getArticleAuthors e
  getAffiliations e
  getAbstract e
  getPubDate e
  getPermissions e
  return mempty

getArticleTitle :: PandocMonad m => Element -> JATS m ()
getArticleTitle e = do
  tit <-  case filterElement (named "article-title") e of
               Just s  -> getInlines s
               Nothing -> return mempty
  subtit <-  case filterElement (named "subtitle") e of
               Just s  -> (text ": " <>) <$>
                           getInlines s
               Nothing -> return mempty
  when (tit /= mempty) $ addMeta "title" tit
  when (subtit /= mempty) $ addMeta "subtitle" subtit


getBookTitle :: PandocMonad m => Element -> JATS m ()
getBookTitle e = do
  tit <-  case (filterElement (named "book-title-group") e >>= filterElement (named "book-title")) of
               Just s  -> getInlines s
               Nothing -> return mempty
  subtit <-  case (filterElement (named "book-title-group") e >>= filterElement (named "subtitle")) of
               Just s  -> (text ": " <>) <$>
                           getInlines s
               Nothing -> return mempty
  when (tit /= mempty) $ addMeta "title" tit
  when (subtit /= mempty) $ addMeta "subtitle" subtit

getArticleAuthors :: PandocMonad m => Element -> JATS m ()
getArticleAuthors e = do
  authors <- mapM getContrib $ filterElements
              (\x -> named "contrib" x &&
                     attrValue "contrib-type" x == "author") e
  authorNotes <- mapM getInlines $ filterElements (named "author-notes") e
  let authors' = case (reverse authors, authorNotes) of
                   ([], _)    -> []
                   (_, [])    -> authors
                   (a:as, ns) -> reverse as ++ [a <> mconcat ns]
  unless (null authors) $ addMeta "author" authors'

getBookAuthors :: PandocMonad m => Element -> JATS m ()
getBookAuthors e = do
  authors <- mapM getContrib $ filterElements (\x -> named "contrib-group" x) e
              >>= filterElements (\x -> named "contrib" x &&
                     attrValue "contrib-type" x == "author")
  authorNotes <- mapM getInlines $ filterElements (named "author-notes") e
  let authors' = case (reverse authors, authorNotes) of
                   ([], _)    -> []
                   (_, [])    -> authors
                   (a:as, ns) -> reverse as ++ [a <> mconcat ns]
  unless (null authors) $ addMeta "author" authors'

getAffiliations :: PandocMonad m => Element -> JATS m ()
getAffiliations x = do
  affs <- mapM getInlines $ filterChildren (named "aff") x
  unless (null affs) $ addMeta "institute" affs

getAbstract :: PandocMonad m => Element -> JATS m ()
getAbstract e =
  case filterElement (named "abstract") e of
    Just s -> do
      blks <- getBlocks s
      addMeta "abstract" blks
    Nothing -> pure ()

getPubDate :: PandocMonad m => Element -> JATS m ()
getPubDate e =
  case filterElement (named "pub-date") e of
    Just d -> getDate d >>= addMeta "date" . text
    Nothing -> pure ()

-- extract a structured date and create an ISO-8901 string date from it
getDate :: PandocMonad m => Element -> JATS m Text
getDate e =
  case maybeAttrValue "iso-8601-date" e of
    Just isod -> pure isod
    Nothing -> do
      let extractDate :: Element -> Maybe Int
          extractDate = safeRead . strContent
      let yr = filterElement (named "year") e >>= extractDate
      let mon = filterElement (named "month") e >>= extractDate
      let day = filterElement (named "day") e >>= extractDate
      let stringDate = strContent <$> filterElement (named "string-date") e
      pure $
        case (yr, mon, day) of
          (Just y, Just m, Just d) -> T.pack $ printf "%04d-%02d-%02d" y m d
          (Just y, Just m, Nothing) -> T.pack $ printf "%04d-%02d" y m
          (Just y, Nothing, Nothing) -> T.pack $ printf "%04d" y
          _ -> fromMaybe mempty stringDate

getPermissions :: PandocMonad m => Element -> JATS m ()
getPermissions e = do
  copyright <- getCopyright e
  license <-  case filterElement (named "license") e of
               Just s  -> getLicense s
               Nothing -> return mempty
  when (copyright /= mempty) $ addMeta "copyright" copyright
  when (license /= mempty) $ addMeta "license" license

getCopyright :: PandocMonad m => Element -> JATS m (Map.Map Text MetaValue)
getCopyright e = do
  let holder = metaElement e "copyright-holder" "holder"
  let statement = metaElement e "copyright-statement" "statement"
  let year = metaElement e "copyright-year" "year"
  return $ Map.fromList (catMaybes $ [holder, statement, year])

getLicense :: PandocMonad m => Element -> JATS m (Map.Map Text MetaValue)
getLicense e = do
  let licenseType = metaAttribute e "license-type" "type"
  let licenseLink = metaElementAliRef e "link"
  let licenseText = metaElement e "license-p" "text"
  return $ Map.fromList (catMaybes $ [licenseType, licenseLink, licenseText])

metaElement :: Element -> Text -> Text -> Maybe (Text, MetaValue)
metaElement e child key =
  case filterElement (named child) e of
    Just content -> Just (key, toMetaValue $ strContent content)
    Nothing -> Nothing

metaElementAliRef :: Element -> Text -> Maybe (Text, MetaValue)
metaElementAliRef e key =
  case filterElement isAliLicenseRef e of
    Just content -> Just (key, toMetaValue $ strContent content)
    Nothing -> Nothing

metaAttribute :: Element -> Text -> Text -> Maybe (Text, MetaValue)
metaAttribute e attr key =
  case maybeAttrValue attr e of
    Just content -> Just (key, toMetaValue content)
    Nothing -> Nothing

getContrib :: PandocMonad m => Element -> JATS m Inlines
getContrib x = do
  given <- maybe (return mempty) getInlines
            $ filterElement (named "given-names") x
  family <- maybe (return mempty) getInlines
            $ filterElement (named "surname") x
  if given == mempty && family == mempty
     then return mempty
     else if given == mempty || family == mempty
          then return $ given <> family
          else return $ given <> space <> family

parseRefList :: PandocMonad m => Element -> JATS m Blocks
parseRefList e = do
  refs <- mapM parseRef $ filterChildren (named "ref") e
  let mbtitle = filterChild (named "title") e
  title <- case mbtitle of
    Nothing -> pure mempty
    Just te -> header 1 <$> parseInline (Elem te)
  addMeta "references" refs
  return $ title <> divWith ("refs",[],[]) mempty

parseRef :: PandocMonad m
         => Element -> JATS m (Map.Map Text MetaValue)
parseRef e = do
  let combineWithDash x y = x <> text "-" <> y
  let getName nm = do
        given <- maybe (return mempty) getInlines
                  $ filterChild (named "given-names") nm
        family <- maybe (return mempty) getInlines
                  $ filterChild (named "surname") nm
        return $ toMetaValue $ Map.fromList [
            ("given" :: Text, given)
          , ("family", family)
          ]
  let refElement :: PandocMonad m
                 => Element -> Element -> JATS m (Maybe (Text, MetaValue))
      refElement c el =
        case qName (elName el) of
          "article-title" -> Just . ("title",) . toMetaValue <$> getInlines el
          "source" ->
            case filterChild (named "article-title") c of
              Just _ -> Just . ("container-title",) . toMetaValue <$> getInlines el
              Nothing -> Just . ("title",) . toMetaValue <$> getInlines el
          "label" -> Just . ("citation-label",) . toMetaValue <$> getInlines el
          "year" -> case filterChild (named "month") c of
                      Just m -> Just . ("issued",) . toMetaValue <$>
                                 (combineWithDash
                                    <$> getInlines el <*> getInlines m)
                      Nothing -> Just . ("issued",) . toMetaValue <$> getInlines el
          "volume" -> Just . ("volume",) . toMetaValue <$> getInlines el
          "issue" -> Just . ("issue",) . toMetaValue <$> getInlines el
          "isbn" -> Just . ("ISBN",) . toMetaValue <$> getInlines el
          "issn" -> Just . ("ISSN",) . toMetaValue <$> getInlines el
          "uri" -> Just . ("url",) . toMetaValue <$> getInlines el
          "fpage" ->
            case filterChild (named "lpage") c of
              Just lp -> Just . ("page",) . toMetaValue <$>
                          (combineWithDash <$> getInlines el <*> getInlines lp)
              Nothing -> Just . ("page-first",) . toMetaValue <$> getInlines el
          "publisher-name" -> Just . ("publisher",) . toMetaValue <$> getInlines el
          "publisher-loc" -> Just . ("publisher-place",) . toMetaValue
                                <$> getInlines el
          "edition" -> pure $ Just ("edition",
                               toMetaValue . T.filter isDigit $ strContent el)
          "person-group" -> do names <- mapM getName
                                            (filterChildren (named "name") el)
                               pure $ Just (attrValue "person-group-type" el,
                                            toMetaValue names)
          "pub-id" -> case attrValue "pub-id-type" el of
                         "doi"  -> pure $ Just ("DOI",  toMetaValue $ strContent el)
                         "pmid" -> pure $ Just ("PMID", toMetaValue $ strContent el)
                         _      -> pure Nothing
          "object-id" -> case attrValue "pub-id-type" el of
                         "doi"  -> pure $ Just ("DOI",  toMetaValue $ strContent el)
                         "pmid" -> pure $ Just ("PMID", toMetaValue $ strContent el)
                         _      -> pure Nothing


          _ -> pure Nothing
  refVariables <-
    case filterChild (named "element-citation") e of
      Just c -> (("type", toMetaValue $ case attrValue "publication-type" c of
                            "journal" -> "article-journal"
                            x -> x) :) .
                  catMaybes <$> mapM (refElement c) (elChildren c)
      Nothing -> pure []   -- TODO handle mixed-citation
  -- allows round-tripping, since JATS writer puts ref- in front of citation ids:
  let stripPref t = fromMaybe t $ T.stripPrefix "ref-" t
  return $ Map.fromList (("id", toMetaValue $ stripPref $ attrValue "id" e)
                        : refVariables)

textContent :: Element -> Text
textContent = strContent

strContentRecursive :: Element -> Text
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

parseInline :: PandocMonad m => Content -> JATS m Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) = return $ maybe (text $ T.toUpper ref) text
                                $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
        "italic" -> innerInlines emph
        "bold" -> innerInlines strong
        "strike" -> innerInlines strikeout
        "sub" -> innerInlines subscript
        "sup" -> innerInlines superscript
        "underline" -> innerInlines underline
        "break" -> return linebreak
        "sc" -> innerInlines smallcaps

        "code" -> codeWithLang
        "monospace" -> codeWithLang

        "inline-graphic" -> getGraphic e
        "disp-quote" -> do
            qt <- gets jatsQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ jatsQuoteType = qt' }
            contents <- innerInlines id
            modify $ \st -> st{ jatsQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents

        "xref" -> do
            ils <- innerInlines id
            let rid = attrValue "rid" e
            let rids = T.words rid
            let refType = ("ref-type",) <$> maybeAttrValue "ref-type" e
            let attr = (attrValue "id" e, [], maybeToList refType)
            return $ if refType == Just ("ref-type","bibr")
                        then cite
                             (map (\id' ->
                               let id'' = fromMaybe id' $
                                           T.stripPrefix "ref-" id'
                                 in Citation { citationId = id''
                                             , citationPrefix = []
                                             , citationSuffix = []
                                             , citationMode = NormalCitation
                                             , citationNoteNum = 0
                                             , citationHash = 0}) rids)
                             ils
                        else linkWith attr ("#" <> rid) "" ils
        "ext-link" -> do
             ils <- innerInlines id
             let title = fromMaybe "" $ findAttr (QName "title" (Just "http://www.w3.org/1999/xlink") Nothing) e
             let href = case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
                               Just h -> h
                               _      -> "#" <> attrValue "rid" e
             let ils' = if ils == mempty then str href else ils
             let attr = (attrValue "id" e, [], [])
             return $ linkWith attr href title ils'

        "alternatives" -> if hasFormulaChild e
                            then inlineFormula math e
                            else innerInlines id
        "disp-formula" -> inlineFormula displayMath e
        "inline-formula" -> inlineFormula math e
        "math" | qURI (elName e) == Just "http://www.w3.org/1998/Math/MathML"
                   -> return . math $ mathML e
        "tex-math" -> return . math $ textContent e

        "email" -> return $ link ("mailto:" <> textContent e) ""
                          $ str $ textContent e
        "uri" -> return $ link (textContent e) "" $ str $ textContent e
        "fn" -> note . mconcat <$>
                         mapM parseBlock (elContent e)
        _          -> innerInlines id
   where innerInlines f = extractSpaces f . mconcat <$>
                          mapM parseInline (elContent e)
         codeWithLang = do
           let classes' = case attrValue "language" e of
                               "" -> []
                               l  -> [l]
           return $ codeWith (attrValue "id" e,classes',[]) $ strContentRecursive e

inlineFormula ::  PandocMonad m => (Text->Inlines) -> Element -> JATS m Inlines
inlineFormula constructor e = do
            let whereToLook = fromMaybe e $ filterElement (named "alternatives") e
                texMaths = map textContent $
                            filterChildren (named  "tex-math") whereToLook
                mathMLs = map mathML $
                            filterChildren isMathML whereToLook
            return . mconcat . take 1 . map constructor $ texMaths ++ mathMLs

blockFormula ::  PandocMonad m => (Text->Inlines) -> Element -> JATS m Blocks
blockFormula constructor e = do
            let whereToLook = fromMaybe e $ filterElement (named "alternatives") e
                texMaths = map textContent $
                            filterChildren (named  "tex-math") whereToLook
                mathMLs = map mathML $
                            filterChildren isMathML whereToLook
            return . para . head . take 1 . map constructor $ texMaths ++ mathMLs

mathML :: Element -> Text
mathML x =
          case readMathML . showElement $ everywhere (mkT removePrefix) x of
                Left _ -> mempty
                Right m -> writeTeX m
          where removePrefix elname = elname { qPrefix = Nothing }

isMathML :: Element -> Bool
isMathML x = qName (elName x) == "math" &&
                      qURI  (elName x) ==
                                      Just "http://www.w3.org/1998/Math/MathML"

formulaChildren :: Element -> [Element]
formulaChildren x = filterChildren isMathML x ++ filterChildren (named "tex-math") x

hasFormulaChild :: Element -> Bool
hasFormulaChild x = length (formulaChildren x) > 0

isAliLicenseRef :: Element -> Bool
isAliLicenseRef x = qName (elName x) == "license_ref" &&
                      qURI  (elName x) ==
                                      Just "http://www.niso.org/schemas/ali/1.0/"

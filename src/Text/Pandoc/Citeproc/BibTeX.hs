{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Input.Bibtex
-- Copyright   :  (c) John MacFarlane
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  John MacFarlane <fiddlosopher@gmail.com>
-- Stability   :  unstable-- Portability :  unportable
--
-----------------------------------------------------------------------------

module Text.Pandoc.Citeproc.BibTeX
    ( Variant(..)
    , readBibtexString
    )
    where

import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Readers.LaTeX (readLaTeX)
import Text.Pandoc.Extensions (Extension(..), extensionsFromList)
import Text.Pandoc.Options (ReaderOptions(..))
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Shared (stringify)
import qualified Text.Pandoc.Walk       as Walk
import Citeproc.Types
import Citeproc.Pandoc ()
import Text.Pandoc.Citeproc.Util (toIETF)
import Text.Pandoc.Citeproc.Data (biblatexStringMap)
import Data.Default
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Map               as Map
import           Data.Maybe
import           Text.Pandoc.Parsing hiding ((<|>), many)
import           Control.Applicative
import           Data.List.Split        (splitOn, splitWhen, wordsBy)
import           Control.Monad.RWS      hiding ((<>))
import qualified Data.Sequence          as Seq
import           Data.Char              (isAlphaNum, isDigit, isLetter,
                                         isUpper, toLower, toUpper,
                                         isLower, isPunctuation)
import           Data.List              (foldl', intercalate)
import           Safe                   (readMay)

data Variant = Bibtex | Biblatex
  deriving (Show, Eq, Ord)

-- | Parse BibTeX or BibLaTeX into a list of 'Reference's.
readBibtexString :: Variant           -- ^ bibtex or biblatex
                 -> Locale            -- ^ Locale
                 -> (Text -> Bool)    -- ^ Filter on citation ids
                 -> Text              -- ^ bibtex/biblatex text
                 -> Either ParseError [Reference Inlines]
readBibtexString variant locale idpred contents = do
  case runParser (((resolveCrossRefs variant <$> bibEntries) <* eof) >>=
                   mapM (itemToReference locale variant) .
                      filter (\item -> idpred (identifier item) &&
                                        entryType item /= "xdata"))
           (fromMaybe defaultLang $ localeLanguage locale, Map.empty)
           "" contents of
          Left err -> Left err
          Right xs -> return xs

defaultLang :: Lang
defaultLang = Lang "en" (Just "US")

-- a map of bibtex "string" macros
type StringMap = Map.Map Text Text

type BibParser = Parser Text (Lang, StringMap)

data Item = Item{ identifier :: Text
                , sourcePos  :: SourcePos
                , entryType  :: Text
                , fields     :: Map.Map Text Text
                }
                deriving (Show, Ord, Eq)

itemToReference :: Locale -> Variant -> Item -> BibParser (Reference Inlines)
itemToReference locale variant item = do
  setPosition (sourcePos item)
  bib item $ do
    let lang = fromMaybe defaultLang $ localeLanguage locale
    modify $ \st -> st{ localeLang = lang,
                        untitlecase = case lang of
                                           (Lang "en" _) -> True
                                           _             -> False }

    id' <- asks identifier
    otherIds <- (Just <$> getRawField "ids")
                  <|> return Nothing
    (reftype, genre) <- getTypeAndGenre
    -- hyphenation:
    let getLangId = do
             langid <- T.strip . T.toLower <$> getRawField "langid"
             idopts <- T.strip . T.toLower . stringify <$>
                           getField "langidopts" <|> return ""
             case (langid, idopts) of
                  ("english","variant=british")    -> return "british"
                  ("english","variant=american")   -> return "american"
                  ("english","variant=us")         -> return "american"
                  ("english","variant=usmax")      -> return "american"
                  ("english","variant=uk")         -> return "british"
                  ("english","variant=australian") -> return "australian"
                  ("english","variant=newzealand") -> return "newzealand"
                  (x,_)                            -> return x
    hyphenation <- (Just . toIETF . T.toLower <$>
                     (getLangId <|> getRawField "hyphenation"))
                  <|> return Nothing
    modify $ \s -> s{ untitlecase = untitlecase s &&
                                      case hyphenation of
                                        Just x -> "en-" `T.isPrefixOf` x
                                        _ -> True }


    opts <- (parseOptions <$> getRawField "options") <|> return []

    et <- asks entryType

    -- titles
    let isArticle = et `elem`
                     ["article", "periodical", "suppperiodical", "review"]
    let isPeriodical = et == "periodical"
    let isChapterlike = et `elem`
           ["inbook","incollection","inproceedings","inreference","bookinbook"]

    let getFieldMaybe f = (Just <$> getField f) <|> return Nothing

    -- names
    let getNameList' f = Just <$>
         getNameList (("bibtex", case variant of
                                      Bibtex   -> "true"
                                      Biblatex -> "false") : opts) f

    author' <- getNameList' "author" <|> return Nothing
    containerAuthor' <- getNameList' "bookauthor" <|> return Nothing
    translator' <- getNameList' "translator" <|> return Nothing
    editortype <- getRawField "editortype" <|> return mempty
    editor'' <- getNameList' "editor" <|> return Nothing
    director'' <- getNameList' "director" <|> return Nothing
    let (editor', director') = case editortype of
                                    "director" -> (Nothing, editor'')
                                    _          -> (editor'', director'')
    -- FIXME: add same for editora, editorb, editorc

    -- dates
    issued' <- (Just <$> (getDate "date" <|> getOldDate mempty)) <|>
               return Nothing
    eventDate' <- (Just <$> (getDate "eventdate" <|> getOldDate "event")) <|>
                   return Nothing
    origDate' <- (Just <$> (getDate "origdate" <|> getOldDate "orig")) <|>
                   return Nothing
    accessed' <- (Just <$> (getDate "urldate" <|> getOldDate "url")) <|>
                    return Nothing

    -- locators
    pages' <- getFieldMaybe "pages"
    volume' <- getFieldMaybe "volume"
    part' <- getFieldMaybe "part"
    volumes' <- getFieldMaybe "volumes"
    pagetotal' <- getFieldMaybe "pagetotal"
    chapter' <- getFieldMaybe "chapter"
    edition' <- getFieldMaybe "edition"
    version' <- getFieldMaybe "version"
    (number', collectionNumber', issue') <-
       (getField "number" >>= \x ->
         if et `elem` ["book","collection","proceedings","reference",
                       "mvbook","mvcollection","mvproceedings", "mvreference",
                       "bookinbook","inbook", "incollection","inproceedings",
                       "inreference", "suppbook","suppcollection"]
         then return (Nothing, Just x, Nothing)
         else if isArticle
              then (getField "issue" >>= \y ->
                      return (Nothing, Nothing, Just $ concatWith ',' [x,y]))
                 <|> return (Nothing, Nothing, Just x)
              else return (Just x, Nothing, Nothing))
        <|> return (Nothing, Nothing, Nothing)

    -- titles
    hasMaintitle <- (True <$ getRawField "maintitle") <|> return False

    title' <- Just <$>
              ((guard isPeriodical >> getTitle "issuetitle")
              <|> (guard hasMaintitle >>
                   guard (not isChapterlike) >>
                   getTitle "maintitle")
              <|> getTitle "title")
              <|> return Nothing

    subtitle' <- (guard isPeriodical >> getTitle "issuesubtitle")
                  <|> (guard hasMaintitle >>
                       guard (not isChapterlike) >>
                       getTitle "mainsubtitle")
                  <|> getTitle "subtitle"
                  <|> return mempty
    titleaddon' <- (guard hasMaintitle >>
                     guard (not isChapterlike) >>
                     getTitle "maintitleaddon")
                    <|> getTitle "titleaddon"
                    <|> return mempty

    volumeTitle' <- Just <$>
                    ((guard hasMaintitle >>
                      guard (not isChapterlike) >>
                      getTitle "title")
                     <|> (guard hasMaintitle >>
                          guard isChapterlike >>
                          getTitle "booktitle"))
                    <|> return Nothing
    volumeSubtitle' <- (guard hasMaintitle >>
                        guard (not isChapterlike) >>
                        getTitle "subtitle")
                       <|> (guard hasMaintitle >>
                            guard isChapterlike >>
                            getTitle "booksubtitle")
                       <|> return mempty
    volumeTitleAddon' <- (guard hasMaintitle >>
                          guard (not isChapterlike) >>
                          getTitle "titleaddon")
                         <|> (guard hasMaintitle >>
                              guard isChapterlike >>
                              getTitle "booktitleaddon")
                         <|> return mempty

    containerTitle' <- Just <$>
                       ((guard isPeriodical >> getPeriodicalTitle "title")
                       <|> (guard isChapterlike >> getTitle "maintitle")
                       <|> (guard isChapterlike >> getTitle "booktitle")
                       <|> getPeriodicalTitle "journaltitle"
                       <|> getPeriodicalTitle "journal")
                       <|> return Nothing
    containerSubtitle' <- (guard isPeriodical >> getPeriodicalTitle "subtitle")
                          <|> (guard isChapterlike >> getTitle "mainsubtitle")
                          <|> (guard isChapterlike >> getTitle "booksubtitle")
                          <|> getPeriodicalTitle "journalsubtitle"
                          <|> return mempty
    containerTitleAddon' <- (guard isPeriodical >>
                             getPeriodicalTitle "titleaddon")
                            <|> (guard isChapterlike >>
                                 getTitle "maintitleaddon")
                            <|> (guard isChapterlike >>
                                 getTitle "booktitleaddon")
                            <|> return mempty
    containerTitleShort' <- Just <$>
                            ((guard isPeriodical >>
                              guard (not hasMaintitle) >>
                              getField "shorttitle")
                            <|> getPeriodicalTitle "shortjournal")
                           <|> return Nothing

    -- change numerical series title to e.g. 'series 3'
    let fixSeriesTitle [Str xs] | isNumber xs =
          [Str (ordinalize locale xs), Space, Str (resolveKey' lang "jourser")]
        fixSeriesTitle xs = xs
    seriesTitle' <- (Just . B.fromList . fixSeriesTitle .
                     B.toList . resolveKey lang <$>
                        getTitle "series") <|>
                    return Nothing
    shortTitle' <- (Just <$> (guard (not hasMaintitle || isChapterlike) >>
                              getTitle "shorttitle"))
                 <|> (if (subtitle' /= mempty || titleaddon' /= mempty) &&
                          not hasMaintitle
                          then getShortTitle False "title"
                          else getShortTitle True  "title")
                 <|> return Nothing

    eventTitle' <- Just <$> getTitle "eventtitle" <|> return Nothing
    origTitle' <- Just <$> getTitle "origtitle" <|> return Nothing

    -- publisher
    pubfields <- mapM (\f -> Just `fmap`
                         (if variant == Bibtex || f == "howpublished"
                          then getField f
                          else getLiteralList' f)
                        <|> return Nothing)
           ["school","institution","organization", "howpublished","publisher"]
    let publisher' = case catMaybes pubfields of
                       [] -> Nothing
                       xs -> Just $ concatWith ';' xs
    origpublisher' <- (Just <$> getField "origpublisher") <|> return Nothing

    -- places
    venue' <- (Just <$> getField "venue") <|> return Nothing
    address' <- Just <$>
                  (if variant == Bibtex
                      then getField "address"
                      else getLiteralList' "address"
                         <|> (guard (et /= "patent") >>
                              getLiteralList' "location"))
                <|> return Nothing
    origLocation' <- Just <$>
                  (if variant == Bibtex
                      then getField "origlocation"
                      else getLiteralList' "origlocation")
                    <|> return Nothing
    jurisdiction' <- if reftype == "patent"
                     then Just <$>
                        (concatWith ';' . map (resolveKey lang) <$>
                             getLiteralList "location") <|> return Nothing
                     else return Nothing

    -- url, doi, isbn, etc.:
    -- note that with eprinttype = arxiv, we take eprint to be a partial url
    -- archivePrefix is an alias for eprinttype
    url' <- (guard (et == "online" || lookup "url" opts /= Just "false")
             >> Just <$> getRawField "url")
         <|> (do etype <- getRawField "eprinttype"
                 eprint <- getRawField "eprint"
                 let baseUrl =
                       case T.toLower etype of
                         "arxiv"       -> "http://arxiv.org/abs/"
                         "jstor"       -> "http://www.jstor.org/stable/"
                         "pubmed"      -> "http://www.ncbi.nlm.nih.gov/pubmed/"
                         "googlebooks" -> "http://books.google.com?id="
                         _             -> ""
                 if T.null baseUrl
                    then mzero
                    else return $ Just $ baseUrl <> eprint)
         <|> return Nothing
    doi' <- (guard (lookup "doi" opts /= Just "false") >>
             Just <$> getRawField "doi")
           <|> return Nothing
    isbn' <- Just <$> getRawField "isbn" <|> return Nothing
    issn' <- Just <$> getRawField "issn" <|> return Nothing
    pmid' <- Just <$> getRawField  "pmid" <|> return Nothing
    pmcid' <- Just <$> getRawField "pmcid" <|> return Nothing
    callNumber' <- Just <$> getRawField "library" <|> return Nothing

    -- notes
    annotation' <- Just <$>
                   (getField "annotation" <|> getField "annote")
                     <|> return Nothing
    abstract' <- Just <$> getField "abstract" <|> return Nothing
    keywords' <- Just <$> getField "keywords" <|> return Nothing
    note' <- if et == "periodical"
             then return Nothing
             else Just <$> getField "note" <|> return Nothing
    addendum' <- if variant == Bibtex
                    then return Nothing
                    else Just <$> getField "addendum"
                 <|> return Nothing
    pubstate' <- (  (Just . resolveKey lang <$> getField "pubstate")
                  <|> case dateLiteral <$> issued' of
                           Just (Just "forthcoming") ->
                             return $ Just $ B.str "forthcoming"
                           _ -> return Nothing
                   )




    let addField (_, Nothing) = id
        addField (f, Just x)  = Map.insert f x
    let vars = foldr addField mempty
                [ ("other-ids", TextVal <$> otherIds)
                , ("genre", TextVal <$> genre)
                , ("language", TextVal <$> hyphenation)
                -- dates
                , ("accessed", DateVal <$> accessed')
                , ("event-date", DateVal <$> eventDate')
                , ("issued", DateVal <$> issued')
                , ("original-date", DateVal <$> origDate')
                -- names
                , ("author", NamesVal <$> author')
                , ("editor", NamesVal <$> editor')
                , ("translator", NamesVal <$> translator')
                , ("director", NamesVal <$> director')
                , ("container-author", NamesVal <$> containerAuthor')
                -- locators
                , ("page", FancyVal . Walk.walk convertEnDash <$> pages')
                , ("number-of-pages", FancyVal <$> pagetotal')
                , ("volume", case (volume', part') of
                               (Nothing, Nothing) -> Nothing
                               (Just v, Nothing) -> Just $ FancyVal v
                               (Nothing, Just p) -> Just $ FancyVal p
                               (Just v, Just p)  ->
                                 Just $ FancyVal $ v <> B.str "." <> p)
                , ("number-of-volumes", FancyVal <$> volumes')
                , ("chapter-number", FancyVal <$> chapter')
                , ("edition", FancyVal <$> edition')
                , ("version", FancyVal <$> version')
                , ("number", FancyVal <$> number')
                , ("collection-number", FancyVal <$> collectionNumber')
                , ("issue", FancyVal <$> issue')
                -- title
                , ("original-title", FancyVal <$> origTitle')
                , ("event", FancyVal <$> eventTitle')
                , ("title", case title' of
                              Just t -> Just $ FancyVal $
                                         concatWith '.' [
                                             concatWith ':' [t, subtitle']
                                           , titleaddon' ]
                              Nothing -> Nothing)
                , ("volume-title",
                            case volumeTitle' of
                              Just t -> Just $ FancyVal $
                                         concatWith '.' [
                                             concatWith ':' [t, volumeSubtitle']
                                           , volumeTitleAddon' ]
                              Nothing -> Nothing)
                , ("container-title",
                            case containerTitle' of
                              Just t -> Just $ FancyVal $
                                         concatWith '.' [
                                             concatWith ':' [t,
                                               containerSubtitle']
                                           , containerTitleAddon' ]
                              Nothing -> Nothing)
                , ("container-title-short", FancyVal <$> containerTitleShort')
                , ("collection-title", FancyVal <$> seriesTitle')
                , ("title-short", FancyVal <$> shortTitle')
                -- publisher
                , ("publisher", FancyVal <$> publisher')
                , ("original-publisher", FancyVal <$> origpublisher')
                -- places
                , ("jurisdiction", FancyVal <$> jurisdiction')
                , ("event-place",  FancyVal <$> venue')
                , ("publisher-place", FancyVal <$> address')
                , ("original-publisher-place", FancyVal <$> origLocation')
                -- urls
                , ("url", TextVal <$> url')
                , ("doi", TextVal <$> doi')
                , ("isbn", TextVal <$> isbn')
                , ("issn", TextVal <$> issn')
                , ("pmcid", TextVal <$> pmcid')
                , ("pmid", TextVal <$> pmid')
                , ("call-number", TextVal <$> callNumber')
                -- notes
                , ("note", case catMaybes [note', addendum'] of
                             [] -> Nothing
                             xs -> return $ FancyVal $ concatWith '.' xs)
                , ("annote", FancyVal <$> annotation')
                , ("abstract", FancyVal <$> abstract')
                , ("keyword", FancyVal <$> keywords')
                , ("status", FancyVal <$> pubstate')
                ]
    return $ Reference
      { referenceId             = ItemId id'
      , referenceType           = reftype
      , referenceDisambiguation = Nothing
      , referenceVariables      = vars }


bib :: Item -> Bib a -> BibParser a
bib entry m = fst <$> evalRWST m entry (BibState True (Lang "en" (Just "US")))

resolveCrossRefs :: Variant -> [Item] -> [Item]
resolveCrossRefs variant entries =
  map (resolveCrossRef variant entries) entries

resolveCrossRef :: Variant -> [Item] -> Item -> Item
resolveCrossRef variant entries entry =
  Map.foldrWithKey go entry (fields entry)
  where go key val entry' =
          if key == "crossref" || key == "xdata"
          then entry'{ fields = fields entry' <>
                          Map.fromList (getXrefFields variant
                                        entry entries val) }
          else entry'

getXrefFields :: Variant -> Item -> [Item] -> Text -> [(Text, Text)]
getXrefFields variant baseEntry entries keys = do
  let keys' = splitKeys keys
  xrefEntry <- [e | e <- entries, identifier e `elem` keys']
  (k, v) <- Map.toList $ fields xrefEntry
  if k == "crossref" || k == "xdata"
     then do
       xs <- mapM (getXrefFields variant baseEntry entries)
                   (splitKeys v)
       (x, y) <- xs
       guard $ isNothing $ Map.lookup x $ fields xrefEntry
       return (x, y)
     else do
       k' <- case variant of
               Bibtex -> return k
               Biblatex -> transformKey
                            (entryType xrefEntry) (entryType baseEntry) k
       guard $ isNothing $ Map.lookup k' $ fields baseEntry
       return (k',v)



data BibState = BibState{
           untitlecase    :: Bool
         , localeLang     :: Lang
         }

type Bib = RWST Item () BibState BibParser

blocksToInlines :: [Block] -> Inlines
blocksToInlines bs =
  case bs of
       [Plain xs] -> B.fromList xs
       [Para  xs] -> B.fromList xs
       _          -> B.fromList $ Walk.query (:[]) bs

adjustSpans :: Lang -> Inline -> Inline
adjustSpans lang (RawInline (Format "latex") s)
  | s == "\\hyphen" || s == "\\hyphen " = Str "-"
  | otherwise = parseRawLaTeX lang s
adjustSpans _ SoftBreak = Space
adjustSpans _ x = x

parseRawLaTeX :: Lang -> Text -> Inline
parseRawLaTeX lang t@(T.stripPrefix "\\" -> Just xs) =
  case parseLaTeX lang contents of
          Right [Para ys]  -> f command ys
          Right [Plain ys] -> f command ys
          Right []         -> f command []
          _                -> RawInline (Format "latex") t
   where (command', contents') = T.break (\c -> c =='{' || c =='\\') xs
         command  = T.strip command'
         contents = T.drop 1 $ T.dropEnd 1 contents'
         f "mkbibquote"    ils = Span nullAttr [Quoted DoubleQuote ils]
         f "mkbibemph"     ils = Span nullAttr [Emph ils]
         f "mkbibitalic"   ils = Span nullAttr [Emph ils]
         f "mkbibbold"     ils = Span nullAttr [Strong ils]
         f "mkbibparens"   ils = Span nullAttr $
                                  [Str "("] ++ ils ++ [Str ")"]
         f "mkbibbrackets" ils = Span nullAttr $
                                  [Str "["] ++ ils ++ [Str "]"]
         -- ... both should be nestable & should work in year fields
         f "autocap"    ils    = Span nullAttr ils
           -- TODO: should work in year fields
         f "textnormal" ils    = Span ("",["nodecor"],[]) ils
         f "bibstring" [Str s] = Str $ resolveKey' lang s
         f "adddot"    []      = Str "."
         f "adddotspace" []    = Span nullAttr [Str ".", Space]
         f "addabbrvspace" []  = Space
         f _            ils    = Span nullAttr ils
parseRawLaTeX _ t = RawInline (Format "latex") t

latex' :: Text -> Bib [Block]
latex' t = do
  lang <- gets localeLang
  case parseLaTeX lang t of
    Left _   -> mzero
    Right bs -> return bs

parseLaTeX :: Lang -> Text -> Either PandocError [Block]
parseLaTeX lang t =
  case runPure (readLaTeX
                def{ readerExtensions =
                      extensionsFromList [Ext_raw_tex, Ext_smart] } t) of
    Left e              -> Left e
    Right (Pandoc _ bs) -> Right $ Walk.walk (adjustSpans lang) bs

latex :: Text -> Bib Inlines
latex = fmap blocksToInlines . latex' . T.strip

type Options = [(Text, Text)]

parseOptions :: Text -> Options
parseOptions = map breakOpt . T.splitOn ","
  where breakOpt x = case T.break (=='=') x of
                          (w,v) -> (T.toLower $ T.strip w,
                                    T.toLower $ T.strip $ T.drop 1 v)

bibEntries :: BibParser [Item]
bibEntries = do
  skipMany nonEntry
  many (bibItem <* skipMany nonEntry)
 where nonEntry = bibSkip <|>
                  try (char '@' >>
                       (bibComment <|> bibPreamble <|> bibString))

bibSkip :: BibParser ()
bibSkip = () <$ take1WhileP (/='@')

bibComment :: BibParser ()
bibComment = do
  cistring "comment"
  spaces
  void inBraces <|> bibSkip <|> return ()

bibPreamble :: BibParser ()
bibPreamble = do
  cistring "preamble"
  spaces
  void inBraces

bibString :: BibParser ()
bibString = do
  cistring "string"
  spaces
  char '{'
  spaces
  (k,v) <- entField
  char '}'
  updateState (\(l,m) -> (l, Map.insert k v m))
  return ()

inBraces :: BibParser Text
inBraces = do
  char '{'
  res <- manyTill
         (  (T.pack <$> many1 (noneOf "{}\\"))
        <|> (char '\\' >> (  (char '{' >> return "\\{")
                         <|> (char '}' >> return "\\}")
                         <|> return "\\"))
        <|> (braced <$> inBraces)
         ) (char '}')
  return $ T.concat res

braced :: Text -> Text
braced = T.cons '{' . flip T.snoc '}'

inQuotes :: BibParser Text
inQuotes = do
  char '"'
  T.concat <$> manyTill
             (  (T.pack <$> many1 (noneOf "\"\\{"))
               <|> (char '\\' >> T.cons '\\' . T.singleton <$> anyChar)
               <|> braced <$> inBraces
            ) (char '"')

fieldName :: BibParser Text
fieldName = resolveAlias . T.toLower
  <$> take1WhileP (\c ->
         isAlphaNum c || c == '-' || c == '_' || c == ':' || c == '+')

isBibtexKeyChar :: Char -> Bool
isBibtexKeyChar c =
  isAlphaNum c || c `elem` (".:;?!`'()/*@_+=-[]*&" :: [Char])

bibItem :: BibParser Item
bibItem = do
  char '@'
  pos <- getPosition
  enttype <- T.toLower <$> take1WhileP isLetter
  spaces
  char '{'
  spaces
  entid <- take1WhileP isBibtexKeyChar
  spaces
  char ','
  spaces
  entfields <- entField `sepEndBy` (char ',' >> spaces)
  spaces
  char '}'
  return $ Item entid pos enttype (Map.fromList entfields)

entField :: BibParser (Text, Text)
entField = do
  k <- fieldName
  spaces
  char '='
  spaces
  vs <- (expandString <|> inQuotes <|> inBraces <|> rawWord) `sepBy`
            try (spaces >> char '#' >> spaces)
  spaces
  return (k, T.concat vs)

resolveAlias :: Text -> Text
resolveAlias "archiveprefix" = "eprinttype"
resolveAlias "primaryclass" = "eprintclass"
resolveAlias s = s

rawWord :: BibParser Text
rawWord = take1WhileP isAlphaNum

expandString :: BibParser Text
expandString = do
  k <- fieldName
  (lang, strs) <- getState
  case Map.lookup k strs of
       Just v  -> return v
       Nothing -> return $ resolveKey' lang k

cistring :: Text -> BibParser Text
cistring s = try (go s)
 where go t = case T.uncons t of
         Nothing     -> return ""
         Just (c,cs) -> do
           x <- char (toLower c) <|> char (toUpper c)
           xs <- go cs
           return (T.cons x xs)

splitKeys :: Text -> [Text]
splitKeys = filter (not . T.null) . T.split (\c -> c == ' ' || c == ',')

-- Biblatex Localization Keys (see Biblatex manual)
-- Currently we only map a subset likely to be used in Biblatex *databases*
-- (in fields such as `type`, and via `\bibstring{}` commands).

parseMonth :: Text -> Maybe Int
parseMonth s =
  case T.toLower s of
         "jan" -> Just 1
         "feb" -> Just 2
         "mar" -> Just 3
         "apr" -> Just 4
         "may" -> Just 5
         "jun" -> Just 6
         "jul" -> Just 7
         "aug" -> Just 8
         "sep" -> Just 9
         "oct" -> Just 10
         "nov" -> Just 11
         "dec" -> Just 12
         _     -> readMay (T.unpack s)

notFound :: Text -> Bib a
notFound f = Prelude.fail $ T.unpack f ++ " not found"

getField :: Text -> Bib Inlines
getField f = do
  fs <- asks fields
  case Map.lookup f fs of
       Just x  -> latex x
       Nothing -> notFound f


getPeriodicalTitle :: Text -> Bib Inlines
getPeriodicalTitle f = do
  ils <- getField f
  return ils

protectCase :: (Inlines -> Inlines) -> (Inlines -> Inlines)
protectCase f = Walk.walk unprotect . f . Walk.walk protect
 where
  protect (Span ("",[],[]) xs) = Span ("",["nocase"],[]) xs
  protect  x = x
  unprotect (Span ("",["nocase"],[]) xs)
    | hasLowercaseWord xs = Span ("",["nocase"],[]) xs
    | otherwise           = Span ("",[],[]) xs
  unprotect x = x
  hasLowercaseWord = any startsWithLowercase . splitStrWhen isPunctuation
  startsWithLowercase (Str (T.uncons -> Just (x,_))) = isLower x
  startsWithLowercase _           = False

unTitlecase :: Maybe Lang -> Inlines -> Inlines
unTitlecase mblang = protectCase (addTextCase mblang SentenceCase)

getTitle :: Text -> Bib Inlines
getTitle f = do
  ils <- getField f
  utc <- gets untitlecase
  lang <- gets localeLang
  let processTitle = if utc then unTitlecase (Just lang) else id
  return $ processTitle ils

getShortTitle :: Bool -> Text -> Bib (Maybe Inlines)
getShortTitle requireColon f = do
  ils <- splitStrWhen (==':') . B.toList <$> getTitle f
  if not requireColon || containsColon ils
     then return $ Just $ B.fromList $ upToColon ils
     else return Nothing

containsColon :: [Inline] -> Bool
containsColon xs = Str ":" `elem` xs

upToColon :: [Inline] -> [Inline]
upToColon xs = takeWhile (/= Str ":") xs

isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Just ('-', ds) -> T.all isDigit ds
  Just _         -> T.all isDigit t
  Nothing        -> False

getDate :: Text -> Bib Date
getDate f = do
  -- the ~ can used for approx dates, but the latex reader
  -- parses this as a nonbreaking space, so we need to convert it back!
  let nbspToTilde '\160' = '~'
      nbspToTilde c      = c
  mbd <- rawDateEDTF . T.map nbspToTilde <$> getRawField f
  case mbd of
    Nothing -> Prelude.fail "expected date"
    Just d  -> return d

-- A negative (BC) year might be written with -- or --- in bibtex:
fixLeadingDash :: Text -> Text
fixLeadingDash t = case T.uncons t of
  Just (c, ds) | (c == '–' || c == '—') && firstIsDigit ds -> T.cons '–' ds
  _ -> t
 where firstIsDigit = maybe False (isDigit . fst) . T.uncons

getOldDate :: Text -> Bib Date
getOldDate prefix = do
  year' <- (readMay . T.unpack . fixLeadingDash . stringify
              <$> getField (prefix <> "year")) <|> return Nothing
  month' <- (parseMonth <$> getRawField (prefix <> "month"))
            <|> return Nothing
  day' <- (readMay . T.unpack <$> getRawField (prefix <> "day"))
          <|> return Nothing
  endyear' <- (readMay . T.unpack . fixLeadingDash . stringify
              <$> getField (prefix <> "endyear")) <|> return Nothing
  endmonth' <- (parseMonth . stringify
                 <$> getField (prefix <> "endmonth")) <|> return Nothing
  endday' <- (readMay . T.unpack . stringify <$>
                 getField (prefix <> "endday")) <|> return Nothing
  let toDateParts (y', m', d') =
              DateParts $
                 case y' of
                   Nothing -> []
                   Just y  ->
                     case m' of
                       Nothing -> [y]
                       Just m  ->
                         case d' of
                           Nothing -> [y,m]
                           Just d  -> [y,m,d]
  let dateparts = filter (\x -> x /= DateParts [])
                  $ map toDateParts [(year',month',day'),
                                     (endyear',endmonth',endday')]
  literal <- if null dateparts
                then Just <$> getRawField (prefix <> "year")
                else return Nothing
  return $
    Date { dateParts = dateparts
         , dateCirca = False
         , dateSeason = Nothing
         , dateLiteral = literal }

getRawField :: Text -> Bib Text
getRawField f =
  (stringify <$> getField f)
  <|> do fs <- asks fields
         case Map.lookup f fs of
              Just x  -> return x
              Nothing -> notFound f

getLiteralList :: Text -> Bib [Inlines]
getLiteralList f = do
  fs <- asks fields
  case Map.lookup f fs of
       Just x  -> latex' x >>= toLiteralList
       Nothing -> notFound f

-- separates items with semicolons
getLiteralList' :: Text -> Bib Inlines
getLiteralList' f = do
  fs <- asks fields
  case Map.lookup f fs of
    Just x    -> do
      x' <- latex' x
      case x' of
        [Para xs]  ->
          return $ B.fromList
                 $ intercalate [Str ";", Space]
                 $ splitByAnd xs
        [Plain xs] ->
          return $ B.fromList
                 $ intercalate [Str ";", Space]
                 $ splitByAnd xs
        _          -> mzero
    Nothing   -> notFound f

splitByAnd :: [Inline] -> [[Inline]]
splitByAnd = splitOn [Space, Str "and", Space]

toLiteralList :: [Block] -> Bib [Inlines]
toLiteralList [Para xs] =
  return $ map B.fromList $ splitByAnd xs
toLiteralList [Plain xs] = toLiteralList [Para xs]
toLiteralList _ = mzero

concatWith :: Char -> [Inlines] -> Inlines
concatWith sep = foldl' go mempty
  where go :: Inlines -> Inlines -> Inlines
        go accum s
          | s == mempty = accum
          | otherwise   =
              case Seq.viewr (B.unMany accum) of
                     Seq.EmptyR -> s
                     _ Seq.:> Str x
                       | not (T.null x) &&
                         T.last x `elem` ("!?.,:;" :: String)
                                    -> accum <> B.space <> s
                     _ -> accum <> B.str (T.singleton sep) <>
                                                B.space <> s


getNameList :: Options -> Text -> Bib [Name]
getNameList opts  f = do
  fs <- asks fields
  case Map.lookup f fs of
       Just x  -> latexNames opts x
       Nothing -> notFound f

toNameList :: Options -> [Block] -> Bib [Name]
toNameList opts [Para xs] =
  filter (/= emptyName) <$> mapM (toName opts . addSpaceAfterPeriod)
                                    (splitByAnd xs)
toNameList opts [Plain xs] = toNameList opts [Para xs]
toNameList _ _ = mzero

latexNames :: Options -> Text -> Bib [Name]
latexNames opts t = latex' (T.strip t) >>= toNameList opts

-- see issue 392 for motivation.  We want to treat
-- "J.G. Smith" and "J. G. Smith" the same.
addSpaceAfterPeriod :: [Inline] -> [Inline]
addSpaceAfterPeriod = go . splitStrWhen (=='.')
  where
    go [] = []
    go (Str (T.unpack -> [c]):Str ".":Str (T.unpack -> [d]):xs)
      | isLetter d
      , isLetter c
      , isUpper c
      , isUpper d
        = Str (T.singleton c):Str ".":Space:go (Str (T.singleton d):xs)
    go (x:xs) = x:go xs

emptyName :: Name
emptyName =
    Name {  nameFamily              = Nothing
          , nameGiven               = Nothing
          , nameDroppingParticle    = Nothing
          , nameNonDroppingParticle = Nothing
          , nameSuffix              = Nothing
          , nameLiteral             = Nothing
          , nameCommaSuffix         = False
          , nameStaticOrdering      = False
          }

toName :: Options -> [Inline] -> Bib Name
toName _ [Str "others"] =
  return emptyName{ nameLiteral = Just "others" }
toName _ [Span ("",[],[]) ils] = -- corporate author
  return emptyName{ nameLiteral = Just $ stringify ils }
 -- extended BibLaTeX name format - see #266
toName _ ils@(Str ys:_) | T.any (== '=') ys = do
  let commaParts = splitWhen (== Str ",")
                   . splitStrWhen (\c -> c == ',' || c == '=' || c == '\160')
                   $ ils
  let addPart ag (Str "given" : Str "=" : xs) =
        ag{ nameGiven = case nameGiven ag of
                          Nothing -> Just $ stringify xs
                          Just t  -> Just $ t <> " " <> stringify xs }
      addPart ag (Str "family" : Str "=" : xs) =
        ag{ nameFamily = Just $ stringify xs }
      addPart ag (Str "prefix" : Str "=" : xs) =
        ag{ nameDroppingParticle =  Just $ stringify xs }
      addPart ag (Str "useprefix" : Str "=" : Str "true" : _) =
        ag{ nameNonDroppingParticle = nameDroppingParticle ag
          , nameDroppingParticle    = Nothing }
      addPart ag (Str "suffix" : Str "=" : xs) =
        ag{ nameSuffix = Just $ stringify xs }
      addPart ag (Space : xs) = addPart ag xs
      addPart ag _ = ag
  return $ foldl' addPart emptyName commaParts
-- First von Last
-- von Last, First
-- von Last, Jr ,First
-- NOTE: biblatex and bibtex differ on:
-- Drummond de Andrade, Carlos
-- bibtex takes "Drummond de" as the von;
-- biblatex takes the whole as a last name.
-- See https://github.com/plk/biblatex/issues/236
-- Here we implement the more sensible biblatex behavior.
toName opts ils = do
  let useprefix = optionSet "useprefix" opts
  let usecomma  = optionSet "juniorcomma" opts
  let bibtex    = optionSet "bibtex" opts
  let words' = wordsBy (\x -> x == Space || x == Str "\160")
  let commaParts = map words' $ splitWhen (== Str ",")
                              $ splitStrWhen
                                   (\c -> c == ',' || c == '\160') ils
  let (first, vonlast, jr) =
          case commaParts of
               --- First is the longest sequence of white-space separated
               -- words starting with an uppercase and that is not the
               -- whole string. von is the longest sequence of whitespace
               -- separated words whose last word starts with lower case
               -- and that is not the whole string.
               [fvl]      -> let (caps', rest') = span isCapitalized fvl
                             in  if null rest' && not (null caps')
                                 then (init caps', [last caps'], [])
                                 else (caps', rest', [])
               [vl,f]     -> (f, vl, [])
               (vl:j:f:_) -> (f, vl, j )
               []         -> ([], [], [])

  let (von, lastname) =
         if bibtex
            then case span isCapitalized $ reverse vonlast of
                        ([],w:ws) -> (reverse ws, [w])
                        (vs, ws)    -> (reverse ws, reverse vs)
            else case break isCapitalized vonlast of
                        (vs@(_:_), []) -> (init vs, [last vs])
                        (vs, ws)       -> (vs, ws)
  let prefix = T.unwords $ map stringify von
  let family = T.unwords $ map stringify lastname
  let suffix = T.unwords $ map stringify jr
  let given = T.unwords $ map stringify first
  return
    Name {  nameFamily              = if T.null family
                                         then Nothing
                                         else Just family
          , nameGiven               = if T.null given
                                         then Nothing
                                         else Just given
          , nameDroppingParticle    = if useprefix || T.null prefix
                                         then Nothing
                                         else Just prefix
          , nameNonDroppingParticle = if useprefix && not (T.null prefix)
                                         then Just prefix
                                         else Nothing
          , nameSuffix              = if T.null suffix
                                         then Nothing
                                         else Just suffix
          , nameLiteral             = Nothing
          , nameCommaSuffix         = usecomma
          , nameStaticOrdering      = False
          }

splitStrWhen :: (Char -> Bool) -> [Inline] -> [Inline]
splitStrWhen _ [] = []
splitStrWhen p (Str xs : ys) = map Str (go xs) ++ splitStrWhen p ys
  where go s =
          let (w,z) = T.break p s
           in if T.null z
                 then if T.null w
                         then []
                         else [w]
                 else if T.null w
                         then (T.take 1 z : go (T.drop 1 z))
                         else (w : T.take 1 z : go (T.drop 1 z))
splitStrWhen p (x : ys) = x : splitStrWhen p ys

ordinalize :: Locale -> Text -> Text
ordinalize locale n =
  let terms = localeTerms locale
      pad0 t = case T.length t of
                 0 -> "00"
                 1 -> "0" <> t
                 _ -> t
   in case Map.lookup ("ordinal-" <> pad0 n) terms <|>
           Map.lookup "ordinal" terms of
        Nothing    -> n
        Just []    -> n
        Just (t:_) -> n <> snd t

isCapitalized :: [Inline] -> Bool
isCapitalized (Str (T.uncons -> Just (c,cs)) : rest)
  | isUpper c = True
  | isDigit c = isCapitalized (Str cs : rest)
  | otherwise = False
isCapitalized (_:rest) = isCapitalized rest
isCapitalized [] = True

optionSet :: Text -> Options -> Bool
optionSet key opts = case lookup key opts of
                      Just "true" -> True
                      Just s      -> s == mempty
                      _           -> False

getTypeAndGenre :: Bib (Text, Maybe Text)
getTypeAndGenre = do
  lang <- gets localeLang
  et <- asks entryType
  reftype' <- resolveKey' lang <$> getRawField "type"
         <|> return mempty
  st <- getRawField "entrysubtype" <|> return mempty
  isEvent <- (True <$ (getRawField "eventdate"
                     <|> getRawField "eventtitle"
                     <|> getRawField "venue")) <|> return False
  let reftype =
        case et of
           "article"
             | st == "magazine"  -> "article-magazine"
             | st == "newspaper" -> "article-newspaper"
             | otherwise         -> "article-journal"
           "book"                -> "book"
           "booklet"             -> "pamphlet"
           "bookinbook"          -> "chapter"
           "collection"          -> "book"
           "dataset"             -> "dataset"
           "electronic"          -> "webpage"
           "inbook"              -> "chapter"
           "incollection"        -> "chapter"
           "inreference"         -> "entry-encyclopedia"
           "inproceedings"       -> "paper-conference"
           "manual"              -> "book"
           "mastersthesis"       -> "thesis"
           "misc"                -> ""
           "mvbook"              -> "book"
           "mvcollection"        -> "book"
           "mvproceedings"       -> "book"
           "mvreference"         -> "book"
           "online"              -> "webpage"
           "patent"              -> "patent"
           "periodical"
             | st == "magazine"  -> "article-magazine"
             | st == "newspaper" -> "article-newspaper"
             | otherwise         -> "article-journal"
           "phdthesis"           -> "thesis"
           "proceedings"         -> "book"
           "reference"           -> "book"
           "report"              -> "report"
           "software"            -> "book"    -- no "software" type in CSL
           "suppbook"            -> "chapter"
           "suppcollection"      -> "chapter"
           "suppperiodical"
             | st == "magazine"  -> "article-magazine"
             | st == "newspaper" -> "article-newspaper"
             | otherwise         -> "article-journal"
           "techreport"          -> "report"
           "thesis"              -> "thesis"
           "unpublished"         -> if isEvent then "speech" else "manuscript"
           "www"                 -> "webpage"
           -- biblatex, "unsupported"
           "artwork"             -> "graphic"
           "audio"               -> "song"    -- for audio *recordings*
           "commentary"          -> "book"
           "image"               -> "graphic"   -- or "figure" ?
           "jurisdiction"        -> "legal_case"
           "legislation"         -> "legislation"  -- or "bill" ?
           "legal"               -> "treaty"
           "letter"              -> "personal_communication"
           "movie"               -> "motion_picture"
           "music"               -> "song"        -- for musical *recordings*
           "performance"         -> "speech"
           "review"              -> "review"      -- or "review-book" ?
           "standard"            -> "legislation"
           "video"               -> "motion_picture"
           -- biblatex-apa:
           "data"                -> "dataset"
           "letters"             -> "personal_communication"
           "newsarticle"         -> "article-newspaper"
           _                     -> ""

  let refgenre =
        case et of
          "mastersthesis"  -> if T.null reftype'
                                 then Just $ resolveKey' lang "mathesis"
                                 else Just reftype'
          "phdthesis"      -> if T.null reftype'
                                 then Just $ resolveKey' lang "phdthesis"
                                 else Just reftype'
          _                -> if T.null reftype'
                                 then Nothing
                                 else Just reftype'
  return (reftype, refgenre)


-- transformKey source target key
-- derived from Appendix C of bibtex manual
transformKey :: Text -> Text -> Text -> [Text]
transformKey _ _ "ids"            = []
transformKey _ _ "crossref"       = []
transformKey _ _ "xref"           = []
transformKey _ _ "entryset"       = []
transformKey _ _ "entrysubtype"   = []
transformKey _ _ "execute"        = []
transformKey _ _ "label"          = []
transformKey _ _ "options"        = []
transformKey _ _ "presort"        = []
transformKey _ _ "related"        = []
transformKey _ _ "relatedoptions" = []
transformKey _ _ "relatedstring"  = []
transformKey _ _ "relatedtype"    = []
transformKey _ _ "shorthand"      = []
transformKey _ _ "shorthandintro" = []
transformKey _ _ "sortkey"        = []
transformKey x y "author"
  | x `elem` ["mvbook", "book"] &&
    y `elem` ["inbook", "bookinbook", "suppbook"] = ["bookauthor", "author"]
-- note: this next clause is not in the biblatex manual, but it makes
-- sense in the context of CSL conversion:
transformKey x y "author"
  | x == "mvbook" && y == "book" = ["bookauthor", "author"]
transformKey "mvbook" y z
  | y `elem` ["book", "inbook", "bookinbook", "suppbook"] = standardTrans z
transformKey x y z
  | x `elem` ["mvcollection", "mvreference"] &&
    y `elem` ["collection", "reference", "incollection", "inreference",
               "suppcollection"] = standardTrans z
transformKey "mvproceedings" y z
  | y `elem` ["proceedings", "inproceedings"] = standardTrans z
transformKey "book" y z
  | y `elem` ["inbook", "bookinbook", "suppbook"] = bookTrans z
transformKey x y z
  | x `elem` ["collection", "reference"] &&
    y `elem` ["incollection", "inreference", "suppcollection"] = bookTrans z
transformKey "proceedings" "inproceedings" z = bookTrans z
transformKey "periodical" y z
  | y `elem` ["article", "suppperiodical"] =
  case z of
       "title"          -> ["journaltitle"]
       "subtitle"       -> ["journalsubtitle"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]
transformKey _ _ x                = [x]

standardTrans :: Text -> [Text]
standardTrans z =
  case z of
       "title"          -> ["maintitle"]
       "subtitle"       -> ["mainsubtitle"]
       "titleaddon"     -> ["maintitleaddon"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]

bookTrans :: Text -> [Text]
bookTrans z =
  case z of
       "title"          -> ["booktitle"]
       "subtitle"       -> ["booksubtitle"]
       "titleaddon"     -> ["booktitleaddon"]
       "shorttitle"     -> []
       "sorttitle"      -> []
       "indextitle"     -> []
       "indexsorttitle" -> []
       _                -> [z]

resolveKey :: Lang -> Inlines -> Inlines
resolveKey lang ils = Walk.walk go ils
  where go (Str s) = Str $ resolveKey' lang s
        go x       = x

resolveKey' :: Lang -> Text -> Text
resolveKey' lang@(Lang l _) k =
  case Map.lookup l biblatexStringMap >>= Map.lookup (T.toLower k) of
    Nothing     -> k
    Just (x, _) -> either (const k) stringify $ parseLaTeX lang x

convertEnDash :: Inline -> Inline
convertEnDash (Str s) = Str (T.map (\c -> if c == '–' then '-' else c) s)
convertEnDash x       = x

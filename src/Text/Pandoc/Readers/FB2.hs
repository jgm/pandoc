{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.FB2
   Copyright   : Copyright (C) 2018-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of FB2 to 'Pandoc' document.
-}

{-

TODO:
 - Tables
 - Named styles
 - Parse ID attribute for all elements that have it

-}

module Text.Pandoc.Readers.FB2 ( readFB2 ) where
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import Data.ByteString.Base64.Lazy
import Data.Functor
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Maybe
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad, insertMedia, report)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

type FB2 m = StateT FB2State m

data FB2State = FB2State{ fb2SectionLevel :: Int
                        , fb2Meta :: Meta
                        , fb2Authors :: [Text]
                        , fb2Notes :: M.Map Text Blocks
                        } deriving Show

instance Default FB2State where
  def = FB2State{ fb2SectionLevel = 1
                , fb2Meta = mempty
                , fb2Authors = []
                , fb2Notes = M.empty
                }

instance HasMeta FB2State where
  setMeta field v s = s {fb2Meta = setMeta field v (fb2Meta s)}
  deleteMeta field s = s {fb2Meta = deleteMeta field (fb2Meta s)}

readFB2 :: (PandocMonad m, ToSources a)
        => ReaderOptions
        -> a
        -> m Pandoc
readFB2 _ inp =
  case parseXMLElement $ TL.fromStrict $ sourcesToText $ toSources inp of
    Left msg -> throwError $ PandocXMLError "" msg
    Right el ->  do
      (bs, st) <- runStateT (parseRootElement el) def
      let authors = if null $ fb2Authors st
                    then id
                    else setMeta "author" (map text $ reverse $ fb2Authors st)
      pure $ Pandoc (authors $ fb2Meta st) $ toList bs

-- * Utility functions

trim :: Text -> Text
trim = T.strip

removeHash :: Text -> Text
removeHash t = case T.uncons t of
  Just ('#', xs) -> xs
  _              -> t

convertEntity :: Text -> Text
convertEntity e = maybe (T.toUpper e) T.pack $ lookupEntity (T.unpack e)

parseInline :: PandocMonad m => Content -> FB2 m Inlines
parseInline (Elem e) =
  case qName $ elName e of
    "strong" -> strong <$> parseStyleType e
    "emphasis" -> emph <$> parseStyleType e
    "style" -> parseNamedStyle e
    "a" -> parseLinkType e
    "strikethrough" -> strikeout <$> parseStyleType e
    "sub" -> subscript <$> parseStyleType e
    "sup" -> superscript <$> parseStyleType e
    "code" -> pure $ code $ strContent e
    "image" -> parseInlineImageElement e
    name -> do
      report $ IgnoredElement name
      pure mempty
parseInline (Text x) = pure $ text $ cdData x
parseInline (CRef r) = pure $ str $ convertEntity r

parseSubtitle :: PandocMonad m => Element -> FB2 m Blocks
parseSubtitle e = headerWith ("", ["unnumbered"], []) <$> gets fb2SectionLevel <*> parsePType e

-- * Root element parser

parseRootElement :: PandocMonad m => Element -> FB2 m Blocks
parseRootElement e =
  case qName $ elName e of
    "FictionBook" -> do
      -- Parse notes before parsing the rest of the content.
      case filterChild isNotesBody e of
        Nothing -> pure ()
        Just notesBody -> parseNotesBody notesBody
      -- Parse metadata and content
      mconcat <$> mapM parseFictionBookChild (elChildren e)
    name -> report (UnexpectedXmlElement name "root") $> mempty

-- | Parse notes
parseNotesBody :: PandocMonad m => Element -> FB2 m ()
parseNotesBody e = mempty <$ mapM parseNotesBodyChild (elChildren e)

-- | Parse a child of @\<body name="notes">@ element.
parseNotesBodyChild :: PandocMonad m => Element -> FB2 m ()
parseNotesBodyChild e =
  case qName $ elName e of
    "section" -> parseNote e
    _ -> pure ()

isNotesBody :: Element -> Bool
isNotesBody e =
  qName (elName e) == "body" &&
  findAttr (unqual "name") e == Just "notes"

parseNote :: PandocMonad m => Element -> FB2 m ()
parseNote e =
  case findAttr (unqual "id") e of
    Nothing -> pure ()
    Just sectionId -> do
      content <- mconcat <$> mapM parseSectionChild (dropTitle $ elChildren e)
      oldNotes <- gets fb2Notes
      modify $ \s -> s { fb2Notes = M.insert ("#" <> sectionId) content oldNotes }
      pure ()
  where
    isTitle x = qName (elName x) == "title"
    dropTitle (x:xs) = if isTitle x
                         then xs -- Drop note section <title> if present
                         else x:xs
    dropTitle [] = []

-- | Parse a child of @\<FictionBook>@ element.
parseFictionBookChild :: PandocMonad m => Element -> FB2 m Blocks
parseFictionBookChild e =
  case qName $ elName e of
    "stylesheet" -> pure mempty -- stylesheet is ignored
    "description" -> mempty <$ mapM_ parseDescriptionChild (elChildren e)
    "body" -> if isNotesBody e
                then pure mempty
                else mconcat <$> mapM parseBodyChild (elChildren e)
    "binary" -> mempty <$ parseBinaryElement e
    name -> report (UnexpectedXmlElement name "FictionBook") $> mempty

-- | Parse a child of @\<description>@ element.
parseDescriptionChild :: PandocMonad m => Element -> FB2 m ()
parseDescriptionChild e =
  case qName $ elName e of
    "title-info" -> mapM_ parseTitleInfoChild (elChildren e)
    "src-title-info" -> pure () -- ignore
    "document-info" -> pure ()
    "publish-info" -> pure ()
    "custom-info" -> pure ()
    "output" -> pure ()
    name -> do
      report $ IgnoredElement $ name <> " in description"
      pure mempty

-- | Parse a child of @\<body>@ element.
parseBodyChild :: PandocMonad m => Element -> FB2 m Blocks
parseBodyChild e =
  case qName $ elName e of
    "image" -> parseImageElement e
    "title" -> header <$> gets fb2SectionLevel <*> parseTitleType (elContent e)
    "epigraph" -> parseEpigraph e
    "section" -> parseSection e
    name -> report (UnexpectedXmlElement name "body") $> mempty

-- | Parse a @\<binary>@ element.
parseBinaryElement :: PandocMonad m => Element -> FB2 m ()
parseBinaryElement e =
  case (findAttr (unqual "id") e, findAttr (unqual "content-type") e) of
    (Nothing, _) -> report $ IgnoredElement "binary without id attribute"
    (Just _, Nothing) ->
      report $ IgnoredElement "binary without content-type attribute"
    (Just filename, contentType) ->
      insertMedia (T.unpack filename) contentType
                    (decodeLenient
                      (UTF8.fromTextLazy . TL.fromStrict . strContent $ e))

-- * Type parsers

-- | Parse @authorType@
parseAuthor :: PandocMonad m => Element -> FB2 m Text
parseAuthor e = T.unwords . catMaybes <$> mapM parseAuthorChild (elChildren e)

parseAuthorChild :: PandocMonad m => Element -> FB2 m (Maybe Text)
parseAuthorChild e =
  case qName $ elName e of
    "first-name" -> pure $ Just $ strContent e
    "middle-name" -> pure $ Just $ strContent e
    "last-name" -> pure $ Just $ strContent e
    "nickname" -> pure $ Just $ strContent e
    "home-page" -> pure $ Just $ strContent e
    "email" -> pure $ Just $ strContent e
    name -> do
      report $ IgnoredElement $ name <> " in author"
      pure Nothing

-- | Parse @titleType@
parseTitle :: PandocMonad m => Element -> FB2 m Blocks
parseTitle e = header <$> gets fb2SectionLevel <*> parseTitleType (elContent e)

parseTitleType :: PandocMonad m => [Content] -> FB2 m Inlines
parseTitleType c = mconcat . intersperse linebreak . catMaybes <$> mapM parseTitleContent c

parseTitleContent :: PandocMonad m => Content -> FB2 m (Maybe Inlines)
parseTitleContent (Elem e) =
  case qName $ elName e of
    "p" -> Just <$> parsePType e
    "empty-line" -> pure $ Just mempty
    _ -> pure mempty
parseTitleContent _ = pure Nothing

-- | Parse @imageType@
parseImageElement :: PandocMonad m => Element -> FB2 m Blocks
parseImageElement e =
  case href of
    Just src -> pure $ para $ imageWith (imgId, [], []) (removeHash src) title alt
    Nothing -> do
      report $ IgnoredElement " image without href"
      pure mempty
  where alt = maybe mempty str $ findAttr (unqual "alt") e
        title = fromMaybe "" $ findAttr (unqual "title") e
        imgId = fromMaybe "" $ findAttr (unqual "id") e
        href = findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e

-- | Parse @pType@
parsePType :: PandocMonad m => Element -> FB2 m Inlines
parsePType = parseStyleType -- TODO add support for optional "id" and "style" attributes

-- | Parse @citeType@
parseCite :: PandocMonad m => Element -> FB2 m Blocks
parseCite e = blockQuote . mconcat <$> mapM parseCiteChild (elChildren e)

-- | Parse @citeType@ child
parseCiteChild :: PandocMonad m => Element -> FB2 m Blocks
parseCiteChild e =
  case qName $ elName e of
    "p" -> para <$> parsePType e
    "poem" -> parsePoem e
    "empty-line" -> pure horizontalRule
    "subtitle" -> parseSubtitle e
    "table" -> parseTable e
    "text-author" -> para <$> parsePType e
    name -> report (UnexpectedXmlElement name "cite") $> mempty

-- | Parse @poemType@
parsePoem :: PandocMonad m => Element -> FB2 m Blocks
parsePoem e = mconcat <$> mapM parsePoemChild (elChildren e)

parsePoemChild :: PandocMonad m => Element -> FB2 m Blocks
parsePoemChild e =
  case qName $ elName e of
    "title" -> parseTitle e
    "subtitle" -> parseSubtitle e
    "epigraph" -> parseEpigraph e
    "stanza" -> parseStanza e
    "text-author" -> para <$> parsePType e
    "date" -> pure $ para $ text $ strContent e
    name -> report (UnexpectedXmlElement name "poem") $> mempty

parseStanza :: PandocMonad m => Element -> FB2 m Blocks
parseStanza e = fromList . joinLineBlocks . toList . mconcat <$> mapM parseStanzaChild (elChildren e)

joinLineBlocks :: [Block] -> [Block]
joinLineBlocks (LineBlock xs:LineBlock ys:zs) = joinLineBlocks (LineBlock (xs ++ ys) : zs)
joinLineBlocks (x:xs) = x:joinLineBlocks xs
joinLineBlocks [] = []

parseStanzaChild :: PandocMonad m => Element -> FB2 m Blocks
parseStanzaChild e =
  case qName $ elName e of
    "title" -> parseTitle e
    "subtitle" -> parseSubtitle e
    "v" -> lineBlock . (:[]) <$> parsePType e
    name -> report (UnexpectedXmlElement name "stanza") $> mempty

-- | Parse @epigraphType@
parseEpigraph :: PandocMonad m => Element -> FB2 m Blocks
parseEpigraph e =
  divWith (divId, ["epigraph"], []) . mconcat <$> mapM parseEpigraphChild (elChildren e)
  where divId = fromMaybe "" $ findAttr (unqual "id") e

parseEpigraphChild :: PandocMonad m => Element -> FB2 m Blocks
parseEpigraphChild e =
  case qName $ elName e of
    "p" -> para <$> parsePType e
    "poem" -> parsePoem e
    "cite" -> parseCite e
    "empty-line" -> pure horizontalRule
    "text-author" -> para <$> parsePType e
    name -> report (UnexpectedXmlElement name "epigraph") $> mempty

-- | Parse @annotationType@
parseAnnotation :: PandocMonad m => Element -> FB2 m Blocks
parseAnnotation e = mconcat <$> mapM parseAnnotationChild (elChildren e)

parseAnnotationChild :: PandocMonad m => Element -> FB2 m Blocks
parseAnnotationChild e =
  case qName $ elName e of
    "p" -> para <$> parsePType e
    "poem" -> parsePoem e
    "cite" -> parseCite e
    "subtitle" -> parseSubtitle e
    "table" -> parseTable e
    "empty-line" -> pure horizontalRule
    name -> report (UnexpectedXmlElement name "annotation") $> mempty

-- | Parse @sectionType@
parseSection :: PandocMonad m => Element -> FB2 m Blocks
parseSection e = do
  n <- gets fb2SectionLevel
  modify $ \st -> st{ fb2SectionLevel = n + 1 }
  let sectionId = fromMaybe "" $ findAttr (unqual "id") e
  bs <- divWith (sectionId, ["section"], []) . mconcat <$> mapM parseSectionChild (elChildren e)
  modify $ \st -> st{ fb2SectionLevel = n }
  pure bs

parseSectionChild :: PandocMonad m => Element -> FB2 m Blocks
parseSectionChild e =
  case qName $ elName e of
    "title" -> parseBodyChild e
    "epigraph" -> parseEpigraph e
    "image" -> parseImageElement e
    "annotation" -> parseAnnotation e
    "poem" -> parsePoem e
    "cite" -> parseCite e
    "empty-line" -> pure horizontalRule
    "table" -> parseTable e
    "subtitle" -> parseSubtitle e
    "p" -> para <$> parsePType e
    "section" -> parseSection e
    name -> report (UnexpectedXmlElement name "section") $> mempty

-- | parse @styleType@
parseStyleType :: PandocMonad m => Element -> FB2 m Inlines
parseStyleType e = mconcat <$> mapM parseInline (elContent e)

-- | Parse @namedStyleType@
parseNamedStyle :: PandocMonad m => Element -> FB2 m Inlines
parseNamedStyle e = do
  content <- mconcat <$> mapM parseNamedStyleChild (elContent e)
  let lang = maybeToList $ ("lang",) <$> findAttr (QName "lang" Nothing (Just "xml")) e
  case findAttr (unqual "name") e of
    Just name -> pure $ spanWith ("", [name], lang) content
    Nothing -> do
      report $ IgnoredElement "link without required name"
      pure mempty

parseNamedStyleChild :: PandocMonad m => Content -> FB2 m Inlines
parseNamedStyleChild (Elem e) =
  case qName (elName e) of
    "strong" -> strong <$> parseStyleType e
    "emphasis" -> emph <$> parseStyleType e
    "style" -> parseNamedStyle e
    "a" -> parseLinkType e
    "strikethrough" -> strikeout <$> parseStyleType e
    "sub" -> subscript <$> parseStyleType e
    "sup" -> superscript <$> parseStyleType e
    "code" -> pure $ code $ strContent e
    "image" -> parseInlineImageElement e
    name -> do
      report $ IgnoredElement $ name <> " in style"
      pure mempty
parseNamedStyleChild x = parseInline x

-- | Parse @linkType@
parseLinkType :: PandocMonad m => Element -> FB2 m Inlines
parseLinkType e = do
  content <- mconcat <$> mapM parseStyleLinkType (elContent e)
  notes <- gets fb2Notes
  case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
    Just href -> case findAttr (QName "type" Nothing Nothing) e of
                   Just "note" -> case M.lookup href notes of
                                    Nothing -> pure $ link href "" content
                                    Just contents -> pure $ note contents
                   _ -> pure $ link href "" content
    Nothing -> do
      report $ IgnoredElement "link without required href"
      pure mempty

-- | Parse @styleLinkType@
parseStyleLinkType :: PandocMonad m => Content -> FB2 m Inlines
parseStyleLinkType x@(Elem e) =
  case qName (elName e) of
    "a" -> do
      report $ IgnoredElement "nested link"
      pure mempty
    _ -> parseInline x
parseStyleLinkType x = parseInline x

-- | Parse @tableType@
parseTable :: PandocMonad m => Element -> FB2 m Blocks
parseTable _ = pure mempty -- TODO: tables are not supported yet

-- | Parse @title-infoType@
parseTitleInfoChild :: PandocMonad m => Element -> FB2 m ()
parseTitleInfoChild e =
  case qName (elName e) of
    "genre" -> pure ()
    "author" -> parseAuthor e >>= \author -> modify (\st -> st {fb2Authors = author:fb2Authors st})
    "book-title" -> modify (setMeta "title" (text $ strContent e))
    "annotation" -> parseAnnotation e >>= modify . setMeta "abstract"
    "keywords" -> modify (setMeta "keywords" (map (MetaString . trim) $ T.splitOn ","
                                                                      $ strContent e))
    "date" -> modify (setMeta "date" (text $ strContent e))
    "coverpage" -> parseCoverPage e
    "lang" -> pure ()
    "src-lang" -> pure ()
    "translator" -> pure ()
    "sequence" -> pure ()
    name -> report $ IgnoredElement $ name <> " in title-info"

parseCoverPage :: PandocMonad m => Element -> FB2 m ()
parseCoverPage e =
  case findChild (QName "image" (Just "http://www.gribuser.ru/xml/fictionbook/2.0") Nothing) e of
    Just img -> case href of
                  Just src -> modify (setMeta "cover-image" (MetaString $ removeHash src))
                  Nothing -> pure ()
                where href = findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) img
    Nothing -> pure ()

-- | Parse @inlineImageType@ element
parseInlineImageElement :: PandocMonad m
                        => Element
                        -> FB2 m Inlines
parseInlineImageElement e =
  case href of
    Just src -> pure $ imageWith ("", [], []) (removeHash src) "" alt
    Nothing -> do
      report $ IgnoredElement "inline image without href"
      pure mempty
  where alt = maybe mempty str $ findAttr (unqual "alt") e
        href = findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e

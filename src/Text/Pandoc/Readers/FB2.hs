{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-
Copyright (C) 2018 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Readers.FB2
   Copyright   : Copyright (C) 2018 Alexander Krotov
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
import Prelude
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import Data.ByteString.Lazy.Char8 ( pack )
import Data.ByteString.Base64.Lazy
import Data.Char (isSpace, toUpper)
import Data.Functor
import Data.List (dropWhileEnd, intersperse)
import Data.List.Split (splitOn)
import Data.Text (Text)
import Data.Default
import Data.Maybe
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Builder
import Text.Pandoc.Class (PandocMonad, insertMedia, report)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared (crFilter)
import Text.XML.Light

type FB2 m = StateT FB2State m

data FB2State = FB2State{ fb2SectionLevel :: Int
                        , fb2Meta :: Meta
                        , fb2Authors :: [String]
                        } deriving Show

instance Default FB2State where
  def = FB2State{ fb2SectionLevel = 1
                , fb2Meta = mempty
                , fb2Authors = []
                }

instance HasMeta FB2State where
  setMeta field v s = s {fb2Meta = setMeta field v (fb2Meta s)}
  deleteMeta field s = s {fb2Meta = deleteMeta field (fb2Meta s)}

readFB2 :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readFB2 _ inp  = do
  (bs, st) <- runStateT (mapM parseBlock $ parseXML (crFilter inp)) def
  let authors = if null $ fb2Authors st
                then id
                else setMeta "author" (map text $ reverse $ fb2Authors st)
  pure $ Pandoc (authors $ fb2Meta st) (toList . mconcat $ bs)

-- * Utility functions

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

removeHash :: String -> String
removeHash ('#':xs) = xs
removeHash xs = xs

convertEntity :: String -> String
convertEntity e = fromMaybe (map toUpper e) (lookupEntity e)

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
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ ".")
parseInline (Text x) = pure $ text $ cdData x
parseInline (CRef r) = pure $ str $ convertEntity r

parseSubtitle :: PandocMonad m => Element -> FB2 m Blocks
parseSubtitle e = headerWith ("", ["unnumbered"], []) <$> gets fb2SectionLevel <*> parsePType e

-- * Root element parser

parseBlock :: PandocMonad m => Content -> FB2 m Blocks
parseBlock (Elem e) =
  case qName $ elName e of
    "?xml"  -> pure mempty
    "FictionBook" -> mconcat <$> mapM parseFictionBookChild (elChildren e)
    name -> report (UnexpectedXmlElement name "root") $> mempty
parseBlock _ = pure mempty

-- | Parse a child of @\<FictionBook>@ element.
parseFictionBookChild :: PandocMonad m => Element -> FB2 m Blocks
parseFictionBookChild e =
  case qName $ elName e of
    "stylesheet" -> pure mempty -- stylesheet is ignored
    "description" -> mempty <$ mapM_ parseDescriptionChild (elChildren e)
    "body" -> mconcat <$> mapM parseBodyChild (elChildren e)
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
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ "in description.")

-- | Parse a child of @\<body>@ element.
parseBodyChild :: PandocMonad m => Element -> FB2 m Blocks
parseBodyChild e =
  case qName $ elName e of
    "image" -> parseImageElement e
    "title" -> header <$> gets fb2SectionLevel <*> parseTitleType (elContent e)
    "epigraph" -> parseEpigraph e
    "section" -> parseSection e
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ " in body.")

-- | Parse a @\<binary>@ element.
parseBinaryElement :: PandocMonad m => Element -> FB2 m ()
parseBinaryElement e =
  case (findAttr (QName "id" Nothing Nothing) e, findAttr (QName "content-type" Nothing Nothing) e) of
    (Nothing, _) -> throwError $ PandocParseError "<binary> element must have an \"id\" attribute"
    (Just _, Nothing) -> throwError $ PandocParseError "<binary> element must have a \"content-type\" attribute"
    (Just filename, contentType) -> insertMedia filename contentType (decodeLenient (pack (strContent e)))

-- * Type parsers

-- | Parse @authorType@
parseAuthor :: PandocMonad m => Element -> FB2 m String
parseAuthor e = unwords <$> mapM parseAuthorChild (elChildren e)

parseAuthorChild :: PandocMonad m => Element -> FB2 m String
parseAuthorChild e =
  case qName $ elName e of
    "first-name" -> pure $ strContent e
    "middle-name" -> pure $ strContent e
    "last-name" -> pure $ strContent e
    "nickname" -> pure $ strContent e
    "home-page" -> pure $ strContent e
    "email" -> pure $ strContent e
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ " in author.")

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
    Nothing -> throwError $ PandocParseError "Couldn't parse FB2 file: image without href."
  where alt = maybe mempty str $ findAttr (QName "alt" Nothing Nothing) e
        title = fromMaybe "" $ findAttr (QName "title" Nothing Nothing) e
        imgId = fromMaybe "" $ findAttr (QName "id" Nothing Nothing) e
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
  where divId = fromMaybe "" $ findAttr (QName "id" Nothing Nothing) e

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
  let sectionId = fromMaybe "" $ findAttr (QName "id" Nothing Nothing) e
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
  case findAttr (QName "name" Nothing Nothing) e of
    Just name -> pure $ spanWith ("", [name], lang) content
    Nothing -> throwError $ PandocParseError "Couldn't parse FB2 file: link without required name."

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
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ ".")
parseNamedStyleChild x = parseInline x

-- | Parse @linkType@
parseLinkType :: PandocMonad m => Element -> FB2 m Inlines
parseLinkType e = do
  content <- mconcat <$> mapM parseStyleLinkType (elContent e)
  case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
    Just href -> pure $ link href "" content
    Nothing -> throwError $ PandocParseError "Couldn't parse FB2 file: link without required href."

-- | Parse @styleLinkType@
parseStyleLinkType :: PandocMonad m => Content -> FB2 m Inlines
parseStyleLinkType x@(Elem e) =
  case qName (elName e) of
    "a" -> throwError $ PandocParseError "Couldn't parse FB2 file: links cannot be nested."
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
    "keywords" -> modify (setMeta "keywords" (map (MetaString . trim) $ splitOn "," $ strContent e))
    "date" -> modify (setMeta "date" (text $ strContent e))
    "coverpage" -> parseCoverPage e
    "lang" -> pure ()
    "src-lang" -> pure ()
    "translator" -> pure ()
    "sequence" -> pure ()
    name -> throwError $ PandocParseError ("Couldn't parse FB2 file: unexpected element " ++ name ++ " in title-info.")

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
    Nothing -> throwError $ PandocParseError "Couldn't parse FB2 file: inline image without href."
  where alt = maybe mempty str $ findAttr (QName "alt" Nothing Nothing) e
        href = findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e

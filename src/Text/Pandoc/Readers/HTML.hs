{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{- |
   Module      : Text.Pandoc.Readers.HTML
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of HTML to 'Pandoc' document.
-}
module Text.Pandoc.Readers.HTML ( readHtml
                                , htmlTag
                                , htmlInBalanced
                                , isInlineTag
                                , isBlockTag
                                , isTextTag
                                , isCommentTag
                                ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, msum, mzero, unless, void)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.Reader (ask, asks, lift, local, runReaderT)
import Data.ByteString.Base64 (encode)
import Data.Char (isAlphaNum, isLetter)
import Data.Default (Default (..), def)
import Data.Foldable (for_)
import Data.List.Split (splitWhen)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid (First (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (nonStrictRelativeTo, parseURIReference)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.CSS (pickStyleAttrProps)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Definition
import Text.Pandoc.Readers.HTML.Parsing
import Text.Pandoc.Readers.HTML.Table (pTable)
import Text.Pandoc.Readers.HTML.TagCategories
import Text.Pandoc.Readers.HTML.Types
import Text.Pandoc.Readers.LaTeX (rawLaTeXInline)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options (
    Extension (Ext_epub_html_exts, Ext_empty_paragraphs, Ext_native_divs,
               Ext_native_spans, Ext_raw_html, Ext_line_blocks, Ext_raw_tex),
    ReaderOptions (readerExtensions, readerStripComments),
    extensionEnabled)
import Text.Pandoc.Parsing hiding ((<|>))
import Text.Pandoc.Shared (
    addMetaField, blocksToInlines', escapeURI, extractSpaces,
    htmlSpanLikeElements, renderTags', safeRead, tshow)
import Text.Pandoc.Walk
import Text.Parsec.Error
import Text.TeXMath (readMathML, writeTeX)

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: (PandocMonad m, ToSources a)
         => ReaderOptions -- ^ Reader options
         -> a             -- ^ Input to parse
         -> m Pandoc
readHtml opts inp = do
  let tags = stripPrefixes $ canonicalizeTags $
             parseTagsOptions parseOptions{ optTagPosition = True }
             (sourcesToText $ toSources inp)
      parseDoc = do
        blocks <- fixPlains False . mconcat <$> manyTill block eof
        meta <- stateMeta . parserState <$> getState
        bs' <- replaceNotes (B.toList blocks)
        reportLogMessages
        return $ Pandoc meta bs'
      getError (errorMessages -> ms) = case ms of
                                         []    -> ""
                                         (m:_) -> messageString m
  result <- flip runReaderT def $
       runParserT parseDoc
       (HTMLState def{ stateOptions = opts }
         [] Nothing Set.empty [] M.empty opts)
       "source" tags
  case result of
    Right doc -> return doc
    Left  err -> throwError $ PandocParseError $ T.pack $ getError err

-- Strip namespace prefixes on tags (not attributes)
stripPrefixes :: [Tag Text] -> [Tag Text]
stripPrefixes = map stripPrefix

stripPrefix :: Tag Text -> Tag Text
stripPrefix (TagOpen s as) = TagOpen (T.takeWhileEnd (/=':') s) as
stripPrefix (TagClose s)   = TagClose (T.takeWhileEnd (/=':') s)
stripPrefix x = x

replaceNotes :: PandocMonad m => [Block] -> TagParser m [Block]
replaceNotes bs = do
  st <- getState
  return $ walk (replaceNotes' (noteTable st)) bs

replaceNotes' :: [(Text, Blocks)] -> Inline -> Inline
replaceNotes' noteTbl (RawInline (Format "noteref") ref) =
  maybe (Str "") (Note . B.toList) $ lookup ref noteTbl
replaceNotes' _ x = x

setInChapter :: PandocMonad m => HTMLParser m s a -> HTMLParser m s a
setInChapter = local (\s -> s {inChapter = True})

setInPlain :: PandocMonad m => HTMLParser m s a -> HTMLParser m s a
setInPlain = local (\s -> s {inPlain = True})

pHtml :: PandocMonad m => TagParser m Blocks
pHtml = do
  (TagOpen "html" attr) <- lookAhead pAny
  for_ (lookup "lang" attr <|> lookup "xml:lang" attr) $
    updateState . B.setMeta "lang" . B.text
  pInTags "html" block

pBody :: PandocMonad m => TagParser m Blocks
pBody = do
  (TagOpen "body" attr) <- lookAhead pAny
  for_ (lookup "lang" attr <|> lookup "xml:lang" attr) $
    updateState . B.setMeta "lang" . B.text
  pInTags "body" block

pHead :: PandocMonad m => TagParser m Blocks
pHead = pInTags "head" $ pTitle <|> pMetaTag <|> pBaseTag <|> (mempty <$ pAny)
  where pTitle = pInTags "title" inline >>= setTitle . trimInlines
        setTitle t = mempty <$ updateState (B.setMeta "title" t)
        pMetaTag = do
          mt <- pSatisfy (matchTagOpen "meta" [])
          let name = fromAttrib "name" mt
          if T.null name
             then return mempty
             else do
               let content = fromAttrib "content" mt
               updateState $ \s ->
                 let ps = parserState s in
                 s{ parserState = ps{
                      stateMeta = addMetaField name (B.text content)
                                   (stateMeta ps) } }
               return mempty
        pBaseTag = do
          bt <- pSatisfy (matchTagOpen "base" [])
          updateState $ \st -> st{ baseHref =
               parseURIReference $ T.unpack $ fromAttrib "href" bt }
          return mempty

block :: PandocMonad m => TagParser m Blocks
block = ((do
  tag <- lookAhead (pSatisfy isBlockTag)
  exts <- getOption readerExtensions
  case tag of
    TagOpen name attr ->
      let type' = fromMaybe "" $
                     lookup "type" attr <|> lookup "epub:type" attr
          epubExts = extensionEnabled Ext_epub_html_exts exts
      in
      case name of
        _ | name `elem` sectioningContent
          , epubExts
          , "chapter" `T.isInfixOf` type'
          -> eSection
        _ | epubExts
          , type' `elem` ["footnote", "rearnote"]
          -> mempty <$ eFootnote
        _ | epubExts
          , type' == "toc"
          -> mempty <$ eTOC
        _ | "titlepage" `T.isInfixOf` type'
          , name `elem` ("section" : groupingContent)
          -> mempty <$ eTitlePage
        "p" -> pPara
        "h1" -> pHeader
        "h2" -> pHeader
        "h3" -> pHeader
        "h4" -> pHeader
        "h5" -> pHeader
        "h6" -> pHeader
        "blockquote" -> pBlockQuote
        "pre" -> pCodeBlock
        "ul" -> pBulletList
        "ol" -> pOrderedList
        "dl" -> pDefinitionList
        "table" -> pTable block
        "hr" -> pHrule
        "html" -> pHtml
        "head" -> pHead
        "body" -> pBody
        "div"
          | extensionEnabled Ext_line_blocks exts
          , Just "line-block" <- lookup "class" attr
          -> pLineBlock
          | otherwise
          -> pDiv
        "section" -> pDiv
        "header" -> pDiv
        "main" -> pDiv
        "figure" -> pFigure
        "iframe" -> pIframe
        "style" -> pRawHtmlBlock
        "textarea" -> pRawHtmlBlock
        "switch"
          | epubExts
          -> eSwitch B.para block
        _ -> mzero
    _ -> mzero) <|> pPlain <|> pRawHtmlBlock) >>= \res ->
        res <$ trace (T.take 60 $ tshow $ B.toList res)

namespaces :: PandocMonad m => [(Text, TagParser m Inlines)]
namespaces = [(mathMLNamespace, pMath True)]

mathMLNamespace :: Text
mathMLNamespace = "http://www.w3.org/1998/Math/MathML"

eSwitch :: (PandocMonad m, Monoid a)
        => (Inlines -> a)
        -> TagParser m a
        -> TagParser m a
eSwitch constructor parser = try $ do
  guardEnabled Ext_epub_html_exts
  pSatisfy (matchTagOpen "switch" [])
  cases <- getFirst . mconcat <$>
            manyTill (First <$> (eCase <* skipMany pBlank) )
              (lookAhead $ try $ pSatisfy (matchTagOpen "default" []))
  skipMany pBlank
  fallback <- pInTags "default" (skipMany pBlank *> parser <* skipMany pBlank)
  skipMany pBlank
  pSatisfy (matchTagClose "switch")
  return $ maybe fallback constructor cases

eCase :: PandocMonad m => TagParser m (Maybe Inlines)
eCase = do
  skipMany pBlank
  TagOpen _ attr' <- lookAhead $ pSatisfy (matchTagOpen "case" [])
  let attr = toStringAttr attr'
  case flip lookup namespaces =<< lookup "required-namespace" attr of
    Just p -> Just <$> pInTags "case" (skipMany pBlank *> p <* skipMany pBlank)
    Nothing -> Nothing <$ manyTill pAny (pSatisfy (matchTagClose "case"))

eFootnote :: PandocMonad m => TagParser m ()
eFootnote = try $ do
  let notes = ["footnote", "rearnote"]
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr') <- lookAhead pAny
  let attr = toStringAttr attr'
  guard $ maybe False (`elem` notes)
          (lookup "type" attr <|> lookup "epub:type" attr)
  let ident = fromMaybe "" (lookup "id" attr)
  content <- pInTags tag block
  addNote ident content

addNote :: PandocMonad m => Text -> Blocks -> TagParser m ()
addNote uid cont = updateState (\s -> s {noteTable = (uid, cont) : noteTable s})

eNoteref :: PandocMonad m => TagParser m Inlines
eNoteref = try $ do
  guardEnabled Ext_epub_html_exts
  TagOpen tag attr <-
    pSatisfy (\case
                 TagOpen _ as
                    -> (lookup "type" as <|> lookup "epub:type" as)
                        == Just "noteref"
                 _  -> False)
  ident <- case lookup "href" attr >>= T.uncons of
             Just ('#', rest) -> return rest
             _ -> mzero
  _ <- manyTill pAny (pSatisfy (\case
                                   TagClose t -> t == tag
                                   _          -> False))
  return $ B.rawInline "noteref" ident

-- Strip TOC if there is one, better to generate again
eTOC :: PandocMonad m => TagParser m ()
eTOC = try $ do
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr) <- lookAhead pAny
  guard $ (lookup "type" attr <|> lookup "epub:type" attr) == Just "toc"
  void (pInTags tag block)

pBulletList :: PandocMonad m => TagParser m Blocks
pBulletList = try $ do
  pSatisfy (matchTagOpen "ul" [])
  let nonItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["li","ol","ul","dl"]) (const True) t) &&
                  not (matchTagClose "ul" t))
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany nonItem
  items <- manyTill (pListItem nonItem) (pCloses "ul")
  return $ B.bulletList $ map (fixPlains True) items

pListItem :: PandocMonad m => TagParser m a -> TagParser m Blocks
pListItem nonItem = do
  TagOpen _ attr' <- lookAhead $ pSatisfy (matchTagOpen "li" [])
  let attr = toStringAttr attr'
  let addId ident bs = case B.toList bs of
                           (Plain ils:xs) -> B.fromList (Plain
                                [Span (ident, [], []) ils] : xs)
                           _ -> B.divWith (ident, [], []) bs
  maybe id addId (lookup "id" attr) <$>
    pInTags "li" block <* skipMany nonItem

parseListStyleType :: Text -> ListNumberStyle
parseListStyleType "lower-roman" = LowerRoman
parseListStyleType "upper-roman" = UpperRoman
parseListStyleType "lower-alpha" = LowerAlpha
parseListStyleType "upper-alpha" = UpperAlpha
parseListStyleType "decimal"     = Decimal
parseListStyleType _             = DefaultStyle

parseTypeAttr :: Text -> ListNumberStyle
parseTypeAttr "i" = LowerRoman
parseTypeAttr "I" = UpperRoman
parseTypeAttr "a" = LowerAlpha
parseTypeAttr "A" = UpperAlpha
parseTypeAttr "1" = Decimal
parseTypeAttr _   = DefaultStyle

pOrderedList :: PandocMonad m => TagParser m Blocks
pOrderedList = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "ol" [])
  let attribs = toStringAttr attribs'
  let start = fromMaybe 1 $ lookup "start" attribs >>= safeRead
  let style = fromMaybe DefaultStyle
         $  (parseTypeAttr      <$> lookup "type" attribs)
        <|> (parseListStyleType <$> lookup "class" attribs)
        <|> (parseListStyleType <$> (lookup "style" attribs >>= pickListStyle))
        where
          pickListStyle = pickStyleAttrProps ["list-style-type", "list-style"]

  let nonItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["li","ol","ul","dl"]) (const True) t) &&
                  not (matchTagClose "ol" t))
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany nonItem
  items <- manyTill (pListItem nonItem) (pCloses "ol")
  return $ B.orderedListWith (start, style, DefaultDelim) $ map (fixPlains True) items

pDefinitionList :: PandocMonad m => TagParser m Blocks
pDefinitionList = try $ do
  pSatisfy (matchTagOpen "dl" [])
  items <- manyTill pDefListItem (pCloses "dl")
  return $ B.definitionList items

pDefListItem :: PandocMonad m => TagParser m (Inlines, [Blocks])
pDefListItem = try $ do
  let nonItem = pSatisfy (\t -> not (matchTagOpen "dt" [] t) &&
                  not (matchTagOpen "dd" [] t) && not (matchTagClose "dl" t))
  terms <- many1 (try $ skipMany nonItem >> pInTags "dt" inline)
  defs  <- many1 (try $ skipMany nonItem >> pInTags "dd" block)
  skipMany nonItem
  let term = foldl' (\x y -> if null x
                                then trimInlines y
                                else x <> B.linebreak <> trimInlines y)
                    mempty terms
  return (term, map (fixPlains True) defs)

fixPlains :: Bool -> Blocks -> Blocks
fixPlains inList bs = if any isParaish bs'
                         then B.fromList $ map plainToPara bs'
                         else bs
  where isParaish Para{}           = True
        isParaish CodeBlock{}      = True
        isParaish Header{}         = True
        isParaish BlockQuote{}     = True
        isParaish BulletList{}     = not inList
        isParaish OrderedList{}    = not inList
        isParaish DefinitionList{} = not inList
        isParaish _                = False
        plainToPara (Plain xs) = Para xs
        plainToPara x          = x
        bs' = B.toList bs

pRawTag :: PandocMonad m => TagParser m Text
pRawTag = do
  tag <- pAny
  let ignorable x = x `elem` ["html","head","body","!DOCTYPE","?xml"]
  if tagOpen ignorable (const True) tag || tagClose ignorable tag
     then return mempty
     else return $ renderTags' [tag]

pLineBlock :: PandocMonad m => TagParser m Blocks
pLineBlock = try $ do
  guardEnabled Ext_line_blocks
  _ <- pSatisfy $ tagOpen (=="div") (== [("class","line-block")])
  ils <- trimInlines . mconcat <$> manyTill inline (pSatisfy (tagClose (=="div")))
  let lns = map B.fromList $
            splitWhen (== LineBreak) $ filter (/= SoftBreak) $
            B.toList ils
  return $ B.lineBlock lns

isDivLike :: Text -> Bool
isDivLike "div"     = True
isDivLike "section" = True
isDivLike "header"  = True
isDivLike "main"    = True
isDivLike _         = False

pDiv :: PandocMonad m => TagParser m Blocks
pDiv = try $ do
  guardEnabled Ext_native_divs
  TagOpen tag attr' <- lookAhead $ pSatisfy $ tagOpen isDivLike (const True)
  let (ident, classes, kvs) = toAttr attr'
  contents <- pInTags tag block
  let classes' = if tag == "section"
                    then "section":classes
                    else classes
      kvs' = if tag == "main" && isNothing (lookup "role" kvs)
               then ("role", "main"):kvs
               else kvs
  return $ B.divWith (ident, classes', kvs') contents

pIframe :: PandocMonad m => TagParser m Blocks
pIframe = try $ do
  guardDisabled Ext_raw_html
  tag <- pSatisfy (tagOpen (=="iframe") (isJust . lookup "src"))
  pCloses "iframe" <|> eof
  url <- canonicalizeUrl $ fromAttrib "src" tag
  if T.null url
     then ignore $ renderTags' [tag, TagClose "iframe"]
     else catchError
       (do (bs, _) <- openURL url
           let inp = UTF8.toText bs
           opts <- readerOpts <$> getState
           Pandoc _ contents <- readHtml opts inp
           return $ B.divWith ("",["iframe"],[]) $ B.fromList contents)
       (\e -> do
         logMessage $ CouldNotFetchResource url (renderError e)
         ignore $ renderTags' [tag, TagClose "iframe"])

pRawHtmlBlock :: PandocMonad m => TagParser m Blocks
pRawHtmlBlock = do
  raw <- pHtmlBlock "script" <|> pHtmlBlock "style" <|> pHtmlBlock "textarea"
          <|> pRawTag
  exts <- getOption readerExtensions
  if extensionEnabled Ext_raw_html exts && not (T.null raw)
     then return $ B.rawBlock "html" raw
     else ignore raw

ignore :: (Monoid a, PandocMonad m) => Text -> TagParser m a
ignore raw = do
  pos <- getPosition
  -- raw can be null for tags like <!DOCTYPE>; see paRawTag
  -- in this case we don't want a warning:
  unless (T.null raw) $
    logMessage $ SkippedContent raw pos
  return mempty

pHtmlBlock :: PandocMonad m => Text -> TagParser m Text
pHtmlBlock t = try $ do
  open <- pSatisfy (matchTagOpen t [])
  contents <- manyTill pAny (pSatisfy (matchTagClose t))
  return $ renderTags' $ [open] <> contents <> [TagClose t]

-- Sets chapter context
eSection :: PandocMonad m => TagParser m Blocks
eSection = try $ do
  let matchChapter as = maybe False (T.isInfixOf "chapter")
                        (lookup "type" as <|> lookup "epub:type" as)
  let sectTag = tagOpen (`elem` sectioningContent) matchChapter
  TagOpen tag _ <- lookAhead $ pSatisfy sectTag
  setInChapter (pInTags tag block)

headerLevel :: Text -> TagParser m Int
headerLevel tagtype =
  case safeRead (T.drop 1 tagtype) of
        Just level ->
--          try (do
--            guardEnabled Ext_epub_html_exts
--            asks inChapter >>= guard
--            return (level - 1))
--            <|>
              return level
        Nothing -> Prelude.fail "Could not retrieve header level"

eTitlePage :: PandocMonad m => TagParser m ()
eTitlePage = try $ do
  let isTitlePage as = maybe False (T.isInfixOf "titlepage")
       (lookup "type" as <|> lookup "epub:type" as)
  let groupTag = tagOpen (\x -> x `elem` groupingContent || x == "section")
                          isTitlePage
  TagOpen tag _ <- lookAhead $ pSatisfy groupTag
  () <$ pInTags tag block

pHeader :: PandocMonad m => TagParser m Blocks
pHeader = try $ do
  TagOpen tagtype attr' <- pSatisfy $
                           tagOpen (`elem` ["h1","h2","h3","h4","h5","h6"])
                           (const True)
  let attr = toStringAttr attr'
  level <- headerLevel tagtype
  contents <- trimInlines . mconcat <$> manyTill inline (pCloses tagtype <|> eof)
  let ident = fromMaybe "" $ lookup "id" attr
  let classes = maybe [] T.words $ lookup "class" attr
  let keyvals = [(k,v) | (k,v) <- attr, k /= "class", k /= "id"]
  attr'' <- registerHeader (ident, classes, keyvals) contents
  return $ B.headerWith attr'' level contents

pHrule :: PandocMonad m => TagParser m Blocks
pHrule = do
  pSelfClosing (=="hr") (const True)
  return B.horizontalRule

pBlockQuote :: PandocMonad m => TagParser m Blocks
pBlockQuote = do
  contents <- pInTags "blockquote" block
  return $ B.blockQuote $ fixPlains False contents

pPlain :: PandocMonad m => TagParser m Blocks
pPlain = do
  contents <- setInPlain $ trimInlines . mconcat <$> many1 inline
  if null contents
     then return mempty
     else return $ B.plain contents

pPara :: PandocMonad m => TagParser m Blocks
pPara = do
  contents <- trimInlines <$> pInTags "p" inline
  (do guardDisabled Ext_empty_paragraphs
      guard (null contents)
      return mempty)
    <|> return (B.para contents)

pFigure :: PandocMonad m => TagParser m Blocks
pFigure = try $ do
  TagOpen _ _ <- pSatisfy (matchTagOpen "figure" [])
  skipMany pBlank
  let pImg  = (\x -> (Just x, Nothing)) <$>
               (pInTag TagsOmittable "p" pImage <* skipMany pBlank)
      pCapt = (\x -> (Nothing, Just x)) <$> do
                bs <- pInTags "figcaption" block
                return $ blocksToInlines' $ B.toList bs
      pSkip = (Nothing, Nothing) <$ pSatisfy (not . matchTagClose "figure")
  res <- many (pImg <|> pCapt <|> pSkip)
  let mbimg = msum $ map fst res
  let mbcap = msum $ map snd res
  TagClose _ <- pSatisfy (matchTagClose "figure")
  let caption = fromMaybe mempty mbcap
  case B.toList <$> mbimg of
       Just [Image attr _ (url, tit)] ->
         return $ B.para $ B.imageWith attr url ("fig:" <> tit) caption
       _ -> mzero

pCodeBlock :: PandocMonad m => TagParser m Blocks
pCodeBlock = try $ do
  TagOpen _ attr' <- pSatisfy (matchTagOpen "pre" [])
  -- if the `pre` has no attributes, try if it is followed by a `code`
  -- element and use those attributes if possible.
  attr <- case attr' of
    _:_ -> pure (toAttr attr')
    []  -> option nullAttr $ do
      TagOpen _ codeAttr <- pSatisfy (matchTagOpen "code" [])
      pure $ toAttr
        [ (k, v') | (k, v) <- codeAttr
                    -- strip language from class
                  , let v' = if k == "class"
                             then fromMaybe v (T.stripPrefix "language-" v)
                             else v ]
  contents <- manyTill pAny (pCloses "pre" <|> eof)
  let rawText = T.concat $ map tagToText contents
  -- drop leading newline if any
  let result' = case T.uncons rawText of
                     Just ('\n', xs) -> xs
                     _               -> rawText
  -- drop trailing newline if any
  let result = case T.unsnoc result' of
                    Just (result'', '\n') -> result''
                    _                     -> result'
  return $ B.codeBlockWith attr result

tagToText :: Tag Text -> Text
tagToText (TagText s)      = s
tagToText (TagOpen "br" _) = "\n"
tagToText _                = ""

inline :: PandocMonad m => TagParser m Inlines
inline = pTagText <|> do
  tag <- lookAhead (pSatisfy isInlineTag)
  exts <- getOption readerExtensions
  case tag of
    TagOpen name attr ->
      case name of
        "a" | extensionEnabled Ext_epub_html_exts exts
          , Just "noteref" <- lookup "type" attr <|> lookup "epub:type" attr
          , Just ('#',_) <- lookup "href" attr >>= T.uncons
            -> eNoteref
            | otherwise -> pLink
        "switch" -> eSwitch id inline
        "q" -> pQ
        "em" -> pEmph
        "i"  -> pEmph
        "strong" -> pStrong
        "b" -> pStrong
        "sup" -> pSuperscript
        "sub" -> pSubscript
        "small" -> pSmall
        "s" -> pStrikeout
        "strike" -> pStrikeout
        "del" -> pStrikeout
        "u" -> pUnderline
        "ins" -> pUnderline
        "br" -> pLineBreak
        "img" -> pImage
        "svg" -> pSvg
        "bdo" -> pBdo
        "code" -> pCode
        "samp" -> pCodeWithClass "samp" "sample"
        "var" -> pCodeWithClass "var" "variable"
        "span" -> pSpan
        "math" -> pMath False
        "script"
          | Just x <- lookup "type" attr
          , "math/tex" `T.isPrefixOf` x -> pScriptMath
        _ | name `elem` htmlSpanLikeElements -> pSpanLike
        _ -> pRawHtmlInline
    TagText _ -> pTagText
    _ -> pRawHtmlInline

pSelfClosing :: PandocMonad m
             => (Text -> Bool) -> ([Attribute Text] -> Bool)
             -> TagParser m (Tag Text)
pSelfClosing f g = do
  open <- pSatisfy (tagOpen f g)
  optional $ pSatisfy (tagClose f)
  return open

pQ :: PandocMonad m => TagParser m Inlines
pQ = do
  TagOpen _ attrs <- pSatisfy $ tagOpenLit "q" (const True)
  case lookup "cite" attrs of
    Just url -> do
      let uid = fromMaybe mempty $
                   lookup "name" attrs <> lookup "id" attrs
      let cls = maybe [] T.words $ lookup "class" attrs
      url' <- canonicalizeUrl url
      makeQuote $ B.spanWith (uid, cls, [("cite", escapeURI url')])
    Nothing -> makeQuote id
 where
  makeQuote wrapper = do
    ctx <- asks quoteContext
    let (constructor, innerContext) = case ctx of
                  InDoubleQuote -> (B.singleQuoted, InSingleQuote)
                  _             -> (B.doubleQuoted, InDoubleQuote)
    content <- withQuoteContext innerContext
                  (mconcat <$> manyTill inline (pCloses "q"))
    return $ extractSpaces (constructor . wrapper) content

pEmph :: PandocMonad m => TagParser m Inlines
pEmph = pInlinesInTags "em" B.emph <|> pInlinesInTags "i" B.emph

pStrong :: PandocMonad m => TagParser m Inlines
pStrong = pInlinesInTags "strong" B.strong <|> pInlinesInTags "b" B.strong

pSuperscript :: PandocMonad m => TagParser m Inlines
pSuperscript = pInlinesInTags "sup" B.superscript

pSubscript :: PandocMonad m => TagParser m Inlines
pSubscript = pInlinesInTags "sub" B.subscript

pSpanLike :: PandocMonad m => TagParser m Inlines
pSpanLike =
  Set.foldr
    (\tagName acc -> acc <|> parseTag tagName)
    mzero
    htmlSpanLikeElements
  where
    parseTag tagName = do
      TagOpen _ attrs <- pSatisfy $ tagOpenLit tagName (const True)
      let (ids, cs, kvs) = toAttr attrs
      content <- mconcat <$> manyTill inline (pCloses tagName <|> eof)
      return $ B.spanWith (ids, tagName : cs, kvs) content

pSmall :: PandocMonad m => TagParser m Inlines
pSmall = pInlinesInTags "small" (B.spanWith ("",["small"],[]))

pStrikeout :: PandocMonad m => TagParser m Inlines
pStrikeout =
  pInlinesInTags "s" B.strikeout <|>
    pInlinesInTags "strike" B.strikeout <|>
    pInlinesInTags "del" B.strikeout <|>
    try (do pSatisfy (matchTagOpen "span" [("class","strikeout")])
            contents <- mconcat <$> manyTill inline (pCloses "span")
            return $ B.strikeout contents)

pUnderline :: PandocMonad m => TagParser m Inlines
pUnderline = pInlinesInTags "u" B.underline <|> pInlinesInTags "ins" B.underline

pLineBreak :: PandocMonad m => TagParser m Inlines
pLineBreak = do
  pSelfClosing (=="br") (const True)
  return B.linebreak

pLink :: PandocMonad m => TagParser m Inlines
pLink = try $ do
  tag <- pSatisfy $ tagOpenLit "a" (const True)
  let title = fromAttrib "title" tag
  -- take id from id attribute if present, otherwise name
  let uid = fromMaybe (fromAttrib "name" tag) $
               maybeFromAttrib "id" tag
  let cls = T.words $ fromAttrib "class" tag
  lab <- mconcat <$> manyTill inline (pCloses "a")
  -- check for href; if href, then a link, otherwise a span
  case maybeFromAttrib "href" tag of
       Nothing   ->
         return $ extractSpaces (B.spanWith (uid, cls, [])) lab
       Just url' -> do
         url <- canonicalizeUrl url'
         return $ extractSpaces (B.linkWith (uid, cls, []) (escapeURI url) title) lab

pImage :: PandocMonad m => TagParser m Inlines
pImage = do
  tag@(TagOpen _ attr') <- pSelfClosing (=="img") (isJust . lookup "src")
  url <- canonicalizeUrl $ fromAttrib "src" tag
  let title = fromAttrib "title" tag
  let alt = fromAttrib "alt" tag
  let attr = toAttr $ filter (\(k,_) -> k /= "alt" && k /= "title" && k /= "src") attr'
  return $ B.imageWith attr (escapeURI url) title (B.text alt)

pSvg :: PandocMonad m => TagParser m Inlines
pSvg = do
  guardDisabled Ext_raw_html
  -- if raw_html enabled, parse svg tag as raw
  opent@(TagOpen _ attr') <- pSatisfy (matchTagOpen "svg" [])
  let (ident,cls,_) = toAttr attr'
  contents <- many (notFollowedBy (pCloses "svg") >> pAny)
  closet <- TagClose "svg" <$ (pCloses "svg" <|> eof)
  let rawText = T.strip $ renderTags' (opent : contents ++ [closet])
  let svgData = "data:image/svg+xml;base64," <>
                   UTF8.toText (encode $ UTF8.fromText rawText)
  return $ B.imageWith (ident,cls,[]) svgData mempty mempty

pCodeWithClass :: PandocMonad m => Text -> Text -> TagParser m Inlines
pCodeWithClass name class' = try $ do
  TagOpen open attr' <- pSatisfy $ tagOpen (== name) (const True)
  result <- manyTill pAny (pCloses open)
  let (ids,cs,kvs) = toAttr attr'
      cs'          = class' : cs
  return . B.codeWith (ids,cs',kvs) .
    T.unwords . T.lines . innerText $ result

pCode :: PandocMonad m => TagParser m Inlines
pCode = try $ do
  (TagOpen open attr') <- pSatisfy $ tagOpen (`elem` ["code","tt"]) (const True)
  let attr = toAttr attr'
  result <- manyTill pAny (pCloses open)
  return $ B.codeWith attr $ T.unwords $ T.lines $ innerText result

-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
-- Bidirectional Text Override
pBdo :: PandocMonad m => TagParser m Inlines
pBdo = try $ do
  TagOpen _ attr' <- lookAhead $ pSatisfy $ tagOpen (=="bdo") (const True)
  let attr = toStringAttr attr'
  contents <- pInTags "bdo" inline
  return $ case lookup "dir" attr of
    -- Only bdo with a direction matters
    Just dir -> B.spanWith ("", [], [("dir",T.toLower dir)]) contents
    Nothing  -> contents

pSpan :: PandocMonad m => TagParser m Inlines
pSpan = try $ do
  guardEnabled Ext_native_spans
  TagOpen _ attr' <- lookAhead $ pSatisfy $ tagOpen (=="span") (const True)
  let attr = toAttr attr'
  contents <- pInTags "span" inline
  let isSmallCaps = fontVariant == "small-caps" || "smallcaps" `elem` classes
                    where styleAttr   = fromMaybe "" $ lookup "style" attr'
                          fontVariant = fromMaybe "" $ pickStyleAttrProps ["font-variant"] styleAttr
                          classes     = maybe []
                                          T.words $ lookup "class" attr'
  let tag = if isSmallCaps then B.smallcaps else B.spanWith attr
  return $ tag contents

pRawHtmlInline :: PandocMonad m => TagParser m Inlines
pRawHtmlInline = do
  inplain <- asks inPlain
  result <- pSatisfy (tagComment (const True))
            <|> if inplain
                   then pSatisfy (not . isBlockTag)
                   else pSatisfy isInlineTag
  exts <- getOption readerExtensions
  let raw = renderTags' [result]
  if extensionEnabled Ext_raw_html exts
     then return $ B.rawInline "html" raw
     else ignore raw

mathMLToTeXMath :: Text -> Either Text Text
mathMLToTeXMath s = writeTeX <$> readMathML s

pScriptMath :: PandocMonad m => TagParser m Inlines
pScriptMath = try $ do
  TagOpen _ attr' <- pSatisfy $ tagOpen (=="script") (const True)
  isdisplay <- case lookup "type" attr' of
                    Just x | "math/tex" `T.isPrefixOf` x
                      -> return $ "display" `T.isSuffixOf` x
                    _ -> mzero
  contents <- innerText <$> manyTill pAny (pSatisfy (matchTagClose "script"))
  return $ (if isdisplay then B.displayMath else B.math) contents

pMath :: PandocMonad m => Bool -> TagParser m Inlines
pMath inCase = try $ do
  open@(TagOpen _ attr') <- pSatisfy $ tagOpen (=="math") (const True)
  -- we'll assume math tags are MathML unless specially marked
  -- otherwise...
  let attr = toStringAttr attr'
  unless inCase $
    guard (maybe True (== mathMLNamespace) (lookup "xmlns" attr))
  contents <- manyTill pAny (pSatisfy (matchTagClose "math"))
  case mathMLToTeXMath (renderTags $
          [open] <> contents <> [TagClose "math"]) of
       Left _   -> return $ B.spanWith ("",["math"],attr) $ B.text $
                             innerText contents
       Right "" -> return mempty
       Right x  -> return $ case lookup "display" attr of
                                 Just "block" -> B.displayMath x
                                 _            -> B.math x

pInlinesInTags :: PandocMonad m => Text -> (Inlines -> Inlines)
               -> TagParser m Inlines
pInlinesInTags tagtype f = extractSpaces f <$> pInTags tagtype inline

pTagText :: PandocMonad m => TagParser m Inlines
pTagText = try $ do
  pos <- getPosition
  (TagText str) <- pSatisfy isTagText
  st <- getState
  qu <- ask
  parsed <- lift $ lift $
            flip runReaderT qu $ runParserT (many pTagContents) st "text"
               (Sources [(pos, str)])
  case parsed of
       Left _        -> throwError $ PandocParseError $
                        "Could not parse `" <> str <> "'"
       Right result  -> return $ mconcat result

type InlinesParser m = HTMLParser m Sources

pTagContents :: PandocMonad m => InlinesParser m Inlines
pTagContents =
      B.displayMath <$> mathDisplay
  <|> B.math        <$> mathInline
  <|> pStr
  <|> pSpace
  <|> smartPunctuation pTagContents
  <|> pRawTeX
  <|> pSymbol
  <|> pBad

pRawTeX :: PandocMonad m => InlinesParser m Inlines
pRawTeX = do
  lookAhead $ try $ do
    char '\\'
    choice $ map (try . string) ["begin", "eqref", "ref"]
  guardEnabled Ext_raw_tex
  inp <- getInput
  st <- getState
  res <- lift $ runParserT (withRaw rawLaTeXInline) st "chunk" inp
  case res of
       Left _                -> mzero
       Right (contents, raw) -> do
         _ <- count (T.length raw) anyChar
         return $ B.rawInline "tex" contents

pStr :: PandocMonad m => InlinesParser m Inlines
pStr = do
  result <- many1 $ satisfy $ \c ->
                     not (isSpace c) && not (isSpecial c) && not (isBad c)
  updateLastStrPos
  return $ B.str $ T.pack result

isSpecial :: Char -> Bool
isSpecial '"'     = True
isSpecial '\''    = True
isSpecial '.'     = True
isSpecial '-'     = True
isSpecial '$'     = True
isSpecial '\\'    = True
isSpecial '\8216' = True
isSpecial '\8217' = True
isSpecial '\8220' = True
isSpecial '\8221' = True
isSpecial _       = False

pSymbol :: PandocMonad m => InlinesParser m Inlines
pSymbol = B.str . T.singleton <$> satisfy isSpecial

isBad :: Char -> Bool
isBad c = c >= '\128' && c <= '\159' -- not allowed in HTML

pBad :: PandocMonad m => InlinesParser m Inlines
pBad = do
  c <- satisfy isBad
  let c' = case c of
                '\128' -> '\8364'
                '\130' -> '\8218'
                '\131' -> '\402'
                '\132' -> '\8222'
                '\133' -> '\8230'
                '\134' -> '\8224'
                '\135' -> '\8225'
                '\136' -> '\710'
                '\137' -> '\8240'
                '\138' -> '\352'
                '\139' -> '\8249'
                '\140' -> '\338'
                '\142' -> '\381'
                '\145' -> '\8216'
                '\146' -> '\8217'
                '\147' -> '\8220'
                '\148' -> '\8221'
                '\149' -> '\8226'
                '\150' -> '\8211'
                '\151' -> '\8212'
                '\152' -> '\732'
                '\153' -> '\8482'
                '\154' -> '\353'
                '\155' -> '\8250'
                '\156' -> '\339'
                '\158' -> '\382'
                '\159' -> '\376'
                _      -> '?'
  return $ B.str $ T.singleton c'

pSpace :: PandocMonad m => InlinesParser m Inlines
pSpace = many1 (satisfy isSpace) >>= \xs ->
            if '\n' `elem` xs
               then return B.softbreak
               else return B.space

getTagName :: Tag Text -> Maybe Text
getTagName (TagOpen t _) = Just t
getTagName (TagClose t)  = Just t
getTagName _             = Nothing

isInlineTag :: Tag Text -> Bool
isInlineTag t = isCommentTag t || case t of
  TagOpen "script" _ -> "math/tex" `T.isPrefixOf` fromAttrib "type" t
  TagClose "script"  -> True
  TagOpen name _     -> isInlineTagName name
  TagClose name      -> isInlineTagName name
  _                  -> False
 where isInlineTagName x =
         x `Set.notMember` blockTags ||
         T.take 1 x == "?" -- processing instr.

isBlockTag :: Tag Text -> Bool
isBlockTag t = isBlockTagName || isTagComment t
                 where isBlockTagName =
                         case getTagName t of
                              Just x
                                | "?" `T.isPrefixOf` x -> True
                                | "!" `T.isPrefixOf` x -> True
                                | otherwise -> x `Set.member` blockTags
                                    || x `Set.member` eitherBlockOrInline
                              Nothing -> False

isTextTag :: Tag Text -> Bool
isTextTag = tagText (const True)

isCommentTag :: Tag Text -> Bool
isCommentTag = tagComment (const True)

--- parsers for use in markdown, textile readers

-- | Matches a stretch of HTML in balanced tags.
htmlInBalanced :: Monad m
               => (Tag Text -> Bool)
               -> ParserT Sources st m Text
htmlInBalanced f = try $ do
  lookAhead (char '<')
  sources <- getInput
  let ts = canonicalizeTags
        $ parseTagsOptions parseOptions{ optTagWarning = True,
                                         optTagPosition = True }
        $ sourcesToText sources
  case ts of
    (TagPosition sr sc : t@(TagOpen tn _) : rest) -> do
       guard $ f t
       guard $ not $ hasTagWarning (t : take 1 rest)
       case htmlInBalanced' tn (t:rest) of
            []  -> mzero
            xs  -> case reverse xs of
                        (TagClose _ : TagPosition er ec : _) -> do
                          let ls = er - sr
                          let cs = ec - sc
                          lscontents <- T.unlines <$> count ls anyLine
                          cscontents <- count cs anyChar
                          closetag <- do
                            x <- many (satisfy (/='>'))
                            char '>'
                            return (x <> ">")
                          return $ lscontents <> T.pack cscontents <> T.pack closetag
                        _ -> mzero
    _ -> mzero

htmlInBalanced' :: Text
                -> [Tag Text]
                -> [Tag Text]
htmlInBalanced' tagname ts = fromMaybe [] $ go 0 ts
  where go :: Int -> [Tag Text] -> Maybe [Tag Text]
        go n (t@(TagOpen tn' _):rest) | tn' == tagname =
              (t :) <$> go (n + 1) rest
        go 1 (t@(TagClose tn'):_) | tn' == tagname =
              return [t]
        go n (t@(TagClose tn'):rest)  | tn' == tagname =
              (t :) <$> go (n - 1) rest
        go n (t:ts') = (t :) <$> go n ts'
        go _ [] = mzero

hasTagWarning :: [Tag Text] -> Bool
hasTagWarning (TagWarning _:_) = True
hasTagWarning _                = False

-- | Matches a tag meeting a certain condition.
htmlTag :: (HasReaderOptions st, Monad m)
        => (Tag Text -> Bool)
        -> ParserT Sources st m (Tag Text, Text)
htmlTag f = try $ do
  lookAhead (char '<')
  startpos <- getPosition
  sources <- getInput
  let inp = sourcesToText sources
  let ts = canonicalizeTags $ parseTagsOptions
                               parseOptions{ optTagWarning = False
                                           , optTagPosition = True }
                               (inp <> " ")
                               -- add space to ensure that
                               -- we get a TagPosition after the tag
  (next, ln, col) <- case ts of
                      (TagPosition{} : next : TagPosition ln col : _)
                        | f next -> return (next, ln, col)
                      _ -> mzero

  -- <www.boe.es/buscar/act.php?id=BOE-A-1996-8930#a66>
  -- should NOT be parsed as an HTML tag, see #2277,
  -- so we exclude . even though it's a valid character
  -- in XML element names
  let isNameChar c = isAlphaNum c || c == ':' || c == '-' || c == '_'
  let isName s = case T.uncons s of
                   Nothing      -> False
                   Just (c, cs) -> isLetter c && T.all isNameChar cs
  let isPI s = case T.uncons s of
                 Just ('?', _) -> True -- processing instruction
                 _             -> False
  let endpos = if ln == 1
                  then setSourceColumn startpos
                         (sourceColumn startpos + (col - 1))
                  else setSourceColumn (setSourceLine startpos
                                        (sourceLine startpos + (ln - 1)))
                         col
  let endAngle = try $
        do char '>'
           pos <- getPosition
           guard $ pos >= endpos

  let handleTag tagname = do
       -- basic sanity check, since the parser is very forgiving
       -- and finds tags in stuff like x<y)
       guard $ isName tagname || isPI tagname
       guard $ not $ T.null tagname
       -- <https://example.org> should NOT be a tag either.
       -- tagsoup will parse it as TagOpen "https:" [("example.org","")]
       guard $ T.last tagname /= ':'
       char '<'
       rendered <- manyTill anyChar endAngle
       return (next, T.pack $ "<" ++ rendered ++ ">")
  case next of
       TagComment s
         | "<!--" `T.isPrefixOf` inp -> do
          string "<!--"
          count (T.length s) anyChar
          string "-->"
          stripComments <- getOption readerStripComments
          if stripComments
             then return (next, "")
             else return (next, "<!--" <> s <> "-->")
         | otherwise -> Prelude.fail "bogus comment mode, HTML5 parse error"
       TagOpen tagname attr -> do
         guard $ isPI tagname || all (isName . fst) attr
         handleTag tagname
       TagClose tagname ->
         handleTag tagname
       _ -> mzero

-- Utilities

-- | Adjusts a url according to the document's base URL.
canonicalizeUrl :: PandocMonad m => Text -> TagParser m Text
canonicalizeUrl url = do
  mbBaseHref <- baseHref <$> getState
  return $ case (parseURIReference (T.unpack url), mbBaseHref) of
                (Just rel, Just bs) -> tshow (rel `nonStrictRelativeTo` bs)
                _                   -> url

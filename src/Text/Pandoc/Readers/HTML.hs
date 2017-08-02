{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
ViewPatterns, OverloadedStrings #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.HTML
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
                                , NamedTag(..)
                                , isTextTag
                                , isCommentTag
                                ) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines, HasMeta(..))
import Text.Pandoc.Shared ( extractSpaces, addMetaField
                          , escapeURI, safeRead, crFilter )
import Text.Pandoc.Options (ReaderOptions(readerExtensions), extensionEnabled,
                               Extension (Ext_epub_html_exts,
                               Ext_raw_html, Ext_native_divs, Ext_native_spans))
import Text.Pandoc.Logging
import Text.Pandoc.Parsing hiding ((<|>))
import Text.Pandoc.Walk
import qualified Data.Map as M
import Data.Foldable ( for_ )
import Data.Maybe ( fromMaybe, isJust)
import Data.List ( intercalate, isPrefixOf )
import Data.Char ( isDigit, isLetter, isAlphaNum )
import Control.Monad ( guard, mzero, void, unless )
import Control.Arrow ((***))
import Control.Applicative ( (<|>) )
import Data.Monoid (First (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.TeXMath (readMathML, writeTeX)
import Data.Default (Default (..), def)
import Control.Monad.Reader (ask, asks, local, ReaderT, runReaderT, lift)
import Network.URI (URI, parseURIReference, nonStrictRelativeTo)
import Text.Pandoc.CSS (foldOrElse, pickStyleAttrProps)
import Data.Monoid ((<>))
import Text.Parsec.Error
import qualified Data.Set as Set
import Text.Pandoc.Error
import Text.Pandoc.Class (PandocMonad(..))
import Control.Monad.Except (throwError)

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: PandocMonad m
         => ReaderOptions -- ^ Reader options
         -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
         -> m Pandoc
readHtml opts inp = do
  let tags = stripPrefixes . canonicalizeTags $
             parseTagsOptions parseOptions{ optTagPosition = True }
             (crFilter inp)
      parseDoc = do
        blocks <- (fixPlains False) . mconcat <$> manyTill block eof
        meta <- stateMeta . parserState <$> getState
        bs' <- replaceNotes (B.toList blocks)
        reportLogMessages
        return $ Pandoc meta bs'
      getError (errorMessages -> ms) = case ms of
                                         []    -> ""
                                         (m:_) -> messageString m
  result <- flip runReaderT def $
       runParserT parseDoc
       (HTMLState def{ stateOptions = opts } [] Nothing Set.empty M.empty [])
       "source" tags
  case result of
    Right doc -> return doc
    Left  err -> throwError $ PandocParseError $ getError err

replaceNotes :: PandocMonad m => [Block] -> TagParser m [Block]
replaceNotes = walkM replaceNotes'

replaceNotes' :: PandocMonad m => Inline -> TagParser m Inline
replaceNotes' (RawInline (Format "noteref") ref) = maybe (Str "") (Note . B.toList) . lookup ref <$> getNotes
  where
    getNotes = noteTable <$> getState
replaceNotes' x = return x

data HTMLState =
  HTMLState
  {  parserState :: ParserState,
     noteTable   :: [(String, Blocks)],
     baseHref    :: Maybe URI,
     identifiers :: Set.Set String,
     headerMap   :: M.Map Inlines String,
     logMessages :: [LogMessage]
  }

data HTMLLocal = HTMLLocal { quoteContext :: QuoteContext
                           , inChapter :: Bool -- ^ Set if in chapter section
                           , inPlain :: Bool -- ^ Set if in pPlain
                           }

setInChapter :: PandocMonad m => HTMLParser m s a -> HTMLParser m s a
setInChapter = local (\s -> s {inChapter = True})

setInPlain :: PandocMonad m => HTMLParser m s a -> HTMLParser m s a
setInPlain = local (\s -> s {inPlain = True})

type HTMLParser m s = ParserT s HTMLState (ReaderT HTMLLocal m)

type TagParser m = HTMLParser m [Tag Text]

pHtml :: PandocMonad m => TagParser m Blocks
pHtml = try $ do
  (TagOpen "html" attr) <- lookAhead $ pAnyTag
  for_ (lookup "lang" attr) $
    updateState . B.setMeta "lang" . B.text . T.unpack
  pInTags "html" block

pBody :: PandocMonad m => TagParser m Blocks
pBody = pInTags "body" block

pHead :: PandocMonad m => TagParser m Blocks
pHead = pInTags "head" $ pTitle <|> pMetaTag <|> pBaseTag <|> (mempty <$ pAnyTag)
  where pTitle = pInTags "title" inline >>= setTitle . trimInlines
        setTitle t = mempty <$ (updateState $ B.setMeta "title" t)
        pMetaTag = do
          mt <- pSatisfy (matchTagOpen "meta" [])
          let name = T.unpack $ fromAttrib "name" mt
          if null name
             then return mempty
             else do
               let content = T.unpack $ fromAttrib "content" mt
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
block = do
  res <- choice
            [ eSection
            , eSwitch B.para block
            , mempty <$ eFootnote
            , mempty <$ eTOC
            , mempty <$ eTitlePage
            , pPara
            , pHeader
            , pBlockQuote
            , pCodeBlock
            , pList
            , pHrule
            , pTable
            , pHtml
            , pHead
            , pBody
            , pDiv
            , pPlain
            , pFigure
            , pRawHtmlBlock
            ]
  trace (take 60 $ show $ B.toList res)
  return res

namespaces :: PandocMonad m => [(String, TagParser m Inlines)]
namespaces = [(mathMLNamespace, pMath True)]

mathMLNamespace :: String
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
  TagOpen _ attr' <- lookAhead $ pSatisfy $ (matchTagOpen "case" [])
  let attr = toStringAttr attr'
  case (flip lookup namespaces) =<< lookup "required-namespace" attr of
    Just p -> Just <$> (pInTags "case" (skipMany pBlank *> p <* skipMany pBlank))
    Nothing -> Nothing <$ manyTill pAnyTag (pSatisfy (matchTagClose "case"))

eFootnote :: PandocMonad m => TagParser m ()
eFootnote = try $ do
  let notes = ["footnote", "rearnote"]
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr') <- lookAhead $ pAnyTag
  let attr = toStringAttr attr'
  guard (maybe False (flip elem notes) (lookup "type" attr))
  let ident = fromMaybe "" (lookup "id" attr)
  content <- pInTags tag block
  addNote ident content

addNote :: PandocMonad m => String -> Blocks -> TagParser m ()
addNote uid cont = updateState (\s -> s {noteTable = (uid, cont) : (noteTable s)})

eNoteref :: PandocMonad m => TagParser m Inlines
eNoteref = try $ do
  guardEnabled Ext_epub_html_exts
  TagOpen tag attr' <- lookAhead $ pAnyTag
  let attr = toStringAttr attr'
  guard (maybe False (== "noteref") (lookup "type" attr))
  let ident = maybe "" (dropWhile (== '#')) (lookup "href" attr)
  guard (not (null ident))
  pInTags tag block
  return $ B.rawInline "noteref" ident

-- Strip TOC if there is one, better to generate again
eTOC :: PandocMonad m => TagParser m ()
eTOC = try $ do
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr) <- lookAhead $ pAnyTag
  guard (maybe False (== "toc") (lookup "type" attr))
  void (pInTags tag block)

pList :: PandocMonad m => TagParser m Blocks
pList = pBulletList <|> pOrderedList <|> pDefinitionList

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
  (maybe id addId (lookup "id" attr)) <$>
    pInTags "li" block <* skipMany nonItem

parseListStyleType :: String -> ListNumberStyle
parseListStyleType "lower-roman" = LowerRoman
parseListStyleType "upper-roman" = UpperRoman
parseListStyleType "lower-alpha" = LowerAlpha
parseListStyleType "upper-alpha" = UpperAlpha
parseListStyleType "decimal"     = Decimal
parseListStyleType _             = DefaultStyle

parseTypeAttr :: String -> ListNumberStyle
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
  let (start, style) = (sta', sty')
                       where sta = fromMaybe "1" $
                                   lookup "start" attribs
                             sta' = if all isDigit sta
                                       then read sta
                                       else 1

                             pickListStyle = pickStyleAttrProps ["list-style-type", "list-style"]

                             typeAttr  = fromMaybe "" $ lookup "type"  attribs
                             classAttr = fromMaybe "" $ lookup "class" attribs
                             styleAttr = fromMaybe "" $ lookup "style" attribs
                             listStyle = fromMaybe "" $ pickListStyle styleAttr

                             sty' = foldOrElse DefaultStyle
                                      [ parseTypeAttr      typeAttr
                                      , parseListStyleType classAttr
                                      , parseListStyleType listStyle
                                      ]
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
  let term = foldl1 (\x y ->  x <> B.linebreak <> y) terms
  return (term, map (fixPlains True) defs)

fixPlains :: Bool -> Blocks -> Blocks
fixPlains inList bs = if any isParaish bs'
                         then B.fromList $ map plainToPara bs'
                         else bs
  where isParaish (Para _) = True
        isParaish (CodeBlock _ _) = True
        isParaish (Header _ _ _) = True
        isParaish (BlockQuote _) = True
        isParaish (BulletList _) = not inList
        isParaish (OrderedList _ _) = not inList
        isParaish (DefinitionList _) = not inList
        isParaish _        = False
        plainToPara (Plain xs) = Para xs
        plainToPara x = x
        bs' = B.toList bs

pRawTag :: PandocMonad m => TagParser m Text
pRawTag = do
  tag <- pAnyTag
  let ignorable x = x `elem` ["html","head","body","!DOCTYPE","?xml"]
  if tagOpen ignorable (const True) tag || tagClose ignorable tag
     then return mempty
     else return $ renderTags' [tag]

pDiv :: PandocMonad m => TagParser m Blocks
pDiv = try $ do
  guardEnabled Ext_native_divs
  let isDivLike "div" = True
      isDivLike "section" = True
      isDivLike _ = False
  TagOpen tag attr' <- lookAhead $ pSatisfy $ tagOpen isDivLike (const True)
  let attr = toStringAttr attr'
  contents <- pInTags tag block
  let (ident, classes, kvs) = mkAttr attr
  let classes' = if tag == "section"
                    then "section":classes
                    else classes
  return $ B.divWith (ident, classes', kvs) contents

pRawHtmlBlock :: PandocMonad m => TagParser m Blocks
pRawHtmlBlock = do
  raw <- T.unpack <$> (pHtmlBlock "script" <|> pHtmlBlock "style" <|> pRawTag)
  exts <- getOption readerExtensions
  if extensionEnabled Ext_raw_html exts && not (null raw)
     then return $ B.rawBlock "html" raw
     else ignore raw

ignore :: (Monoid a, PandocMonad m) => String -> TagParser m a
ignore raw = do
  pos <- getPosition
  -- raw can be null for tags like <!DOCTYPE>; see paRawTag
  -- in this case we don't want a warning:
  unless (null raw) $
    logMessage $ SkippedContent raw pos
  return mempty

pHtmlBlock :: PandocMonad m => Text -> TagParser m Text
pHtmlBlock t = try $ do
  open <- pSatisfy (matchTagOpen t [])
  contents <- manyTill pAnyTag (pSatisfy (matchTagClose t))
  return $ renderTags' $ [open] <> contents <> [TagClose t]

-- Sets chapter context
eSection :: PandocMonad m => TagParser m Blocks
eSection = try $ do
  let matchChapter as = maybe False (T.isInfixOf "chapter") (lookup "type" as)
  let sectTag = tagOpen (`elem` sectioningContent) matchChapter
  TagOpen tag _ <- lookAhead $ pSatisfy sectTag
  setInChapter (pInTags tag block)

headerLevel :: PandocMonad m => Text -> TagParser m Int
headerLevel tagtype = do
  case safeRead (T.unpack (T.drop 1 tagtype)) of
        Just level ->
          (try $ do
            guardEnabled Ext_epub_html_exts
            asks inChapter >>= guard
            return (level - 1))
            <|>
              return level
        Nothing -> fail "Could not retrieve header level"

eTitlePage :: PandocMonad m => TagParser m ()
eTitlePage = try $ do
  let isTitlePage as = maybe False (T.isInfixOf "titlepage") (lookup "type" as)
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
  let bodyTitle = TagOpen tagtype attr' ~== TagOpen ("h1" :: Text)
                                               [("class","title")]
  level <- headerLevel tagtype
  contents <- trimInlines . mconcat <$> manyTill inline (pCloses tagtype <|> eof)
  let ident = fromMaybe "" $ lookup "id" attr
  let classes = maybe [] words $ lookup "class" attr
  let keyvals = [(k,v) | (k,v) <- attr, k /= "class", k /= "id"]
  attr'' <- registerHeader (ident, classes, keyvals) contents
  return $ if bodyTitle
              then mempty  -- skip a representation of the title in the body
              else B.headerWith attr'' level contents

pHrule :: PandocMonad m => TagParser m Blocks
pHrule = do
  pSelfClosing (=="hr") (const True)
  return B.horizontalRule

pTable :: PandocMonad m => TagParser m Blocks
pTable = try $ do
  TagOpen _ _ <- pSatisfy (matchTagOpen "table" [])
  skipMany pBlank
  caption <- option mempty $ pInTags "caption" inline <* skipMany pBlank
  widths' <- (mconcat <$> many1 pColgroup) <|> many pCol
  let pTh = option [] $ pInTags "tr" (pCell "th")
      pTr = try $ skipMany pBlank >> pInTags "tr" (pCell "td" <|> pCell "th")
      pTBody = do pOptInTag "tbody" $ many1 pTr
  head'' <- pOptInTag "thead" pTh
  head'  <- pOptInTag "tbody" $ do
              if null head''
                 then pTh
                 else return head''
  rowsLs <- many pTBody
  rows'  <- pOptInTag "tfoot" $ many pTr
  TagClose _ <- pSatisfy (matchTagClose "table")
  let rows'' = (concat rowsLs) <> rows'
  -- fail on empty table
  guard $ not $ null head' && null rows''
  let isSinglePlain x = case B.toList x of
                             []        -> True
                             [Plain _] -> True
                             _         -> False
  let isSimple = all isSinglePlain $ concat (head':rows'')
  let cols = length $ if null head' then head rows'' else head'
  -- add empty cells to short rows
  let addEmpties r = case cols - length r of
                           n | n > 0 -> r <> replicate n mempty
                             | otherwise -> r
  let rows = map addEmpties rows''
  let aligns = replicate cols AlignDefault
  let widths = if null widths'
                  then if isSimple
                       then replicate cols 0
                       else replicate cols (1.0 / fromIntegral cols)
                  else widths'
  return $ B.table caption (zip aligns widths) head' rows

pCol :: PandocMonad m => TagParser m Double
pCol = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "col" [])
  let attribs = toStringAttr attribs'
  skipMany pBlank
  optional $ pSatisfy (matchTagClose "col")
  skipMany pBlank
  return $ case lookup "width" attribs of
           Nothing -> case lookup "style" attribs of
               Just ('w':'i':'d':'t':'h':':':xs) | '%' `elem` xs ->
                 fromMaybe 0.0 $ safeRead ('0':'.':filter
                   (`notElem` (" \t\r\n%'\";" :: [Char])) xs)
               _ -> 0.0
           Just x | not (null x) && last x == '%' ->
             fromMaybe 0.0 $ safeRead ('0':'.':init x)
           _ -> 0.0

pColgroup :: PandocMonad m => TagParser m [Double]
pColgroup = try $ do
  pSatisfy (matchTagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

noColOrRowSpans :: Tag Text -> Bool
noColOrRowSpans t = isNullOrOne "colspan" && isNullOrOne "rowspan"
  where isNullOrOne x = case fromAttrib x t of
                              ""  -> True
                              "1" -> True
                              _   -> False

pCell :: PandocMonad m => Text -> TagParser m [Blocks]
pCell celltype = try $ do
  skipMany pBlank
  res <- pInTags' celltype noColOrRowSpans block
  skipMany pBlank
  return [res]

pBlockQuote :: PandocMonad m => TagParser m Blocks
pBlockQuote = do
  contents <- pInTags "blockquote" block
  return $ B.blockQuote $ fixPlains False contents

pPlain :: PandocMonad m => TagParser m Blocks
pPlain = do
  contents <- setInPlain $ trimInlines . mconcat <$> many1 inline
  if B.isNull contents
     then return mempty
     else return $ B.plain contents

pPara :: PandocMonad m => TagParser m Blocks
pPara = do
  contents <- trimInlines <$> pInTags "p" inline
  return $ B.para contents

pFigure :: PandocMonad m => TagParser m Blocks
pFigure = do
  TagOpen _ _ <- pSatisfy (matchTagOpen "figure" [])
  skipMany pBlank
  let pImg  = pOptInTag "p" pImage <* skipMany pBlank
      pCapt = option mempty $ pInTags "figcaption" inline <* skipMany pBlank
      pImgCapt = do
        img <- pImg
        cap <- pCapt
        return (img, cap)
      pCaptImg = do
        cap <- pCapt
        img <- pImg
        return (img, cap)
  (imgMany, caption) <- pImgCapt <|> pCaptImg
  TagClose _ <- pSatisfy (matchTagClose "figure")
  let (Image attr _ (url, tit)):_ = B.toList imgMany
  return $ B.para $ B.imageWith attr url ("fig:" ++ tit) caption

pCodeBlock :: PandocMonad m => TagParser m Blocks
pCodeBlock = try $ do
  TagOpen _ attr' <- pSatisfy (matchTagOpen "pre" [])
  let attr = toStringAttr attr'
  contents <- manyTill pAnyTag (pCloses "pre" <|> eof)
  let rawText = concatMap tagToString contents
  -- drop leading newline if any
  let result' = case rawText of
                     '\n':xs  -> xs
                     _        -> rawText
  -- drop trailing newline if any
  let result = case reverse result' of
                    '\n':_   -> init result'
                    _        -> result'
  return $ B.codeBlockWith (mkAttr attr) result

tagToString :: Tag Text -> String
tagToString (TagText s) = T.unpack s
tagToString (TagOpen "br" _) = "\n"
tagToString _ = ""

inline :: PandocMonad m => TagParser m Inlines
inline = choice
           [ eNoteref
           , eSwitch id inline
           , pTagText
           , pQ
           , pEmph
           , pStrong
           , pSuperscript
           , pSubscript
           , pStrikeout
           , pLineBreak
           , pLink
           , pImage
           , pCode
           , pSpan
           , pMath False
           , pRawHtmlInline
           ]

pLocation :: PandocMonad m => TagParser m ()
pLocation = do
  (TagPosition r c) <- pSat isTagPosition
  setPosition $ newPos "input" r c

pSat :: PandocMonad m => (Tag Text -> Bool) -> TagParser m (Tag Text)
pSat f = do
  pos <- getPosition
  token show (const pos) (\x -> if f x then Just x else Nothing)

pSatisfy :: PandocMonad m => (Tag Text -> Bool) -> TagParser m (Tag Text)
pSatisfy f = try $ optional pLocation >> pSat f

pAnyTag :: PandocMonad m => TagParser m (Tag Text)
pAnyTag = pSatisfy (const True)

pSelfClosing :: PandocMonad m
             => (Text -> Bool) -> ([Attribute Text] -> Bool)
             -> TagParser m (Tag Text)
pSelfClosing f g = do
  open <- pSatisfy (tagOpen f g)
  optional $ pSatisfy (tagClose f)
  return open

pQ :: PandocMonad m => TagParser m Inlines
pQ = do
  context <- asks quoteContext
  let quoteType = case context of
                       InDoubleQuote -> SingleQuote
                       _             -> DoubleQuote
  let innerQuoteContext = if quoteType == SingleQuote
                             then InSingleQuote
                             else InDoubleQuote
  let constructor = case quoteType of
                            SingleQuote -> B.singleQuoted
                            DoubleQuote -> B.doubleQuoted
  withQuoteContext innerQuoteContext $
    pInlinesInTags "q" constructor

pEmph :: PandocMonad m => TagParser m Inlines
pEmph = pInlinesInTags "em" B.emph <|> pInlinesInTags "i" B.emph

pStrong :: PandocMonad m => TagParser m Inlines
pStrong = pInlinesInTags "strong" B.strong <|> pInlinesInTags "b" B.strong

pSuperscript :: PandocMonad m => TagParser m Inlines
pSuperscript = pInlinesInTags "sup" B.superscript

pSubscript :: PandocMonad m => TagParser m Inlines
pSubscript = pInlinesInTags "sub" B.subscript

pStrikeout :: PandocMonad m => TagParser m Inlines
pStrikeout = do
  pInlinesInTags "s" B.strikeout <|>
    pInlinesInTags "strike" B.strikeout <|>
    pInlinesInTags "del" B.strikeout <|>
    try (do pSatisfy (matchTagOpen "span" [("class","strikeout")])
            contents <- mconcat <$> manyTill inline (pCloses "span")
            return $ B.strikeout contents)

pLineBreak :: PandocMonad m => TagParser m Inlines
pLineBreak = do
  pSelfClosing (=="br") (const True)
  return B.linebreak

-- Unlike fromAttrib from tagsoup, this distinguishes
-- between a missing attribute and an attribute with empty content.
maybeFromAttrib :: String -> Tag Text -> Maybe String
maybeFromAttrib name (TagOpen _ attrs) =
  T.unpack <$> lookup (T.pack name) attrs
maybeFromAttrib _ _ = Nothing

pLink :: PandocMonad m => TagParser m Inlines
pLink = try $ do
  tag <- pSatisfy $ tagOpenLit "a" (const True)
  let title = T.unpack $ fromAttrib "title" tag
  -- take id from id attribute if present, otherwise name
  let uid = maybe (T.unpack $ fromAttrib "name" tag) id $
               maybeFromAttrib "id" tag
  let cls = words $ T.unpack $ fromAttrib "class" tag
  lab <- trimInlines . mconcat <$> manyTill inline (pCloses "a")
  -- check for href; if href, then a link, otherwise a span
  case maybeFromAttrib "href" tag of
       Nothing   ->
         return $ B.spanWith (uid, cls, []) lab
       Just url' -> do
         mbBaseHref <- baseHref <$> getState
         let url = case (parseURIReference url', mbBaseHref) of
                        (Just rel, Just bs) ->
                          show (rel `nonStrictRelativeTo` bs)
                        _                   -> url'
         return $ B.linkWith (uid, cls, []) (escapeURI url) title lab

pImage :: PandocMonad m => TagParser m Inlines
pImage = do
  tag <- pSelfClosing (=="img") (isJust . lookup "src")
  mbBaseHref <- baseHref <$> getState
  let url' = T.unpack $ fromAttrib "src" tag
  let url = case (parseURIReference url', mbBaseHref) of
                 (Just rel, Just bs) -> show (rel `nonStrictRelativeTo` bs)
                 _                   -> url'
  let title = T.unpack $ fromAttrib "title" tag
  let alt = T.unpack $ fromAttrib "alt" tag
  let uid = T.unpack $ fromAttrib "id" tag
  let cls = words $ T.unpack $ fromAttrib "class" tag
  let getAtt k = case fromAttrib k tag of
                   "" -> []
                   v  -> [(T.unpack k, T.unpack v)]
  let kvs = concat $ map getAtt ["width", "height", "sizes", "srcset"]
  return $ B.imageWith (uid, cls, kvs) (escapeURI url) title (B.text alt)

pCode :: PandocMonad m => TagParser m Inlines
pCode = try $ do
  (TagOpen open attr') <- pSatisfy $ tagOpen (`elem` ["code","tt"]) (const True)
  let attr = toStringAttr attr'
  result <- manyTill pAnyTag (pCloses open)
  return $ B.codeWith (mkAttr attr) $ intercalate " " $ lines $ T.unpack $
           innerText result

pSpan :: PandocMonad m => TagParser m Inlines
pSpan = try $ do
  guardEnabled Ext_native_spans
  TagOpen _ attr' <- lookAhead $ pSatisfy $ tagOpen (=="span") (const True)
  let attr = toStringAttr attr'
  contents <- pInTags "span" inline
  let isSmallCaps = fontVariant == "small-caps" || "smallcaps" `elem` classes
                    where styleAttr   = fromMaybe "" $ lookup "style" attr
                          fontVariant = fromMaybe "" $ pickStyleAttrProps ["font-variant"] styleAttr
                          classes     = fromMaybe [] $
                                          words <$> lookup "class" attr
  let tag = if isSmallCaps then B.smallcaps else B.spanWith (mkAttr attr)
  return $ tag contents

pRawHtmlInline :: PandocMonad m => TagParser m Inlines
pRawHtmlInline = do
  inplain <- asks inPlain
  result <- pSatisfy (tagComment (const True))
            <|> if inplain
                   then pSatisfy (not . isBlockTag)
                   else pSatisfy isInlineTag
  exts <- getOption readerExtensions
  let raw = T.unpack $ renderTags' [result]
  if extensionEnabled Ext_raw_html exts
     then return $ B.rawInline "html" raw
     else ignore raw

mathMLToTeXMath :: String -> Either String String
mathMLToTeXMath s = writeTeX <$> readMathML s

toStringAttr :: [(Text, Text)] -> [(String, String)]
toStringAttr = map go
  where go (x,y) = (T.unpack x, T.unpack y)

pMath :: PandocMonad m => Bool -> TagParser m Inlines
pMath inCase = try $ do
  open@(TagOpen _ attr') <- pSatisfy $ tagOpen (=="math") (const True)
  -- we'll assume math tags are MathML unless specially marked
  -- otherwise...
  let attr = toStringAttr attr'
  unless inCase $
    guard (maybe True (== mathMLNamespace) (lookup "xmlns" attr))
  contents <- manyTill pAnyTag (pSatisfy (matchTagClose "math"))
  case mathMLToTeXMath (T.unpack $ renderTags $
          [open] <> contents <> [TagClose "math"]) of
       Left _   -> return $ B.spanWith ("",["math"],attr) $ B.text $
                             T.unpack $ innerText contents
       Right [] -> return mempty
       Right x  -> return $ case lookup "display" attr of
                                 Just "block" -> B.displayMath x
                                 _            -> B.math x

pInlinesInTags :: PandocMonad m => Text -> (Inlines -> Inlines)
               -> TagParser m Inlines
pInlinesInTags tagtype f = extractSpaces f <$> pInTags tagtype inline

pInTags :: (PandocMonad m, Monoid a) => Text -> TagParser m a -> TagParser m a
pInTags tagtype parser = pInTags' tagtype (const True) parser

pInTags' :: (PandocMonad m, Monoid a)
         => Text
         -> (Tag Text -> Bool)
         -> TagParser m a
         -> TagParser m a
pInTags' tagtype tagtest parser = try $ do
  pSatisfy (\t -> t ~== TagOpen tagtype [] && tagtest t)
  mconcat <$> manyTill parser (pCloses tagtype <|> eof)

-- parses p, preceeded by an optional opening tag
-- and followed by an optional closing tags
pOptInTag :: PandocMonad m => Text -> TagParser m a -> TagParser m a
pOptInTag tagtype p = try $ do
  skipMany pBlank
  optional $ pSatisfy (matchTagOpen tagtype [])
  skipMany pBlank
  x <- p
  skipMany pBlank
  optional $ pSatisfy (matchTagClose tagtype)
  skipMany pBlank
  return x

pCloses :: PandocMonad m => Text -> TagParser m ()
pCloses tagtype = try $ do
  t <- lookAhead $ pSatisfy $ \tag -> isTagClose tag || isTagOpen tag
  case t of
       (TagClose t') | t' == tagtype -> pAnyTag >> return ()
       (TagOpen t' _) | t' `closes` tagtype -> return ()
       (TagClose "ul") | tagtype == "li" -> return ()
       (TagClose "ol") | tagtype == "li" -> return ()
       (TagClose "dl") | tagtype == "dd" -> return ()
       (TagClose "table") | tagtype == "td" -> return ()
       (TagClose "table") | tagtype == "tr" -> return ()
       (TagClose t') | tagtype == "p" && t' `Set.member` blockHtmlTags
                                            -> return () -- see #3794
       _ -> mzero

pTagText :: PandocMonad m => TagParser m Inlines
pTagText = try $ do
  (TagText str) <- pSatisfy isTagText
  st <- getState
  qu <- ask
  parsed <- lift $ lift $
            flip runReaderT qu $ runParserT (many pTagContents) st "text" str
  case parsed of
       Left _        -> throwError $ PandocParseError $ "Could not parse `" <> T.unpack str <> "'"
       Right result  -> return $ mconcat result

pBlank :: PandocMonad m => TagParser m ()
pBlank = try $ do
  (TagText str) <- pSatisfy isTagText
  guard $ T.all isSpace str

type InlinesParser m = HTMLParser m Text

pTagContents :: PandocMonad m => InlinesParser m Inlines
pTagContents =
      B.displayMath <$> mathDisplay
  <|> B.math        <$> mathInline
  <|> pStr
  <|> pSpace
  <|> smartPunctuation pTagContents
  <|> pSymbol
  <|> pBad

pStr :: PandocMonad m => InlinesParser m Inlines
pStr = do
  result <- many1 $ satisfy $ \c ->
                     not (isSpace c) && not (isSpecial c) && not (isBad c)
  updateLastStrPos
  return $ B.str result

isSpecial :: Char -> Bool
isSpecial '"' = True
isSpecial '\'' = True
isSpecial '.' = True
isSpecial '-' = True
isSpecial '$' = True
isSpecial '\8216' = True
isSpecial '\8217' = True
isSpecial '\8220' = True
isSpecial '\8221' = True
isSpecial _ = False

pSymbol :: PandocMonad m => InlinesParser m Inlines
pSymbol = satisfy isSpecial >>= return . B.str . (:[])

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
  return $ B.str [c']

pSpace :: PandocMonad m => InlinesParser m Inlines
pSpace = many1 (satisfy isSpace) >>= \xs ->
            if '\n' `elem` xs
               then return B.softbreak
               else return B.space

--
-- Constants
--

eitherBlockOrInline :: Set.Set Text
eitherBlockOrInline = Set.fromList
  ["audio", "applet", "button", "iframe", "embed",
   "del", "ins", "progress", "map", "area", "noscript", "script",
   "object", "svg", "video", "source"]

blockHtmlTags :: Set.Set Text
blockHtmlTags = Set.fromList
   ["?xml", "!DOCTYPE", "address", "article", "aside",
    "blockquote", "body", "canvas",
    "caption", "center", "col", "colgroup", "dd", "details",
    "dir", "div", "dl", "dt", "fieldset", "figcaption", "figure",
    "footer", "form", "h1", "h2", "h3", "h4",
    "h5", "h6", "head", "header", "hgroup", "hr", "html",
    "isindex", "menu", "noframes", "ol", "output", "p", "pre",
    "section", "table", "tbody", "textarea",
    "thead", "tfoot", "ul", "dd",
    "dt", "frameset", "li", "tbody", "td", "tfoot",
    "th", "thead", "tr", "script", "style"]

-- We want to allow raw docbook in markdown documents, so we
-- include docbook block tags here too.
blockDocBookTags :: Set.Set Text
blockDocBookTags = Set.fromList
   ["calloutlist", "bibliolist", "glosslist", "itemizedlist",
    "orderedlist", "segmentedlist", "simplelist",
    "variablelist", "caution", "important", "note", "tip",
    "warning", "address", "literallayout", "programlisting",
    "programlistingco", "screen", "screenco", "screenshot",
    "synopsis", "example", "informalexample", "figure",
    "informalfigure", "table", "informaltable", "para",
    "simpara", "formalpara", "equation", "informalequation",
    "figure", "screenshot", "mediaobject", "qandaset",
    "procedure", "task", "cmdsynopsis", "funcsynopsis",
    "classsynopsis", "blockquote", "epigraph", "msgset",
    "sidebar", "title"]

epubTags :: Set.Set Text
epubTags = Set.fromList ["case", "switch", "default"]

blockTags :: Set.Set Text
blockTags = Set.unions [blockHtmlTags, blockDocBookTags, epubTags]

class NamedTag a where
  getTagName :: a -> Maybe Text

instance NamedTag (Tag Text) where
  getTagName (TagOpen t _) = Just t
  getTagName (TagClose t)  = Just t
  getTagName _             = Nothing

instance NamedTag (Tag String) where
  getTagName (TagOpen t _) = Just (T.pack t)
  getTagName (TagClose t)  = Just (T.pack t)
  getTagName _             = Nothing

isInlineTag :: NamedTag (Tag a) => Tag a -> Bool
isInlineTag t = isInlineTagName || isCommentTag t
                 where isInlineTagName = case getTagName t of
                                              Just x -> x
                                                  `Set.notMember` blockTags
                                              Nothing -> False

isBlockTag :: NamedTag (Tag a) => Tag a -> Bool
isBlockTag t = isBlockTagName || isTagComment t
                 where isBlockTagName =
                         case getTagName t of
                              Just x
                                | "?" `T.isPrefixOf` x -> True
                                | "!" `T.isPrefixOf` x -> True
                                | otherwise -> x `Set.member` blockTags
                                    || x `Set.member` eitherBlockOrInline
                              Nothing -> False

isTextTag :: Tag a -> Bool
isTextTag = tagText (const True)

isCommentTag :: Tag a -> Bool
isCommentTag = tagComment (const True)

-- taken from HXT and extended
-- See http://www.w3.org/TR/html5/syntax.html sec 8.1.2.4 optional tags
closes :: Text -> Text -> Bool
_ `closes` "body" = False
_ `closes` "html" = False
"body" `closes` "head" = True
"a" `closes` "a" = True
"li" `closes` "li" = True
"th" `closes` t | t `elem` ["th","td"] = True
"tr" `closes` t | t `elem` ["th","td","tr"] = True
"dd" `closes` t | t `elem` ["dt", "dd"] = True
"dt" `closes` t | t `elem` ["dt","dd"] = True
"rt" `closes` t | t `elem` ["rb", "rt", "rtc"] = True
"optgroup" `closes` "optgroup" = True
"optgroup" `closes` "option" = True
"option" `closes` "option" = True
-- http://www.w3.org/TR/html-markup/p.html
x `closes` "p" | x `elem` ["address", "article", "aside", "blockquote",
   "dir", "div", "dl", "fieldset", "footer", "form", "h1", "h2", "h3", "h4",
   "h5", "h6", "header", "hr", "menu", "nav", "ol", "p", "pre", "section",
   "table", "ul"] = True
"meta" `closes` "meta" = True
"form" `closes` "form" = True
"label" `closes` "label" = True
"map" `closes` "map" = True
"object" `closes` "object" = True
_ `closes` t | t `elem` ["option","style","script","textarea","title"] = True
t `closes` "select" | t /= "option" = True
"thead" `closes` t | t `elem` ["colgroup"] = True
"tfoot" `closes` t | t `elem` ["thead","colgroup"] = True
"tbody" `closes` t | t `elem` ["tbody","tfoot","thead","colgroup"] = True
t `closes` t2 |
   t `elem` ["h1","h2","h3","h4","h5","h6","dl","ol","ul","table","div","p"] &&
   t2 `elem` ["h1","h2","h3","h4","h5","h6","p" ] = True -- not "div"
t1 `closes` t2 |
   t1 `Set.member` blockTags &&
   t2 `Set.notMember` blockTags &&
   t2 `Set.notMember` eitherBlockOrInline = True
_ `closes` _ = False

--- parsers for use in markdown, textile readers

-- | Matches a stretch of HTML in balanced tags.
htmlInBalanced :: (Monad m)
               => (Tag String -> Bool)
               -> ParserT String st m String
htmlInBalanced f = try $ do
  lookAhead (char '<')
  inp <- getInput
  let ts = canonicalizeTags $
        parseTagsOptions parseOptions{ optTagWarning = True,
                                       optTagPosition = True } inp
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
                          lscontents <- unlines <$> count ls anyLine
                          cscontents <- count cs anyChar
                          closetag <- do
                            x <- many (satisfy (/='>'))
                            char '>'
                            return (x <> ">")
                          return (lscontents <> cscontents <> closetag)
                        _ -> mzero
    _ -> mzero

htmlInBalanced' :: String
                -> [Tag String]
                -> [Tag String]
htmlInBalanced' tagname ts = fromMaybe [] $ go 0 ts
  where go :: Int -> [Tag String] -> Maybe [Tag String]
        go n (t@(TagOpen tn' _):rest) | tn' == tagname =
              (t :) <$> go (n + 1) rest
        go 1 (t@(TagClose tn'):_) | tn' == tagname =
              return [t]
        go n (t@(TagClose tn'):rest)  | tn' == tagname =
              (t :) <$> go (n - 1) rest
        go n (t:ts') = (t :) <$> go n ts'
        go _ [] = mzero

hasTagWarning :: [Tag a] -> Bool
hasTagWarning (TagWarning _:_) = True
hasTagWarning _ = False

-- | Matches a tag meeting a certain condition.
htmlTag :: Monad m
        => (Tag String -> Bool)
        -> ParserT [Char] st m (Tag String, String)
htmlTag f = try $ do
  lookAhead (char '<')
  inp <- getInput
  let (next : _) = canonicalizeTags $ parseTagsOptions
                       parseOptions{ optTagWarning = False } inp
  guard $ f next

  -- <www.boe.es/buscar/act.php?id=BOE-A-1996-8930#a66>
  -- should NOT be parsed as an HTML tag, see #2277,
  -- so we exclude . even though it's a valid character
  -- in XML elemnet names
  let isNameChar c = isAlphaNum c || c == ':' || c == '-' || c == '_'
  let isName s = case s of
                      [] -> False
                      (c:cs) -> isLetter c && all isNameChar cs

  let handleTag tagname = do
       -- basic sanity check, since the parser is very forgiving
       -- and finds tags in stuff like x<y)
       guard $ isName tagname
       guard $ not $ null tagname
       -- <https://example.org> should NOT be a tag either.
       -- tagsoup will parse it as TagOpen "https:" [("example.org","")]
       guard $ last tagname /= ':'
       rendered <- manyTill anyChar (char '>')
       return (next, rendered <> ">")
  case next of
       TagComment s
         | "<!--" `isPrefixOf` inp -> do
          count (length s + 4) anyChar
          skipMany (satisfy (/='>'))
          char '>'
          return (next, "<!--" <> s <> "-->")
         | otherwise -> fail "bogus comment mode, HTML5 parse error"
       TagOpen tagname attr -> do
         guard $ all (isName . fst) attr
         handleTag tagname
       TagClose tagname ->
         handleTag tagname
       _ -> mzero

mkAttr :: [(String, String)] -> Attr
mkAttr attr = (attribsId, attribsClasses, attribsKV)
  where attribsId = fromMaybe "" $ lookup "id" attr
        attribsClasses = (words $ fromMaybe "" $ lookup "class" attr) <> epubTypes
        attribsKV = filter (\(k,_) -> k /= "class" && k /= "id") attr
        epubTypes = words $ fromMaybe "" $ lookup "epub:type" attr

-- Strip namespace prefixes
stripPrefixes :: [Tag Text] -> [Tag Text]
stripPrefixes = map stripPrefix

stripPrefix :: Tag Text -> Tag Text
stripPrefix (TagOpen s as) =
    TagOpen (stripPrefix' s) (map (stripPrefix' *** id) as)
stripPrefix (TagClose s) = TagClose (stripPrefix' s)
stripPrefix x = x

stripPrefix' :: Text -> Text
stripPrefix' s =
  if T.null t then s else T.drop 1 t
  where (_, t) = T.span (/= ':') s

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace '\r' = True
isSpace _    = False

-- Instances

instance HasIdentifierList HTMLState where
  extractIdentifierList = identifiers
  updateIdentifierList f s = s{ identifiers = f (identifiers s) }

instance HasHeaderMap HTMLState where
  extractHeaderMap = headerMap
  updateHeaderMap  f s = s{ headerMap = f (headerMap s) }

instance HasLogMessages HTMLState where
  addLogMessage m s = s{ logMessages = m : logMessages s }
  getLogMessages = reverse . logMessages

-- This signature should be more general
-- MonadReader HTMLLocal m => HasQuoteContext st m
instance PandocMonad m => HasQuoteContext HTMLState (ReaderT HTMLLocal m) where
  getQuoteContext = asks quoteContext
  withQuoteContext q = local (\s -> s{quoteContext = q})

instance HasReaderOptions HTMLState where
    extractReaderOptions = extractReaderOptions . parserState

instance HasMeta HTMLState where
  setMeta s b st = st {parserState = setMeta s b $ parserState st}
  deleteMeta s st = st {parserState = deleteMeta s $ parserState st}

instance Default HTMLLocal where
  def = HTMLLocal NoQuote False False

instance HasLastStrPosition HTMLState where
  setLastStrPos s st = st {parserState = setLastStrPos s (parserState st)}
  getLastStrPos = getLastStrPos . parserState

-- For now we need a special verison here; the one in Shared has String type
renderTags' :: [Tag Text] -> Text
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = matchTags ["hr", "br", "img",
                                                       "meta", "link"]
                            , optRawTag   = matchTags ["script", "style"] }
              where matchTags = \tags -> flip elem tags . T.toLower


-- EPUB Specific
--
--
sectioningContent :: [Text]
sectioningContent = ["article", "aside", "nav", "section"]


groupingContent :: [Text]
groupingContent = ["p", "hr", "pre", "blockquote", "ol"
                  , "ul", "li", "dl", "dt", "dt", "dd"
                  , "figure", "figcaption", "div", "main"]

matchTagClose :: Text -> (Tag Text -> Bool)
matchTagClose t = (~== TagClose t)

matchTagOpen :: Text -> [(Text, Text)] -> (Tag Text -> Bool)
matchTagOpen t as = (~== TagOpen t as)

{-

types :: [(String, ([String], Int))]
types =  -- Document divisions
   map (\s -> (s, (["section", "body"], 0)))
    ["volume", "part", "chapter", "division"]
  <> -- Document section and components
  [
    ("abstract",  ([], 0))]
-}

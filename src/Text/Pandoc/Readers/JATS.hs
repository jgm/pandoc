{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
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
   Module      : Text.Pandoc.Readers.JATS
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of JATS to 'Pandoc' document.
-}
module Text.Pandoc.Readers.JATS ( readJATS
                                ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (guard, mplus, unless, void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask, asks, lift, local, runReaderT)
import Data.Char (isDigit)
import Data.Default (Default (..), def)
import Data.Foldable (for_)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import Data.Maybe (maybeToList, fromMaybe, isJust)
import Data.Monoid (First (..), (<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI, nonStrictRelativeTo, parseURIReference)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Pandoc.Builder (Blocks, HasMeta (..), Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options (Extension (Ext_epub_html_exts, Ext_raw_html),
                            ReaderOptions (readerExtensions),
                            extensionEnabled)
import Text.Pandoc.Parsing hiding ((<|>))
import Text.Pandoc.Shared (addMetaField, crFilter, escapeURI, extractSpaces,
                           safeRead, underlineSpan)
import Text.Pandoc.Walk
import Text.Parsec.Error
import Text.TeXMath (readMathML, writeTeX)

-- | Convert JATS-formatted string to 'Pandoc' document.
readJATS :: PandocMonad m
         => ReaderOptions -- ^ Reader options
         -> Text        -- ^ String to parse (assumes @'\n'@ line endings)
         -> m Pandoc
readJATS opts inp = do
  let tags = stripPrefixes . canonicalizeTags $
             parseTagsOptions parseOptions{ optTagPosition = True }
             (crFilter inp)
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
       (JATSState def{ stateOptions = opts } [] Nothing Set.empty M.empty [])
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

data JATSState =
  JATSState
  {  parserState :: ParserState,
     noteTable   :: [(String, Blocks)],
     baseHref    :: Maybe URI,
     identifiers :: Set.Set String,
     headerMap   :: M.Map Inlines String,
     logMessages :: [LogMessage]
  }

data JATSLocal = JATSLocal { quoteContext :: QuoteContext
                           , inPlain      :: Bool -- ^ Set if in pPlain
                           , secLevel     :: Int
                           , secId        :: String
                           , inCaption    :: Bool
                           }

setInPlain :: PandocMonad m => JATSParser m s a -> JATSParser m s a
setInPlain = local (\s -> s {inPlain = True})

setSec :: PandocMonad m => String -> JATSParser m s a -> JATSParser m s a
setSec i = local (\s -> s {secLevel = succ (secLevel s), secId = i})

setInCaption :: PandocMonad m => JATSParser m s a -> JATSParser m s a
setInCaption = local (\s -> s {inCaption = True})

type JATSParser m s = ParserT s JATSState (ReaderT JATSLocal m)

type TagParser m = JATSParser m [Tag Text]

pArticle :: PandocMonad m => TagParser m Blocks
pArticle = try $ do
  (TagOpen "article" attr) <- lookAhead pAnyTag
  for_ (lookup "lang" attr) $
    updateState . B.setMeta "lang" . B.text . T.unpack
  pInTags "article" block

pBody :: PandocMonad m => TagParser m Blocks
pBody = pInTags "body" block

pFront :: PandocMonad m => TagParser m Blocks
pFront = pInTags "front" $ pArticleMeta <|> (mempty <$ pAnyTag)

pBack :: PandocMonad m => TagParser m Blocks
pBack = pInTags "back" block

pArticleMeta :: PandocMonad m => TagParser m Blocks
pArticleMeta = pInTags "article-meta" $ pAbstract <|> pTransAbstract <|> pOtherMeta <|> (mempty <$ pAnyTag)
  where pAbstract = pInTags "abstract" block >>= setAbstract
        setAbstract a = mempty <$ updateState (B.setMeta "abstract" a)
        pTransAbstract = pInTags "trans-abstract" block >>= setTransAbstract
        setTransAbstract a = mempty <$ updateState (B.setMeta "trans-abstract" a)
        pOtherMeta = do
          (TagOpen name _) <- pSatisfy $ tagOpen (const True) (const True)
          content <- manyTill block (pCloses name)
          updateState $ \s ->
            let ps = parserState s in
            s{ parserState = ps{
               stateMeta = addMetaField (T.unpack name) content
                           (stateMeta ps) } }
          return mempty

block :: PandocMonad m => TagParser m Blocks
block = do
  res <- choice
            [ eSwitch B.para block
            , mempty <$ eFootnote
            , mempty <$ eTOC
            , pSec
            , pFig
            , pTableWrap
            , pCaption
            , pPara
            , pTitle
            , pBlockQuote
            , pCodeBlock
            , pList
            , pHrule
            , pTable
            , pArticle
            , pFront
            , pBody
            , pBack
            , pRefList
            , pPlain
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
  TagOpen _ attr' <- lookAhead $ pSatisfy $ matchTagOpen "case" []
  let attr = toStringAttr attr'
  case flip lookup namespaces =<< lookup "required-namespace" attr of
    Just p -> Just <$> pInTags "case" (skipMany pBlank *> p <* skipMany pBlank)
    Nothing -> Nothing <$ manyTill pAnyTag (pSatisfy (matchTagClose "case"))

eFootnote :: PandocMonad m => TagParser m ()
eFootnote = try $ do
  let notes = ["footnote", "rearnote"]
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr') <- lookAhead pAnyTag
  let attr = toStringAttr attr'
  guard (maybe False (`elem` notes) (lookup "type" attr))
  let ident = fromMaybe "" (lookup "id" attr)
  content <- pInTags tag block
  addNote ident content

addNote :: PandocMonad m => String -> Blocks -> TagParser m ()
addNote uid cont = updateState (\s -> s {noteTable = (uid, cont) : noteTable s})

eNoteref :: PandocMonad m => TagParser m Inlines
eNoteref = try $ do
  guardEnabled Ext_epub_html_exts
  TagOpen tag attr' <- lookAhead pAnyTag
  let attr = toStringAttr attr'
  guard ((== Just "noteref") (lookup "type" attr))
  let ident = maybe "" (dropWhile (== '#')) (lookup "href" attr)
  guard (not (null ident))
  pInTags tag block
  return $ B.rawInline "noteref" ident

-- Strip TOC if there is one, better to generate again
eTOC :: PandocMonad m => TagParser m ()
eTOC = try $ do
  guardEnabled Ext_epub_html_exts
  (TagOpen tag attr) <- lookAhead pAnyTag
  guard ((== Just "toc") (lookup "type" attr))
  void (pInTags tag block)

pList :: PandocMonad m => TagParser m Blocks
pList = pBulletList <|> pOrderedList <|> pDefinitionList

pNonListItem :: PandocMonad m => TagParser m (Tag Text)
pNonListItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["list-item","list","def-list"]) (const True) t) &&
                  not (matchTagClose "list" t))

pBulletList :: PandocMonad m => TagParser m Blocks
pBulletList = try $ do
  pSatisfy (matchTagOpen "list" [("list-type", "bullet")])
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany pNonListItem
  items <- manyTill (pListItem pNonListItem) (pCloses "list")
  return $ B.bulletList $ map (fixPlains True) items

pListItem :: PandocMonad m => TagParser m a -> TagParser m Blocks
pListItem nonItem = do
  TagOpen _ attr' <- lookAhead $ pSatisfy (matchTagOpen "list-item" [])
  let attr = toStringAttr attr'
  let addId ident bs = case B.toList bs of
                           (Plain ils:xs) -> B.fromList (Plain
                                [Span (ident, [], []) ils] : xs)
                           _ -> B.divWith (ident, [], []) bs
  maybe id addId (lookup "id" attr) <$>
    pInTags "list-item" block <* skipMany nonItem

parseListStyleType :: String -> ListNumberStyle
parseListStyleType "roman-lower" = LowerRoman
parseListStyleType "roman-upper" = UpperRoman
parseListStyleType "alpha-lower" = LowerAlpha
parseListStyleType "alpha-upper" = UpperAlpha
parseListStyleType _             = DefaultStyle

pOrderedList :: PandocMonad m => TagParser m Blocks
pOrderedList = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "list" [])
  let attribs = toStringAttr attribs'
  let (start, style) = (sta', sty')
                       where sta = fromMaybe "1" $
                                   lookup "start" attribs
                             sta' = if all isDigit sta
                                       then read sta
                                       else 1

                             typeAttr  = fromMaybe "" $ lookup "list-type"  attribs

                             sty' = parseListStyleType typeAttr
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany pNonListItem
  items <- manyTill (pListItem pNonListItem) (pCloses "list")
  return $ B.orderedListWith (start, style, DefaultDelim) $ map (fixPlains True) items

pDefinitionList :: PandocMonad m => TagParser m Blocks
pDefinitionList = try $ do
  pSatisfy (matchTagOpen "def-list" [])
  items <- manyTill pDefListItem (pCloses "def-list")
  return $ B.definitionList items

pDefListItem :: PandocMonad m => TagParser m (Inlines, [Blocks])
pDefListItem = try $ do
  let nonItem = pSatisfy (\t -> not (matchTagOpen "term" [] t) &&
                  not (matchTagOpen "def" [] t) && not (matchTagClose "def-list" t))
  terms <- many1 (try $ skipMany nonItem >> trimInlines <$> pInTags "term" inline)
  defs  <- many1 (try $ skipMany nonItem >> pInTags "def" block)
  skipMany nonItem
  let term = foldl1 (\x y ->  x <> B.linebreak <> y) terms
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
  tag <- pAnyTag
  let ignorable x = x `elem` ["html","head","body","!DOCTYPE","?xml"]
  if tagOpen ignorable (const True) tag || tagClose ignorable tag
     then return mempty
     else return $ renderTags' [tag]

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

pSec :: PandocMonad m => TagParser m Blocks
pSec = do
  TagOpen _ attr' <- pSatisfy $ matchTagOpen "sec" []
  let attr = toStringAttr attr'
  let ident = fromMaybe "" (lookup "id" attr)
  setSec ident $ mconcat <$> manyTill block (pCloses "sec")

pAsDiv :: PandocMonad m => Text -> TagParser m Blocks
pAsDiv tagType = do
  TagOpen _ attr' <- pSatisfy $ matchTagOpen tagType []
  let attr = toStringAttr attr'
  let ident = fromMaybe "" (lookup "id" attr)
  B.singleton . Div (ident, [T.unpack tagType], []) . B.toList . mconcat <$> manyTill block (pCloses tagType)

pFig :: PandocMonad m => TagParser m Blocks
pFig = pAsDiv "fig"

pTableWrap :: PandocMonad m => TagParser m Blocks
pTableWrap = pAsDiv "table-wrap"

pCaption :: PandocMonad m => TagParser m Blocks
pCaption = setInCaption $ pAsDiv "caption"

pRefList :: PandocMonad m => TagParser m Blocks
pRefList = B.singleton . Div ("refs", [], []) . B.toList <$> pInTags "ref-list" block

headingLevel :: JATSLocal -> Int
headingLevel s = if inCaption s then 6 else min 6 (secLevel s)

pTitle :: PandocMonad m => TagParser m Blocks
pTitle = try $ do
  TagOpen tagtype attr' <- pSatisfy $
                           tagOpen (`elem` ["title"])
                           (const True)
  let attr = toStringAttr attr'
  level <- asks headingLevel
  contents <- trimInlines . mconcat <$> manyTill inline (pCloses tagtype <|> eof)
  let ident = fromMaybe "" $ lookup "id" attr
  let classes = maybe [] words $ lookup "class" attr
  let keyvals = [(k,v) | (k,v) <- attr, k /= "class", k /= "id"]
  attr'' <- registerHeader (ident, classes, keyvals) contents
  return $ B.headerWith attr'' level contents

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
      pTr = try $ skipMany pBlank >>
                  pInTags "tr" (pCell "td" <|> pCell "th")
      pTBody = pOptInTag "tbody" $ many1 pTr
  head'' <- pOptInTag "thead" pTh
  head'  <- map snd <$>
             pOptInTag "tbody" (
               if null head'' then pTh else return head'')
  rowsLs <- many pTBody
  rows'  <- pOptInTag "tfoot" $ many pTr
  TagClose _ <- pSatisfy (matchTagClose "table")
  let rows'' = concat rowsLs <> rows'
  let rows''' = map (map snd) rows''
  -- let rows''' = map (map snd) rows''
  -- fail on empty table
  guard $ not $ null head' && null rows'''
  let isSinglePlain x = case B.toList x of
                             []        -> True
                             [Plain _] -> True
                             _         -> False
  let isSimple = all isSinglePlain $ concat (head':rows''')
  let cols = length $ if null head' then head rows''' else head'
  -- add empty cells to short rows
  let addEmpties r = case cols - length r of
                           n | n > 0 -> r <> replicate n mempty
                             | otherwise -> r
  let rows = map addEmpties rows'''
  let aligns = case rows'' of
                    (cs:_) -> map fst cs
                    _      -> replicate cols AlignDefault
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
                   (`notElem` (" \t\r\n%'\";" :: String)) xs)
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

pCell :: PandocMonad m => Text -> TagParser m [(Alignment, Blocks)]
pCell celltype = try $ do
  skipMany pBlank
  tag <- lookAhead $
           pSatisfy (\t -> t ~== TagOpen celltype [] && noColOrRowSpans t)
  let extractAlign' []                 = ""
      extractAlign' ("text-align":x:_) = x
      extractAlign' (_:xs)             = extractAlign' xs
  let extractAlign = extractAlign' . wordsBy (`elem` [' ','\t',';',':'])
  let align = case maybeFromAttrib "align" tag `mplus`
                   (extractAlign <$> maybeFromAttrib "style" tag) of
                   Just "left"   -> AlignLeft
                   Just "right"  -> AlignRight
                   Just "center" -> AlignCenter
                   _             -> AlignDefault
  res <- pInTags' celltype noColOrRowSpans block
  skipMany pBlank
  return [(align, res)]

pBlockQuote :: PandocMonad m => TagParser m Blocks
pBlockQuote = do
  contents <- pInTags "disp-quote" block
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

pCodeBlock :: PandocMonad m => TagParser m Blocks
pCodeBlock = try $ do
  TagOpen _ attr' <- pSatisfy (matchTagOpen "preformat" [])
  let attr = toStringAttr attr'
  contents <- manyTill pAnyTag (pCloses "preformat" <|> eof)
  let rawText = concatMap tagToString contents
  -- drop leading newline if any
  let result' = case rawText of
                     '\n':xs -> xs
                     _       -> rawText
  -- drop trailing newline if any
  let result = case reverse result' of
                    '\n':_ -> init result'
                    _      -> result'
  return $ B.codeBlockWith (mkAttr attr) result

tagToString :: Tag Text -> String
tagToString (TagText s)      = T.unpack s
tagToString (TagOpen "break" _) = "\n"
tagToString _                = ""

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
           , pUnderline
           , pLineBreak
           , pLink
           , pXRef
           , pImage
           , pCode
           , pSmallCaps
           , pMath False
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
pEmph = pInlinesInTags "italic" B.emph

pStrong :: PandocMonad m => TagParser m Inlines
pStrong = pInlinesInTags "bold" B.strong

pSuperscript :: PandocMonad m => TagParser m Inlines
pSuperscript = pInlinesInTags "sup" B.superscript

pSubscript :: PandocMonad m => TagParser m Inlines
pSubscript = pInlinesInTags "sub" B.subscript

pStrikeout :: PandocMonad m => TagParser m Inlines
pStrikeout = pInlinesInTags "strike" B.strikeout

pUnderline :: PandocMonad m => TagParser m Inlines
pUnderline = pInlinesInTags "underline" underlineSpan

pLineBreak :: PandocMonad m => TagParser m Inlines
pLineBreak = do
  pSelfClosing (=="break") (const True)
  return B.linebreak

-- Unlike fromAttrib from tagsoup, this distinguishes
-- between a missing attribute and an attribute with empty content.
maybeFromAttrib :: String -> Tag Text -> Maybe String
maybeFromAttrib name (TagOpen _ attrs) =
  T.unpack <$> lookup (T.pack name) attrs
maybeFromAttrib _ _ = Nothing

pLink :: PandocMonad m => TagParser m Inlines
pLink = try $ do
  tag <- pSatisfy $ tagOpenLit "ext-link" (const True)
  let title = T.unpack $ fromAttrib "title" tag
  -- take id from id attribute if present, otherwise name
  let uid = fromMaybe (T.unpack $ fromAttrib "name" tag) $
               maybeFromAttrib "id" tag
  let cls = words $ T.unpack $ fromAttrib "class" tag
  lab <- trimInlines . mconcat <$> manyTill inline (pCloses "ext-link")
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

pXRef :: PandocMonad m => TagParser m Inlines
pXRef = try $ do
  tag <- pSatisfy $ tagOpenLit "xref" (const True)
  let title = T.unpack $ fromAttrib "title" tag
  -- take id from id attribute if present, otherwise name
  let uid = fromMaybe (T.unpack $ fromAttrib "name" tag) $
               maybeFromAttrib "id" tag
  let cls = words $ T.unpack $ fromAttrib "class" tag
  let refType = maybeToList $ ("ref-type",) <$> maybeFromAttrib "ref-type" tag
  lab <- trimInlines . mconcat <$> manyTill inline (pCloses "xref")
  -- check for href; if href, then a link, otherwise a span
  case maybeFromAttrib "rid" tag of
       Nothing   ->
         return $ B.spanWith (uid, cls, refType) lab
       Just rid ->
         return $ B.linkWith (uid, cls, refType) (escapeURI ("#" <> rid)) title lab

pImage :: PandocMonad m => TagParser m Inlines
pImage = try $ do
  alt <- pInlinesInTags "alt-text" id <|> return mempty
  tag <- pSelfClosing (`elem` ["graphic", "inline-graphic"]) (isJust . lookup "href")
  mbBaseHref <- baseHref <$> getState
  let url' = T.unpack $ fromAttrib "href" tag
  let url = case (parseURIReference url', mbBaseHref) of
                 (Just rel, Just bs) -> show (rel `nonStrictRelativeTo` bs)
                 _                   -> url'
  let title = T.unpack $ fromAttrib "title" tag
  let uid = T.unpack $ fromAttrib "id" tag
  let cls = words $ T.unpack $ fromAttrib "class" tag
  let getAtt k = case fromAttrib k tag of
                   "" -> []
                   v  -> [(T.unpack k, T.unpack v)]
  let kvs = concatMap getAtt ["width", "height", "sizes", "srcset"]
  return $ B.imageWith (uid, cls, kvs) (escapeURI url) title alt

pCode :: PandocMonad m => TagParser m Inlines
pCode = try $ do
  (TagOpen open attr') <- pSatisfy $ tagOpen (`elem` ["monospace"]) (const True)
  let attr = toStringAttr attr'
  result <- manyTill pAnyTag (pCloses open)
  return $ B.codeWith (mkAttr attr) $ unwords $ lines $ T.unpack $
           innerText result

pSmallCaps :: PandocMonad m => TagParser m Inlines
pSmallCaps = pInlinesInTags "sc" B.smallcaps

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
pInTags tagtype = pInTags' tagtype (const True)

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
pCloses = void . pSatisfy . matchTagClose

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

type InlinesParser m = JATSParser m Text

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
isSpecial '"'     = True
isSpecial '\''    = True
isSpecial '.'     = True
isSpecial '-'     = True
isSpecial '$'     = True
isSpecial '\8216' = True
isSpecial '\8217' = True
isSpecial '\8220' = True
isSpecial '\8221' = True
isSpecial _       = False

pSymbol :: PandocMonad m => InlinesParser m Inlines
pSymbol = (B.str . (: [])) <$> satisfy isSpecial

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

mkAttr :: [(String, String)] -> Attr
mkAttr attr = (attribsId, attribsClasses, attribsKV)
  where attribsId = fromMaybe "" $ lookup "id" attr
        attribsClasses = words (fromMaybe "" $ lookup "class" attr) <> epubTypes
        attribsKV = filter (\(k,_) -> k /= "class" && k /= "id") attr
        epubTypes = words $ fromMaybe "" $ lookup "epub:type" attr

-- Strip namespace prefixes
stripPrefixes :: [Tag Text] -> [Tag Text]
stripPrefixes = map stripPrefix

stripPrefix :: Tag Text -> Tag Text
stripPrefix (TagOpen s as) =
    TagOpen (stripPrefix' s) (map (first stripPrefix') as)
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

instance HasIdentifierList JATSState where
  extractIdentifierList = identifiers
  updateIdentifierList f s = s{ identifiers = f (identifiers s) }

instance HasHeaderMap JATSState where
  extractHeaderMap = headerMap
  updateHeaderMap  f s = s{ headerMap = f (headerMap s) }

instance HasLogMessages JATSState where
  addLogMessage m s = s{ logMessages = m : logMessages s }
  getLogMessages = reverse . logMessages

-- This signature should be more general
-- MonadReader JATSLocal m => HasQuoteContext st m
instance PandocMonad m => HasQuoteContext JATSState (ReaderT JATSLocal m) where
  getQuoteContext = asks quoteContext
  withQuoteContext q = local (\s -> s{quoteContext = q})

instance HasReaderOptions JATSState where
    extractReaderOptions = extractReaderOptions . parserState

instance HasMeta JATSState where
  setMeta s b st = st {parserState = setMeta s b $ parserState st}
  deleteMeta s st = st {parserState = deleteMeta s $ parserState st}

instance Default JATSLocal where
  def = JATSLocal NoQuote False 0 "" False

instance HasLastStrPosition JATSState where
  setLastStrPos s st = st {parserState = setLastStrPos s (parserState st)}
  getLastStrPos = getLastStrPos . parserState

-- For now we need a special verison here; the one in Shared has String type
renderTags' :: [Tag Text] -> Text
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = matchTags ["hr", "break", "inline-graphic",
                                                       "meta", "link"]
                            , optRawTag   = matchTags ["script", "style"] }
              where matchTags tags = flip elem tags . T.toLower


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

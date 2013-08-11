{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
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

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( intercalate )
import Data.Char ( isDigit )
import Control.Monad ( liftM, guard, when, mzero )
import Control.Applicative ( (<$>), (<$), (<*) )

isSpace :: Char -> Bool
isSpace ' '  = True
isSpace '\t' = True
isSpace '\n' = True
isSpace _    = False

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: ReaderOptions -- ^ Reader options
         -> String        -- ^ String to parse (assumes @'\n'@ line endings)
         -> Pandoc
readHtml opts inp =
  case runParser parseDoc def{ stateOptions = opts } "source" tags of
          Left err'    -> error $ "\nError at " ++ show  err'
          Right result -> result
    where tags = canonicalizeTags $
                   parseTagsOptions parseOptions{ optTagPosition = True } inp
          parseDoc = do
             blocks <- (fixPlains False . concat) <$> manyTill block eof
             meta <- stateMeta <$> getState
             return $ Pandoc meta blocks

type TagParser = Parser [Tag String] ParserState

pBody :: TagParser [Block]
pBody = pInTags "body" block

pHead :: TagParser [Block]
pHead = pInTags "head" $ pTitle <|> ([] <$ pAnyTag)
  where pTitle = pInTags "title" inline >>= setTitle . normalizeSpaces
        setTitle t = [] <$ (updateState $ B.setMeta "title" (B.fromList t))

block :: TagParser [Block]
block = choice
            [ pPara
            , pHeader
            , pBlockQuote
            , pCodeBlock
            , pList
            , pHrule
            , pTable
            , pHead
            , pBody
            , pPlain
            , pRawHtmlBlock
            ]

pList :: TagParser [Block]
pList = pBulletList <|> pOrderedList <|> pDefinitionList

pBulletList :: TagParser [Block]
pBulletList = try $ do
  pSatisfy (~== TagOpen "ul" [])
  let nonItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["li","ol","ul","dl"]) (const True) t) &&
                  not (t ~== TagClose "ul"))
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany nonItem
  items <- manyTill (pInTags "li" block >>~ skipMany nonItem) (pCloses "ul")
  return [BulletList $ map (fixPlains True) items]

pOrderedList :: TagParser [Block]
pOrderedList = try $ do
  TagOpen _ attribs <- pSatisfy (~== TagOpen "ol" [])
  let (start, style) = (sta', sty')
                       where sta = fromMaybe "1" $
                                   lookup "start" attribs
                             sta' = if all isDigit sta
                                       then read sta
                                       else 1
                             sty = fromMaybe (fromMaybe "" $
                                   lookup "style" attribs) $
                                   lookup "class" attribs
                             sty' = case sty of
                                     "lower-roman"  -> LowerRoman
                                     "upper-roman"  -> UpperRoman
                                     "lower-alpha"  -> LowerAlpha
                                     "upper-alpha"  -> UpperAlpha
                                     "decimal"      -> Decimal
                                     _              -> DefaultStyle
  let nonItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["li","ol","ul","dl"]) (const True) t) &&
                  not (t ~== TagClose "ol"))
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany nonItem
  items <- manyTill (pInTags "li" block >>~ skipMany nonItem) (pCloses "ol")
  return [OrderedList (start, style, DefaultDelim) $ map (fixPlains True) items]

pDefinitionList :: TagParser [Block]
pDefinitionList = try $ do
  pSatisfy (~== TagOpen "dl" [])
  items <- manyTill pDefListItem (pCloses "dl")
  return [DefinitionList items]

pDefListItem :: TagParser ([Inline],[[Block]])
pDefListItem = try $ do
  let nonItem = pSatisfy (\t -> not (t ~== TagOpen "dt" []) &&
                  not (t ~== TagOpen "dd" []) && not (t ~== TagClose "dl"))
  terms <- many1 (try $ skipMany nonItem >> pInTags "dt" inline)
  defs  <- many1 (try $ skipMany nonItem >> pInTags "dd" block)
  skipMany nonItem
  let term = intercalate [LineBreak] terms
  return (term, map (fixPlains True) defs)

fixPlains :: Bool -> [Block] -> [Block]
fixPlains inList bs = if any isParaish bs
                         then map plainToPara bs
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

pRawTag :: TagParser String
pRawTag = do
  tag <- pAnyTag
  let ignorable x = x `elem` ["html","head","body"]
  if tagOpen ignorable (const True) tag || tagClose ignorable tag
     then return []
     else return $ renderTags' [tag]

pRawHtmlBlock :: TagParser [Block]
pRawHtmlBlock = do
  raw <- pHtmlBlock "script" <|> pHtmlBlock "style" <|> pRawTag
  parseRaw <- getOption readerParseRaw
  if parseRaw && not (null raw)
     then return [RawBlock (Format "html") raw]
     else return []

pHtmlBlock :: String -> TagParser String
pHtmlBlock t = try $ do
  open <- pSatisfy (~== TagOpen t [])
  contents <- manyTill pAnyTag (pSatisfy (~== TagClose t))
  return $ renderTags' $ [open] ++ contents ++ [TagClose t]

pHeader :: TagParser [Block]
pHeader = try $ do
  TagOpen tagtype attr <- pSatisfy $
                           tagOpen (`elem` ["h1","h2","h3","h4","h5","h6"])
                           (const True)
  let bodyTitle = TagOpen tagtype attr ~== TagOpen "h1" [("class","title")]
  let level = read (drop 1 tagtype)
  contents <- liftM concat $ manyTill inline (pCloses tagtype <|> eof)
  let ident = maybe "" id $ lookup "id" attr
  let classes = maybe [] words $ lookup "class" attr
  let keyvals = [(k,v) | (k,v) <- attr, k /= "class", k /= "id"]
  return $ if bodyTitle
              then []  -- skip a representation of the title in the body
              else [Header level (ident, classes, keyvals) $
                    normalizeSpaces contents]

pHrule :: TagParser [Block]
pHrule = do
  pSelfClosing (=="hr") (const True)
  return [HorizontalRule]

pTable :: TagParser [Block]
pTable = try $ do
  TagOpen _ _ <- pSatisfy (~== TagOpen "table" [])
  skipMany pBlank
  caption <- option [] $ pInTags "caption" inline >>~ skipMany pBlank
  -- TODO actually read these and take width information from them
  widths' <- pColgroup <|> many pCol
  head' <- option [] $ pOptInTag "thead" $ pInTags "tr" (pCell "th")
  skipMany pBlank
  rows <- pOptInTag "tbody"
          $ many1 $ try $ skipMany pBlank >> pInTags "tr" (pCell "td")
  skipMany pBlank
  TagClose _ <- pSatisfy (~== TagClose "table")
  let isSinglePlain []        = True
      isSinglePlain [Plain _] = True
      isSinglePlain _         = False
  let isSimple = all isSinglePlain $ concat (head':rows)
  let cols = length $ if null head'
                         then head rows
                         else head'
  -- fail if there are colspans or rowspans
  guard $ all (\r -> length r == cols) rows
  let aligns = replicate cols AlignLeft
  let widths = if null widths'
                  then if isSimple
                       then replicate cols 0
                       else replicate cols (1.0 / fromIntegral cols)
                  else widths'
  return [Table caption aligns widths head' rows]

pCol :: TagParser Double
pCol = try $ do
  TagOpen _ attribs <- pSatisfy (~== TagOpen "col" [])
  optional $ pSatisfy (~== TagClose "col")
  skipMany pBlank
  return $ case lookup "width" attribs of
           Just x | not (null x) && last x == '%' ->
             maybe 0.0 id $ safeRead ('0':'.':init x)
           _ -> 0.0

pColgroup :: TagParser [Double]
pColgroup = try $ do
  pSatisfy (~== TagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

pCell :: String -> TagParser [TableCell]
pCell celltype = try $ do
  skipMany pBlank
  res <- pInTags celltype block
  skipMany pBlank
  return [res]

pBlockQuote :: TagParser [Block]
pBlockQuote = do
  contents <- pInTags "blockquote" block
  return [BlockQuote $ fixPlains False contents]

pPlain :: TagParser [Block]
pPlain = do
  contents <- liftM (normalizeSpaces . concat) $ many1 inline
  if null contents
     then return []
     else return [Plain contents]

pPara :: TagParser [Block]
pPara = do
  contents <- pInTags "p" inline
  return [Para $ normalizeSpaces contents]

pCodeBlock :: TagParser [Block]
pCodeBlock = try $ do
  TagOpen _ attr <- pSatisfy (~== TagOpen "pre" [])
  contents <- manyTill pAnyTag (pCloses "pre" <|> eof)
  let rawText = concatMap fromTagText $ filter isTagText contents
  -- drop leading newline if any
  let result' = case rawText of
                     '\n':xs  -> xs
                     _        -> rawText
  -- drop trailing newline if any
  let result = case reverse result' of
                    '\n':_   -> init result'
                    _        -> result'
  let attribsId = fromMaybe "" $ lookup "id" attr
  let attribsClasses = words $ fromMaybe "" $ lookup "class" attr
  let attribsKV = filter (\(k,_) -> k /= "class" && k /= "id") attr
  let attribs = (attribsId, attribsClasses, attribsKV)
  return [CodeBlock attribs result]

inline :: TagParser [Inline]
inline = choice
           [ pTagText
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
           , pRawHtmlInline
           ]

pLocation :: TagParser ()
pLocation = do
  (TagPosition r c) <- pSat isTagPosition
  setPosition $ newPos "input" r c

pSat :: (Tag String -> Bool) -> TagParser (Tag String)
pSat f = do
  pos <- getPosition
  token show (const pos) (\x -> if f x then Just x else Nothing)

pSatisfy :: (Tag String -> Bool) -> TagParser (Tag String)
pSatisfy f = try $ optional pLocation >> pSat f

pAnyTag :: TagParser (Tag String)
pAnyTag = pSatisfy (const True)

pSelfClosing :: (String -> Bool) -> ([Attribute String] -> Bool)
             -> TagParser (Tag String)
pSelfClosing f g = do
  open <- pSatisfy (tagOpen f g)
  optional $ pSatisfy (tagClose f)
  return open

pQ :: TagParser [Inline]
pQ = do
  quoteContext <- stateQuoteContext `fmap` getState
  let quoteType = case quoteContext of
                       InDoubleQuote -> SingleQuote
                       _             -> DoubleQuote
  let innerQuoteContext = if quoteType == SingleQuote
                             then InSingleQuote
                             else InDoubleQuote
  withQuoteContext innerQuoteContext $ pInlinesInTags "q" (Quoted quoteType)

pEmph :: TagParser [Inline]
pEmph = pInlinesInTags "em" Emph <|> pInlinesInTags "i" Emph

pStrong :: TagParser [Inline]
pStrong = pInlinesInTags "strong" Strong <|> pInlinesInTags "b" Strong

pSuperscript :: TagParser [Inline]
pSuperscript = pInlinesInTags "sup" Superscript

pSubscript :: TagParser [Inline]
pSubscript = pInlinesInTags "sub" Subscript

pStrikeout :: TagParser [Inline]
pStrikeout = do
  pInlinesInTags "s" Strikeout <|>
    pInlinesInTags "strike" Strikeout <|>
    pInlinesInTags "del" Strikeout <|>
    try (do pSatisfy (~== TagOpen "span" [("class","strikeout")])
            contents <- liftM concat $ manyTill inline (pCloses "span")
            return [Strikeout contents])

pLineBreak :: TagParser [Inline]
pLineBreak = do
  pSelfClosing (=="br") (const True)
  return [LineBreak]

pLink :: TagParser [Inline]
pLink = try $ do
  tag <- pSatisfy (tagOpenLit "a" (isJust . lookup "href"))
  let url = fromAttrib "href" tag
  let title = fromAttrib "title" tag
  lab <- liftM concat $ manyTill inline (pCloses "a")
  return [Link (normalizeSpaces lab) (escapeURI url, title)]

pImage :: TagParser [Inline]
pImage = do
  tag <- pSelfClosing (=="img") (isJust . lookup "src")
  let url = fromAttrib "src" tag
  let title = fromAttrib "title" tag
  let alt = fromAttrib "alt" tag
  return [Image (B.toList $ B.text alt) (escapeURI url, title)]

pCode :: TagParser [Inline]
pCode = try $ do
  (TagOpen open attr) <- pSatisfy $ tagOpen (`elem` ["code","tt"]) (const True)
  result <- manyTill pAnyTag (pCloses open)
  let ident = fromMaybe "" $ lookup "id" attr
  let classes = words $ fromMaybe [] $ lookup "class" attr
  let rest = filter (\(x,_) -> x /= "id" && x /= "class") attr
  return [Code (ident,classes,rest)
         $ intercalate " " $ lines $ innerText result]

pRawHtmlInline :: TagParser [Inline]
pRawHtmlInline = do
  result <- pSatisfy (tagComment (const True)) <|> pSatisfy isInlineTag
  parseRaw <- getOption readerParseRaw
  if parseRaw
     then return [RawInline (Format "html") $ renderTags' [result]]
     else return []

pInlinesInTags :: String -> ([Inline] -> Inline)
               -> TagParser [Inline]
pInlinesInTags tagtype f = do
  contents <- pInTags tagtype inline
  return [f $ normalizeSpaces contents]

pInTags :: String -> TagParser [a]
        -> TagParser [a]
pInTags tagtype parser = try $ do
  pSatisfy (~== TagOpen tagtype [])
  liftM concat $ manyTill parser (pCloses tagtype <|> eof)

pOptInTag :: String -> TagParser a
          -> TagParser a
pOptInTag tagtype parser = try $ do
  open <- option False (pSatisfy (~== TagOpen tagtype []) >> return True)
  skipMany pBlank
  x <- parser
  skipMany pBlank
  when open $ pCloses tagtype
  return x

pCloses :: String -> TagParser ()
pCloses tagtype = try $ do
  t <- lookAhead $ pSatisfy $ \tag -> isTagClose tag || isTagOpen tag
  case t of
       (TagClose t')  | t' == tagtype -> pAnyTag >> return ()
       (TagOpen t' _) | t' `closes` tagtype -> return ()
       (TagClose "ul") | tagtype == "li" -> return ()
       (TagClose "ol") | tagtype == "li" -> return ()
       (TagClose "dl") | tagtype == "li" -> return ()
       _ -> mzero

pTagText :: TagParser [Inline]
pTagText = try $ do
  (TagText str) <- pSatisfy isTagText
  st <- getState
  case runParser (many pTagContents) st "text" str of
       Left _        -> fail $ "Could not parse `" ++ str ++ "'"
       Right result  -> return result

pBlank :: TagParser ()
pBlank = try $ do
  (TagText str) <- pSatisfy isTagText
  guard $ all isSpace str

pTagContents :: Parser [Char] ParserState Inline
pTagContents =
  pStr <|> pSpace <|> smartPunctuation pTagContents <|> pSymbol <|> pBad

pStr :: Parser [Char] ParserState Inline
pStr = do
  result <- many1 $ satisfy $ \c ->
                     not (isSpace c) && not (isSpecial c) && not (isBad c)
  pos <- getPosition
  updateState $ \s -> s{ stateLastStrPos = Just pos }
  return $ Str result

isSpecial :: Char -> Bool
isSpecial '"' = True
isSpecial '\'' = True
isSpecial '.' = True
isSpecial '-' = True
isSpecial '\8216' = True
isSpecial '\8217' = True
isSpecial '\8220' = True
isSpecial '\8221' = True
isSpecial _ = False

pSymbol :: Parser [Char] ParserState Inline
pSymbol = satisfy isSpecial >>= return . Str . (:[])

isBad :: Char -> Bool
isBad c = c >= '\128' && c <= '\159' -- not allowed in HTML

pBad :: Parser [Char] ParserState Inline
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
  return $ Str [c']

pSpace :: Parser [Char] ParserState Inline
pSpace = many1 (satisfy isSpace) >> return Space

--
-- Constants
--

eitherBlockOrInline :: [String]
eitherBlockOrInline = ["applet", "button", "del", "iframe", "ins",
                  "map", "area", "object"]

{-
inlineHtmlTags :: [[Char]]
inlineHtmlTags = ["a", "abbr", "acronym", "b", "basefont", "bdo", "big",
                  "br", "cite", "code", "dfn", "em", "font", "i", "img",
                  "input", "kbd", "label", "q", "s", "samp", "select",
                  "small", "span", "strike", "strong", "sub", "sup",
                  "textarea", "tt", "u", "var"]
-}

blockHtmlTags :: [String]
blockHtmlTags = ["address", "article", "aside", "blockquote", "body", "button", "canvas",
                 "caption", "center", "col", "colgroup", "dd", "dir", "div",
                 "dl", "dt", "embed", "fieldset", "figcaption", "figure", "footer",
                 "form", "h1", "h2", "h3", "h4",
                 "h5", "h6", "head", "header", "hgroup", "hr", "html", "isindex", "map", "menu",
                 "noframes", "noscript", "object", "ol", "output", "p", "pre", "progress",
                 "section", "table", "tbody", "textarea", "thead", "tfoot", "ul", "dd",
                 "dt", "frameset", "li", "tbody", "td", "tfoot",
                 "th", "thead", "tr", "script", "style", "video"]

-- We want to allow raw docbook in markdown documents, so we
-- include docbook block tags here too.
blockDocBookTags :: [String]
blockDocBookTags = ["calloutlist", "bibliolist", "glosslist", "itemizedlist",
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

blockTags :: [String]
blockTags = blockHtmlTags ++ blockDocBookTags

isInlineTag :: Tag String -> Bool
isInlineTag t = tagOpen (`notElem` blockTags) (const True) t ||
                tagClose (`notElem` blockTags) t ||
                tagComment (const True) t

isBlockTag :: Tag String -> Bool
isBlockTag t = tagOpen (`elem` blocktags) (const True) t ||
               tagClose (`elem` blocktags) t ||
               tagComment (const True) t
                 where blocktags = blockTags ++ eitherBlockOrInline

isTextTag :: Tag String -> Bool
isTextTag = tagText (const True)

isCommentTag :: Tag String -> Bool
isCommentTag = tagComment (const True)

-- taken from HXT and extended

closes :: String -> String -> Bool
_ `closes` "body" = False
_ `closes` "html" = False
"a" `closes` "a" = True
"li" `closes` "li" = True
"th" `closes` t | t `elem` ["th","td"] = True
"tr" `closes` t | t `elem` ["th","td","tr"] = True
"dt" `closes` t | t `elem` ["dt","dd"] = True
"hr" `closes` "p" = True
"p" `closes` "p" = True
"meta" `closes` "meta" = True
"colgroup" `closes` "colgroup" = True
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
   t1 `elem` blockTags &&
   t2 `notElem` (blockTags ++ eitherBlockOrInline) = True
_ `closes` _ = False

--- parsers for use in markdown, textile readers

-- | Matches a stretch of HTML in balanced tags.
htmlInBalanced :: (Tag String -> Bool) -> Parser [Char] ParserState String
htmlInBalanced f = try $ do
  (TagOpen t _, tag) <- htmlTag f
  guard $ '/' `notElem` tag      -- not a self-closing tag
  let stopper = htmlTag (~== TagClose t)
  let anytag = liftM snd $ htmlTag (const True)
  contents <- many $ notFollowedBy' stopper >>
                     (htmlInBalanced f <|> anytag <|> count 1 anyChar)
  endtag <- liftM snd stopper
  return $ tag ++ concat contents ++ endtag

-- | Matches a tag meeting a certain condition.
htmlTag :: (Tag String -> Bool) -> Parser [Char] st (Tag String, String)
htmlTag f = try $ do
  lookAhead $ char '<' >> (oneOf "/!?" <|> letter)
  (next : _) <- getInput >>= return . canonicalizeTags . parseTags
  guard $ f next
  -- advance the parser
  case next of
       TagComment s -> do
          count (length s + 4) anyChar
          skipMany (satisfy (/='>'))
          char '>'
          return (next, "<!--" ++ s ++ "-->")
       _            -> do
          rendered <- manyTill anyChar (char '>')
          return (next, rendered ++ ">")

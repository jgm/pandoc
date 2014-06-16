{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
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
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( intercalate )
import Data.Char ( isDigit )
import Control.Monad ( liftM, guard, when, mzero )
import Control.Applicative ( (<$>), (<$), (<*) )
import Data.Monoid

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
             blocks <- (fixPlains False) . mconcat <$> manyTill block eof
             meta <- stateMeta <$> getState
             return $ Pandoc meta (B.toList blocks)

type TagParser = Parser [Tag String] ParserState

pBody :: TagParser Blocks
pBody = pInTags "body" block

pHead :: TagParser Blocks
pHead = pInTags "head" $ pTitle <|> pMetaTag <|> (mempty <$ pAnyTag)
  where pTitle = pInTags "title" inline >>= setTitle . trimInlines
        setTitle t = mempty <$ (updateState $ B.setMeta "title" t)
        pMetaTag = do
          mt <- pSatisfy (~== TagOpen "meta" [])
          let name = fromAttrib "name" mt
          if null name
             then return mempty
             else do
               let content = fromAttrib "content" mt
               updateState $ B.setMeta name (B.text content)
               return mempty

block :: TagParser Blocks
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
            , pDiv
            , pRawHtmlBlock
            ]

pList :: TagParser Blocks
pList = pBulletList <|> pOrderedList <|> pDefinitionList

pBulletList :: TagParser Blocks
pBulletList = try $ do
  pSatisfy (~== TagOpen "ul" [])
  let nonItem = pSatisfy (\t ->
                  not (tagOpen (`elem` ["li","ol","ul","dl"]) (const True) t) &&
                  not (t ~== TagClose "ul"))
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  skipMany nonItem
  items <- manyTill (pInTags "li" block >>~ skipMany nonItem) (pCloses "ul")
  return $ B.bulletList $ map (fixPlains True) items

pOrderedList :: TagParser Blocks
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
  return $ B.orderedListWith (start, style, DefaultDelim) $ map (fixPlains True) items

pDefinitionList :: TagParser Blocks
pDefinitionList = try $ do
  pSatisfy (~== TagOpen "dl" [])
  items <- manyTill pDefListItem (pCloses "dl")
  return $ B.definitionList items

pDefListItem :: TagParser (Inlines, [Blocks])
pDefListItem = try $ do
  let nonItem = pSatisfy (\t -> not (t ~== TagOpen "dt" []) &&
                  not (t ~== TagOpen "dd" []) && not (t ~== TagClose "dl"))
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

pRawTag :: TagParser String
pRawTag = do
  tag <- pAnyTag
  let ignorable x = x `elem` ["html","head","body"]
  if tagOpen ignorable (const True) tag || tagClose ignorable tag
     then return []
     else return $ renderTags' [tag]

pDiv :: TagParser Blocks
pDiv = try $ do
  getOption readerParseRaw >>= guard
  TagOpen _ attr <- lookAhead $ pSatisfy $ tagOpen (=="div") (const True)
  contents <- pInTags "div" block
  return $ B.divWith (mkAttr attr) contents

pRawHtmlBlock :: TagParser Blocks
pRawHtmlBlock = do
  raw <- pHtmlBlock "script" <|> pHtmlBlock "style" <|> pRawTag
  parseRaw <- getOption readerParseRaw
  if parseRaw && not (null raw)
     then return $ B.rawBlock "html" raw
     else return mempty

pHtmlBlock :: String -> TagParser String
pHtmlBlock t = try $ do
  open <- pSatisfy (~== TagOpen t [])
  contents <- manyTill pAnyTag (pSatisfy (~== TagClose t))
  return $ renderTags' $ [open] ++ contents ++ [TagClose t]

pHeader :: TagParser Blocks
pHeader = try $ do
  TagOpen tagtype attr <- pSatisfy $
                           tagOpen (`elem` ["h1","h2","h3","h4","h5","h6"])
                           (const True)
  let bodyTitle = TagOpen tagtype attr ~== TagOpen "h1" [("class","title")]
  let level = read (drop 1 tagtype)
  contents <- trimInlines . mconcat <$> manyTill inline (pCloses tagtype <|> eof)
  let ident = fromMaybe "" $ lookup "id" attr
  let classes = maybe [] words $ lookup "class" attr
  let keyvals = [(k,v) | (k,v) <- attr, k /= "class", k /= "id"]
  return $ if bodyTitle
              then mempty  -- skip a representation of the title in the body
              else B.headerWith (ident, classes, keyvals) level contents

pHrule :: TagParser Blocks
pHrule = do
  pSelfClosing (=="hr") (const True)
  return B.horizontalRule

pTable :: TagParser Blocks
pTable = try $ do
  TagOpen _ _ <- pSatisfy (~== TagOpen "table" [])
  skipMany pBlank
  caption <- option mempty $ pInTags "caption" inline >>~ skipMany pBlank
  -- TODO actually read these and take width information from them
  widths' <- pColgroup <|> many pCol
  head' <- option mempty $ pOptInTag "thead" $ pInTags "tr" (pCell "th")
  skipMany pBlank
  rows <- pOptInTag "tbody"
          $ many1 $ try $ skipMany pBlank >> pInTags "tr" (pCell "td")
  skipMany pBlank
  TagClose _ <- pSatisfy (~== TagClose "table")
  let isSinglePlain []        = True
      isSinglePlain [Plain _] = True
      isSinglePlain _         = False
  let lHead = B.toList head'
  let lRows = map B.toList rows
  let isSimple = all isSinglePlain (lHead:lRows)
  let cols = length $ if null lHead
                         then head lRows
                         else lHead
  -- fail if there are colspans or rowspans
  guard $ all (\r -> length r == cols) lRows
  let aligns = replicate cols AlignLeft
  let widths = if null widths'
                  then if isSimple
                       then replicate cols 0
                       else replicate cols (1.0 / fromIntegral cols)
                  else widths'
  return $ B.table caption (zip aligns widths) [head'] [rows]

pCol :: TagParser Double
pCol = try $ do
  TagOpen _ attribs <- pSatisfy (~== TagOpen "col" [])
  optional $ pSatisfy (~== TagClose "col")
  skipMany pBlank
  return $ case lookup "width" attribs of
           Just x | not (null x) && last x == '%' ->
             fromMaybe 0.0 $ safeRead ('0':'.':init x)
           _ -> 0.0

pColgroup :: TagParser [Double]
pColgroup = try $ do
  pSatisfy (~== TagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

pCell :: String -> TagParser Blocks
pCell celltype = try $ do
  skipMany pBlank
  res <- pInTags celltype block
  skipMany pBlank
  return res

pBlockQuote :: TagParser Blocks
pBlockQuote = do
  contents <- pInTags "blockquote" block
  return $ B.blockQuote $ fixPlains False contents

pPlain :: TagParser Blocks
pPlain = do
  contents <- trimInlines . mconcat <$> many1 inline
  if B.isNull contents
     then return mempty
     else return $ B.plain contents

pPara :: TagParser Blocks
pPara = do
  contents <- trimInlines <$> pInTags "p" inline
  return $ B.para contents

pCodeBlock :: TagParser Blocks
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
  return $ B.codeBlockWith (mkAttr attr) result

inline :: TagParser Inlines
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
           , pSpan
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

pQ :: TagParser Inlines
pQ = do
  quoteContext <- stateQuoteContext `fmap` getState
  let quoteType = case quoteContext of
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

pEmph :: TagParser Inlines
pEmph = pInlinesInTags "em" B.emph <|> pInlinesInTags "i" B.emph

pStrong :: TagParser Inlines
pStrong = pInlinesInTags "strong" B.strong <|> pInlinesInTags "b" B.strong

pSuperscript :: TagParser Inlines
pSuperscript = pInlinesInTags "sup" B.superscript

pSubscript :: TagParser Inlines
pSubscript = pInlinesInTags "sub" B.subscript

pStrikeout :: TagParser Inlines
pStrikeout = do
  pInlinesInTags "s" B.strikeout <|>
    pInlinesInTags "strike" B.strikeout <|>
    pInlinesInTags "del" B.strikeout <|>
    try (do pSatisfy (~== TagOpen "span" [("class","strikeout")])
            contents <- mconcat <$> manyTill inline (pCloses "span")
            return $ B.strikeout contents)

pLineBreak :: TagParser Inlines
pLineBreak = do
  pSelfClosing (=="br") (const True)
  return B.linebreak

pLink :: TagParser Inlines
pLink = try $ do
  tag <- pSatisfy (tagOpenLit "a" (isJust . lookup "href"))
  let url = fromAttrib "href" tag
  let title = fromAttrib "title" tag
  lab <- trimInlines . mconcat <$> manyTill inline (pCloses "a")
  return $ B.link (escapeURI url) title lab

pImage :: TagParser Inlines
pImage = do
  tag <- pSelfClosing (=="img") (isJust . lookup "src")
  let url = fromAttrib "src" tag
  let title = fromAttrib "title" tag
  let alt = fromAttrib "alt" tag
  return $ B.image (escapeURI url) title (B.text alt)

pCode :: TagParser Inlines
pCode = try $ do
  (TagOpen open attr) <- pSatisfy $ tagOpen (`elem` ["code","tt"]) (const True)
  result <- manyTill pAnyTag (pCloses open)
  return $ B.codeWith (mkAttr attr) $ intercalate " " $ lines $ innerText result

pSpan :: TagParser Inlines
pSpan = try $ do
  getOption readerParseRaw >>= guard
  TagOpen _ attr <- lookAhead $ pSatisfy $ tagOpen (=="span") (const True)
  contents <- pInTags "span" inline
  return $ B.spanWith (mkAttr attr) contents

pRawHtmlInline :: TagParser Inlines
pRawHtmlInline = do
  result <- pSatisfy (tagComment (const True)) <|> pSatisfy isInlineTag
  parseRaw <- getOption readerParseRaw
  if parseRaw
     then return $ B.rawInline "html" $ renderTags' [result]
     else return mempty

pInlinesInTags :: String -> (Inlines -> Inlines)
               -> TagParser Inlines
pInlinesInTags tagtype f = extractSpaces f <$> pInTags tagtype inline

pInTags :: (Monoid a) => String -> TagParser a
        -> TagParser a
pInTags tagtype parser = try $ do
  pSatisfy (~== TagOpen tagtype [])
  mconcat <$> manyTill parser (pCloses tagtype <|> eof)

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
       (TagClose t') | t' == tagtype -> pAnyTag >> return ()
       (TagOpen t' _) | t' `closes` tagtype -> return ()
       (TagClose "ul") | tagtype == "li" -> return ()
       (TagClose "ol") | tagtype == "li" -> return ()
       (TagClose "dl") | tagtype == "li" -> return ()
       _ -> mzero

pTagText :: TagParser Inlines
pTagText = try $ do
  (TagText str) <- pSatisfy isTagText
  st <- getState
  case runParser (many pTagContents) st "text" str of
       Left _        -> fail $ "Could not parse `" ++ str ++ "'"
       Right result  -> return $ mconcat result

pBlank :: TagParser ()
pBlank = try $ do
  (TagText str) <- pSatisfy isTagText
  guard $ all isSpace str

pTagContents :: Parser [Char] ParserState Inlines
pTagContents =
      B.displayMath <$> mathDisplay
  <|> B.math        <$> mathInline
  <|> pStr
  <|> pSpace
  <|> smartPunctuation pTagContents
  <|> pSymbol
  <|> pBad

pStr :: Parser [Char] ParserState Inlines
pStr = do
  result <- many1 $ satisfy $ \c ->
                     not (isSpace c) && not (isSpecial c) && not (isBad c)
  pos <- getPosition
  updateState $ \s -> s{ stateLastStrPos = Just pos }
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

pSymbol :: Parser [Char] ParserState Inlines
pSymbol = satisfy isSpecial >>= return . B.str . (:[])

isBad :: Char -> Bool
isBad c = c >= '\128' && c <= '\159' -- not allowed in HTML

pBad :: Parser [Char] ParserState Inlines
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

pSpace :: Parser [Char] ParserState Inlines
pSpace = many1 (satisfy isSpace) >> return B.space

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
                 "th", "thead", "tr", "script", "style", "svg", "video"]

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
isInlineTag t = tagOpen isInlineTagName (const True) t ||
                tagClose isInlineTagName t ||
                tagComment (const True) t
                 where isInlineTagName x = x `notElem` blockTags

isBlockTag :: Tag String -> Bool
isBlockTag t = tagOpen isBlockTagName (const True) t ||
               tagClose isBlockTagName t ||
               tagComment (const True) t
                 where isBlockTagName ('?':_) = True
                       isBlockTagName ('!':_) = True
                       isBlockTagName x       = x `elem` blockTags
                                             || x `elem` eitherBlockOrInline

isTextTag :: Tag String -> Bool
isTextTag = tagText (const True)

isCommentTag :: Tag String -> Bool
isCommentTag = tagComment (const True)

-- taken from HXT and extended
-- See http://www.w3.org/TR/html5/syntax.html sec 8.1.2.4 optional tags
closes :: String -> String -> Bool
_ `closes` "body" = False
_ `closes` "html" = False
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

mkAttr :: [(String, String)] -> Attr
mkAttr attr = (attribsId, attribsClasses, attribsKV)
  where attribsId = fromMaybe "" $ lookup "id" attr
        attribsClasses = words $ fromMaybe "" $ lookup "class" attr
        attribsKV = filter (\(k,_) -> k /= "class" && k /= "id") attr



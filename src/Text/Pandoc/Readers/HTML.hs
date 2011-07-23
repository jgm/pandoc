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

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.Pandoc.Definition
import Text.Pandoc.Builder (text, toList)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( intercalate )
import Data.Char ( isSpace, isDigit )
import Control.Monad ( liftM, guard, when )

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: ParserState   -- ^ Parser state
         -> String        -- ^ String to parse (assumes @'\n'@ line endings)
         -> Pandoc
readHtml st inp = Pandoc meta blocks
  where blocks  = readWith parseBody st rest
        tags    = canonicalizeTags $
                   parseTagsOptions parseOptions{ optTagPosition = True } inp
        hasHeader = any (~== TagOpen "head" []) tags
        (meta, rest) = if hasHeader
                          then parseHeader tags
                          else (Meta [] [] [], tags)

type TagParser = GenParser (Tag String) ParserState

-- TODO - fix this - not every header has a title tag
parseHeader :: [Tag String] -> (Meta, [Tag String])
parseHeader tags = (Meta{docTitle = tit'', docAuthors = [], docDate = []}, rest)
  where (tit,_) = break (~== TagClose "title") $ drop 1 $
                   dropWhile (\t -> not $ t ~== TagOpen "title" []) tags
        tit' = concatMap fromTagText $ filter isTagText tit
        tit'' = normalizeSpaces $ toList $ text tit'
        rest  = drop 1 $ dropWhile (\t -> not $ t ~== TagClose "head" ||
                                                t ~== TagOpen "body" []) tags

parseBody :: TagParser [Block]
parseBody = liftM (fixPlains False . concat) $ manyTill block eof

block :: TagParser [Block]
block = choice
            [ pPara
            , pHeader
            , pBlockQuote
            , pCodeBlock
            , pList
            , pHrule
            , pSimpleTable
            , pPlain
            , pRawHtmlBlock
            ]

renderTags' :: [Tag String] -> String
renderTags' = renderTagsOptions
               renderOptions{ optMinimize = (`elem` ["hr","br","img"]) }

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
  st <- getState
  let (start, style) =  if stateStrict st
                           then (1, DefaultStyle) 
                           else (sta', sty')
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
        isParaish (Header _ _) = True
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
  state <- getState
  if stateParseRaw state && not (null raw)
     then return [RawBlock "html" raw]
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
  return $ if bodyTitle
              then []  -- skip a representation of the title in the body
              else [Header level $ normalizeSpaces contents]

pHrule :: TagParser [Block]
pHrule = do
  pSelfClosing (=="hr") (const True)
  return [HorizontalRule]

pSimpleTable :: TagParser [Block]
pSimpleTable = try $ do
  TagOpen _ _ <- pSatisfy (~== TagOpen "table" [])
  skipMany pBlank
  head' <- option [] $ pOptInTag "thead" $ pInTags "tr" (pCell "th")
  rows <- pOptInTag "tbody"
          $ many1 $ try $ skipMany pBlank >> pInTags "tr" (pCell "td")
  skipMany pBlank
  TagClose _ <- pSatisfy (~== TagClose "table") 
  let cols = maximum $ map length rows
  let aligns = replicate cols AlignLeft
  let widths = replicate cols 0
  return [Table [] aligns widths head' rows]

pCell :: String -> TagParser [TableCell]
pCell celltype = try $ do
  skipMany pBlank
  res <- pInTags celltype pPlain
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
  st <- getState
  let attribs = if stateStrict st
                   then ("",[],[])
                   else (attribsId, attribsClasses, attribsKV)
  return [CodeBlock attribs result]

inline :: TagParser [Inline]
inline = choice
           [ pTagText
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

pEmph :: TagParser [Inline]
pEmph = pInlinesInTags "em" Emph <|> pInlinesInTags "i" Emph

pStrong :: TagParser [Inline]
pStrong = pInlinesInTags "strong" Strong <|> pInlinesInTags "b" Strong

pSuperscript :: TagParser [Inline]
pSuperscript = failIfStrict >> pInlinesInTags "sup" Superscript

pSubscript :: TagParser [Inline]
pSubscript = failIfStrict >> pInlinesInTags "sub" Subscript

pStrikeout :: TagParser [Inline]
pStrikeout = do
  failIfStrict
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
  return [Image (toList $ text alt) (escapeURI url, title)]

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
  state <- getState
  if stateParseRaw state
     then return [RawInline "html" $ renderTags' [result]]
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
       _ -> pzero

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

pTagContents :: GenParser Char ParserState Inline
pTagContents =
  pStr <|> pSpace <|> smartPunctuation pTagContents <|> pSymbol <|> pBad

pStr :: GenParser Char ParserState Inline
pStr = liftM Str $ many1 $ satisfy $ \c ->
           not (isSpace c) && not (isSpecial c) && not (isBad c)

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

pSymbol :: GenParser Char ParserState Inline
pSymbol = satisfy isSpecial >>= return . Str . (:[])

isBad :: Char -> Bool
isBad c = c >= '\128' && c <= '\159' -- not allowed in HTML

pBad :: GenParser Char ParserState Inline
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

pSpace :: GenParser Char ParserState Inline
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
blockHtmlTags = ["address", "blockquote", "body", "center", "dir", "div",
                 "dl", "fieldset", "form", "h1", "h2", "h3", "h4",
                 "h5", "h6", "head", "hr", "html", "isindex", "menu",
                 "noframes", "noscript", "ol", "p", "pre", "table", "ul", "dd",
                 "dt", "frameset", "li", "tbody", "td", "tfoot",
                 "th", "thead", "tr", "script", "style"]

isInlineTag :: Tag String -> Bool
isInlineTag t = tagOpen (`notElem` blockHtmlTags) (const True) t ||
                tagClose (`notElem` blockHtmlTags) t ||
                tagComment (const True) t

isBlockTag :: Tag String -> Bool
isBlockTag t = tagOpen (`elem` blocktags) (const True) t ||
               tagClose (`elem` blocktags) t ||
               tagComment (const True) t
                 where blocktags = blockHtmlTags ++ eitherBlockOrInline

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
   t1 `elem` blockHtmlTags &&
   t2 `notElem` (blockHtmlTags ++ eitherBlockOrInline) = True
_ `closes` _ = False

--- parsers for use in markdown, textile readers

-- | Matches a stretch of HTML in balanced tags.
htmlInBalanced :: (Tag String -> Bool) -> GenParser Char ParserState String
htmlInBalanced f = try $ do
  (TagOpen t _, tag) <- htmlTag f
  guard $ '/' `notElem` tag      -- not a self-closing tag
  let nonTagChunk = many1 $ satisfy (/= '<')
  let stopper = htmlTag (~== TagClose t)
  let anytag = liftM snd $ htmlTag (const True)
  contents <- many $ notFollowedBy' stopper >>
                     (nonTagChunk <|> htmlInBalanced (const True) <|> anytag)
  endtag <- liftM snd stopper
  return $ tag ++ concat contents ++ endtag

-- | Matches a tag meeting a certain condition.
htmlTag :: (Tag String -> Bool) -> GenParser Char ParserState (Tag String, String)
htmlTag f = try $ do
  lookAhead (char '<')
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

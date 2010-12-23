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
module Text.Pandoc.Readers.HTML ( 
                                 readHtml, 
                                 rawHtmlInline, 
                                 rawHtmlBlock,
                                 htmlTag,
                                 anyHtmlBlockTag, 
                                 anyHtmlInlineTag,  
                                 anyHtmlTag,
                                 anyHtmlEndTag,
                                 htmlEndTag,
                                 extractTagType,
                                 htmlBlockElement,
                                 htmlComment,
                                ) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Data.Maybe ( fromMaybe )
import Data.List ( isPrefixOf, isSuffixOf, intercalate )
import Data.Char ( toLower, isAlphaNum )
import Control.Monad ( liftM, when )

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: ParserState   -- ^ Parser state
         -> String        -- ^ String to parse (assumes @'\n'@ line endings)
         -> Pandoc
readHtml = readWith parseHtml

--
-- Constants
--

eitherBlockOrInline :: [[Char]]
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

blockHtmlTags :: [[Char]]
blockHtmlTags = ["address", "blockquote", "body", "center", "dir", "div",
                 "dl", "fieldset", "form", "h1", "h2", "h3", "h4",
                 "h5", "h6", "head", "hr", "html", "isindex", "menu", "noframes",
                 "noscript", "ol", "p", "pre", "table", "ul", "dd",
                 "dt", "frameset", "li", "tbody", "td", "tfoot",
                 "th", "thead", "tr", "script", "style"]

-- taken from HXT and extended

closes :: String -> String -> Bool
"EOF" `closes` _ = True
_ `closes` "body" = False
_ `closes` "html" = False
"a" `closes` "a" = True
"li" `closes` "li" = True
"th" `closes` t | t `elem` ["th","td"] = True
"td" `closes` t | t `elem` ["th","td"] = True
"tr" `closes` t | t `elem` ["th","td","tr"] = True
"dt" `closes` t | t `elem` ["dt","dd"] = True
"dd" `closes` t | t `elem` ["dt","dd"] = True
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

--
-- HTML utility functions
--

-- | Read blocks until end tag.
blocksTilEnd :: String -> GenParser Char ParserState [Block]
blocksTilEnd tag = do
  blocks <- manyTill (block >>~ spaces) (htmlEndTag tag)
  return $ filter (/= Null) blocks

-- | Read inlines until end tag.
inlinesTilEnd :: String -> GenParser Char ParserState [Inline]
inlinesTilEnd tag = manyTill inline (htmlEndTag tag)

-- | Parse blocks between open and close tag.
blocksIn :: String -> GenParser Char ParserState [Block]
blocksIn tag = try $ htmlOpenTag tag >> spaces >> blocksTilEnd tag

-- | Parse inlines between open and close tag.
inlinesIn :: String -> GenParser Char ParserState [Inline]
inlinesIn tag = try $ htmlOpenTag tag >> spaces >> inlinesTilEnd tag

-- | Extract type from a tag:  e.g. @br@ from @\<br\>@
extractTagType :: String -> String
extractTagType ('<':rest) = 
  let isSpaceOrSlash c = c `elem` "/ \n\t" in
  map toLower $ takeWhile isAlphaNum $ dropWhile isSpaceOrSlash rest
extractTagType _ = ""

-- Parse any HTML tag (opening or self-closing) and return tag type
anyOpener :: GenParser Char ParserState [Char]
anyOpener = try $ do
  char '<'
  spaces
  tag <- many1 alphaNum
  skipMany htmlAttribute
  spaces
  option "" (string "/")
  spaces
  char '>'
  return $ map toLower tag 

-- | Parse any HTML tag (opening or self-closing) and return text of tag
anyHtmlTag :: GenParser Char ParserState [Char]
anyHtmlTag = try $ do
  char '<'
  spaces
  first <- letter
  rest <- many (alphaNum <|> char ':')
  let tag = first : rest
  attribs <- many htmlAttribute
  spaces
  ender <- option "" (string "/")
  let ender' = if null ender then "" else " /"
  spaces
  char '>'
  let result = "<" ++ tag ++ 
               concatMap (\(_, _, raw) -> (' ':raw)) attribs ++ ender' ++ ">"
  return result

anyHtmlEndTag :: GenParser Char ParserState [Char]
anyHtmlEndTag = try $ do
  char '<'   
  spaces
  char '/'
  spaces
  first <- letter
  rest <- many (alphaNum <|> char ':')
  let tag = first : rest
  spaces
  char '>'
  let result = "</" ++ tag ++ ">"
  return result 

htmlTag :: Bool
        -> String
        -> GenParser Char ParserState (String, [(String, String)])
htmlTag selfClosing tag = try $ do
  char '<'
  spaces
  stringAnyCase tag
  attribs <- many htmlAttribute
  spaces
  -- note: we want to handle both HTML and XHTML,
  -- so we don't require the /
  when selfClosing $ optional $ char '/' >> spaces
  char '>'
  return (tag, (map (\(name, content, _) -> (name, content)) attribs))

htmlOpenTag :: String
             -> GenParser Char ParserState (String, [(String, String)])
htmlOpenTag = htmlTag False

htmlCloseTag :: String
             -> GenParser Char ParserState (String, [(String, String)])
htmlCloseTag = htmlTag False . ('/':)

htmlSelfClosingTag :: String
                   -> GenParser Char ParserState (String, [(String, String)])
htmlSelfClosingTag = htmlTag True

-- parses a quoted html attribute value
quoted :: Char -> GenParser Char st (String, String)
quoted quoteChar = do
  result <- between (char quoteChar) (char quoteChar) 
                    (many (noneOf [quoteChar]))
  return (result, [quoteChar])

htmlAttribute :: GenParser Char ParserState ([Char], [Char], [Char])
htmlAttribute = do
  attr <- htmlRegularAttribute <|> htmlMinimizedAttribute
  return attr

-- minimized boolean attribute
htmlMinimizedAttribute :: GenParser Char st ([Char], [Char], [Char])
htmlMinimizedAttribute = try $ do
  many1 space
  name <- many1 (choice [letter, oneOf ".-_:"])
  return (name, name, name)

htmlRegularAttribute :: GenParser Char st ([Char], [Char], [Char])
htmlRegularAttribute = try $ do
  many1 space
  name <- many1 (choice [letter, oneOf ".-_:"])
  spaces
  char '='
  spaces
  (content, quoteStr) <- choice [ (quoted '\''), 
                                  (quoted '"'), 
                                  (do
                                     a <- many (noneOf " \t\n\r\"'<>")
                                     return (a,"")) ]
  return (name, content,
          (name ++ "=" ++ quoteStr ++ content ++ quoteStr))

-- | Parse an end tag of type 'tag'
htmlEndTag :: [Char] -> GenParser Char ParserState [Char]
htmlEndTag tag = try $ do
  closedByNext <- lookAhead $ option False $ liftM (`closes` tag) $
                   anyOpener <|> (eof >> return "EOF")
  if closedByNext
     then return ""
     else do char '<'   
             spaces
             char '/'
             spaces
             stringAnyCase tag
             spaces
             char '>'
             return $ "</" ++ tag ++ ">"

-- | Returns @True@ if the tag is (or can be) a block tag.
isBlock :: String -> Bool
isBlock tag = (extractTagType tag) `elem` (blockHtmlTags ++ eitherBlockOrInline)

anyHtmlBlockTag :: GenParser Char ParserState [Char]
anyHtmlBlockTag = try $ do
  tag <- anyHtmlTag <|> anyHtmlEndTag
  if isBlock tag then return tag else fail "not a block tag"

anyHtmlInlineTag :: GenParser Char ParserState [Char]
anyHtmlInlineTag = try $ do
  tag <- anyHtmlTag <|> anyHtmlEndTag
  if not (isBlock tag) then return tag else fail "not an inline tag"

-- | Parses material between script tags.
-- Scripts must be treated differently, because they can contain '<>' etc.
htmlScript :: GenParser Char ParserState [Char]
htmlScript = try $ do
  lookAhead $ htmlOpenTag "script"
  open <- anyHtmlTag
  rest <- liftM concat $ manyTill scriptChunk (htmlEndTag "script")
  return $ open ++ rest ++ "</script>"

scriptChunk :: GenParser Char ParserState [Char]
scriptChunk = jsComment <|> jsString <|> jsChars
  where jsComment = jsEndlineComment <|> jsMultilineComment
        jsString  = jsSingleQuoteString <|> jsDoubleQuoteString
        jsChars   = many1 (noneOf "<\"'*/") <|> count 1 anyChar
        jsEndlineComment = try $ do
           string "//"
           res <- manyTill anyChar newline
           return ("//" ++ res)
        jsMultilineComment = try $ do
           string "/*"
           res <- manyTill anyChar (try $ string "*/")
           return ("/*" ++ res ++ "*/")
        jsSingleQuoteString = stringwith '\''
        jsDoubleQuoteString = stringwith '"'
        charWithEsc escapable = try $
           (try $ char '\\' >> oneOf ('\\':escapable) >>= \x -> return ['\\',x])
          <|> count 1 anyChar
        stringwith c = try $ do
           char c
           res <- liftM concat $ manyTill (charWithEsc [c]) (char c)
           return (c : (res ++ [c]))

-- | Parses material between style tags.
-- Style tags must be treated differently, because they can contain CSS
htmlStyle :: GenParser Char ParserState [Char]
htmlStyle = try $ do
  lookAhead $ htmlOpenTag "style"
  open <- anyHtmlTag
  rest <- manyTill anyChar (htmlEndTag "style")
  return $ open ++ rest ++ "</style>"

htmlBlockElement :: GenParser Char ParserState [Char]
htmlBlockElement = choice [ htmlScript, htmlStyle, htmlComment, xmlDec, definition ]

rawHtmlBlock :: GenParser Char ParserState Block
rawHtmlBlock = try $ do
  body <- htmlBlockElement <|> rawVerbatimBlock <|> anyHtmlBlockTag
  state <- getState
  if stateParseRaw state then return (RawHtml body) else return Null

-- This is a block whose contents should be passed through verbatim, not interpreted.
rawVerbatimBlock :: GenParser Char ParserState [Char]
rawVerbatimBlock = try $ do
  start <- anyHtmlBlockTag
  let tagtype = extractTagType start
  if tagtype `elem` ["pre"]
     then do
       contents <- many (notFollowedBy' (htmlEndTag tagtype) >> anyChar)
       end <- htmlEndTag tagtype
       return $ start ++ contents ++ end
     else fail "Not a verbatim block"

-- We don't want to parse </body> or </html> as raw HTML, since these
-- are handled in parseHtml.
rawHtmlBlock' :: GenParser Char ParserState Block
rawHtmlBlock' = do notFollowedBy' (htmlCloseTag "body" <|>
                                   htmlCloseTag "html")
                   rawHtmlBlock

-- | Parses an HTML comment.
htmlComment :: GenParser Char st [Char]
htmlComment = try $ do
  string "<!--"
  comment <- many $ noneOf "-"
               <|> try (char '-' >>~ notFollowedBy (try (char '-' >> char '>')))
  string "-->"
  return $ "<!--" ++ comment ++ "-->"

--
-- parsing documents
--

xmlDec :: GenParser Char st [Char]
xmlDec = try $ do
  string "<?"
  rest <- manyTill anyChar (char '>')
  return $ "<?" ++ rest ++ ">"

definition :: GenParser Char st [Char]
definition = try $ do
  string "<!"
  rest <- manyTill anyChar (char '>')
  return $ "<!" ++ rest ++ ">"

nonTitleNonHead :: GenParser Char ParserState Char
nonTitleNonHead = try $ do
  notFollowedBy $ (htmlOpenTag "title" >> return ' ') <|> 
                  (htmlEndTag "head" >> return ' ')
  (rawHtmlBlock >> return ' ') <|> anyChar

parseTitle :: GenParser Char ParserState [Inline]
parseTitle = try $ do
  (tag, _) <- htmlOpenTag "title"
  contents <- inlinesTilEnd tag
  spaces
  return contents

-- parse header and return meta-information (for now, just title)
parseHead :: GenParser Char ParserState Meta
parseHead = try $ do
  htmlOpenTag "head"
  spaces
  skipMany nonTitleNonHead
  contents <- option [] parseTitle
  skipMany nonTitleNonHead
  htmlEndTag "head"
  return $ Meta contents [] []

-- h1 class="title" representation of title in body
bodyTitle :: GenParser Char ParserState [Inline]
bodyTitle = try $ do
  (_, attribs) <- htmlOpenTag "h1"  
  case (extractAttribute "class" attribs) of
       Just "title" -> return ""
       _            -> fail "not title"
  inlinesTilEnd "h1"

endOfDoc :: GenParser Char ParserState ()
endOfDoc = try $ do
  spaces
  optional (htmlEndTag "body")
  spaces
  optional (htmlEndTag "html" >> many anyChar) -- ignore stuff after </html>
  eof 

parseHtml :: GenParser Char ParserState Pandoc
parseHtml = do
  sepEndBy (choice [xmlDec, definition, htmlComment]) spaces
  spaces
  optional $ htmlOpenTag "html"
  spaces
  meta <- option (Meta [] [] []) parseHead
  spaces
  optional $ htmlOpenTag "body"
  spaces
  optional bodyTitle  -- skip title in body, because it's represented in meta
  blocks <- parseBlocks
  endOfDoc
  return $ Pandoc meta blocks

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = spaces >> sepEndBy block spaces >>= (return . filter (/= Null))

block :: GenParser Char ParserState Block
block = choice [ codeBlock
               , header
               , hrule
               , list
               , blockQuote
               , para
               , plain
               , rawHtmlBlock'
               , notFollowedBy' endOfDoc >> char '<' >> return Null
               ] <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = choice (map headerLevel (enumFromTo 1 5)) <?> "header"

headerLevel :: Int -> GenParser Char ParserState Block
headerLevel n = try $ do
    let level = "h" ++ show n
    htmlOpenTag level
    contents <- inlinesTilEnd level
    return $ Header n (normalizeSpaces contents)

--
-- hrule block
--

hrule :: GenParser Char ParserState Block
hrule = try  $ do
  (_, attribs) <- htmlSelfClosingTag "hr"
  state <- getState
  if not (null attribs) && stateParseRaw state
     then unexpected "attributes in hr" -- parse as raw in this case
     else return HorizontalRule

--
-- code blocks
--

-- Note:  HTML tags in code blocks (e.g. for syntax highlighting) are 
-- skipped, because they are not portable to output formats other than HTML.
codeBlock :: GenParser Char ParserState Block
codeBlock = try $ do
    htmlOpenTag "pre" 
    result <- manyTill 
              (many1 (satisfy (/= '<')) <|> 
               ((anyHtmlTag <|> anyHtmlEndTag) >> return ""))
              (htmlEndTag "pre")
    let result' = concat result
    -- drop leading newline if any
    let result'' = if "\n" `isPrefixOf` result'
                      then drop 1 result'
                      else result'
    -- drop trailing newline if any
    let result''' = if "\n" `isSuffixOf` result''
                       then init result''
                       else result''
    return $ CodeBlock ("",[],[]) $ decodeCharacterReferences result'''

--
-- block quotes
--

blockQuote :: GenParser Char ParserState Block
blockQuote = try $ htmlOpenTag "blockquote" >> spaces >> 
                   blocksTilEnd "blockquote" >>= (return . BlockQuote)

--
-- list blocks
--

list :: GenParser Char ParserState Block
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  (_, attribs) <- htmlOpenTag "ol"
  (start, style) <- option (1, DefaultStyle) $
                           do failIfStrict
                              let sta = fromMaybe "1" $ 
                                        lookup "start" attribs
                              let sty = fromMaybe (fromMaybe "" $
                                        lookup "style" attribs) $
                                        lookup "class" attribs
                              let sty' = case sty of
                                          "lower-roman"  -> LowerRoman
                                          "upper-roman"  -> UpperRoman
                                          "lower-alpha"  -> LowerAlpha
                                          "upper-alpha"  -> UpperAlpha
                                          "decimal"      -> Decimal
                                          _              -> DefaultStyle
                              return (read sta, sty')
  spaces
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  items <- sepEndBy1 (blocksIn "li" <|> liftM (:[]) list) spaces
  htmlEndTag "ol"
  return $ OrderedList (start, style, DefaultDelim) items

bulletList :: GenParser Char ParserState Block
bulletList = try $ do
  htmlOpenTag "ul"
  spaces
  -- note: if they have an <ol> or <ul> not in scope of a <li>,
  -- treat it as a list item, though it's not valid xhtml...
  items <- sepEndBy1 (blocksIn "li" <|> liftM (:[]) list) spaces
  htmlEndTag "ul"
  return $ BulletList items

definitionList :: GenParser Char ParserState Block
definitionList = try $ do
  failIfStrict  -- def lists not part of standard markdown
  htmlOpenTag "dl"
  spaces
  items <- sepEndBy1 definitionListItem spaces
  htmlEndTag "dl"
  return $ DefinitionList items

definitionListItem :: GenParser Char ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  terms <- sepEndBy1 (inlinesIn "dt") spaces
  defs <- sepEndBy1 (blocksIn "dd") spaces
  let term = intercalate [LineBreak] terms
  return (term, defs)

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = try $ htmlOpenTag "p" >> inlinesTilEnd "p" >>= 
             return . Para . normalizeSpaces

-- 
-- plain block
--

plain :: GenParser Char ParserState Block
plain = many1 inline >>= return . Plain . normalizeSpaces

-- 
-- inline
--

inline :: GenParser Char ParserState Inline
inline = choice [ str
                , strong
                , emph
                , superscript
                , subscript
                , strikeout
                , spanStrikeout
                , code
                , linebreak
                , whitespace
                , link
                , image
                , smartPunctuation inline
                , charRef
                , rawHtmlInline
                , symbol
                ] <?> "inline"

code :: GenParser Char ParserState Inline
code = try $ do 
  result <- (htmlOpenTag "code" >> manyTill (noneOf "<>") (htmlEndTag "code"))
        <|> (htmlOpenTag "tt"   >> manyTill (noneOf "<>") (htmlEndTag "tt"))
  -- remove internal line breaks, leading and trailing space,
  -- and decode character references
  return $ Code $ decodeCharacterReferences $ removeLeadingTrailingSpace $ 
                  intercalate " " $ lines result

rawHtmlInline :: GenParser Char ParserState Inline
rawHtmlInline = do
  result <- anyHtmlInlineTag <|> htmlComment
  state <- getState
  if stateParseRaw state then return (HtmlInline result) else return (Str "")

symbol :: GenParser Char ParserState Inline
symbol = do
  notFollowedBy (char '<')
  c <- oneOf specialChars
  return $ Str [c]

betweenTags :: [Char] -> GenParser Char ParserState [Inline]
betweenTags tag = try $ htmlOpenTag tag >> inlinesTilEnd tag >>= 
                        return . normalizeSpaces

emph :: GenParser Char ParserState Inline
emph = (betweenTags "em" <|> betweenTags "i") >>= return . Emph

strong :: GenParser Char ParserState Inline
strong = (betweenTags "b" <|> betweenTags "strong") >>= return . Strong

superscript :: GenParser Char ParserState Inline
superscript = failIfStrict >> betweenTags "sup" >>= return . Superscript

subscript :: GenParser Char ParserState Inline
subscript = failIfStrict >> betweenTags "sub" >>= return . Subscript

strikeout :: GenParser Char ParserState Inline
strikeout = failIfStrict >> (betweenTags "s" <|> betweenTags "strike") >>=
            return . Strikeout

spanStrikeout :: GenParser Char ParserState Inline
spanStrikeout = try $ do
  failIfStrict -- strict markdown has no strikeout, so treat as raw HTML
  (_, attributes) <- htmlOpenTag "span" 
  result <- case (extractAttribute "class" attributes) of
              Just "strikeout" -> inlinesTilEnd "span"
              _                -> fail "not a strikeout"
  return $ Strikeout result

whitespace :: GenParser Char st Inline
whitespace = many1 space >> return Space

-- hard line break
linebreak :: GenParser Char ParserState Inline
linebreak = htmlSelfClosingTag "br" >> optional newline >> return LineBreak

str :: GenParser Char st Inline
str = many1 (noneOf $ specialChars ++ " \t\n") >>= return . Str

specialChars :: [Char]
specialChars = "<&-\"'.\8216\8217\8220\8221"

--
-- links and images
--

-- extract contents of attribute (attribute names are case-insensitive)
extractAttribute :: [Char] -> [([Char], String)] -> Maybe String
extractAttribute _    [] = Nothing
extractAttribute name ((attrName, contents):rest) = 
  let name'     = map toLower name 
      attrName' = map toLower attrName
  in  if attrName' == name'
         then Just (decodeCharacterReferences contents)
         else extractAttribute name rest

link :: GenParser Char ParserState Inline
link = try $ do
  (_, attributes) <- htmlOpenTag "a"  
  url <- case (extractAttribute "href" attributes) of
           Just url -> return url
           Nothing  -> fail "no href"
  let title = fromMaybe "" $ extractAttribute "title" attributes
  lab <- inlinesTilEnd "a"
  return $ Link (normalizeSpaces lab) (escapeURI url, title)

image :: GenParser Char ParserState Inline
image = try $ do
  (_, attributes) <- htmlSelfClosingTag "img"
  url <- case (extractAttribute "src" attributes) of
           Just url -> return url
           Nothing  -> fail "no src"
  let title = fromMaybe "" $ extractAttribute "title" attributes
  let alt = fromMaybe "" (extractAttribute "alt" attributes)
  return $ Image [Str alt] (escapeURI url, title)


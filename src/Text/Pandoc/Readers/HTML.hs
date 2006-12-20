{- |
   Module      : Text.Pandoc.Readers.HTML
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha 
   Portability : portable

Conversion of HTML to 'Pandoc' document.
-}
module Text.Pandoc.Readers.HTML ( 
                                 readHtml, 
                                 rawHtmlInline, 
                                 rawHtmlBlock, 
                                 anyHtmlBlockTag, 
                                 anyHtmlInlineTag  
                                ) where

import Text.Regex ( matchRegex, mkRegex )
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.HtmlEntities ( decodeEntities, htmlEntityToChar )
import Maybe ( fromMaybe )
import Char ( toUpper, toLower )

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: ParserState   -- ^ Parser state
         -> String        -- ^ String to parse
         -> Pandoc
readHtml = readWith parseHtml

-- for testing 
testString :: String -> IO ()
testString = testStringWith parseHtml

--
-- Constants
--

inlineHtmlTags = ["a", "abbr", "acronym", "b", "basefont", "bdo", "big",
                  "br", "cite", "code", "dfn", "em", "font", "i", "img",
                  "input", "kbd", "label", "q", "s", "samp", "select",
                  "small", "span", "strike", "strong", "sub", "sup",
                  "textarea", "tt", "u", "var"]

--
-- HTML utility functions
--

-- | Read blocks until end tag.
blocksTilEnd tag = try (do
  blocks <- manyTill (do {b <- block; spaces; return b}) (htmlEndTag tag)
  return blocks)

-- | Read inlines until end tag.
inlinesTilEnd tag = try (do
  inlines <- manyTill inline (htmlEndTag tag)
  return inlines)

-- extract type from a tag:  e.g. br from <br>, < br >, </br>, etc.
extractTagType tag = 
    case (matchRegex (mkRegex  "<[[:space:]]*/?([A-Za-z0-9]+)") tag) of
          Just [match]   -> (map toLower match)
          Nothing        -> ""

anyHtmlTag = try (do
  char '<'
  spaces
  tag <- many1 alphaNum
  attribs <- htmlAttributes  
  spaces
  ender <- option "" (string "/")
  let ender' = if (null ender) then "" else " /"
  spaces
  char '>'
  return ("<" ++ tag ++ attribs ++ ender' ++ ">"))

anyHtmlEndTag = try (do
  char '<'   
  spaces
  char '/'
  spaces
  tagType <- many1 alphaNum
  spaces
  char '>'
  return ("</" ++ tagType ++ ">"))

htmlTag :: String -> GenParser Char st (String, [(String, String)])
htmlTag tag = try (do
  char '<'
  spaces
  stringAnyCase tag
  attribs <- many htmlAttribute
  spaces
  option "" (string "/")
  spaces
  char '>'
  return (tag, (map (\(name, content, raw) -> (name, content)) attribs)))

-- parses a quoted html attribute value
quoted quoteChar = do
  result <- between (char quoteChar) (char quoteChar) 
                    (many (noneOf [quoteChar]))
  return (result, [quoteChar])

htmlAttributes = do
  attrList <- many htmlAttribute
  return (concatMap (\(name, content, raw) -> raw) attrList)

htmlAttribute = htmlRegularAttribute <|> htmlMinimizedAttribute

-- minimized boolean attribute (no = and value)
htmlMinimizedAttribute = try (do
  spaces
  name <- many1 (choice [letter, oneOf ".-_:"])
  spaces
  notFollowedBy (char '=')
  let content = name
  return (name, content, (" " ++ name)))

htmlRegularAttribute = try (do
  spaces
  name <- many1 (choice [letter, oneOf ".-_:"])
  spaces
  char '='
  spaces
  (content, quoteStr) <- choice [ (quoted '\''), 
                                  (quoted '"'), 
                                  (do
                                     a <- many (alphaNum <|> (oneOf "-._:"))
                                     return (a,"")) ]
  return (name, content, 
          (" " ++ name ++ "=" ++ quoteStr ++ content ++ quoteStr)))

htmlEndTag tag = try (do
  char '<'   
  spaces
  char '/'
  spaces
  stringAnyCase tag
  spaces
  char '>'
  return ("</" ++ tag ++ ">"))

-- | Returns @True@ if the tag is an inline tag.
isInline tag = (extractTagType tag) `elem` inlineHtmlTags

anyHtmlBlockTag = try (do
  tag <- choice [anyHtmlTag, anyHtmlEndTag]
  if isInline tag then fail "inline tag" else return tag)

anyHtmlInlineTag = try (do
  tag <- choice [ anyHtmlTag, anyHtmlEndTag ]
  if isInline tag then return tag else fail "not an inline tag")

-- scripts must be treated differently, because they can contain <> etc.
htmlScript = try (do
  open <- string "<script"
  rest <- manyTill anyChar (htmlEndTag "script")
  return (open ++ rest ++ "</script>"))

rawHtmlBlock = try (do
  notFollowedBy' (choice [htmlTag "/body", htmlTag "/html"])
  body <- choice [htmlScript, anyHtmlBlockTag, htmlComment, xmlDec, 
                  definition]
  sp <- (many space)
  state <- getState
  if stateParseRaw state then return (RawHtml (body ++ sp)) else return Null)

htmlComment = try (do
  string "<!--"
  comment <- manyTill anyChar (try (string "-->"))
  return ("<!--" ++ comment ++ "-->"))

--
-- parsing documents
--

xmlDec = try (do
  string "<?"
  rest <- manyTill anyChar (char '>')
  return ("<?" ++ rest ++ ">"))

definition = try (do
  string "<!"
  rest <- manyTill anyChar (char '>')
  return ("<!" ++ rest ++ ">"))

nonTitleNonHead = try (do
  notFollowedBy' (htmlTag "title")
  notFollowedBy' (htmlTag "/head")
  result <- choice [do {rawHtmlBlock; return ' '}, anyChar]
  return result)

parseTitle = try (do
  (tag, attribs) <- htmlTag "title"
  contents <- inlinesTilEnd tag
  spaces
  return contents)

-- parse header and return meta-information (for now, just title)
parseHead = try (do
  htmlTag "head"
  spaces
  skipMany nonTitleNonHead
  contents <- option [] parseTitle
  skipMany nonTitleNonHead
  htmlTag "/head"
  return (contents, [], ""))

skipHtmlTag tag = option ("",[]) (htmlTag tag)

-- h1 class="title" representation of title in body
bodyTitle = try (do
  (tag, attribs) <- htmlTag "h1"  
  cl <- case (extractAttribute "class" attribs) of
          Just "title" -> do {return ""}
          otherwise    -> fail "not title"
  inlinesTilEnd "h1"
  return "")

parseHtml = do
  sepEndBy (choice [xmlDec, definition, htmlComment]) spaces
  skipHtmlTag "html"
  spaces
  (title, authors, date) <- option ([], [], "") parseHead 
  spaces
  skipHtmlTag "body"
  spaces
  option "" bodyTitle  -- skip title in body, because it's represented in meta
  blocks <- parseBlocks
  spaces
  option "" (htmlEndTag "body")
  spaces
  option "" (htmlEndTag "html")
  many anyChar -- ignore anything after </html>
  eof
  state <- getState
  let keyBlocks = stateKeyBlocks state 
  return (Pandoc (Meta title authors date) (blocks ++ (reverse keyBlocks)))

--
-- parsing blocks
--

parseBlocks = do
  spaces
  result <- sepEndBy block spaces
  return result

block = choice [ codeBlock, header, hrule, list, blockQuote, para, plain, 
                 rawHtmlBlock ] <?> "block"

--
-- header blocks
--

header = choice (map headerLevel (enumFromTo 1 5)) <?> "header"

headerLevel n = try (do
    let level = "h" ++ show n
    (tag, attribs) <- htmlTag level
    contents <- inlinesTilEnd level
    return (Header n (normalizeSpaces contents)))

--
-- hrule block
--

hrule = try (do
  (tag, attribs) <- htmlTag "hr"
  state <- getState
  if (not (null attribs)) && (stateParseRaw state)
     then -- in this case we want to parse it as raw html
          unexpected "attributes in hr"
     else return HorizontalRule)

--
-- code blocks
--

codeBlock = choice [ preCodeBlock, bareCodeBlock ] <?> "code block"

preCodeBlock = try (do
    htmlTag "pre" 
    spaces
    htmlTag "code"
    result <- manyTill anyChar (htmlEndTag "code")
    spaces
    htmlEndTag "pre"
    return (CodeBlock (stripTrailingNewlines (decodeEntities result))))

bareCodeBlock = try (do
    htmlTag "code"
    result <- manyTill anyChar (htmlEndTag "code")
    return (CodeBlock (stripTrailingNewlines (decodeEntities result))))

--
-- block quotes
--

blockQuote = try (do
    tag <- htmlTag "blockquote"
    spaces
    blocks <- blocksTilEnd "blockquote"
    return (BlockQuote blocks))

--
-- list blocks
--

list = choice [ bulletList, orderedList ] <?> "list"

orderedList = try (do
    tag <- htmlTag "ol"
    spaces
    items <- sepEndBy1 listItem spaces
    htmlEndTag "ol"
    return (OrderedList items))

bulletList = try (do
    tag <- htmlTag "ul"
    spaces
    items <- sepEndBy1 listItem spaces
    htmlEndTag "ul"
    return (BulletList items))

listItem = try (do
    tag <- htmlTag "li"
    spaces
    blocks <- blocksTilEnd "li"
    return blocks)

--
-- paragraph block
--

para = try (do 
  tag <- htmlTag "p"
  result <- inlinesTilEnd "p"
  return (Para (normalizeSpaces result)))

-- 
-- plain block
--

plain = do
    result <- many1 inline
    return (Plain (normalizeSpaces result))

-- 
-- inline
--

inline = choice [ text, special ] <?> "inline"

text =  choice [ entity, strong, emph, code, str, linebreak, whitespace ] <?> "text"

special = choice [ link, image, rawHtmlInline ] <?> 
                 "link, inline html, or image"

entity = try (do
  char '&'
  body <- choice [(many1 letter), (try (do
                                          char '#'
                                          num <- many1 digit
                                          return ("#" ++ num)))]
  char ';'
  return (Str [fromMaybe '?' (htmlEntityToChar ("&" ++ body ++ ";"))]))

code = try (do 
  htmlTag "code"
  result <- manyTill anyChar (htmlEndTag "code")
  -- remove internal line breaks, leading and trailing space,
  -- and decode entities
  let result' = decodeEntities $ removeLeadingTrailingSpace $ 
                joinWithSep " " $ lines result 
  return (Code result'))

rawHtmlInline = do
  result <- choice [htmlScript, anyHtmlInlineTag]
  state <- getState
  if stateParseRaw state then return (HtmlInline result) else return (Str "")

betweenTags tag = try (do
  htmlTag tag
  result <- inlinesTilEnd tag
  return (normalizeSpaces result))

emph = try (do 
  result <- choice [betweenTags "em", betweenTags "it"]
  return (Emph result))

strong = try (do 
  result <- choice [betweenTags "b", betweenTags "strong"]
  return (Strong result))

whitespace = do
  many1 space
  return Space

-- hard line break
linebreak = do
  htmlTag "br"
  return LineBreak 

str = do 
  result <- many1 (noneOf "<& \t\n")
  return (Str (decodeEntities result))

--
-- links and images
--

-- extract contents of attribute (attribute names are case-insensitive)
extractAttribute name [] = Nothing
extractAttribute name ((attrName, contents):rest) = 
  let name' = map toLower name 
      attrName' = map toLower attrName in
  if (attrName' == name') then Just contents else extractAttribute name rest

link = try (do
  (tag, attributes) <- htmlTag "a"  
  url <- case (extractAttribute "href" attributes) of
           Just url -> do {return url}
           Nothing  -> fail "no href"
  let title = fromMaybe "" (extractAttribute "title" attributes)
  label <- inlinesTilEnd "a"
  ref <- generateReference url title
  return (Link (normalizeSpaces label) ref))

image = try (do
  (tag, attributes) <- htmlTag "img" 
  url <- case (extractAttribute "src" attributes) of
           Just url -> do {return url}
           Nothing  -> fail "no src"
  let title = fromMaybe "" (extractAttribute "title" attributes)
  let alt = fromMaybe "" (extractAttribute "alt" attributes)
  ref <- generateReference url title
  return (Image [Str alt] ref)) 

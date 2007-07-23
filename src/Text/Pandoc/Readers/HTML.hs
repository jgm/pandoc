{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
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
                                 anyHtmlBlockTag, 
                                 anyHtmlInlineTag,  
                                 anyHtmlTag,
                                 anyHtmlEndTag,
                                 htmlEndTag,
                                 extractTagType,
                                 htmlBlockElement 
                                ) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.ParserCombinators
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Entities ( characterEntity, decodeEntities )
import Data.Maybe ( fromMaybe )
import Data.List ( intersect, takeWhile, dropWhile )
import Data.Char ( toUpper, toLower, isAlphaNum )

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

-- | Parse blocks between open and close tag.
blocksIn tag = try $ do
  htmlTag tag
  spaces
  blocksTilEnd tag

-- | Parse inlines between open and close tag.
inlinesIn tag = try $ do
  htmlTag tag
  spaces
  inlinesTilEnd tag

-- | Extract type from a tag:  e.g. @br@ from @\<br\>@
extractTagType :: String -> String
extractTagType ('<':rest) = 
  let isSpaceOrSlash c = c `elem` "/ \n\t" in
  map toLower $ takeWhile isAlphaNum $ dropWhile isSpaceOrSlash rest
extractTagType _ = ""

-- | Parse any HTML tag (closing or opening) and return text of tag
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
  many1 space
  name <- many1 (choice [letter, oneOf ".-_:"])
  spaces
  notFollowedBy (char '=')
  let content = name
  return (name, content, (" " ++ name)))

htmlRegularAttribute = try (do
  many1 space
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

-- | Parse an end tag of type 'tag'
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

-- | Parses material between script tags.
-- Scripts must be treated differently, because they can contain '<>' etc.
htmlScript = try (do
  open <- string "<script"
  rest <- manyTill anyChar (htmlEndTag "script")
  return (open ++ rest ++ "</script>"))

htmlBlockElement = choice [ htmlScript, htmlComment, xmlDec, definition ]

rawHtmlBlock = try (do
  notFollowedBy' (choice [htmlTag "/body", htmlTag "/html"])
  body <- htmlBlockElement <|> anyHtmlBlockTag
  sp <- (many space)
  state <- getState
  if stateParseRaw state then return (RawHtml (body ++ sp)) else return Null)

-- | Parses an HTML comment.
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
  return (Pandoc (Meta title authors date) blocks)

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

list = choice [ bulletList, orderedList, definitionList ] <?> "list"

orderedList = try $ do
    htmlTag "ol"
    spaces
    items <- sepEndBy1 (blocksIn "li") spaces
    htmlEndTag "ol"
    return (OrderedList items)

bulletList = try $ do
    htmlTag "ul"
    spaces
    items <- sepEndBy1 (blocksIn "li") spaces
    htmlEndTag "ul"
    return (BulletList items)

definitionList = try $ do
    failIfStrict  -- def lists not part of standard markdown
    tag <- htmlTag "dl"
    spaces
    items <- sepEndBy1 definitionListItem spaces
    htmlEndTag "dl"
    return (DefinitionList items)

definitionListItem = try $ do
    terms <- sepEndBy1 (inlinesIn "dt") spaces
    defs <- sepEndBy1 (blocksIn "dd") spaces
    let term = joinWithSep [LineBreak] terms
    return (term, concat defs)

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

text =  choice [ entity, strong, emph, superscript, subscript,
                 strikeout, spanStrikeout, code, str,
                 linebreak, whitespace ] <?> "text"

special = choice [ link, image, rawHtmlInline ] <?> 
                 "link, inline html, or image"

entity = do
  ent <- characterEntity
  return $ Str [ent]

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

superscript = try $ do
  failIfStrict -- strict markdown has no superscript, so treat as raw HTML
  result <- betweenTags "sup"
  return (Superscript result)  

subscript = try $ do
  failIfStrict -- strict markdown has no subscript, so treat as raw HTML
  result <- betweenTags "sub"
  return (Subscript result)  

strikeout = try $ do
  failIfStrict -- strict markdown has no strikeout, so treat as raw HTML
  result <- choice [betweenTags "s", betweenTags "strike"]
  return (Strikeout result)

spanStrikeout = try $ do
  failIfStrict -- strict markdown has no strikeout, so treat as raw HTML
  (tag, attributes) <- htmlTag "span" 
  result <- case (extractAttribute "class" attributes) of
              Just "strikeout" -> inlinesTilEnd "span"
              _                -> fail "not a strikeout"
  return (Strikeout result)

strong = try (do 
  result <- choice [betweenTags "b", betweenTags "strong"]
  return (Strong result))

whitespace = do
  many1 space
  return Space

-- hard line break
linebreak = do
  htmlTag "br"
  option ' ' newline
  return LineBreak 

str = do 
  result <- many1 (noneOf "<& \t\n")
  return (Str result)

--
-- links and images
--

-- extract contents of attribute (attribute names are case-insensitive)
extractAttribute name [] = Nothing
extractAttribute name ((attrName, contents):rest) = 
  let name' = map toLower name 
      attrName' = map toLower attrName in
  if (attrName' == name')
     then Just (decodeEntities contents)
     else extractAttribute name rest

link = try $ do
  (tag, attributes) <- htmlTag "a"  
  url <- case (extractAttribute "href" attributes) of
           Just url -> do {return url}
           Nothing  -> fail "no href"
  let title = fromMaybe "" (extractAttribute "title" attributes)
  label <- inlinesTilEnd "a"
  return $ Link (normalizeSpaces label) (url, title)

image = try $ do
  (tag, attributes) <- htmlTag "img" 
  url <- case (extractAttribute "src" attributes) of
           Just url -> do {return url}
           Nothing  -> fail "no src"
  let title = fromMaybe "" (extractAttribute "title" attributes)
  let alt = fromMaybe "" (extractAttribute "alt" attributes)
  return $ Image [Str alt] (url, title)


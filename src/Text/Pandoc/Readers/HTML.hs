{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-8 John MacFarlane
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
                                 htmlBlockElement,
                                 unsanitaryURI
                                ) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Data.Maybe ( fromMaybe )
import Data.List ( takeWhile, dropWhile, isPrefixOf, isSuffixOf, intercalate )
import Data.Char ( toLower, isAlphaNum )
import Network.URI ( parseURIReference, URI (..) )

-- | Convert HTML-formatted string to 'Pandoc' document.
readHtml :: ParserState   -- ^ Parser state
         -> String        -- ^ String to parse
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
                  "textarea", "tt", "u", "var"] ++ eitherBlockOrInline
-}

blockHtmlTags :: [[Char]]
blockHtmlTags = ["address", "blockquote", "body", "center", "dir", "div",
                 "dl", "fieldset", "form", "h1", "h2", "h3", "h4",
                 "h5", "h6", "hr", "html", "isindex", "menu", "noframes",
                 "noscript", "ol", "p", "pre", "table", "ul", "dd",
                 "dt", "frameset", "li", "tbody", "td", "tfoot",
                 "th", "thead", "tr", "script"] ++ eitherBlockOrInline

sanitaryTags :: [[Char]]
sanitaryTags = ["a", "abbr", "acronym", "address", "area", "b", "big",
                "blockquote", "br", "button", "caption", "center",
                "cite", "code", "col", "colgroup", "dd", "del", "dfn",
                "dir", "div", "dl", "dt", "em", "fieldset", "font",
                "form", "h1", "h2", "h3", "h4", "h5", "h6", "hr",
                "i", "img", "input", "ins", "kbd", "label", "legend",
                "li", "map", "menu", "ol", "optgroup", "option", "p",
                "pre", "q", "s", "samp", "select", "small", "span",
                "strike", "strong", "sub", "sup", "table", "tbody",
                "td", "textarea", "tfoot", "th", "thead", "tr", "tt",
                "u", "ul", "var"]

sanitaryAttributes :: [[Char]]
sanitaryAttributes = ["abbr", "accept", "accept-charset",
                      "accesskey", "action", "align", "alt", "axis",
                      "border", "cellpadding", "cellspacing", "char",
                      "charoff", "charset", "checked", "cite", "class",
                      "clear", "cols", "colspan", "color", "compact",
                      "coords", "datetime", "dir", "disabled",
                      "enctype", "for", "frame", "headers", "height",
                      "href", "hreflang", "hspace", "id", "ismap",
                      "label", "lang", "longdesc", "maxlength", "media",
                      "method", "multiple", "name", "nohref", "noshade",
                      "nowrap", "prompt", "readonly", "rel", "rev",
                      "rows", "rowspan", "rules", "scope", "selected",
                      "shape", "size", "span", "src", "start",
                      "summary", "tabindex", "target", "title", "type",
                      "usemap", "valign", "value", "vspace", "width"]

--
-- HTML utility functions
--

-- | Returns @True@ if sanitization is specified and the specified tag is 
--  not on the sanitized tag list.
unsanitaryTag :: [Char]
              -> GenParser tok ParserState Bool
unsanitaryTag tag = do
  st <- getState
  return $ stateSanitizeHTML st && tag `notElem` sanitaryTags

-- | returns @True@ if sanitization is specified and the specified attribute
--  is not on the sanitized attribute list.
unsanitaryAttribute :: ([Char], String, t)
                    -> GenParser tok ParserState Bool
unsanitaryAttribute (attr, val, _) = do
  st <- getState
  return $ stateSanitizeHTML st &&
           (attr `notElem` sanitaryAttributes ||
             (attr `elem` ["href","src"] && unsanitaryURI val))

-- | Returns @True@ if the specified URI is potentially a security risk.
unsanitaryURI :: String -> Bool
unsanitaryURI u =
  let safeURISchemes = [ "", "http:", "https:", "ftp:", "mailto:", "file:",
             "telnet:", "gopher:", "aaa:", "aaas:", "acap:", "cap:", "cid:",
             "crid:", "dav:", "dict:", "dns:", "fax:", "go:", "h323:", "im:",
             "imap:", "ldap:", "mid:", "news:", "nfs:", "nntp:", "pop:",
             "pres:", "sip:", "sips:", "snmp:", "tel:", "urn:", "wais:",
             "xmpp:", "z39.50r:", "z39.50s:", "aim:", "callto:", "cvs:",
             "ed2k:", "feed:", "fish:", "gg:", "irc:", "ircs:", "lastfm:",
             "ldaps:", "magnet:", "mms:", "msnim:", "notes:", "rsync:",
             "secondlife:", "skype:", "ssh:", "sftp:", "smb:", "sms:",
             "snews:", "webcal:", "ymsgr:"]
  in  case parseURIReference u of
           Just p  -> (map toLower $ uriScheme p) `notElem` safeURISchemes
           Nothing -> True

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
blocksIn tag = try $ htmlTag tag >> spaces >> blocksTilEnd tag

-- | Parse inlines between open and close tag.
inlinesIn :: String -> GenParser Char ParserState [Inline]
inlinesIn tag = try $ htmlTag tag >> spaces >> inlinesTilEnd tag

-- | Extract type from a tag:  e.g. @br@ from @\<br\>@
extractTagType :: String -> String
extractTagType ('<':rest) = 
  let isSpaceOrSlash c = c `elem` "/ \n\t" in
  map toLower $ takeWhile isAlphaNum $ dropWhile isSpaceOrSlash rest
extractTagType _ = ""

-- | Parse any HTML tag (opening or self-closing) and return text of tag
anyHtmlTag :: GenParser Char ParserState [Char]
anyHtmlTag = try $ do
  char '<'
  spaces
  tag <- many1 alphaNum
  attribs <- many htmlAttribute
  spaces
  ender <- option "" (string "/")
  let ender' = if null ender then "" else " /"
  spaces
  char '>'
  let result = "<" ++ tag ++ 
               concatMap (\(_, _, raw) -> (' ':raw)) attribs ++ ender' ++ ">"
  unsanitary <- unsanitaryTag tag
  if unsanitary
     then return $ "<!-- unsafe HTML removed -->"
     else return result

anyHtmlEndTag :: GenParser Char ParserState [Char]
anyHtmlEndTag = try $ do
  char '<'   
  spaces
  char '/'
  spaces
  tag <- many1 alphaNum
  spaces
  char '>'
  let result = "</" ++ tag ++ ">"
  unsanitary <- unsanitaryTag tag
  if unsanitary
     then return $ "<!-- unsafe HTML removed -->"
     else return result 

htmlTag :: String -> GenParser Char ParserState (String, [(String, String)])
htmlTag tag = try $ do
  char '<'
  spaces
  stringAnyCase tag
  attribs <- many htmlAttribute
  spaces
  optional (string "/")
  spaces
  char '>'
  return (tag, (map (\(name, content, _) -> (name, content)) attribs))

-- parses a quoted html attribute value
quoted :: Char -> GenParser Char st (String, String)
quoted quoteChar = do
  result <- between (char quoteChar) (char quoteChar) 
                    (many (noneOf [quoteChar]))
  return (result, [quoteChar])

nullAttribute :: ([Char], [Char], [Char])
nullAttribute = ("", "", "") 

htmlAttribute :: GenParser Char ParserState ([Char], [Char], [Char])
htmlAttribute = do
  attr <- htmlRegularAttribute <|> htmlMinimizedAttribute
  unsanitary <- unsanitaryAttribute attr
  if unsanitary
     then return nullAttribute
     else return attr

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
                                     a <- many (alphaNum <|> (oneOf "-._:"))
                                     return (a,"")) ]
  return (name, content,
          (name ++ "=" ++ quoteStr ++ content ++ quoteStr))

-- | Parse an end tag of type 'tag'
htmlEndTag :: [Char] -> GenParser Char st [Char]
htmlEndTag tag = try $ do
  char '<'   
  spaces
  char '/'
  spaces
  stringAnyCase tag
  spaces
  char '>'
  return $ "</" ++ tag ++ ">"

{-
-- | Returns @True@ if the tag is (or can be) an inline tag.
isInline :: String -> Bool
isInline tag = (extractTagType tag) `elem` inlineHtmlTags
-}

-- | Returns @True@ if the tag is (or can be) a block tag.
isBlock :: String -> Bool
isBlock tag = (extractTagType tag) `elem` blockHtmlTags 

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
  open <- string "<script"
  rest <- manyTill anyChar (htmlEndTag "script")
  st <- getState
  if stateSanitizeHTML st && not ("script" `elem` sanitaryTags)
     then return "<!-- unsafe HTML removed -->"
     else return $ open ++ rest ++ "</script>"

-- | Parses material between style tags.
-- Style tags must be treated differently, because they can contain CSS
htmlStyle :: GenParser Char ParserState [Char]
htmlStyle = try $ do
  open <- string "<style"
  rest <- manyTill anyChar (htmlEndTag "style")
  st <- getState
  if stateSanitizeHTML st && not ("style" `elem` sanitaryTags)
     then return "<!-- unsafe HTML removed -->"
     else return $ open ++ rest ++ "</style>"

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
rawHtmlBlock' = do notFollowedBy' (htmlTag "/body" <|> htmlTag "/html")
                   rawHtmlBlock

-- | Parses an HTML comment.
htmlComment :: GenParser Char st [Char]
htmlComment = try $ do
  string "<!--"
  comment <- many $ noneOf "-"
                 <|> try (char '-' >>~ notFollowedBy (char '-' >> char '>'))
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
  notFollowedBy $ (htmlTag "title" >> return ' ') <|> 
                  (htmlEndTag "head" >> return ' ')
  (rawHtmlBlock >> return ' ') <|> anyChar

parseTitle :: GenParser Char ParserState [Inline]
parseTitle = try $ do
  (tag, _) <- htmlTag "title"
  contents <- inlinesTilEnd tag
  spaces
  return contents

-- parse header and return meta-information (for now, just title)
parseHead :: GenParser Char ParserState ([Inline], [a], [Char])
parseHead = try $ do
  htmlTag "head"
  spaces
  skipMany nonTitleNonHead
  contents <- option [] parseTitle
  skipMany nonTitleNonHead
  htmlEndTag "head"
  return (contents, [], "")

skipHtmlTag :: String -> GenParser Char ParserState ()
skipHtmlTag tag = optional (htmlTag tag)

-- h1 class="title" representation of title in body
bodyTitle :: GenParser Char ParserState [Inline]
bodyTitle = try $ do
  (_, attribs) <- htmlTag "h1"  
  case (extractAttribute "class" attribs) of
       Just "title" -> return ""
       _            -> fail "not title"
  inlinesTilEnd "h1"

parseHtml :: GenParser Char ParserState Pandoc
parseHtml = do
  sepEndBy (choice [xmlDec, definition, htmlComment]) spaces
  skipHtmlTag "html"
  spaces
  (title, authors, date) <- option ([], [], "") parseHead 
  spaces
  skipHtmlTag "body"
  spaces
  optional bodyTitle  -- skip title in body, because it's represented in meta
  blocks <- parseBlocks
  spaces
  optional (htmlEndTag "body")
  spaces
  optional (htmlEndTag "html" >> many anyChar) -- ignore anything after </html>
  eof
  return $ Pandoc (Meta title authors date) blocks

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
               ] <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = choice (map headerLevel (enumFromTo 1 5)) <?> "header"

headerLevel :: Int -> GenParser Char ParserState Block
headerLevel n = try $ do
    let level = "h" ++ show n
    htmlTag level
    contents <- inlinesTilEnd level
    return $ Header n (normalizeSpaces contents)

--
-- hrule block
--

hrule :: GenParser Char ParserState Block
hrule = try  $ do
  (_, attribs) <- htmlTag "hr"
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
    htmlTag "pre" 
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
blockQuote = try $ htmlTag "blockquote" >> spaces >> 
                   blocksTilEnd "blockquote" >>= (return . BlockQuote)

--
-- list blocks
--

list :: GenParser Char ParserState Block
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  (_, attribs) <- htmlTag "ol"
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
  items <- sepEndBy1 (blocksIn "li") spaces
  htmlEndTag "ol"
  return $ OrderedList (start, style, DefaultDelim) items

bulletList :: GenParser Char ParserState Block
bulletList = try $ do
  htmlTag "ul"
  spaces
  items <- sepEndBy1 (blocksIn "li") spaces
  htmlEndTag "ul"
  return $ BulletList items

definitionList :: GenParser Char ParserState Block
definitionList = try $ do
  failIfStrict  -- def lists not part of standard markdown
  htmlTag "dl"
  spaces
  items <- sepEndBy1 definitionListItem spaces
  htmlEndTag "dl"
  return $ DefinitionList items

definitionListItem :: GenParser Char ParserState ([Inline], [Block])
definitionListItem = try $ do
  terms <- sepEndBy1 (inlinesIn "dt") spaces
  defs <- sepEndBy1 (blocksIn "dd") spaces
  let term = intercalate [LineBreak] terms
  return (term, concat defs)

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = try $ htmlTag "p" >> inlinesTilEnd "p" >>= 
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
inline = choice [ charRef
                , strong
                , emph
                , superscript
                , subscript
                , strikeout
                , spanStrikeout
                , code
                , str
                , linebreak
                , whitespace
                , link
                , image
                , rawHtmlInline
                ] <?> "inline"

code :: GenParser Char ParserState Inline
code = try $ do 
  htmlTag "code"
  result <- manyTill anyChar (htmlEndTag "code")
  -- remove internal line breaks, leading and trailing space,
  -- and decode character references
  return $ Code $ decodeCharacterReferences $ removeLeadingTrailingSpace $ 
                  intercalate " " $ lines result

rawHtmlInline :: GenParser Char ParserState Inline
rawHtmlInline = do
  result <- htmlScript <|> htmlStyle <|> htmlComment <|> anyHtmlInlineTag
  state <- getState
  if stateParseRaw state then return (HtmlInline result) else return (Str "")

betweenTags :: [Char] -> GenParser Char ParserState [Inline]
betweenTags tag = try $ htmlTag tag >> inlinesTilEnd tag >>= 
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
  (_, attributes) <- htmlTag "span" 
  result <- case (extractAttribute "class" attributes) of
              Just "strikeout" -> inlinesTilEnd "span"
              _                -> fail "not a strikeout"
  return $ Strikeout result

whitespace :: GenParser Char st Inline
whitespace = many1 space >> return Space

-- hard line break
linebreak :: GenParser Char ParserState Inline
linebreak = htmlTag "br" >> optional newline >> return LineBreak

str :: GenParser Char st Inline
str = many1 (noneOf "<& \t\n") >>= return . Str

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
  (_, attributes) <- htmlTag "a"  
  url <- case (extractAttribute "href" attributes) of
           Just url -> return url
           Nothing  -> fail "no href"
  let title = fromMaybe "" $ extractAttribute "title" attributes
  lab <- inlinesTilEnd "a"
  return $ Link (normalizeSpaces lab) (url, title)

image :: GenParser Char ParserState Inline
image = try $ do
  (_, attributes) <- htmlTag "img" 
  url <- case (extractAttribute "src" attributes) of
           Just url -> return url
           Nothing  -> fail "no src"
  let title = fromMaybe "" $ extractAttribute "title" attributes
  let alt = fromMaybe "" (extractAttribute "alt" attributes)
  return $ Image [Str alt] (url, title)


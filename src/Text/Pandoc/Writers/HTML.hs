-- | Converts Pandoc to HTML.
module Text.Pandoc.Writers.HTML ( 
                                 writeHtml
                                ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Html ( stringToHtmlString )
import Text.Regex ( mkRegex )
import Numeric ( showHex )
import Char ( ord )
import List ( isPrefixOf )

-- | Convert Pandoc document to string in HTML format.
writeHtml :: WriterOptions -> Pandoc -> String
writeHtml options (Pandoc (Meta title authors date) blocks) = 
    let titlePrefix = writerTitlePrefix options in
    let topTitle = if not (null titlePrefix) then 
                       [Str titlePrefix] ++ (if not (null title) then [Str " - "] ++ title else [])
                   else
                       title in
    let head = if (writerStandalone options) then 
                   htmlHeader options (Meta topTitle authors date)
               else 
                   "" 
        titleBlocks = if (writerStandalone options) && (not (null title)) && 
                      (not (writerS5 options))  then
                         [RawHtml "<h1 class=\"title\">", Plain title, RawHtml "</h1>\n"]
                     else
                         []
        foot = if (writerStandalone options) then "</body>\n</html>\n" else "" 
        body = (writerIncludeBefore options) ++ 
               concatMap (blockToHtml options) (replaceReferenceLinks (titleBlocks ++ blocks)) ++
                             (writerIncludeAfter options) in
    head ++ body ++ foot

-- | Obfuscate a "mailto:" link using Javascript.
obfuscateLink :: WriterOptions -> [Inline] -> String -> String
obfuscateLink options text src =
  let text' = inlineListToHtml options text in
  let linkText = if src == ("mailto:" ++ text') then "e" else "'" ++ text' ++ "'" 
      altText = if src == ("mailto:" ++ text') then "\\1 [at] \\2" else text' ++ " (\\1 [at] \\2)" in  
  gsub "mailto:([^@]*)@(.*)" ("<script type=\"text/javascript\">h='\\2';n='\\1';e=n+'@'+h;document.write('<a href=\"mailto:'+e+'\">'+" ++ linkText  ++ "+'<\\/a>');</script><noscript>" ++ altText ++ "</noscript>") src 

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char = let num = ord char in
                 let numstr = if even num then (show num) else ("x" ++ (showHex num "")) in
                 "&#" ++ numstr ++ ";"

-- | Escape string, preserving character entities and quote.
stringToHtml :: String -> String
stringToHtml str = escapePreservingRegex stringToHtmlString (mkRegex "\"|(&[[:alnum:]]*;)") str

-- | Escape string as in 'stringToHtml' but add smart typography filter.
stringToSmartHtml :: String -> String
stringToSmartHtml = 
    let escapeDoubleQuotes = 
            gsub "(\"|&quot;)" "&rdquo;" . -- rest are right quotes
            gsub "(\"|&quot;)(&r[sd]quo;)" "&rdquo;\\2" . -- never left quo before right quo
            gsub "(&l[sd]quo;)(\"|&quot;)" "\\2&ldquo;" . -- never right quo after left quo
            gsub "([ \t])(\"|&quot;)" "\\1&ldquo;" . -- never right quo after space 
            gsub "(\"|&quot;)([^,.;:!?^) \t-])" "&ldquo;\\2" . -- "word left
            gsub "(\"|&quot;)('|`|&lsquo;)" "&rdquo;&rsquo;" . -- right if it got through last filter
            gsub "(\"|&quot;)('|`|&lsquo;)([^,.;:!?^) \t-])" "&ldquo;&lsquo;\\3" . -- "'word left
            gsub "``" "&ldquo;" .
            gsub "''" "&rdquo;"
        escapeSingleQuotes =
            gsub "'" "&rsquo;"  . -- otherwise right
            gsub "'(&r[sd]quo;)" "&rsquo;\\1" . -- never left quo before right quo
            gsub "(&l[sd]quo;)'" "\\1&lsquo;" . -- never right quo after left quo
            gsub "([ \t])'" "\\1&lsquo;" . -- never right quo after space 
            gsub "`" "&lsquo;"  . -- ` is left
            gsub "([^,.;:!?^) \t-])'" "\\1&rsquo;" .  -- word' right
            gsub "^('|`)([^,.;:!?^) \t-])" "&lsquo;\\2" . -- 'word left 
            gsub "('|`)(\"|&quot;|&ldquo;|``)" "&lsquo;&ldquo;" .  -- '"word left
            gsub "([^,.;:!?^) \t-])'(s|S)" "\\1&rsquo;\\2" . -- possessive
            gsub "([[:space:]])'([^,.;:!?^) \t-])" "\\1&lsquo;\\2" . -- 'word left
            gsub "'([0-9][0-9](s|S))" "&rsquo;\\1"  -- '80s - decade abbrevs.
        escapeDashes = gsub " ?-- ?" "&mdash;" .
                       gsub " ?--- ?" "&mdash;" .
                       gsub "([0-9])--?([0-9])" "\\1&ndash;\\2" 
        escapeEllipses = gsub "\\.\\.\\.|\\. \\. \\." "&hellip;" in
    escapeSingleQuotes . escapeDoubleQuotes . escapeDashes . escapeEllipses . stringToHtml 

-- | Escape code string as needed for HTML.
codeStringToHtml :: String -> String
codeStringToHtml [] = []
codeStringToHtml (x:xs) = case x of
    '&' -> "&amp;" ++ codeStringToHtml xs
    '<' -> "&lt;"  ++ codeStringToHtml xs
    _   -> x:(codeStringToHtml xs) 

-- | Escape string to HTML appropriate for attributes
attributeStringToHtml :: String -> String
attributeStringToHtml = gsub "\"" "&quot;"

-- | Returns an HTML header with appropriate bibliographic information.
htmlHeader :: WriterOptions -> Meta -> String
htmlHeader options (Meta title authors date) = 
    let titletext = "<title>" ++ (inlineListToHtml options title) ++ "</title>\n"
        authortext = if (null authors) then 
                         "" 
                     else 
                         "<meta name=\"author\" content=\"" ++ 
                         (joinWithSep ", " (map stringToHtml authors)) ++ "\" />\n" 
        datetext = if (date == "") then 
                       "" 
                   else 
                       "<meta name=\"date\" content=\"" ++ (stringToHtml date) ++ "\" />\n" in
    (writerHeader options) ++ authortext ++ datetext ++ titletext ++ "</head>\n<body>\n"

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> String
blockToHtml options Blank = "\n" 
blockToHtml options Null = ""
blockToHtml options (Plain lst) = inlineListToHtml options lst 
blockToHtml options (Para lst) = "<p>" ++ (inlineListToHtml options lst) ++ "</p>\n"
blockToHtml options (BlockQuote blocks) = 
    if (writerS5 options) then  -- in S5, treat list in blockquote specially
        -- if default is incremental, make it nonincremental; otherwise incremental
        let inc = not (writerIncremental options) in
        case blocks of 
          [BulletList lst]  -> blockToHtml (options {writerIncremental = inc}) (BulletList lst)
          [OrderedList lst] -> blockToHtml (options {writerIncremental = inc}) (OrderedList lst)
          otherwise         -> "<blockquote>\n" ++ (concatMap (blockToHtml options) blocks) ++ 
                               "</blockquote>\n"
    else
        "<blockquote>\n" ++ (concatMap (blockToHtml options) blocks) ++ "</blockquote>\n"
blockToHtml options (Note ref lst) = 
    let marker = "<span class=\"pandocNoteMarker\"><a name=\"note_" ++ ref ++ 
                 "\" href=\"#ref_" ++ ref ++ "\">(" ++ ref ++ ")</a></span> " in
    let contents = (concatMap (blockToHtml options) lst) in
    let contents' = case contents of
                      ('<':'p':'>':rest) -> "<p class=\"first\">" ++ marker ++ rest ++ "\n"
                      otherwise -> marker ++ contents ++ "\n" in
    "<div class=\"pandocNote\">\n" ++ contents' ++ "</div>\n"
blockToHtml options (Key _ _) = ""
blockToHtml options (CodeBlock str) = "<pre><code>" ++ (codeStringToHtml str) ++ 
                                      "\n</code></pre>\n"
blockToHtml options (RawHtml str) = str 
blockToHtml options (BulletList lst) = 
    let attribs = if (writerIncremental options) then " class=\"incremental\"" else "" in
    "<ul" ++ attribs ++ ">\n" ++ (concatMap (listItemToHtml options) lst) ++ "</ul>\n"
blockToHtml options (OrderedList lst) = 
    let attribs = if (writerIncremental options) then " class=\"incremental\"" else "" in
    "<ol" ++ attribs ++ ">\n" ++ (concatMap (listItemToHtml options) lst) ++ "</ol>\n"
blockToHtml options HorizontalRule = "<hr />\n"
blockToHtml options (Header level lst) = if ((level > 0) && (level <= 6)) then
                                             "<h" ++ (show level) ++ ">" ++ 
                                                      (inlineListToHtml options lst) ++ 
                                                      "</h" ++ (show level) ++ ">\n"
                                         else 
                                             "<p>" ++ (inlineListToHtml options lst) ++ "</p>\n"
listItemToHtml options list = "<li>" ++ (concatMap (blockToHtml options) list) ++ "</li>\n"

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> String
inlineListToHtml options lst = 
    -- consolidate adjacent Str and Space elements for more intelligent 
    -- smart typography filtering
    let lst' = consolidateList lst in
    concatMap (inlineToHtml options) lst'

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> String
inlineToHtml options (Emph lst) = "<em>" ++ (inlineListToHtml options lst) ++ "</em>"
inlineToHtml options (Strong lst) = "<strong>" ++ (inlineListToHtml options lst) ++ "</strong>"
inlineToHtml options (Code str) = "<code>" ++ (codeStringToHtml str) ++ "</code>"
inlineToHtml options (Str str) = if (writerSmart options) then 
                                     stringToSmartHtml str 
                                 else 
                                     stringToHtml str
inlineToHtml options (TeX str) = (codeStringToHtml str)
inlineToHtml options (HtmlInline str) = str
inlineToHtml options (LineBreak) = "<br />\n"
inlineToHtml options Space = " "
inlineToHtml options (Link text (Src src tit)) = 
    let title = attributeStringToHtml tit in
    if (isPrefixOf "mailto:" src) then 
        obfuscateLink options text src 
    else 
        "<a href=\"" ++ (codeStringToHtml src) ++ "\"" ++ 
                         (if tit /= "" then " title=\"" ++ title  ++ "\">" else ">") ++ 
                         (inlineListToHtml options text) ++ "</a>"
inlineToHtml options (Link text (Ref [])) = "[" ++ (inlineListToHtml options text) ++ "]" 
inlineToHtml options (Link text (Ref ref)) = "[" ++ (inlineListToHtml options text) ++ "][" ++ 
       (inlineListToHtml options ref) ++ "]"  -- this is what markdown does, for better or worse
inlineToHtml options (Image alt (Src source tit)) = 
    let title = attributeStringToHtml tit
        alternate = inlineListToHtml options alt in 
    "<img src=\"" ++ source ++ "\"" ++ 
                      (if tit /= "" then " title=\"" ++ title ++ "\"" else "") ++ 
                      (if alternate /= "" then " alt=\"" ++ alternate ++ "\"" else "") ++ ">"
inlineToHtml options (Image alternate (Ref [])) = 
    "![" ++ (inlineListToHtml options alternate) ++ "]"
inlineToHtml options (Image alternate (Ref ref)) = 
    "![" ++ (inlineListToHtml options alternate) ++ "][" ++ (inlineListToHtml options ref) ++ "]"
inlineToHtml options (NoteRef ref) = 
    "<span class=\"pandocNoteRef\"><a name=\"ref_" ++ ref ++ "\" href=\"#note_" ++ ref ++ 
                                                       "\">(" ++ ref ++ ")</a></span>"


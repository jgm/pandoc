{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
   Module      : Text.Pandoc.Writers.HTML
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to HTML.
-}
module Text.Pandoc.Writers.HTML ( writeHtml , writeHtmlString ) where
import Text.Pandoc.Definition
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Text.Pandoc.Shared
import Text.Pandoc.Templates
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Highlighting ( highlightHtml, defaultHighlightingCss )
import Text.Pandoc.XML (stripTags, escapeStringForXML)
import Network.HTTP ( urlEncode )
import Numeric ( showHex )
import Data.Char ( ord, toLower )
import Data.List ( isPrefixOf, intersperse )
import Data.Maybe ( catMaybes )
import Control.Monad.State
import Text.XHtml.Transitional hiding ( stringToHtml, unordList, ordList )
import qualified Text.XHtml.Transitional as XHtml
import Text.TeXMath
import Text.XML.Light.Output
import System.FilePath (takeExtension)

data WriterState = WriterState
    { stNotes            :: [Html]  -- ^ List of notes
    , stMath             :: Bool    -- ^ Math is used in document
    , stHighlighting     :: Bool    -- ^ Syntax highlighting is used
    , stSecNum           :: [Int]   -- ^ Number of current section
    } deriving Show

defaultWriterState :: WriterState
defaultWriterState = WriterState {stNotes= [], stMath = False, stHighlighting = False, stSecNum = []}

-- Helpers to render HTML with the appropriate function.

-- | Modified version of Text.XHtml's stringToHtml.
-- Use unicode characters wherever possible.
stringToHtml :: WriterOptions -> String -> Html
stringToHtml opts = if writerAscii opts
                       then XHtml.stringToHtml
                       else primHtml . escapeStringForXML

-- | Hard linebreak.
nl :: WriterOptions -> Html
nl opts = if writerWrapText opts
             then primHtml "\n"
             else noHtml

-- | Convert Pandoc document to Html string.
writeHtmlString :: WriterOptions -> Pandoc -> String
writeHtmlString opts d =
  let (tit, auths, date, toc, body', newvars) = evalState (pandocToHtml opts d)
                                                 defaultWriterState
  in  if writerStandalone opts
         then inTemplate opts tit auths date toc body' newvars
         else dropWhile (=='\n') $ showHtmlFragment body'

-- | Convert Pandoc document to Html structure.
writeHtml :: WriterOptions -> Pandoc -> Html
writeHtml opts d =
  let (tit, auths, date, toc, body', newvars) = evalState (pandocToHtml opts d)
                                                 defaultWriterState
  in  if writerStandalone opts
         then inTemplate opts tit auths date toc body' newvars
         else body'

-- result is (title, authors, date, toc, body, new variables)
pandocToHtml :: WriterOptions
             -> Pandoc
             -> State WriterState (Html, [Html], Html, Maybe Html, Html, [(String,String)])
pandocToHtml opts (Pandoc (Meta title' authors' date') blocks) = do
  let standalone = writerStandalone opts
  tit <- if standalone
            then inlineListToHtml opts title'
            else return noHtml
  auths <- if standalone
              then mapM (inlineListToHtml opts) authors'
              else return []
  date <- if standalone
             then inlineListToHtml opts date'
             else return noHtml
  let sects = hierarchicalize $
              if writerSlideVariant opts == NoSlides
                 then blocks
                 else case blocks of
                           (Header 1 _ : _) -> blocks
                           _                ->
                                let isL1 (Header 1 _) = True
                                    isL1 _            = False
                                    (preBlocks, rest) = break isL1 blocks
                                in  (RawBlock "html" "<div class=\"slide\">" :
                                    preBlocks) ++ (RawBlock "html" "</div>" :
                                    rest)
  toc <- if writerTableOfContents opts
            then tableOfContents opts sects
            else return Nothing
  blocks' <- liftM (toHtmlFromList . intersperse (nl opts)) $
                 mapM (elementToHtml opts) sects
  st <- get
  let notes = reverse (stNotes st)
  let thebody = blocks' +++ footnoteSection opts notes
  let  math = if stMath st
                then case writerHTMLMathMethod opts of
                           LaTeXMathML (Just url) ->
                              script !
                              [src url, thetype "text/javascript"] $ noHtml
                           MathML (Just url) ->
                              script !
                              [src url, thetype "text/javascript"] $ noHtml
                           MathJax url ->
                              script ! [src url, thetype "text/javascript"] $ noHtml
                           JsMath (Just url) ->
                              script !
                              [src url, thetype "text/javascript"] $ noHtml
                           _ -> case lookup "mathml-script" (writerVariables opts) of
                                      Just s ->
                                        script ! [thetype "text/javascript"] <<
                                           primHtml ("/*<![CDATA[*/\n" ++ s ++
                                                     "/*]]>*/\n")
                                      Nothing -> noHtml
                else noHtml
  let newvars = [("highlighting-css", defaultHighlightingCss) |
                   stHighlighting st] ++
                [("math", showHtmlFragment math) | stMath st]
  return (tit, auths, date, toc, thebody, newvars)

inTemplate :: TemplateTarget a
           => WriterOptions
           -> Html
           -> [Html]
           -> Html
           -> Maybe Html
           -> Html
           -> [(String,String)]
           -> a
inTemplate opts tit auths date toc body' newvars =
  let renderedTit = showHtmlFragment tit
      topTitle'   = stripTags renderedTit
      authors     = map (stripTags . showHtmlFragment) auths
      date'       = stripTags $ showHtmlFragment date
      variables   = writerVariables opts ++ newvars
      context     = variables ++
                    [ ("body", dropWhile (=='\n') $ showHtmlFragment body')
                    , ("pagetitle", topTitle')
                    , ("title", dropWhile (=='\n') $ showHtmlFragment tit)
                    , ("date", date')
                    , ("idprefix", writerIdentifierPrefix opts)
                    , ("slidy-url", "http://www.w3.org/Talks/Tools/Slidy2")
                    , ("s5-url", "ui/default") ] ++
                    [ ("html5","true") | writerHtml5 opts ] ++
                    (case toc of
                         Just t  -> [ ("toc", showHtmlFragment t)]
                         Nothing -> [])  ++
                    [ ("author", a) | a <- authors ]
  in  renderTemplate context $ writerTemplate opts

-- | Like Text.XHtml's identifier, but adds the writerIdentifierPrefix
prefixedId :: WriterOptions -> String -> HtmlAttr
prefixedId opts s = identifier $ writerIdentifierPrefix opts ++ s

-- | Replacement for Text.XHtml's unordList.
unordList :: WriterOptions -> ([Html] -> Html)
unordList opts items = ulist << toListItems opts items

-- | Replacement for Text.XHtml's ordList.
ordList :: WriterOptions -> ([Html] -> Html)
ordList opts items = olist << toListItems opts items

-- | Construct table of contents from list of elements.
tableOfContents :: WriterOptions -> [Element] -> State WriterState (Maybe Html)
tableOfContents _ [] = return Nothing
tableOfContents opts sects = do
  let opts'        = opts { writerIgnoreNotes = True }
  contents  <- mapM (elementToListItem opts') sects
  let tocList = catMaybes contents
  return $ if null tocList
              then Nothing
              else Just $ unordList opts tocList

-- | Convert section number to string
showSecNum :: [Int] -> String
showSecNum = concat . intersperse "." . map show

-- | Converts an Element to a list item for a table of contents,
-- retrieving the appropriate identifier from state.
elementToListItem :: WriterOptions -> Element -> State WriterState (Maybe Html)
elementToListItem _ (Blk _) = return Nothing
elementToListItem opts (Sec _ num id' headerText subsecs) = do
  let sectnum = if writerNumberSections opts
                   then (thespan ! [theclass "toc-section-number"] << showSecNum num) +++
                         stringToHtml opts" "
                   else noHtml
  txt <- liftM (sectnum +++) $ inlineListToHtml opts headerText
  subHeads <- mapM (elementToListItem opts) subsecs >>= return . catMaybes
  let subList = if null subHeads
                   then noHtml
                   else unordList opts subHeads
  return $ Just $ (anchor ! [href ("#" ++ writerIdentifierPrefix opts ++ id')] $ txt) +++ subList

-- | Convert an Element to Html.
elementToHtml :: WriterOptions -> Element -> State WriterState Html
elementToHtml opts (Blk HorizontalRule) | writerSlideVariant opts /= NoSlides =
  return $ primHtml "</div>" +++ nl opts +++ primHtml "<div class=\"slide\">"
elementToHtml opts (Blk block) = blockToHtml opts block
elementToHtml opts (Sec level num id' title' elements) = do
  modify $ \st -> st{stSecNum = num}  -- update section number
  header' <- blockToHtml opts (Header level title')
  innerContents <- mapM (elementToHtml opts) elements
  let header'' = header' !  [prefixedId opts id' |
                             not (writerStrictMarkdown opts ||
                                  writerSectionDivs opts ||
                                  writerSlideVariant opts == S5Slides)]
  let stuff = header'' : innerContents
  let slide = writerSlideVariant opts /= NoSlides && level == 1
  let stuff' =  if slide
                   then [thediv ! [theclass "slide"] <<
                          (nl opts : intersperse (nl opts) stuff ++ [nl opts])]
                   else intersperse (nl opts) stuff
  let inNl x = nl opts : x ++ [nl opts]
  return $ if writerSectionDivs opts
              then if writerHtml5 opts
                      then tag "section" ! [prefixedId opts id'] << inNl stuff'
                      else thediv ! [prefixedId opts id'] << inNl stuff'
              else toHtmlFromList stuff'

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: WriterOptions -> [Html] -> Html
footnoteSection opts notes =
  if null notes
     then noHtml
     else nl opts +++ (thediv ! [theclass "footnotes"]
          $ nl opts +++ hr +++ nl opts +++
            (olist << (notes ++ [nl opts])) +++ nl opts)


-- | Parse a mailto link; return Just (name, domain) or Nothing.
parseMailto :: String -> Maybe (String, String)
parseMailto ('m':'a':'i':'l':'t':'o':':':addr) =
  let (name', rest) = span (/='@') addr
      domain = drop 1 rest
  in  Just (name', domain)
parseMailto _ = Nothing

-- | Obfuscate a "mailto:" link.
obfuscateLink :: WriterOptions -> String -> String -> Html
obfuscateLink opts txt s | writerEmailObfuscation opts == NoObfuscation =
  anchor ! [href s] << txt
obfuscateLink opts txt s =
  let meth = writerEmailObfuscation opts
      s' = map toLower s
  in  case parseMailto s' of
        (Just (name', domain)) ->
          let domain'  = substitute "." " dot " domain
              at'      = obfuscateChar '@'
              (linkText, altText) =
                 if txt == drop 7 s' -- autolink
                    then ("'<code>'+e+'</code>'", name' ++ " at " ++ domain')
                    else ("'" ++ txt ++ "'", txt ++ " (" ++ name' ++ " at " ++
                          domain' ++ ")")
          in  case meth of
                ReferenceObfuscation ->
                     -- need to use primHtml or &'s are escaped to &amp; in URL
                     primHtml $ "<a href=\"" ++ (obfuscateString s')
                     ++ "\">" ++ (obfuscateString txt) ++ "</a>"
                JavascriptObfuscation ->
                     (script ! [thetype "text/javascript"] $
                     primHtml ("\n<!--\nh='" ++
                     obfuscateString domain ++ "';a='" ++ at' ++ "';n='" ++
                     obfuscateString name' ++ "';e=n+a+h;\n" ++
                     "document.write('<a h'+'ref'+'=\"ma'+'ilto'+':'+e+'\">'+" ++
                     linkText  ++ "+'<\\/'+'a'+'>');\n// -->\n")) +++
                     noscript (primHtml $ obfuscateString altText)
                _ -> error $ "Unknown obfuscation method: " ++ show meth
        _ -> anchor ! [href s] $ stringToHtml opts txt  -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char =
  let num    = ord char
      numstr = if even num then show num else "x" ++ showHex num ""
  in  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = concatMap obfuscateChar . decodeCharacterReferences

attrsToHtml :: WriterOptions -> Attr -> [HtmlAttr]
attrsToHtml opts (id',classes',keyvals) =
  [theclass (unwords classes') | not (null classes')] ++
  [prefixedId opts id' | not (null id')] ++
  map (\(x,y) -> strAttr x y) keyvals

imageExts :: [String]
imageExts = [ "art", "bmp", "cdr", "cdt", "cpt", "cr2", "crw", "djvu", "erf",
              "gif", "ico", "ief", "jng", "jpg", "jpeg", "nef", "orf", "pat", "pbm",
              "pcx", "pgm", "png", "pnm", "ppm", "psd", "ras", "rgb", "svg", "tiff",
              "wbmp", "xbm", "xpm", "xwd" ]

treatAsImage :: FilePath -> Bool
treatAsImage fp =
  let ext = map toLower $ drop 1 $ takeExtension fp
  in  null ext || ext `elem` imageExts

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> State WriterState Html
blockToHtml _ Null = return noHtml
blockToHtml opts (Plain lst) = inlineListToHtml opts lst
blockToHtml opts (Para [Image txt (s,tit)]) = do
  img <- inlineToHtml opts (Image txt (s,tit))
  capt <- inlineListToHtml opts txt
  return $ if writerHtml5 opts
              then tag "figure" <<
                    [nl opts, img, tag "figcaption" << capt, nl opts]
              else thediv ! [theclass "figure"] <<
                    [nl opts, img, paragraph ! [theclass "caption"] << capt,
                    nl opts]
blockToHtml opts (Para lst) = do
  contents <- inlineListToHtml opts lst
  return $ paragraph contents
blockToHtml _ (RawBlock "html" str) = return $ primHtml str
blockToHtml _ (RawBlock _ _) = return noHtml
blockToHtml _ (HorizontalRule) = return hr
blockToHtml opts (CodeBlock (id',classes,keyvals) rawCode) = do
  let classes' = if writerLiterateHaskell opts
                    then classes
                    else filter (/= "literate") classes
  case highlightHtml False (id',classes',keyvals) rawCode of
         Left _  -> -- change leading newlines into <br /> tags, because some
                    -- browsers ignore leading newlines in pre blocks
                    let (leadingBreaks, rawCode') = span (=='\n') rawCode
                        attrs = attrsToHtml opts (id', classes', keyvals)
                        addBird = if "literate" `elem` classes'
                                     then unlines . map ("> " ++) . lines
                                     else unlines . lines
                    in  return $ pre ! attrs $ thecode <<
                                 (replicate (length leadingBreaks) br +++
                                 [stringToHtml opts $ addBird rawCode'])
         Right h -> modify (\st -> st{ stHighlighting = True }) >>
                    return h
blockToHtml opts (BlockQuote blocks) =
  -- in S5, treat list in blockquote specially
  -- if default is incremental, make it nonincremental;
  -- otherwise incremental
  if writerSlideVariant opts /= NoSlides
     then let inc = not (writerIncremental opts) in
          case blocks of
             [BulletList lst]  -> blockToHtml (opts {writerIncremental = inc})
                                  (BulletList lst)
             [OrderedList attribs lst] ->
                                  blockToHtml (opts {writerIncremental = inc})
                                  (OrderedList attribs lst)
             _                 -> do contents <- blockListToHtml opts blocks
                                     return $ blockquote (nl opts +++
                                               contents +++ nl opts)
     else do
       contents <- blockListToHtml opts blocks
       return $ blockquote (nl opts +++ contents +++ nl opts)
blockToHtml opts (Header level lst) = do
  contents <- inlineListToHtml opts lst
  secnum <- liftM stSecNum get
  let contents' = if writerNumberSections opts
                     then (thespan ! [theclass "header-section-number"] << showSecNum secnum) +++
                            stringToHtml opts " " +++ contents
                     else contents
  let contents''  = if writerTableOfContents opts
                       then anchor ! [href $ "#" ++ writerIdentifierPrefix opts ++ "TOC"] $ contents'
                       else contents'
  return $ (case level of
              1 -> h1 contents''
              2 -> h2 contents''
              3 -> h3 contents''
              4 -> h4 contents''
              5 -> h5 contents''
              6 -> h6 contents''
              _ -> paragraph contents'')
blockToHtml opts (BulletList lst) = do
  contents <- mapM (blockListToHtml opts) lst
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else []
  return $ (unordList opts contents) ! attribs
blockToHtml opts (OrderedList (startnum, numstyle, _) lst) = do
  contents <- mapM (blockListToHtml opts) lst
  let numstyle' = camelCaseToHyphenated $ show numstyle
  let attribs = (if writerIncremental opts
                   then [theclass "incremental"]
                   else []) ++
                (if startnum /= 1
                   then [start startnum]
                   else []) ++
                (if numstyle /= DefaultStyle
                   then if writerHtml5 opts
                           then [strAttr "type" $
                                 case numstyle of
                                      Decimal    -> "1"
                                      LowerAlpha -> "a"
                                      UpperAlpha -> "A"
                                      LowerRoman -> "i"
                                      UpperRoman -> "I"
                                      _          -> "1"]
                           else [thestyle $ "list-style-type: " ++
                                   numstyle']
                   else [])
  return $ (ordList opts contents) ! attribs
blockToHtml opts (DefinitionList lst) = do
  contents <- mapM (\(term, defs) ->
                  do term' <- liftM (dterm <<) $ inlineListToHtml opts term
                     defs' <- mapM ((liftM (\x -> ddef << (x +++ nl opts))) .
                                    blockListToHtml opts) defs
                     return $ nl opts : term' : nl opts : defs') lst
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else []
  return $ dlist ! attribs << (concat contents +++ nl opts)
blockToHtml opts (Table capt aligns widths headers rows') = do
  captionDoc <- if null capt
                   then return noHtml
                   else do
                     cs <- inlineListToHtml opts capt
                     return $ caption cs +++ nl opts
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let widthAttrs w = if writerHtml5 opts
                        then [thestyle $ "width: " ++ percent w]
                        else [width $ percent w]
  let coltags = if all (== 0.0) widths
                   then noHtml
                   else concatHtml $ map
                         (\w -> (col ! (widthAttrs w)) noHtml +++ nl opts)
                         widths
  head' <- if all null headers
              then return noHtml
              else do
                contents <- tableRowToHtml opts aligns 0 headers
                return $ thead << (nl opts +++ contents) +++ nl opts
  body' <- liftM (\x -> tbody << (nl opts +++ x)) $
               zipWithM (tableRowToHtml opts aligns) [1..] rows'
  return $ table $ nl opts +++ captionDoc +++ coltags +++ head' +++
                   body' +++ nl opts

tableRowToHtml :: WriterOptions
               -> [Alignment]
               -> Int
               -> [[Block]]
               -> State WriterState Html
tableRowToHtml opts aligns rownum cols' = do
  let mkcell = if rownum == 0 then th else td
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToHtml opts mkcell alignment item)
            aligns cols'
  return $ (tr ! [theclass rowclass] $ nl opts +++ toHtmlFromList cols'')
          +++ nl opts

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToHtml :: WriterOptions
                -> (Html -> Html)
                -> Alignment
                -> [Block]
                -> State WriterState Html
tableItemToHtml opts tag' align' item = do
  contents <- blockListToHtml opts item
  let alignAttrs = if writerHtml5 opts
                      then [thestyle $ "align: " ++ alignmentToString align']
                      else [align $ alignmentToString align']
  return $ (tag' ! alignAttrs) contents +++ nl opts

toListItems :: WriterOptions -> [Html] -> [Html]
toListItems opts items = map (toListItem opts) items ++ [nl opts]

toListItem :: WriterOptions -> Html -> Html
toListItem opts item = nl opts +++ li item

blockListToHtml :: WriterOptions -> [Block] -> State WriterState Html
blockListToHtml opts lst =
  mapM (blockToHtml opts) lst >>=
  return . toHtmlFromList . intersperse (nl opts)

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> State WriterState Html
inlineListToHtml opts lst =
  mapM (inlineToHtml opts) lst >>= return . toHtmlFromList

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> State WriterState Html
inlineToHtml opts inline =
  case inline of
    (Str str)        -> return $ stringToHtml opts str
    (Space)          -> return $ stringToHtml opts " "
    (LineBreak)      -> return br
    (EmDash)         -> return $ stringToHtml opts "—"
    (EnDash)         -> return $ stringToHtml opts "–"
    (Ellipses)       -> return $ stringToHtml opts "…"
    (Apostrophe)     -> return $ stringToHtml opts "’"
    (Emph lst)       -> inlineListToHtml opts lst >>= return . emphasize
    (Strong lst)     -> inlineListToHtml opts lst >>= return . strong
    (Code attr str)  -> case highlightHtml True attr str of
                             Left _  -> return
                                        $ thecode ! (attrsToHtml opts attr)
                                        $ stringToHtml opts str
                             Right h -> return h
    (Strikeout lst)  -> inlineListToHtml opts lst >>=
                        return . (thespan ! [thestyle "text-decoration: line-through;"])
    (SmallCaps lst)   -> inlineListToHtml opts lst >>=
                         return . (thespan ! [thestyle "font-variant: small-caps;"])
    (Superscript lst) -> inlineListToHtml opts lst >>= return . sup
    (Subscript lst)   -> inlineListToHtml opts lst >>= return . sub
    (Quoted quoteType lst) ->
                        let (leftQuote, rightQuote) = case quoteType of
                              SingleQuote -> (stringToHtml opts "‘",
                                              stringToHtml opts "’")
                              DoubleQuote -> (stringToHtml opts "“",
                                              stringToHtml opts "”")
                        in  do contents <- inlineListToHtml opts lst
                               return $ leftQuote +++ contents +++ rightQuote
    (Math t str) ->     modify (\st -> st {stMath = True}) >>
                        (case writerHTMLMathMethod opts of
                               LaTeXMathML _ ->
                                  -- putting LaTeXMathML in container with class "LaTeX" prevents
                                  -- non-math elements on the page from being treated as math by
                                  -- the javascript
                                  return $ thespan ! [theclass "LaTeX"] $
                                         case t of
                                           InlineMath  -> primHtml ("$" ++ str ++ "$")
                                           DisplayMath -> primHtml ("$$" ++ str ++ "$$")
                               JsMath _ -> do
                                  let m = primHtml str
                                  return $ case t of
                                           InlineMath -> thespan ! [theclass "math"] $ m
                                           DisplayMath -> thediv ! [theclass "math"] $ m
                               WebTeX url -> do
                                  let m = image ! [src (url ++ urlEncode str),
                                                         alt str, title str]
                                  return $ case t of
                                            InlineMath  -> m
                                            DisplayMath -> br +++ m +++ br
                               GladTeX ->
                                  return $ case t of
                                             InlineMath -> primHtml $ "<EQ ENV=\"math\">" ++ str ++ "</EQ>"
                                             DisplayMath -> primHtml $ "<EQ ENV=\"displaymath\">" ++ str ++ "</EQ>"
                               MathML _ -> do
                                  let dt = if t == InlineMath
                                              then DisplayInline
                                              else DisplayBlock
                                  let conf = useShortEmptyTags (const False)
                                               defaultConfigPP
                                  case texMathToMathML dt str of
                                        Right r -> return $ primHtml $
                                                    ppcElement conf r
                                        Left  _ -> inlineListToHtml opts
                                                   (readTeXMath str) >>= return .
                                                     (thespan !  [theclass "math"])
                               MathJax _ -> return $ primHtml $
                                  case t of
                                    InlineMath  -> "\\(" ++ str ++ "\\)"
                                    DisplayMath -> "\\[" ++ str ++ "\\]"
                               PlainMath -> do
                                  x <- inlineListToHtml opts (readTeXMath str)
                                  let m = thespan ! [theclass "math"] $ x
                                  return  $ case t of
                                             InlineMath  -> m
                                             DisplayMath -> br +++ m +++ br )
    (RawInline "latex" str) -> case writerHTMLMathMethod opts of
                               LaTeXMathML _ -> do modify (\st -> st {stMath = True})
                                                   return $ primHtml str
                               _             -> return noHtml
    (RawInline "html" str) -> return $ primHtml str
    (RawInline _ _) -> return noHtml
    (Link [Code _ str] (s,_)) | "mailto:" `isPrefixOf` s ->
                        return $ obfuscateLink opts str s
    (Link txt (s,_)) | "mailto:" `isPrefixOf` s -> do
                        linkText <- inlineListToHtml opts txt
                        return $ obfuscateLink opts (show linkText) s
    (Link txt (s,tit)) -> do
                        linkText <- inlineListToHtml opts txt
                        return $ anchor ! ([href s] ++
                                 if null tit then [] else [title tit]) $
                                 linkText
    (Image txt (s,tit)) | treatAsImage s -> do
                        let alternate' = stringify txt
                        let attributes = [src s] ++
                                         (if null tit
                                            then []
                                            else [title tit]) ++
                                         if null txt
                                            then []
                                            else [alt alternate']
                        return $ image ! attributes
                        -- note:  null title included, as in Markdown.pl
    (Image _ (s,tit)) -> do
                        let attributes = [src s] ++
                                         (if null tit
                                            then []
                                            else [title tit])
                        return $ itag "embed" ! attributes
                        -- note:  null title included, as in Markdown.pl
    (Note contents)          -> do
                        st <- get
                        let notes = stNotes st
                        let number = (length notes) + 1
                        let ref = show number
                        htmlContents <- blockListToNote opts ref contents
                        -- push contents onto front of notes
                        put $ st {stNotes = (htmlContents:notes)}
                        return $ sup <<
                                 anchor ! [href ("#" ++ writerIdentifierPrefix opts ++ "fn" ++ ref),
                                           theclass "footnoteRef",
                                           prefixedId opts ("fnref" ++ ref)] << ref
    (Cite _ il)  -> inlineListToHtml opts il

blockListToNote :: WriterOptions -> String -> [Block] -> State WriterState Html
blockListToNote opts ref blocks =
  -- If last block is Para or Plain, include the backlink at the end of
  -- that block. Otherwise, insert a new Plain block with the backlink.
  let backlink = [RawInline "html" $ " <a href=\"#" ++ writerIdentifierPrefix opts ++ "fnref" ++ ref ++
                 "\" class=\"footnoteBackLink\">" ++
                 (if writerAscii opts then "&#8617;" else "↩") ++ "</a>"]
      blocks'  = if null blocks
                    then []
                    else let lastBlock   = last blocks
                             otherBlocks = init blocks
                         in  case lastBlock of
                                  (Para lst)  -> otherBlocks ++
                                                 [Para (lst ++ backlink)]
                                  (Plain lst) -> otherBlocks ++
                                                 [Plain (lst ++ backlink)]
                                  _           -> otherBlocks ++ [lastBlock,
                                                 Plain backlink]
  in  do contents <- blockListToHtml opts blocks'
         return $ nl opts +++ (li ! [prefixedId opts ("fn" ++ ref)]) contents

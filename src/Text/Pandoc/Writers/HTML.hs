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
import Text.XHtml.Transitional hiding ( stringToHtml )
import Text.TeXMath
import Text.XML.Light.Output

data WriterState = WriterState
    { stNotes            :: [Html]  -- ^ List of notes
    , stMath             :: Bool    -- ^ Math is used in document
    , stHighlighting     :: Bool    -- ^ Syntax highlighting is used
    , stSecNum           :: [Int]   -- ^ Number of current section
    } deriving Show

defaultWriterState :: WriterState
defaultWriterState = WriterState {stNotes= [], stMath = False, stHighlighting = False, stSecNum = []}

-- Helpers to render HTML with the appropriate function.

renderFragment :: (HTML html) => WriterOptions -> html -> String
renderFragment opts = if writerWrapText opts
                         then renderHtmlFragment
                         else showHtmlFragment

-- | Modified version of Text.XHtml's stringToHtml.
-- Use unicode characters wherever possible.
stringToHtml :: String -> Html
stringToHtml = primHtml . escapeStringForXML

-- | Convert Pandoc document to Html string.
writeHtmlString :: WriterOptions -> Pandoc -> String
writeHtmlString opts d =
  let (tit, auths, date, toc, body', newvars) = evalState (pandocToHtml opts d)
                                                 defaultWriterState
  in  if writerStandalone opts
         then inTemplate opts tit auths date toc body' newvars
         else renderFragment opts body'

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
  let sects = hierarchicalize blocks
  toc <- if writerTableOfContents opts 
            then tableOfContents opts sects
            else return Nothing
  let startSlide = RawHtml "<div class=\"slide\">\n"
      endSlide   = RawHtml "</div>\n"
  let cutUp (HorizontalRule : Header 1 ys : xs) = cutUp (Header 1 ys : xs)
      cutUp (HorizontalRule : xs) = [endSlide, startSlide] ++ cutUp xs
      cutUp (Header 1 ys : xs)    = [endSlide, startSlide] ++
                                    (Header 1 ys : cutUp xs)
      cutUp (x:xs)                = x : cutUp xs
      cutUp []                    = [] 
  let slides = case blocks of
                (HorizontalRule : xs) -> [startSlide] ++ cutUp xs ++ [endSlide]
                (Header 1 ys : xs)    -> [startSlide, Header 1 ys] ++
                                           cutUp xs ++ [endSlide]
                _                     -> [startSlide] ++ cutUp blocks ++
                                           [endSlide]
  blocks' <- liftM toHtmlFromList $
              if writerSlideVariant opts `elem` [SlidySlides, S5Slides]
                 then mapM (blockToHtml opts) slides
                 else mapM (elementToHtml opts) sects
  st <- get
  let notes = reverse (stNotes st)
  let thebody = blocks' +++ footnoteSection notes
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
                                           primHtml s
                                      Nothing -> noHtml
                else noHtml
  let newvars = [("highlighting-css", defaultHighlightingCss) |
                   stHighlighting st] ++
                [("math", renderHtmlFragment math) | stMath st]
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
                    [ ("body", renderHtmlFragment body')
                    , ("pagetitle", topTitle')
                    , ("title", renderHtmlFragment tit)
                    , ("date", date') ] ++
                    (case toc of
                         Just t  -> [ ("toc", renderHtmlFragment t)]
                         Nothing -> [])  ++
                    [ ("author", a) | a <- authors ]
  in  renderTemplate context $ writerTemplate opts

-- | Like Text.XHtml's identifier, but adds the writerIdentifierPrefix
prefixedId :: WriterOptions -> String -> HtmlAttr
prefixedId opts s = identifier $ writerIdentifierPrefix opts ++ s

-- | Construct table of contents from list of elements.
tableOfContents :: WriterOptions -> [Element] -> State WriterState (Maybe Html)
tableOfContents _ [] = return Nothing
tableOfContents opts sects = do
  let opts'        = opts { writerIgnoreNotes = True }
  contents  <- mapM (elementToListItem opts') sects
  let tocList = catMaybes contents
  return $ if null tocList
              then Nothing
              else Just $ thediv ! [prefixedId opts' "TOC"] $ unordList tocList

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
                         stringToHtml " "
                   else noHtml
  txt <- liftM (sectnum +++) $ inlineListToHtml opts headerText
  subHeads <- mapM (elementToListItem opts) subsecs >>= return . catMaybes
  let subList = if null subHeads
                   then noHtml
                   else unordList subHeads
  return $ Just $ (anchor ! [href ("#" ++ writerIdentifierPrefix opts ++ id')] $ txt) +++ subList

-- | Convert an Element to Html.
elementToHtml :: WriterOptions -> Element -> State WriterState Html
elementToHtml opts (Blk block) = blockToHtml opts block 
elementToHtml opts (Sec level num id' title' elements) = do
  innerContents <- mapM (elementToHtml opts) elements
  modify $ \st -> st{stSecNum = num}  -- update section number
  header' <- blockToHtml opts (Header level title')
  let slides = writerSlideVariant opts `elem` [SlidySlides, S5Slides]
  let header'' = header' !  [prefixedId opts id' |
                             not (writerStrictMarkdown opts ||
                                  writerSectionDivs opts || slides)]
  let stuff = header'' : innerContents
  return $ if slides   -- S5 gets confused by the extra divs around sections
              then toHtmlFromList stuff
              else if writerSectionDivs opts
                      then thediv ! [prefixedId opts id'] << stuff
                      else toHtmlFromList stuff

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: [Html] -> Html
footnoteSection notes =
  if null notes 
     then noHtml
     else thediv ! [theclass "footnotes"] $ hr +++ (olist << notes)


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
        _ -> anchor ! [href s] $ stringToHtml txt  -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char = 
  let num    = ord char
      numstr = if even num then show num else "x" ++ showHex num ""
  in  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = concatMap obfuscateChar . decodeCharacterReferences

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> State WriterState Html
blockToHtml _ Null = return $ noHtml 
blockToHtml opts (Plain lst) = inlineListToHtml opts lst
blockToHtml opts (Para [Image txt (s,tit)]) = do
  img <- inlineToHtml opts (Image txt (s,tit))
  capt <- inlineListToHtml opts txt
  return $ thediv ! [theclass "figure"] <<
             [img, paragraph ! [theclass "caption"] << capt]
blockToHtml opts (Para lst) = inlineListToHtml opts lst >>= (return . paragraph)
blockToHtml _ (RawHtml str) = return $ primHtml str
blockToHtml _ (HorizontalRule) = return $ hr
blockToHtml opts (CodeBlock (id',classes,keyvals) rawCode) = do
  let classes' = if writerLiterateHaskell opts
                    then classes
                    else filter (/= "literate") classes
  case highlightHtml (id',classes',keyvals) rawCode of
         Left _  -> -- change leading newlines into <br /> tags, because some
                    -- browsers ignore leading newlines in pre blocks
                    let (leadingBreaks, rawCode') = span (=='\n') rawCode
                        attrs = [theclass (unwords classes') | not (null classes')] ++
                                [prefixedId opts id' | not (null id')] ++
                                map (\(x,y) -> strAttr x y) keyvals
                        addBird = if "literate" `elem` classes'
                                     then unlines . map ("> " ++) . lines
                                     else unlines . lines
                    in  return $ pre ! attrs $ thecode <<
                                 (replicate (length leadingBreaks) br +++
                                 [stringToHtml $ addBird rawCode'])
         Right h -> modify (\st -> st{ stHighlighting = True }) >> return h
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
             _                 -> blockListToHtml opts blocks >>= 
                                  (return . blockquote)
     else blockListToHtml opts blocks >>= (return . blockquote)
blockToHtml opts (Header level lst) = do 
  contents <- inlineListToHtml opts lst
  secnum <- liftM stSecNum get
  let contents' = if writerNumberSections opts
                     then (thespan ! [theclass "header-section-number"] << showSecNum secnum) +++
                            stringToHtml " " +++ contents
                     else contents
  let contents''  = if writerTableOfContents opts
                       then anchor ! [href $ "#" ++ writerIdentifierPrefix opts ++ "TOC"] $ contents'
                       else contents'
  return $ case level of
              1 -> h1 contents''
              2 -> h2 contents''
              3 -> h3 contents''
              4 -> h4 contents''
              5 -> h5 contents''
              6 -> h6 contents''
              _ -> paragraph contents''
blockToHtml opts (BulletList lst) = do
  contents <- mapM (blockListToHtml opts) lst
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else []
  return $ unordList ! attribs $ contents
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
                   then [thestyle $ "list-style-type: " ++ numstyle' ++ ";"]
                   else [])
  return $ ordList ! attribs $ contents
blockToHtml opts (DefinitionList lst) = do
  contents <- mapM (\(term, defs) ->
                  do term' <- liftM (dterm <<) $ inlineListToHtml opts term
                     defs' <- mapM (liftM (ddef <<) . blockListToHtml opts) defs
                     return $ term' : defs') lst
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else []
  return $ dlist ! attribs << concat contents
blockToHtml opts (Table capt aligns widths headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return noHtml
                   else inlineListToHtml opts capt >>= return . caption
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then noHtml
                   else concatHtml $ map
                         (\w -> col ! [width $ percent w] $ noHtml) widths
  head' <- if all null headers
              then return noHtml
              else liftM (thead <<) $ tableRowToHtml opts alignStrings 0 headers
  body' <- liftM (tbody <<) $
               zipWithM (tableRowToHtml opts alignStrings) [1..] rows'
  return $ table $ captionDoc +++ coltags +++ head' +++ body'

tableRowToHtml :: WriterOptions
               -> [String]
               -> Int
               -> [[Block]]
               -> State WriterState Html
tableRowToHtml opts alignStrings rownum cols' = do
  let mkcell = if rownum == 0 then th else td
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith 
            (\alignment item -> tableItemToHtml opts mkcell alignment item) 
            alignStrings cols'
  return $ tr ! [theclass rowclass] $ toHtmlFromList cols''

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToHtml :: WriterOptions
                -> (Html -> Html)
                -> [Char]
                -> [Block]
                -> State WriterState Html
tableItemToHtml opts tag' align' item = do
  contents <- blockListToHtml opts item
  return $ tag' ! [align align'] $ contents

blockListToHtml :: WriterOptions -> [Block] -> State WriterState Html
blockListToHtml opts lst = 
  mapM (blockToHtml opts) lst >>= return . toHtmlFromList

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> State WriterState Html
inlineListToHtml opts lst = 
  mapM (inlineToHtml opts) lst >>= return . toHtmlFromList

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> State WriterState Html
inlineToHtml opts inline =
  case inline of  
    (Str str)        -> return $ stringToHtml str
    (Space)          -> return $ stringToHtml " "
    (LineBreak)      -> return br
    (EmDash)         -> return $ stringToHtml "—"
    (EnDash)         -> return $ stringToHtml "–"
    (Ellipses)       -> return $ stringToHtml "…"
    (Apostrophe)     -> return $ stringToHtml "’"
    (Emph lst)       -> inlineListToHtml opts lst >>= return . emphasize
    (Strong lst)     -> inlineListToHtml opts lst >>= return . strong
    (Code str)       -> return $ thecode << str
    (Strikeout lst)  -> inlineListToHtml opts lst >>=
                        return . (thespan ! [thestyle "text-decoration: line-through;"])
    (SmallCaps lst)   -> inlineListToHtml opts lst >>=
                         return . (thespan ! [thestyle "font-variant: small-caps;"])
    (Superscript lst) -> inlineListToHtml opts lst >>= return . sup
    (Subscript lst)   -> inlineListToHtml opts lst >>= return . sub
    (Quoted quoteType lst) ->
                        let (leftQuote, rightQuote) = case quoteType of
                              SingleQuote -> (stringToHtml "‘",
                                              stringToHtml "’")
                              DoubleQuote -> (stringToHtml "“",
                                              stringToHtml "”")
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
    (TeX str)        -> case writerHTMLMathMethod opts of
                              LaTeXMathML _ -> do modify (\st -> st {stMath = True})
                                                  return $ primHtml str
                              _             -> return noHtml
    (HtmlInline str) -> return $ primHtml str 
    (Link [Code str] (s,_)) | "mailto:" `isPrefixOf` s ->
                        return $ obfuscateLink opts str s
    (Link txt (s,_)) | "mailto:" `isPrefixOf` s -> do
                        linkText <- inlineListToHtml opts txt  
                        return $ obfuscateLink opts (show linkText) s
    (Link txt (s,tit)) -> do
                        linkText <- inlineListToHtml opts txt
                        return $ anchor ! ([href s] ++ 
                                 if null tit then [] else [title tit]) $ 
                                 linkText
    (Image txt (s,tit)) -> do
                        alternate <- inlineListToHtml opts txt
                        let alternate' = renderFragment opts alternate
                        let attributes = [src s] ++
                                         (if null tit 
                                            then [] 
                                            else [title tit]) ++ 
                                         if null txt 
                                            then [] 
                                            else [alt alternate']
                        return $ image ! attributes 
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
  let backlink = [HtmlInline $ " <a href=\"#" ++ writerIdentifierPrefix opts ++ "fnref" ++ ref ++ 
                 "\" class=\"footnoteBackLink\"" ++
                 " title=\"Jump back to footnote " ++ ref ++ "\">↩</a>"]
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
         return $ li ! [prefixedId opts ("fn" ++ ref)] $ contents


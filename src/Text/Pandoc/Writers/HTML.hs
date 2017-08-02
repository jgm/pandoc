{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to HTML.
-}
module Text.Pandoc.Writers.HTML (
  writeHtml4,
  writeHtml4String,
  writeHtml5,
  writeHtml5String,
  writeHtmlStringForEPUB,
  writeS5,
  writeSlidy,
  writeSlideous,
  writeDZSlides,
  writeRevealJs
  ) where
import Control.Monad.State.Strict
import Data.Char (ord, toLower)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.List (intersperse, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import Data.String (fromString)
import Network.HTTP (urlEncode)
import Network.URI (URI (..), parseURIReference, unEscapeString)
import Numeric (showHex)
import Text.Blaze.Html hiding (contents)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatHtmlBlock, formatHtmlInline, highlight,
                                 styleToCss)
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Slides
import Text.Pandoc.Templates
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML (escapeStringForXML, fromEntities)
#if MIN_VERSION_blaze_markup(0,6,3)
#else
import Text.Blaze.Internal (preEscapedString, preEscapedText)
#endif
#if MIN_VERSION_blaze_html(0,5,1)
import qualified Text.Blaze.XHtml5 as H5
#else
import qualified Text.Blaze.Html5 as H5
#endif
import Control.Monad.Except (throwError)
import Data.Aeson (Value)
import System.FilePath (takeExtension, takeBaseName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.TeXMath
import Text.XML.Light (elChildren, unode, unqual)
import qualified Text.XML.Light as XML
import Text.XML.Light.Output

data WriterState = WriterState
    { stNotes        :: [Html]  -- ^ List of notes
    , stMath         :: Bool    -- ^ Math is used in document
    , stQuotes       :: Bool    -- ^ <q> tag is used
    , stHighlighting :: Bool    -- ^ Syntax highlighting is used
    , stSecNum       :: [Int]   -- ^ Number of current section
    , stElement      :: Bool    -- ^ Processing an Element
    , stHtml5        :: Bool    -- ^ Use HTML5
    , stEPUBVersion  :: Maybe EPUBVersion -- ^ EPUB version if for epub
    , stSlideVariant :: HTMLSlideVariant
    }

defaultWriterState :: WriterState
defaultWriterState = WriterState {stNotes= [], stMath = False, stQuotes = False,
                                  stHighlighting = False, stSecNum = [],
                                  stElement = False, stHtml5 = False,
                                  stEPUBVersion = Nothing,
                                  stSlideVariant = NoSlides}

-- Helpers to render HTML with the appropriate function.

strToHtml :: String -> Html
strToHtml ('\'':xs) = preEscapedString "\'" `mappend` strToHtml xs
strToHtml xs@(_:_)  = case break (=='\'') xs of
                           (_ ,[]) -> toHtml xs
                           (ys,zs) -> toHtml ys `mappend` strToHtml zs
strToHtml [] = ""

-- | Hard linebreak.
nl :: WriterOptions -> Html
nl opts = if writerWrapText opts == WrapNone
             then mempty
             else preEscapedString "\n"

-- | Convert Pandoc document to Html 5 string.
writeHtml5String :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHtml5String = writeHtmlString'
                      defaultWriterState{ stHtml5 = True }

-- | Convert Pandoc document to Html 5 structure.
writeHtml5 :: PandocMonad m => WriterOptions -> Pandoc -> m Html
writeHtml5 = writeHtml' defaultWriterState{ stHtml5 = True }

-- | Convert Pandoc document to Html 4 string.
writeHtml4String :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHtml4String = writeHtmlString'
                      defaultWriterState{ stHtml5 = False }

-- | Convert Pandoc document to Html 4 structure.
writeHtml4 :: PandocMonad m => WriterOptions -> Pandoc -> m Html
writeHtml4 = writeHtml' defaultWriterState{ stHtml5 = False }

-- | Convert Pandoc document to Html appropriate for an epub version.
writeHtmlStringForEPUB :: PandocMonad m
                       => EPUBVersion -> WriterOptions -> Pandoc
                       -> m Text
writeHtmlStringForEPUB version o = writeHtmlString'
                      defaultWriterState{ stHtml5 = version == EPUB3,
                                          stEPUBVersion = Just version } o

-- | Convert Pandoc document to Reveal JS HTML slide show.
writeRevealJs :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeRevealJs = writeHtmlSlideShow' RevealJsSlides

-- | Convert Pandoc document to S5 HTML slide show.
writeS5 :: PandocMonad m
        => WriterOptions -> Pandoc -> m Text
writeS5 = writeHtmlSlideShow' S5Slides

-- | Convert Pandoc document to Slidy HTML slide show.
writeSlidy :: PandocMonad m
           => WriterOptions -> Pandoc -> m Text
writeSlidy = writeHtmlSlideShow' SlidySlides

-- | Convert Pandoc document to Slideous HTML slide show.
writeSlideous :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeSlideous = writeHtmlSlideShow' SlideousSlides

-- | Convert Pandoc document to DZSlides HTML slide show.
writeDZSlides :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeDZSlides = writeHtmlSlideShow' DZSlides

writeHtmlSlideShow' :: PandocMonad m
                    => HTMLSlideVariant -> WriterOptions -> Pandoc -> m Text
writeHtmlSlideShow' variant = writeHtmlString'
    defaultWriterState{ stSlideVariant = variant
                      , stHtml5 = case variant of
                                       RevealJsSlides -> True
                                       S5Slides       -> False
                                       SlidySlides    -> False
                                       DZSlides       -> True
                                       SlideousSlides -> False
                                       NoSlides       -> False
                      }

renderHtml' :: Html -> Text
renderHtml' = TL.toStrict . renderHtml

writeHtmlString' :: PandocMonad m
                 => WriterState -> WriterOptions -> Pandoc -> m Text
writeHtmlString' st opts d = do
  (body, context) <- evalStateT (pandocToHtml opts d) st
  case writerTemplate opts of
       Nothing -> return $ renderHtml' body
       Just tpl -> do
         -- warn if empty lang
         when (isNothing (getField "lang" context :: Maybe String)) $
           report NoLangSpecified
         -- check for empty pagetitle
         context' <-
            case getField "pagetitle" context of
                 Just (s :: String) | not (null s) -> return context
                 _ -> do
                   let fallback = fromMaybe "Untitled" $ takeBaseName <$>
                           lookup "sourcefile" (writerVariables opts)
                   report $ NoTitleElement fallback
                   return $ resetField "pagetitle" fallback context
         renderTemplate' tpl $
                    defField "body" (renderHtml' body) context'

writeHtml' :: PandocMonad m => WriterState -> WriterOptions -> Pandoc -> m Html
writeHtml' st opts d = do
  case writerTemplate opts of
       Just _ -> preEscapedText <$> writeHtmlString' st opts d
       Nothing  -> do
        (body, _) <- evalStateT (pandocToHtml opts d) st
        return body

-- result is (title, authors, date, toc, body, new variables)
pandocToHtml :: PandocMonad m
             => WriterOptions
             -> Pandoc
             -> StateT WriterState m (Html, Value)
pandocToHtml opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap renderHtml' . blockListToHtml opts)
              (fmap renderHtml' . inlineListToHtml opts)
              meta
  let stringifyHTML = escapeStringForXML . stringify
  let authsMeta = map stringifyHTML $ docAuthors meta
  let dateMeta  = stringifyHTML $ docDate meta
  let slideLevel = fromMaybe (getSlideLevel blocks) $ writerSlideLevel opts
  slideVariant <- gets stSlideVariant
  let sects = hierarchicalize $
              if slideVariant == NoSlides
                 then blocks
                 else prepSlides slideLevel blocks
  toc <- if writerTableOfContents opts && slideVariant /= S5Slides
            then fmap renderHtml' <$> tableOfContents opts sects
            else return Nothing
  blocks' <- liftM (mconcat . intersperse (nl opts)) $
                 mapM (elementToHtml slideLevel opts) sects
  st <- get
  notes <- footnoteSection opts (reverse (stNotes st))
  let thebody = blocks' >> notes
  let  math = case writerHTMLMathMethod opts of
                      LaTeXMathML (Just url) ->
                         H.script ! A.src (toValue url)
                                  ! A.type_ "text/javascript"
                                  $ mempty
                      MathJax url
                        | slideVariant /= RevealJsSlides ->
                        -- mathjax is handled via a special plugin in revealjs
                         H.script ! A.src (toValue url)
                                  ! A.type_ "text/javascript"
                                  $ case slideVariant of
                                         SlideousSlides ->
                                            preEscapedString
                                            "MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);"
                                         _ -> mempty
                      JsMath (Just url) ->
                         H.script ! A.src (toValue url)
                                  ! A.type_ "text/javascript"
                                  $ mempty
                      KaTeX js css ->
                         (H.script ! A.src (toValue js) $ mempty) <>
                         (H.link ! A.rel "stylesheet" ! A.href (toValue css)) <>
                         (H.script ! A.type_ "text/javascript" $ toHtml renderKaTeX)
                      _ -> case lookup "mathml-script" (writerVariables opts) of
                                 Just s | not (stHtml5 st) ->
                                   H.script ! A.type_ "text/javascript"
                                      $ preEscapedString
                                       ("/*<![CDATA[*/\n" ++ s ++ "/*]]>*/\n")
                                        | otherwise -> mempty
                                 Nothing -> mempty
  let context =   (if stHighlighting st
                      then case writerHighlightStyle opts of
                                Just sty -> defField "highlighting-css"
                                              (styleToCss sty)
                                Nothing  -> id
                      else id) $
                  (if stMath st
                      then defField "math" (renderHtml' math)
                      else id) $
                  defField "mathjax"
                      (case writerHTMLMathMethod opts of
                            MathJax _ -> True
                            _         -> False) $
                  defField "quotes" (stQuotes st) $
                  -- for backwards compatibility we populate toc
                  -- with the contents of the toc, rather than a
                  -- boolean:
                  maybe id (defField "toc") toc $
                  maybe id (defField "table-of-contents") toc $
                  defField "author-meta" authsMeta $
                  maybe id (defField "date-meta") (normalizeDate dateMeta) $
                  defField "pagetitle" (stringifyHTML (docTitle meta)) $
                  defField "idprefix" (writerIdentifierPrefix opts) $
                  -- these should maybe be set in pandoc.hs
                  defField "slidy-url"
                    ("http://www.w3.org/Talks/Tools/Slidy2" :: String) $
                  defField "slideous-url" ("slideous" :: String) $
                  defField "revealjs-url" ("reveal.js" :: String) $
                  defField "s5-url" ("s5/default" :: String) $
                  defField "html5" (stHtml5 st) $
                  metadata
  return (thebody, context)

-- | Like Text.XHtml's identifier, but adds the writerIdentifierPrefix
prefixedId :: WriterOptions -> String -> Attribute
prefixedId opts s =
  case s of
    "" -> mempty
    _  -> A.id $ toValue $ writerIdentifierPrefix opts ++ s

toList :: PandocMonad m
       => (Html -> Html)
       -> WriterOptions
       -> [Html]
       -> StateT WriterState m Html
toList listop opts items = do
    slideVariant <- gets stSlideVariant
    return $
      if (writerIncremental opts)
         then if (slideVariant /= RevealJsSlides)
                 then (listop $ mconcat items) ! A.class_ "incremental"
                 else listop $ mconcat $ map (! A.class_ "fragment") items
         else listop $ mconcat items

unordList :: PandocMonad m
          => WriterOptions -> [Html] -> StateT WriterState m Html
unordList opts = toList H.ul opts . toListItems opts

ordList :: PandocMonad m
        => WriterOptions -> [Html] -> StateT WriterState m Html
ordList opts = toList H.ol opts . toListItems opts

defList :: PandocMonad m
        => WriterOptions -> [Html] -> StateT WriterState m Html
defList opts items = toList H.dl opts (items ++ [nl opts])

-- | Construct table of contents from list of elements.
tableOfContents :: PandocMonad m => WriterOptions -> [Element] -> StateT WriterState m (Maybe Html)
tableOfContents _ [] = return Nothing
tableOfContents opts sects = do
  contents  <- mapM (elementToListItem opts) sects
  let tocList = catMaybes contents
  if null tocList
     then return Nothing
     else Just <$> unordList opts tocList

-- | Convert section number to string
showSecNum :: [Int] -> String
showSecNum = concat . intersperse "." . map show

-- | Converts an Element to a list item for a table of contents,
-- retrieving the appropriate identifier from state.
elementToListItem :: PandocMonad m => WriterOptions -> Element -> StateT WriterState m (Maybe Html)
-- Don't include the empty headers created in slide shows
-- shows when an hrule is used to separate slides without a new title:
elementToListItem _ (Sec _ _ _ [Str "\0"] _) = return Nothing
elementToListItem opts (Sec lev num (id',classes,_) headerText subsecs)
  | lev <= writerTOCDepth opts = do
  let num' = zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  let sectnum = if writerNumberSections opts && not (null num) &&
                   "unnumbered" `notElem` classes
                   then (H.span ! A.class_ "toc-section-number"
                        $ toHtml $ showSecNum num') >> preEscapedString " "
                   else mempty
  txt <- liftM (sectnum >>) $ inlineListToHtml opts $ walk deNote headerText
  subHeads <- mapM (elementToListItem opts) subsecs >>= return . catMaybes
  subList <- if null subHeads
                then return mempty
                else unordList opts subHeads
  -- in reveal.js, we need #/apples, not #apples:
  slideVariant <- gets stSlideVariant
  let revealSlash = ['/' | slideVariant== RevealJsSlides]
  return $ Just
         $ if null id'
              then (H.a $ toHtml txt) >> subList
              else (H.a ! A.href (toValue $ "#" ++ revealSlash ++
                    writerIdentifierPrefix opts ++ id')
                       $ toHtml txt) >> subList
elementToListItem _ _ = return Nothing

-- | Convert an Element to Html.
elementToHtml :: PandocMonad m => Int -> WriterOptions -> Element -> StateT WriterState m Html
elementToHtml _slideLevel opts (Blk block) = blockToHtml opts block
elementToHtml slideLevel opts (Sec level num (id',classes,keyvals) title' elements) = do
  slideVariant <- gets stSlideVariant
  let slide = slideVariant /= NoSlides && level <= slideLevel
  let num' = zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  modify $ \st -> st{stSecNum = num'}  -- update section number
  html5 <- gets stHtml5
  let titleSlide = slide && level < slideLevel
  header' <- if title' == [Str "\0"]  -- marker for hrule
                then return mempty
                else do
                  modify (\st -> st{ stElement = True})
                  res <- blockToHtml opts
                           (Header level (id',classes,keyvals) title')
                  modify (\st -> st{ stElement = False})
                  return res

  let isSec (Sec _ _ _ _ _) = True
      isSec (Blk _)         = False
  let isPause (Blk x) = x == Para [Str ".",Space,Str ".",Space,Str "."]
      isPause _       = False
  let fragmentClass = case slideVariant of
                           RevealJsSlides -> "fragment"
                           _              -> "incremental"
  let inDiv xs = Blk (RawBlock (Format "html") ("<div class=\""
                       ++ fragmentClass ++ "\">")) :
                   (xs ++ [Blk (RawBlock (Format "html") "</div>")])
  innerContents <- mapM (elementToHtml slideLevel opts)
                   $ if titleSlide
                        -- title slides have no content of their own
                        then filter isSec elements
                        else case splitBy isPause elements of
                                  []     -> []
                                  (x:xs) -> x ++ concatMap inDiv xs
  let inNl x = mconcat $ nl opts : intersperse (nl opts) x ++ [nl opts]
  let classes' = ["titleslide" | titleSlide] ++ ["slide" | slide] ++
                  ["section" | (slide || writerSectionDivs opts) &&
                               not html5 ] ++
                  ["level" ++ show level | slide || writerSectionDivs opts ]
                  ++ classes
  let secttag  = if html5
                    then H5.section
                    else H.div
  let attr = (id',classes',keyvals)
  if titleSlide
     then do
       t <- addAttrs opts attr $ secttag $ header'
       return $
         (if slideVariant == RevealJsSlides
                then H5.section
                else id) $ mconcat $ t : innerContents
     else if writerSectionDivs opts || slide
          then addAttrs opts attr
               $ secttag $ inNl $ header' : innerContents
          else do
            t <- addAttrs opts attr header'
            return $ mconcat $ intersperse (nl opts) (t : innerContents)

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: PandocMonad m
                => WriterOptions -> [Html] -> StateT WriterState m Html
footnoteSection opts notes = do
  html5 <- gets stHtml5
  slideVariant <- gets stSlideVariant
  let hrtag = if html5 then H5.hr else H.hr
  let container x = if html5
                       then H5.section ! A.class_ "footnotes" $ x
                       else if slideVariant /= NoSlides
                            then H.div ! A.class_ "footnotes slide" $ x
                            else H.div ! A.class_ "footnotes" $ x
  return $
    if null notes
       then mempty
       else nl opts >> (container
            $ nl opts >> hrtag >> nl opts >>
              H.ol (mconcat notes >> nl opts) >> nl opts)

-- | Parse a mailto link; return Just (name, domain) or Nothing.
parseMailto :: String -> Maybe (String, String)
parseMailto s = do
  case break (==':') s of
       (xs,':':addr) | map toLower xs == "mailto" -> do
         let (name', rest) = span (/='@') addr
         let domain = drop 1 rest
         return (name', domain)
       _ -> fail "not a mailto: URL"

-- | Obfuscate a "mailto:" link.
obfuscateLink :: PandocMonad m
              => WriterOptions -> Attr -> Html -> String
              -> StateT WriterState m Html
obfuscateLink opts attr txt s | writerEmailObfuscation opts == NoObfuscation =
  addAttrs opts attr $ H.a ! A.href (toValue s) $ txt
obfuscateLink opts attr (TL.unpack . renderHtml -> txt) s =
  let meth = writerEmailObfuscation opts
      s' = map toLower (take 7 s) ++ drop 7 s
  in  case parseMailto s' of
        (Just (name', domain)) ->
          let domain'  = substitute "." " dot " domain
              at'      = obfuscateChar '@'
              (linkText, altText) =
                 if txt == drop 7 s' -- autolink
                    then ("e", name' ++ " at " ++ domain')
                    else ("'" ++ obfuscateString txt ++ "'",
                          txt ++ " (" ++ name' ++ " at " ++ domain' ++ ")")
              (_, classNames, _) = attr
              classNamesStr = concatMap (' ':) classNames
          in  case meth of
                ReferenceObfuscation ->
                     -- need to use preEscapedString or &'s are escaped to &amp; in URL
                     return $
                     preEscapedString $ "<a href=\"" ++ (obfuscateString s')
                     ++ "\" class=\"email\">" ++ (obfuscateString txt) ++ "</a>"
                JavascriptObfuscation ->
                     return $
                     (H.script ! A.type_ "text/javascript" $
                     preEscapedString ("\n<!--\nh='" ++
                     obfuscateString domain ++ "';a='" ++ at' ++ "';n='" ++
                     obfuscateString name' ++ "';e=n+a+h;\n" ++
                     "document.write('<a h'+'ref'+'=\"ma'+'ilto'+':'+e+'\" clas'+'s=\"em' + 'ail" ++
                     classNamesStr ++ "\">'+" ++
                     linkText  ++ "+'<\\/'+'a'+'>');\n// -->\n")) >>
                     H.noscript (preEscapedString $ obfuscateString altText)
                _ -> throwError $ PandocSomeError $ "Unknown obfuscation method: " ++ show meth
        _ -> addAttrs opts attr $ H.a ! A.href (toValue s) $ toHtml txt  -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char =
  let num    = ord char
      numstr = if even num then show num else "x" ++ showHex num ""
  in  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = concatMap obfuscateChar . fromEntities

addAttrs :: PandocMonad m
         => WriterOptions -> Attr -> Html -> StateT WriterState m Html
addAttrs opts attr h = foldl (!) h <$> attrsToHtml opts attr

toAttrs :: PandocMonad m
        => [(String, String)] -> StateT WriterState m [Attribute]
toAttrs kvs = do
  html5 <- gets stHtml5
  return $ map (\(x,y) ->
     customAttribute
        (fromString (if not html5 || x `Set.member` html5Attributes
                        then x
                        else "data-" ++ x)) (toValue y)) kvs

attrsToHtml :: PandocMonad m
            => WriterOptions -> Attr -> StateT WriterState m [Attribute]
attrsToHtml opts (id',classes',keyvals) = do
  attrs <- toAttrs keyvals
  return $
    [prefixedId opts id' | not (null id')] ++
    [A.class_ (toValue $ unwords classes') | not (null classes')] ++ attrs

imgAttrsToHtml :: PandocMonad m
               => WriterOptions -> Attr -> StateT WriterState m [Attribute]
imgAttrsToHtml opts attr = do
  attrs <- attrsToHtml opts (ident,cls,kvs')
  dimattrs <- toAttrs (dimensionsToAttrList attr)
  return $ attrs ++ dimattrs
  where
    (ident,cls,kvs) = attr
    kvs' = filter isNotDim kvs
    isNotDim ("width", _)  = False
    isNotDim ("height", _) = False
    isNotDim _             = True

dimensionsToAttrList :: Attr -> [(String, String)]
dimensionsToAttrList attr = (go Width) ++ (go Height)
  where
    go dir = case (dimension dir attr) of
               (Just (Pixel a))  -> [(show dir, show a)]
               (Just x)          -> [("style", show dir ++ ":" ++ show x)]
               Nothing           -> []

imageExts :: [String]
imageExts = [ "art", "bmp", "cdr", "cdt", "cpt", "cr2", "crw", "djvu", "erf",
              "gif", "ico", "ief", "jng", "jpg", "jpeg", "nef", "orf", "pat", "pbm",
              "pcx", "pgm", "png", "pnm", "ppm", "psd", "ras", "rgb", "svg", "tiff",
              "wbmp", "xbm", "xpm", "xwd" ]

treatAsImage :: FilePath -> Bool
treatAsImage fp =
  let path = case uriPath `fmap` parseURIReference fp of
                  Nothing -> fp
                  Just up -> up
      ext  = map toLower $ drop 1 $ takeExtension path
  in  null ext || ext `elem` imageExts

figure :: PandocMonad m
       => WriterOptions -> Attr -> [Inline] -> (String, String)
       -> StateT WriterState m Html
figure opts attr txt (s,tit) = do
  img <- inlineToHtml opts (Image attr txt (s,tit))
  html5 <- gets stHtml5
  let tocapt = if html5
                  then H5.figcaption
                  else H.p ! A.class_ "caption"
  capt <- if null txt
             then return mempty
             else tocapt `fmap` inlineListToHtml opts txt
  return $ if html5
              then H5.figure $ mconcat
                    [nl opts, img, capt, nl opts]
              else H.div ! A.class_ "figure" $ mconcat
                    [nl opts, img, nl opts, capt, nl opts]

-- | Convert Pandoc block element to HTML.
blockToHtml :: PandocMonad m => WriterOptions -> Block -> StateT WriterState m Html
blockToHtml _ Null = return mempty
blockToHtml opts (Plain lst) = inlineListToHtml opts lst
blockToHtml opts (Para [Image attr@(_,classes,_) txt (src,tit)])
  | "stretch" `elem` classes = do
  slideVariant <- gets stSlideVariant
  case slideVariant of
       RevealJsSlides ->
         -- a "stretched" image in reveal.js must be a direct child
         -- of the slide container
         inlineToHtml opts (Image attr txt (src, tit))
       _ -> figure opts attr txt (src, tit)
-- title beginning with fig: indicates that the image is a figure
blockToHtml opts (Para [Image attr txt (s,'f':'i':'g':':':tit)]) =
  figure opts attr txt (s,tit)
blockToHtml opts (Para lst)
  | isEmptyRaw lst = return mempty
  | otherwise = do
      contents <- inlineListToHtml opts lst
      return $ H.p contents
  where
    isEmptyRaw [RawInline f _] = f `notElem` [Format "html",
                                    Format "html4", Format "html5"]
    isEmptyRaw _               = False
blockToHtml opts (LineBlock lns) =
  if writerWrapText opts == WrapNone
  then blockToHtml opts $ linesToPara lns
  else do
    let lf = preEscapedString "\n"
    htmlLines <- mconcat . intersperse lf <$> mapM (inlineListToHtml opts) lns
    return $ H.div ! A.class_ "line-block" $ htmlLines
blockToHtml opts (Div attr@(ident, classes, kvs) bs) = do
  html5 <- gets stHtml5
  let speakerNotes = "notes" `elem` classes
  -- we don't want incremental output inside speaker notes, see #1394
  let opts' = if speakerNotes then opts{ writerIncremental = False } else opts
  contents <- blockListToHtml opts' bs
  let contents' = nl opts >> contents >> nl opts
  let (divtag, classes') = if html5 && "section" `elem` classes
                              then (H5.section, filter (/= "section") classes)
                              else (H.div, classes)
  slideVariant <- gets stSlideVariant
  if speakerNotes
     then case slideVariant of
               RevealJsSlides -> addAttrs opts' attr $ H5.aside $ contents'
               DZSlides       -> do
                 t <- addAttrs opts' attr $ H5.div $ contents'
                 return $ t ! (H5.customAttribute "role" "note")
               NoSlides       -> addAttrs opts' attr $ H.div $ contents'
               _              -> return mempty
     else addAttrs opts (ident, classes', kvs) $ divtag $ contents'
blockToHtml opts (RawBlock f str) = do
  ishtml <- isRawHtml f
  if ishtml
     then return $ preEscapedString str
     else if (f == Format "latex" || f == Format "tex") &&
             allowsMathEnvironments (writerHTMLMathMethod opts) &&
             isMathEnvironment str
             then blockToHtml opts $ Plain [Math DisplayMath str]
             else do
               report $ BlockNotRendered (RawBlock f str)
               return mempty
blockToHtml _ (HorizontalRule) = do
  html5 <- gets stHtml5
  return $ if html5 then H5.hr else H.hr
blockToHtml opts (CodeBlock (id',classes,keyvals) rawCode) = do
  let tolhs = isEnabled Ext_literate_haskell opts &&
                any (\c -> map toLower c == "haskell") classes &&
                any (\c -> map toLower c == "literate") classes
      classes' = if tolhs
                    then map (\c -> if map toLower c == "haskell"
                                       then "literatehaskell"
                                       else c) classes
                    else classes
      adjCode  = if tolhs
                    then unlines . map ("> " ++) . lines $ rawCode
                    else rawCode
      hlCode   = if isJust (writerHighlightStyle opts)
                    then highlight (writerSyntaxMap opts) formatHtmlBlock
                            (id',classes',keyvals) adjCode
                    else Left ""
  case hlCode of
         Left msg -> do
           unless (null msg) $
             report $ CouldNotHighlight msg
           addAttrs opts (id',classes,keyvals)
             $ H.pre $ H.code $ toHtml adjCode
         Right h -> modify (\st -> st{ stHighlighting = True }) >>
                    addAttrs opts (id',[],keyvals) h
blockToHtml opts (BlockQuote blocks) = do
  -- in S5, treat list in blockquote specially
  -- if default is incremental, make it nonincremental;
  -- otherwise incremental
  slideVariant <- gets stSlideVariant
  if slideVariant /= NoSlides
     then let inc = not (writerIncremental opts) in
          case blocks of
             [BulletList lst]  -> blockToHtml (opts {writerIncremental = inc})
                                  (BulletList lst)
             [OrderedList attribs lst] ->
                                  blockToHtml (opts {writerIncremental = inc})
                                  (OrderedList attribs lst)
             [DefinitionList lst] ->
                                  blockToHtml (opts {writerIncremental = inc})
                                  (DefinitionList lst)
             _                 -> do contents <- blockListToHtml opts blocks
                                     return $ H.blockquote
                                            $ nl opts >> contents >> nl opts
     else do
       contents <- blockListToHtml opts blocks
       return $ H.blockquote $ nl opts >> contents >> nl opts
blockToHtml opts (Header level attr@(_,classes,_) lst) = do
  contents <- inlineListToHtml opts lst
  secnum <- liftM stSecNum get
  let contents' = if writerNumberSections opts && not (null secnum)
                     && "unnumbered" `notElem` classes
                     then (H.span ! A.class_ "header-section-number" $ toHtml
                          $ showSecNum secnum) >> strToHtml " " >> contents
                     else contents
  inElement <- gets stElement
  (if inElement then return else addAttrs opts attr)
         $ case level of
              1 -> H.h1 contents'
              2 -> H.h2 contents'
              3 -> H.h3 contents'
              4 -> H.h4 contents'
              5 -> H.h5 contents'
              6 -> H.h6 contents'
              _ -> H.p contents'
blockToHtml opts (BulletList lst) = do
  contents <- mapM (blockListToHtml opts) lst
  unordList opts contents
blockToHtml opts (OrderedList (startnum, numstyle, _) lst) = do
  contents <- mapM (blockListToHtml opts) lst
  html5 <- gets stHtml5
  let numstyle' = case numstyle of
                       Example -> "decimal"
                       _       -> camelCaseToHyphenated $ show numstyle
  let attribs = (if startnum /= 1
                   then [A.start $ toValue startnum]
                   else []) ++
                (if numstyle == Example
                    then [A.class_ "example"]
                    else []) ++
                (if numstyle /= DefaultStyle
                   then if html5
                           then [A.type_ $
                                 case numstyle of
                                      Decimal    -> "1"
                                      LowerAlpha -> "a"
                                      UpperAlpha -> "A"
                                      LowerRoman -> "i"
                                      UpperRoman -> "I"
                                      _          -> "1"]
                           else [A.style $ toValue $ "list-style-type: " ++
                                   numstyle']
                   else [])
  l <- ordList opts contents
  return $ foldl (!) l attribs
blockToHtml opts (DefinitionList lst) = do
  contents <- mapM (\(term, defs) ->
                  do term' <- if null term
                                 then return mempty
                                 else liftM H.dt $ inlineListToHtml opts term
                     defs' <- mapM ((liftM (\x -> H.dd $ (x >> nl opts))) .
                                    blockListToHtml opts) defs
                     return $ mconcat $ nl opts : term' : nl opts :
                                        intersperse (nl opts) defs') lst
  defList opts contents
blockToHtml opts (Table capt aligns widths headers rows') = do
  captionDoc <- if null capt
                   then return mempty
                   else do
                     cs <- inlineListToHtml opts capt
                     return $ H.caption cs >> nl opts
  html5 <- gets stHtml5
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then mempty
                   else do
                     H.colgroup $ do
                       nl opts
                       mapM_ (\w -> do
                            if html5
                               then H.col ! A.style (toValue $ "width: " ++
                                                      percent w)
                               else H.col ! A.width (toValue $ percent w)
                            nl opts) widths
                     nl opts
  head' <- if all null headers
              then return mempty
              else do
                contents <- tableRowToHtml opts aligns 0 headers
                return $ H.thead (nl opts >> contents) >> nl opts
  body' <- liftM (\x -> H.tbody (nl opts >> mconcat x)) $
               zipWithM (tableRowToHtml opts aligns) [1..] rows'
  let tbl = H.table $
              nl opts >> captionDoc >> coltags >> head' >> body' >> nl opts
  let totalWidth = sum widths
  -- When widths of columns are < 100%, we need to set width for the whole
  -- table, or some browsers give us skinny columns with lots of space between:
  return $ if totalWidth == 0 || totalWidth == 1
              then tbl
              else tbl ! A.style (toValue $ "width:" ++
                              show (round (totalWidth * 100) :: Int) ++ "%;")

tableRowToHtml :: PandocMonad m
               => WriterOptions
               -> [Alignment]
               -> Int
               -> [[Block]]
               -> StateT WriterState m Html
tableRowToHtml opts aligns rownum cols' = do
  let mkcell = if rownum == 0 then H.th else H.td
  let rowclass = case rownum of
                      0 -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _ -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToHtml opts mkcell alignment item)
            aligns cols'
  return $ (H.tr ! A.class_ rowclass $ nl opts >> mconcat cols'')
          >> nl opts

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> ""

tableItemToHtml :: PandocMonad m
                => WriterOptions
                -> (Html -> Html)
                -> Alignment
                -> [Block]
                -> StateT WriterState m Html
tableItemToHtml opts tag' align' item = do
  contents <- blockListToHtml opts item
  html5 <- gets stHtml5
  let alignStr = alignmentToString align'
  let attribs = if html5
                   then A.style (toValue $ "text-align: " ++ alignStr ++ ";")
                   else A.align (toValue alignStr)
  let tag'' = if null alignStr
                 then tag'
                 else tag' ! attribs
  return $ (tag'' $ contents) >> nl opts

toListItems :: WriterOptions -> [Html] -> [Html]
toListItems opts items = map (toListItem opts) items ++ [nl opts]

toListItem :: WriterOptions -> Html -> Html
toListItem opts item = nl opts >> H.li item

blockListToHtml :: PandocMonad m => WriterOptions -> [Block] -> StateT WriterState m Html
blockListToHtml opts lst =
  fmap (mconcat . intersperse (nl opts)) $ mapM (blockToHtml opts) lst

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: PandocMonad m => WriterOptions -> [Inline] -> StateT WriterState m Html
inlineListToHtml opts lst =
  mapM (inlineToHtml opts) lst >>= return . mconcat

-- | Annotates a MathML expression with the tex source
annotateMML :: XML.Element -> String -> XML.Element
annotateMML e tex = math (unode "semantics" [cs, unode "annotation" (annotAttrs, tex)])
  where
    cs = case elChildren e of
          []  -> unode "mrow" ()
          [x] -> x
          xs  -> unode "mrow" xs
    math childs = XML.Element q as [XML.Elem childs] l
      where
        (XML.Element q as _ l) = e
    annotAttrs = [XML.Attr (unqual "encoding") "application/x-tex"]


-- | Convert Pandoc inline element to HTML.
inlineToHtml :: PandocMonad m
             => WriterOptions -> Inline -> StateT WriterState m Html
inlineToHtml opts inline = do
  html5 <- gets stHtml5
  case inline of
    (Str str)        -> return $ strToHtml str
    (Space)          -> return $ strToHtml " "
    (SoftBreak)      -> return $ case writerWrapText opts of
                                       WrapNone     -> preEscapedString " "
                                       WrapAuto     -> preEscapedString " "
                                       WrapPreserve -> preEscapedString "\n"
    (LineBreak)      -> return $ (if html5 then H5.br else H.br)
                                 <> strToHtml "\n"
    (Span (id',classes,kvs) ils)
                     -> inlineListToHtml opts ils >>=
                           addAttrs opts attr' . H.span
                        where attr' = (id',classes',kvs')
                              classes' = filter (`notElem` ["csl-no-emph",
                                              "csl-no-strong",
                                              "csl-no-smallcaps"]) classes
                              kvs' = if null styles
                                        then kvs
                                        else (("style", concat styles) : kvs)
                              styles = ["font-style:normal;"
                                         | "csl-no-emph" `elem` classes]
                                    ++ ["font-weight:normal;"
                                         | "csl-no-strong" `elem` classes]
                                    ++ ["font-variant:normal;"
                                         | "csl-no-smallcaps" `elem` classes]
    (Emph lst)       -> inlineListToHtml opts lst >>= return . H.em
    (Strong lst)     -> inlineListToHtml opts lst >>= return . H.strong
    (Code attr str)  -> case hlCode of
                             Left msg -> do
                               unless (null msg) $
                                 report $ CouldNotHighlight msg
                               addAttrs opts attr $ H.code $ strToHtml str
                             Right h -> do
                               modify $ \st -> st{ stHighlighting = True }
                               addAttrs opts (id',[],keyvals) h
                        where (id',_,keyvals) = attr
                              hlCode = if isJust (writerHighlightStyle opts)
                                          then highlight
                                                 (writerSyntaxMap opts)
                                                 formatHtmlInline attr str
                                          else Left ""
    (Strikeout lst)  -> inlineListToHtml opts lst >>=
                        return . H.del
    (SmallCaps lst)   -> inlineListToHtml opts lst >>=
                         return . (H.span ! A.class_ "smallcaps")
    (Superscript lst) -> inlineListToHtml opts lst >>= return . H.sup
    (Subscript lst)   -> inlineListToHtml opts lst >>= return . H.sub
    (Quoted quoteType lst) ->
                        let (leftQuote, rightQuote) = case quoteType of
                              SingleQuote -> (strToHtml "‘",
                                              strToHtml "’")
                              DoubleQuote -> (strToHtml "“",
                                              strToHtml "”")
                        in  if writerHtmlQTags opts
                               then do
                                 modify $ \st -> st{ stQuotes = True }
                                 H.q `fmap` inlineListToHtml opts lst
                               else (\x -> leftQuote >> x >> rightQuote)
                                    `fmap` inlineListToHtml opts lst
    (Math t str) -> do
      modify (\st -> st {stMath = True})
      let mathClass = toValue $ ("math " :: String) ++
                      if t == InlineMath then "inline" else "display"
      case writerHTMLMathMethod opts of
           LaTeXMathML _ ->
              -- putting LaTeXMathML in container with class "LaTeX" prevents
              -- non-math elements on the page from being treated as math by
              -- the javascript
              return $ H.span ! A.class_ "LaTeX" $
                     case t of
                       InlineMath  -> toHtml ("$" ++ str ++ "$")
                       DisplayMath -> toHtml ("$$" ++ str ++ "$$")
           JsMath _ -> do
              let m = preEscapedString str
              return $ case t of
                       InlineMath  -> H.span ! A.class_ mathClass $ m
                       DisplayMath -> H.div ! A.class_ mathClass $ m
           WebTeX url -> do
              let imtag = if html5 then H5.img else H.img
              let m = imtag ! A.style "vertical-align:middle"
                            ! A.src (toValue $ url ++ urlEncode str)
                            ! A.alt (toValue str)
                            ! A.title (toValue str)
              let brtag = if html5 then H5.br else H.br
              return $ case t of
                        InlineMath  -> m
                        DisplayMath -> brtag >> m >> brtag
           GladTeX ->
              return $ case t of
                         InlineMath -> preEscapedString $ "<EQ ENV=\"math\">" ++ str ++ "</EQ>"
                         DisplayMath -> preEscapedString $ "<EQ ENV=\"displaymath\">" ++ str ++ "</EQ>"
           MathML -> do
              let conf = useShortEmptyTags (const False)
                           defaultConfigPP
              res <- lift $ convertMath writeMathML t str
              case res of
                    Right r  -> return $ preEscapedString $
                        ppcElement conf (annotateMML r str)
                    Left il  -> (H.span ! A.class_ mathClass) <$>
                                   inlineToHtml opts il
           MathJax _ -> return $ H.span ! A.class_ mathClass $ toHtml $
              case t of
                InlineMath  -> "\\(" ++ str ++ "\\)"
                DisplayMath -> "\\[" ++ str ++ "\\]"
           KaTeX _ _ -> return $ H.span ! A.class_ mathClass $
              toHtml (case t of
                        InlineMath  -> str
                        DisplayMath -> "\\displaystyle " ++ str)
           PlainMath -> do
              x <- lift (texMathToInlines t str) >>= inlineListToHtml opts
              let m = H.span ! A.class_ mathClass $ x
              let brtag = if html5 then H5.br else H.br
              return  $ case t of
                         InlineMath  -> m
                         DisplayMath -> brtag >> m >> brtag
    (RawInline f str) -> do
      ishtml <- isRawHtml f
      if ishtml
         then return $ preEscapedString str
         else if (f == Format "latex" || f == Format "tex") &&
                "\\begin" `isPrefixOf` str &&
                allowsMathEnvironments (writerHTMLMathMethod opts) &&
                isMathEnvironment str
                then inlineToHtml opts $ Math DisplayMath str
                else do
                  report $ InlineNotRendered inline
                  return mempty
    (Link attr txt (s,_)) | "mailto:" `isPrefixOf` s -> do
                        linkText <- inlineListToHtml opts txt
                        obfuscateLink opts attr linkText s
    (Link (ident,classes,kvs) txt (s,tit)) -> do
                        linkText <- inlineListToHtml opts txt
                        slideVariant <- gets stSlideVariant
                        let s' = case s of
                                   '#':xs -> let prefix = if slideVariant == RevealJsSlides
                                                             then "/"
                                                             else writerIdentifierPrefix opts
                                             in  '#' : prefix ++ xs
                                   _ -> s
                        let link = H.a ! A.href (toValue s') $ linkText
                        let attr = if txt == [Str (unEscapeString s)]
                                      then (ident, "uri" : classes, kvs)
                                      else (ident, classes, kvs)
                        link' <- addAttrs opts attr link
                        return $ if null tit
                                    then link'
                                    else link' ! A.title (toValue tit)
    (Image attr txt (s,tit)) | treatAsImage s -> do
                        let alternate' = stringify txt
                        slideVariant <- gets stSlideVariant
                        let isReveal = slideVariant == RevealJsSlides
                        attrs <- imgAttrsToHtml opts attr
                        let attributes =
                              -- reveal.js uses data-src for lazy loading
                              (if isReveal
                                  then customAttribute "data-src" $ toValue s
                                  else A.src $ toValue s) :
                              [A.title $ toValue tit | not (null tit)] ++
                              [A.alt $ toValue alternate' | not (null txt)] ++
                              attrs
                        let tag = if html5 then H5.img else H.img
                        return $ foldl (!) tag attributes
                        -- note:  null title included, as in Markdown.pl
    (Image attr _ (s,tit)) -> do
                        slideVariant <- gets stSlideVariant
                        let isReveal = slideVariant == RevealJsSlides
                        attrs <- imgAttrsToHtml opts attr
                        let attributes =
                              (if isReveal
                                  then customAttribute "data-src" $ toValue s
                                  else A.src $ toValue s) :
                              [A.title $ toValue tit | not (null tit)] ++
                              attrs
                        return $ foldl (!) H5.embed attributes
                        -- note:  null title included, as in Markdown.pl
    (Note contents) -> do
                        notes <- gets stNotes
                        let number = (length notes) + 1
                        let ref = show number
                        htmlContents <- blockListToNote opts ref contents
                        epubVersion <- gets stEPUBVersion
                        -- push contents onto front of notes
                        modify $ \st -> st {stNotes = (htmlContents:notes)}
                        slideVariant <- gets stSlideVariant
                        let revealSlash = ['/' | slideVariant == RevealJsSlides]
                        let link = H.a ! A.href (toValue $ "#" ++
                                         revealSlash ++
                                         writerIdentifierPrefix opts ++ "fn" ++ ref)
                                       ! A.class_ "footnoteRef"
                                       ! prefixedId opts ("fnref" ++ ref)
                                       $ (if isJust epubVersion
                                             then id
                                             else H.sup)
                                       $ toHtml ref
                        return $ case epubVersion of
                                      Just EPUB3 -> link ! customAttribute "epub:type" "noteref"
                                      _          -> link
    (Cite cits il)-> do contents <- inlineListToHtml opts il
                        let citationIds = unwords $ map citationId cits
                        let result = H.span ! A.class_ "citation" $ contents
                        return $ if html5
                                    then result ! customAttribute "data-cites" (toValue citationIds)
                                    else result

blockListToNote :: PandocMonad m => WriterOptions -> String -> [Block] -> StateT WriterState m Html
blockListToNote opts ref blocks =
  -- If last block is Para or Plain, include the backlink at the end of
  -- that block. Otherwise, insert a new Plain block with the backlink.
  let backlink = [Link ("",["footnoteBack"],[]) [Str "↩"] ("#" ++ writerIdentifierPrefix opts ++ "fnref" ++ ref,[])]
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
         let noteItem = H.li ! (prefixedId opts ("fn" ++ ref)) $ contents
         epubVersion <- gets stEPUBVersion
         let noteItem' = case epubVersion of
                              Just EPUB3 -> noteItem ! customAttribute "epub:type" "footnote"
                              _          -> noteItem
         return $ nl opts >> noteItem'

-- Javascript snippet to render all KaTeX elements
renderKaTeX :: String
renderKaTeX = unlines [
    "window.onload = function(){var mathElements = document.getElementsByClassName(\"math\");"
  , "for (var i=0; i < mathElements.length; i++)"
  , "{"
  , " var texText = mathElements[i].firstChild"
  , " katex.render(texText.data, mathElements[i])"
  , "}}"
  ]

isMathEnvironment :: String -> Bool
isMathEnvironment s = "\\begin{" `isPrefixOf` s &&
                         envName `elem` mathmlenvs
  where envName = takeWhile (/= '}') (drop 7 s)
        mathmlenvs = [ "align"
                     , "align*"
                     , "alignat"
                     , "alignat*"
                     , "aligned"
                     , "alignedat"
                     , "array"
                     , "Bmatrix"
                     , "bmatrix"
                     , "cases"
                     , "CD"
                     , "eqnarray"
                     , "eqnarray*"
                     , "equation"
                     , "equation*"
                     , "gather"
                     , "gather*"
                     , "gathered"
                     , "matrix"
                     , "multline"
                     , "multline*"
                     , "pmatrix"
                     , "smallmatrix"
                     , "split"
                     , "subarray"
                     , "Vmatrix"
                     , "vmatrix" ]

allowsMathEnvironments :: HTMLMathMethod -> Bool
allowsMathEnvironments (MathJax _) = True
allowsMathEnvironments (MathML)    = True
allowsMathEnvironments (WebTeX _)  = True
allowsMathEnvironments _           = False

isRawHtml :: PandocMonad m => Format -> StateT WriterState m Bool
isRawHtml f = do
  html5 <- gets stHtml5
  return $ f == Format "html" ||
           ((html5 && f == Format "html5") || f == Format "html4")

html5Attributes :: Set.Set String
html5Attributes = Set.fromList
  [ "abbr"
  , "accept"
  , "accept-charset"
  , "accesskey"
  , "action"
  , "allowfullscreen"
  , "allowpaymentrequest"
  , "allowusermedia"
  , "alt"
  , "as"
  , "async"
  , "autocomplete"
  , "autofocus"
  , "autoplay"
  , "charset"
  , "checked"
  , "cite"
  , "class"
  , "color"
  , "cols"
  , "colspan"
  , "content"
  , "contenteditable"
  , "controls"
  , "coords"
  , "crossorigin"
  , "data"
  , "datetime"
  , "default"
  , "defer"
  , "dir"
  , "dirname"
  , "disabled"
  , "download"
  , "draggable"
  , "enctype"
  , "for"
  , "form"
  , "formaction"
  , "formenctype"
  , "formmethod"
  , "formnovalidate"
  , "formtarget"
  , "headers"
  , "height"
  , "hidden"
  , "high"
  , "href"
  , "hreflang"
  , "http-equiv"
  , "id"
  , "inputmode"
  , "integrity"
  , "is"
  , "ismap"
  , "itemid"
  , "itemprop"
  , "itemref"
  , "itemscope"
  , "itemtype"
  , "kind"
  , "label"
  , "lang"
  , "list"
  , "loop"
  , "low"
  , "manifest"
  , "max"
  , "maxlength"
  , "media"
  , "method"
  , "min"
  , "minlength"
  , "multiple"
  , "muted"
  , "name"
  , "nomodule"
  , "nonce"
  , "novalidate"
  , "onabort"
  , "onafterprint"
  , "onauxclick"
  , "onbeforeprint"
  , "onbeforeunload"
  , "onblur"
  , "oncancel"
  , "oncanplay"
  , "oncanplaythrough"
  , "onchange"
  , "onclick"
  , "onclose"
  , "oncontextmenu"
  , "oncopy"
  , "oncuechange"
  , "oncut"
  , "ondblclick"
  , "ondrag"
  , "ondragend"
  , "ondragenter"
  , "ondragexit"
  , "ondragleave"
  , "ondragover"
  , "ondragstart"
  , "ondrop"
  , "ondurationchange"
  , "onemptied"
  , "onended"
  , "onerror"
  , "onfocus"
  , "onhashchange"
  , "oninput"
  , "oninvalid"
  , "onkeydown"
  , "onkeypress"
  , "onkeyup"
  , "onlanguagechange"
  , "onload"
  , "onloadeddata"
  , "onloadedmetadata"
  , "onloadend"
  , "onloadstart"
  , "onmessage"
  , "onmessageerror"
  , "onmousedown"
  , "onmouseenter"
  , "onmouseleave"
  , "onmousemove"
  , "onmouseout"
  , "onmouseover"
  , "onmouseup"
  , "onoffline"
  , "ononline"
  , "onpagehide"
  , "onpageshow"
  , "onpaste"
  , "onpause"
  , "onplay"
  , "onplaying"
  , "onpopstate"
  , "onprogress"
  , "onratechange"
  , "onrejectionhandled"
  , "onreset"
  , "onresize"
  , "onscroll"
  , "onsecuritypolicyviolation"
  , "onseeked"
  , "onseeking"
  , "onselect"
  , "onstalled"
  , "onstorage"
  , "onsubmit"
  , "onsuspend"
  , "ontimeupdate"
  , "ontoggle"
  , "onunhandledrejection"
  , "onunload"
  , "onvolumechange"
  , "onwaiting"
  , "onwheel"
  , "open"
  , "optimum"
  , "pattern"
  , "ping"
  , "placeholder"
  , "playsinline"
  , "poster"
  , "preload"
  , "readonly"
  , "referrerpolicy"
  , "rel"
  , "required"
  , "reversed"
  , "rows"
  , "rowspan"
  , "sandbox"
  , "scope"
  , "selected"
  , "shape"
  , "size"
  , "sizes"
  , "slot"
  , "span"
  , "spellcheck"
  , "src"
  , "srcdoc"
  , "srclang"
  , "srcset"
  , "start"
  , "step"
  , "style"
  , "tabindex"
  , "target"
  , "title"
  , "translate"
  , "type"
  , "typemustmatch"
  , "updateviacache"
  , "usemap"
  , "value"
  , "width"
  , "workertype"
  , "wrap"
  ]

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.HTML
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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
  writeRevealJs,
  tagWithAttributes
  ) where
import Control.Monad.State.Strict
import Data.Char (ord, toLower)
import Data.List (intercalate, intersperse, isPrefixOf, partition)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP (urlEncode)
import Network.URI (URI (..), parseURIReference)
import Numeric (showHex)
import Text.DocLayout (render, literal)
import Prelude
import Text.Blaze.Internal (MarkupM (Empty), customLeaf, customParent)
import Text.DocTemplates (FromContext (lookupContext))
#if MIN_VERSION_blaze_markup(0,6,3)
#else
import Text.Blaze.Internal (preEscapedString, preEscapedText)
#endif
import Text.Blaze.Html hiding (contents)
import Text.DocTemplates (Context (..))
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatHtmlBlock, formatHtmlInline, highlight,
                                 styleToCss)
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Slides
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML (escapeStringForXML, fromEntities, toEntities)
#if MIN_VERSION_blaze_markup(0,6,3)
#else
import Text.Blaze.Internal (preEscapedString, preEscapedText)
#endif
#if MIN_VERSION_blaze_html(0,5,1)
import qualified Text.Blaze.XHtml5 as H5
import qualified Text.Blaze.XHtml5.Attributes as A5
#else
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as A5
#endif
import Control.Monad.Except (throwError)
import System.FilePath (takeBaseName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import Text.Pandoc.Class (PandocMonad, report, runPure)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (mediaCategory)
import Text.TeXMath
import Text.XML.Light (elChildren, unode, unqual)
import qualified Text.XML.Light as XML
import Text.XML.Light.Output

data WriterState = WriterState
    { stNotes        :: [Html]  -- ^ List of notes
    , stMath         :: Bool    -- ^ Math is used in document
    , stQuotes       :: Bool    -- ^ <q> tag is used
    , stHighlighting :: Bool    -- ^ Syntax highlighting is used
    , stHtml5        :: Bool    -- ^ Use HTML5
    , stEPUBVersion  :: Maybe EPUBVersion -- ^ EPUB version if for epub
    , stSlideVariant :: HTMLSlideVariant
    , stSlideLevel   :: Int     -- ^ Slide level
    , stCodeBlockNum :: Int     -- ^ Number of code block
    }

defaultWriterState :: WriterState
defaultWriterState = WriterState {stNotes= [], stMath = False, stQuotes = False,
                                  stHighlighting = False,
                                  stHtml5 = False,
                                  stEPUBVersion = Nothing,
                                  stSlideVariant = NoSlides,
                                  stSlideLevel = 1,
                                  stCodeBlockNum = 0}

-- Helpers to render HTML with the appropriate function.

strToHtml :: String -> Html
strToHtml ('\'':xs) = preEscapedString "\'" `mappend` strToHtml xs
strToHtml ('"' :xs) = preEscapedString "\"" `mappend` strToHtml xs
strToHtml (x:xs) | needsVariationSelector x
                 = preEscapedString [x, '\xFE0E'] `mappend`
                   case xs of
                     ('\xFE0E':ys) -> strToHtml ys
                     _             -> strToHtml xs
strToHtml xs@(_:_)  = case break (\c -> c == '\'' || c == '"' ||
                                        needsVariationSelector c) xs of
                           (_ ,[]) -> toHtml xs
                           (ys,zs) -> toHtml ys `mappend` strToHtml zs
strToHtml [] = ""

-- See #5469: this prevents iOS from substituting emojis.
needsVariationSelector :: Char -> Bool
needsVariationSelector '↩' = True
needsVariationSelector '↔' = True
needsVariationSelector _   = False

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
  (if writerPreferAscii opts
      then toEntities
      else id) <$>
    case writerTemplate opts of
       Nothing -> return $ renderHtml' body
       Just tpl -> do
         -- warn if empty lang
         when (isNothing (getField "lang" context :: Maybe Text)) $
           report NoLangSpecified
         -- check for empty pagetitle
         context' <-
            case getField "pagetitle" context of
                 Just (s :: Text) | not (T.null s) -> return context
                 _ -> do
                   let fallback = maybe "Untitled" (takeBaseName . T.unpack) $
                           lookupContext "sourcefile" (writerVariables opts)
                   report $ NoTitleElement fallback
                   return $ resetField "pagetitle" (T.pack fallback) context
         return $ render Nothing $ renderTemplate tpl
             (defField "body" (renderHtml' body) context')

writeHtml' :: PandocMonad m => WriterState -> WriterOptions -> Pandoc -> m Html
writeHtml' st opts d =
  case writerTemplate opts of
       Just _ -> preEscapedText <$> writeHtmlString' st opts d
       Nothing
         | writerPreferAscii opts
            -> preEscapedText <$> writeHtmlString' st opts d
         | otherwise -> do
            (body, _) <- evalStateT (pandocToHtml opts d) st
            return body

-- result is (title, authors, date, toc, body, new variables)
pandocToHtml :: PandocMonad m
             => WriterOptions
             -> Pandoc
             -> StateT WriterState m (Html, Context Text)
pandocToHtml opts (Pandoc meta blocks) = do
  let slideLevel = fromMaybe (getSlideLevel blocks) $ writerSlideLevel opts
  modify $ \st -> st{ stSlideLevel = slideLevel }
  metadata <- metaToContext opts
              (fmap (literal . renderHtml') . blockListToHtml opts)
              (fmap (literal . renderHtml') . inlineListToHtml opts)
              meta
  let stringifyHTML = escapeStringForXML . stringify
  let authsMeta = map stringifyHTML $ docAuthors meta
  let dateMeta  = stringifyHTML $ docDate meta
  slideVariant <- gets stSlideVariant
  let sects = makeSections (writerNumberSections opts) Nothing $
              if slideVariant == NoSlides
                 then blocks
                 else prepSlides slideLevel blocks
  toc <- if writerTableOfContents opts && slideVariant /= S5Slides
            then fmap renderHtml' <$> tableOfContents opts sects
            else return Nothing
  blocks' <- blockListToHtml opts sects
  st <- get
  notes <- footnoteSection opts (reverse (stNotes st))
  let thebody = blocks' >> notes
  let math = case writerHTMLMathMethod opts of
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
        KaTeX url -> do
          H.script !
            A.src (toValue $ url ++ "katex.min.js") $ mempty
          nl opts
          let katexFlushLeft =
                case lookupContext "classoption" metadata of
                  Just clsops | "fleqn" `elem` (clsops :: [Text]) -> "true"
                  _ -> "false"
          H.script $ text $ T.unlines [
              "document.addEventListener(\"DOMContentLoaded\", function () {"
            , " var mathElements = document.getElementsByClassName(\"math\");"
            , " for (var i = 0; i < mathElements.length; i++) {"
            , "  var texText = mathElements[i].firstChild;"
            , "  if (mathElements[i].tagName == \"SPAN\") {"
            , "   katex.render(texText.data, mathElements[i], {"
            , "    displayMode: mathElements[i].classList.contains('display'),"
            , "    throwOnError: false,"
            , "    fleqn: " <> katexFlushLeft
            , "   });"
            , "}}});"
            ]
          nl opts
          H.link ! A.rel "stylesheet" !
            A.href (toValue $ url ++ "katex.min.css")

        _ -> case lookupContext "mathml-script"
                  (writerVariables opts) of
                    Just s | not (stHtml5 st) ->
                      H.script ! A.type_ "text/javascript"
                        $ preEscapedString
                          ("/*<![CDATA[*/\n" ++ T.unpack s ++
                          "/*]]>*/\n")
                          | otherwise -> mempty
                    Nothing -> mempty
  let context =   (if stHighlighting st
                      then case writerHighlightStyle opts of
                                Just sty -> defField "highlighting-css"
                                              (T.pack $ styleToCss sty)
                                Nothing  -> id
                      else id) $
                  (if stMath st
                      then defField "math" (renderHtml' math)
                      else id) $
                  (case writerHTMLMathMethod opts of
                        MathJax u -> defField "mathjax" True .
                                     defField "mathjaxurl"
                                       (T.pack $ takeWhile (/='?') u)
                        _         -> defField "mathjax" False) $
                  defField "quotes" (stQuotes st) $
                  -- for backwards compatibility we populate toc
                  -- with the contents of the toc, rather than a
                  -- boolean:
                  maybe id (defField "toc") toc $
                  maybe id (defField "table-of-contents") toc $
                  defField "author-meta" (map T.pack authsMeta) $
                  maybe id (defField "date-meta" . T.pack)
                    (normalizeDate dateMeta) $
                  defField "pagetitle"
                      (T.pack . stringifyHTML . docTitle $ meta) $
                  defField "idprefix" (T.pack $ writerIdentifierPrefix opts) $
                  -- these should maybe be set in pandoc.hs
                  defField "slidy-url"
                    ("https://www.w3.org/Talks/Tools/Slidy2" :: Text) $
                  defField "slideous-url" ("slideous" :: Text) $
                  defField "revealjs-url" ("reveal.js" :: Text) $
                  defField "s5-url" ("s5/default" :: Text) $
                  defField "html5" (stHtml5 st)
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
      if writerIncremental opts
         then if slideVariant /= RevealJsSlides
                 then  listop (mconcat items) ! A.class_ "incremental"
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

listItemToHtml :: PandocMonad m
               => WriterOptions -> [Block] -> StateT WriterState m Html
listItemToHtml opts bls
  | Plain (Str "☐":Space:is) : bs <- bls = taskListItem False id  is bs
  | Plain (Str "☒":Space:is) : bs <- bls = taskListItem True  id  is bs
  | Para  (Str "☐":Space:is) : bs <- bls = taskListItem False H.p is bs
  | Para  (Str "☒":Space:is) : bs <- bls = taskListItem True  H.p is bs
  | otherwise = blockListToHtml opts bls
  where
    taskListItem checked constr is bs = do
      let checkbox  = if checked
                      then checkbox' ! A.checked ""
                      else checkbox'
          checkbox' = H.input ! A.type_ "checkbox" ! A.disabled "" >> nl opts
      isContents <- inlineListToHtml opts is
      bsContents <- blockListToHtml opts bs
      return $ constr (checkbox >> isContents) >> bsContents

-- | Construct table of contents from list of elements.
tableOfContents :: PandocMonad m => WriterOptions -> [Block]
                -> StateT WriterState m (Maybe Html)
tableOfContents _ [] = return Nothing
tableOfContents opts sects = do
  -- in reveal.js, we need #/apples, not #apples:
  slideVariant <- gets stSlideVariant
  let opts' = case slideVariant of
                RevealJsSlides ->
                  opts{ writerIdentifierPrefix =
                          '/' : writerIdentifierPrefix opts }
                _ -> opts
  case toTableOfContents opts sects of
    bl@(BulletList (_:_)) -> Just <$> blockToHtml opts' bl
    _                     -> return Nothing

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: PandocMonad m
                => WriterOptions -> [Html] -> StateT WriterState m Html
footnoteSection opts notes = do
  html5 <- gets stHtml5
  slideVariant <- gets stSlideVariant
  let hrtag = if html5 then H5.hr else H.hr
  epubVersion <- gets stEPUBVersion
  let container x
        | html5
        , epubVersion == Just EPUB3
                = H5.section ! A.class_ "footnotes"
                             ! customAttribute "epub:type" "footnotes" $ x
        | html5 = H5.section ! A.class_ "footnotes"
                             ! customAttribute "role" "doc-endnotes"
                             $ x
        | slideVariant /= NoSlides = H.div ! A.class_ "footnotes slide" $ x
        | otherwise = H.div ! A.class_ "footnotes" $ x
  return $
    if null notes
       then mempty
       else nl opts >> container (nl opts >> hrtag >> nl opts >>
              H.ol (mconcat notes >> nl opts) >> nl opts)

-- | Parse a mailto link; return Just (name, domain) or Nothing.
parseMailto :: String -> Maybe (String, String)
parseMailto s =
  case break (==':') s of
       (xs,':':addr) | map toLower xs == "mailto" -> do
         let (name', rest) = span (/='@') addr
         let domain = drop 1 rest
         return (name', domain)
       _ -> Prelude.fail "not a mailto: URL"

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
                     preEscapedString $ "<a href=\"" ++ obfuscateString s'
                     ++ "\" class=\"email\">" ++ obfuscateString txt ++ "</a>"
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

-- | Create HTML tag with attributes.
tagWithAttributes :: WriterOptions
                  -> Bool -- ^ True for HTML5
                  -> Bool -- ^ True if self-closing tag
                  -> Text -- ^ Tag text
                  -> Attr -- ^ Pandoc style tag attributes
                  -> Text
tagWithAttributes opts html5 selfClosing tagname attr =
  let mktag = (TL.toStrict . renderHtml <$> evalStateT
               (addAttrs opts attr (customLeaf (textTag tagname) selfClosing))
               defaultWriterState{ stHtml5 = html5 })
  in  case runPure mktag of
           Left _  -> mempty
           Right t -> t

addAttrs :: PandocMonad m
         => WriterOptions -> Attr -> Html -> StateT WriterState m Html
addAttrs opts attr h = foldl (!) h <$> attrsToHtml opts attr

toAttrs :: PandocMonad m
        => [(String, String)] -> StateT WriterState m [Attribute]
toAttrs kvs = do
  html5 <- gets stHtml5
  mbEpubVersion <- gets stEPUBVersion
  return $ mapMaybe (\(x,y) ->
            if html5
               then
                  if x `Set.member` (html5Attributes <> rdfaAttributes)
                     || ':' `elem` x -- e.g. epub: namespace
                     || "data-" `isPrefixOf` x
                     || "aria-" `isPrefixOf` x
                     then Just $ customAttribute (fromString x) (toValue y)
                     else Just $ customAttribute (fromString ("data-" ++ x))
                                  (toValue y)
               else
                 if mbEpubVersion == Just EPUB2 &&
                    not (x `Set.member` (html4Attributes <> rdfaAttributes) ||
                         "xml:" `isPrefixOf` x)
                    then Nothing
                    else Just $ customAttribute (fromString x) (toValue y))
            kvs

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
dimensionsToAttrList attr = consolidateStyles $ go Width ++ go Height
  where
    consolidateStyles :: [(String, String)] -> [(String, String)]
    consolidateStyles xs =
      case partition isStyle xs of
           ([], _)    -> xs
           (ss, rest) -> ("style", intercalate ";" $ map snd ss) : rest
    isStyle ("style", _) = True
    isStyle _            = False
    go dir = case dimension dir attr of
               (Just (Pixel a)) -> [(show dir, show a)]
               (Just x)         -> [("style", show dir ++ ":" ++ show x)]
               Nothing          -> []

figure :: PandocMonad m
       => WriterOptions -> Attr -> [Inline] -> (String, String)
       -> StateT WriterState m Html
figure opts attr txt (s,tit) = do
  img <- inlineToHtml opts (Image attr [Str ""] (s,tit))
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

showSecNum :: [Int] -> String
showSecNum = intercalate "." . map show

getNumber :: WriterOptions -> Attr -> String
getNumber opts (_,_,kvs) =
  showSecNum $ zipWith (+) num (writerNumberOffset opts ++ repeat 0)
  where
   num = maybe [] (map (fromMaybe 0 . safeRead) . splitWhen (=='.')) $
          lookup "number" kvs

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
blockToHtml opts (Para lst) = do
  contents <- inlineListToHtml opts lst
  case contents of
       Empty _ | not (isEnabled Ext_empty_paragraphs opts) -> return mempty
       _ -> return $ H.p contents
blockToHtml opts (LineBlock lns) =
  if writerWrapText opts == WrapNone
  then blockToHtml opts $ linesToPara lns
  else do
    htmlLines <- inlineListToHtml opts $ intercalate [LineBreak] lns
    return $ H.div ! A.class_ "line-block" $ htmlLines
blockToHtml opts (Div (ident, "section":dclasses, dkvs)
                   (Header level hattr ils : xs)) = do
  slideVariant <- gets stSlideVariant
  slideLevel <- gets stSlideLevel
  let slide = slideVariant /= NoSlides &&
               level <= slideLevel {- DROPPED old fix for #5168 here -}
  html5 <- gets stHtml5
  let titleSlide = slide && level < slideLevel
  let level' = if level <= slideLevel && slideVariant == SlidySlides
                  then 1 -- see #3566
                  else level
  header' <- if ils == [Str "\0"]  -- marker for hrule
                then return mempty
                else blockToHtml opts (Header level' hattr ils)
  let isSec (Div (_,"section":_,_) _) = True
      isSec (Div _ zs)                = any isSec zs
      isSec _                         = False
  let isPause (Para [Str ".",Space,Str ".",Space,Str "."]) = True
      isPause _                                            = False
  let fragmentClass = case slideVariant of
                           RevealJsSlides -> "fragment"
                           _              -> "incremental"
  let inDiv zs = (RawBlock (Format "html") ("<div class=\""
                       ++ fragmentClass ++ "\">")) :
                   (zs ++ [RawBlock (Format "html") "</div>"])
  let (titleBlocks, innerSecs) =
        if titleSlide
           -- title slides have no content of their own
           then break isSec xs
           else case splitBy isPause xs of
                     []     -> ([],[])
                     (z:zs) -> ([],z ++ concatMap inDiv zs)
  titleContents <- blockListToHtml opts titleBlocks
  innerContents <- blockListToHtml opts innerSecs
  let classes' = ["title-slide" | titleSlide] ++ ["slide" | slide] ++
                  ["section" | (slide || writerSectionDivs opts) &&
                               not html5 ] ++
                  ["level" ++ show level | slide || writerSectionDivs opts ]
                  ++ dclasses
  let secttag  = if html5
                    then H5.section
                    else H.div
  let attr = (ident, classes', dkvs)
  if titleSlide
     then do
       t <- addAttrs opts attr $ secttag $ header' <> titleContents
       return $
         (if slideVariant == RevealJsSlides && not (null innerSecs)
             -- revealjs doesn't like more than one level of section nesting:
               {- REMOVED && isNothing mbparentlevel -}
                then H5.section
                else id) $ t <> if null innerSecs
                                   then mempty
                                   else nl opts <> innerContents
     else if writerSectionDivs opts || slide || not (null dclasses) ||
              not (null dkvs)
          then addAttrs opts attr
               $ secttag
               $ nl opts <> header' <> nl opts <>
                 if null innerSecs
                    then mempty
                    else innerContents <> nl opts
          else do
            t <- addAttrs opts attr header'
            return $ t <> if null innerSecs
                             then mempty
                             else nl opts <> innerContents
blockToHtml opts (Div attr@(ident, classes, kvs') bs) = do
  html5 <- gets stHtml5
  slideVariant <- gets stSlideVariant
  let kvs = [(k,v) | (k,v) <- kvs', k /= "width"] ++
            [("style", "width:" ++ w ++ ";")
             | ("width",w) <- kvs', "column" `elem` classes] ++
            [("role", "doc-bibliography") | ident == "refs" && html5] ++
            [("role", "doc-biblioentry")
              | "ref-item" `isPrefixOf` ident && html5]
  let speakerNotes = "notes" `elem` classes
  -- we don't want incremental output inside speaker notes, see #1394
  let opts' = if | speakerNotes -> opts{ writerIncremental = False }
                 | "incremental" `elem` classes -> opts{ writerIncremental = True }
                 | "nonincremental" `elem` classes -> opts{ writerIncremental = False }
                 | otherwise -> opts
      -- we remove "incremental" and "nonincremental" if we're in a
      -- slide presentaiton format.
      classes' = case slideVariant of
        NoSlides -> classes
        _ -> filter (\k -> k /= "incremental" && k /= "nonincremental") classes
  contents <- if "columns" `elem` classes'
                 then -- we don't use blockListToHtml because it inserts
                      -- a newline between the column divs, which throws
                      -- off widths! see #4028
                      mconcat <$> mapM (blockToHtml opts) bs
                 else blockListToHtml opts' bs
  let contents' = nl opts >> contents >> nl opts
  let (divtag, classes'') = if html5 && "section" `elem` classes'
                            then (H5.section, filter (/= "section") classes')
                            else (H.div, classes')
  if speakerNotes
     then case slideVariant of
               RevealJsSlides -> addAttrs opts' attr $
                           H5.aside contents'
               DZSlides       -> do
                 t <- addAttrs opts' attr $
                             H5.div contents'
                 return $ t ! H5.customAttribute "role" "note"
               NoSlides       -> addAttrs opts' attr $
                           H.div contents'
               _              -> return mempty
     else addAttrs opts (ident, classes'', kvs) $
              divtag contents'
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
blockToHtml _ HorizontalRule = do
  html5 <- gets stHtml5
  return $ if html5 then H5.hr else H.hr
blockToHtml opts (CodeBlock (id',classes,keyvals) rawCode) = do
  id'' <- if null id'
             then do
               modify $ \st -> st{ stCodeBlockNum = stCodeBlockNum st + 1 }
               codeblocknum <- gets stCodeBlockNum
               return (writerIdentifierPrefix opts ++ "cb" ++ show codeblocknum)
             else return (writerIdentifierPrefix opts ++ id')
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
                            (id'',classes',keyvals) adjCode
                    else Left ""
  case hlCode of
         Left msg -> do
           unless (null msg) $
             report $ CouldNotHighlight msg
           addAttrs opts (id',classes,keyvals)
             $ H.pre $ H.code $ toHtml adjCode
         Right h -> modify (\st -> st{ stHighlighting = True }) >>
                    -- we set writerIdentifierPrefix to "" since id'' already
                    -- includes it:
                    addAttrs opts{writerIdentifierPrefix = ""} (id'',[],keyvals) h
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
  let secnum = getNumber opts attr
  let contents' = if writerNumberSections opts && not (null secnum)
                     && "unnumbered" `notElem` classes
                     then (H.span ! A.class_ "header-section-number"
                             $ toHtml secnum) >> strToHtml " " >> contents
                     else contents
  addAttrs opts attr
         $ case level of
              1 -> H.h1 contents'
              2 -> H.h2 contents'
              3 -> H.h3 contents'
              4 -> H.h4 contents'
              5 -> H.h5 contents'
              6 -> H.h6 contents'
              _ -> H.p ! A.class_ "heading" $ contents'
blockToHtml opts (BulletList lst) = do
  contents <- mapM (listItemToHtml opts) lst
  unordList opts contents
blockToHtml opts (OrderedList (startnum, numstyle, _) lst) = do
  contents <- mapM (listItemToHtml opts) lst
  html5 <- gets stHtml5
  let numstyle' = case numstyle of
                       Example -> "decimal"
                       _       -> camelCaseToHyphenated $ show numstyle
  let attribs = [A.start $ toValue startnum | startnum /= 1] ++
                [A.class_ "example" | numstyle == Example] ++
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
                  do term' <- liftM H.dt $ inlineListToHtml opts term
                     defs' <- mapM (liftM (\x -> H.dd (x >> nl opts)) .
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
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- zipWithM
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
  return $ tag'' contents >> nl opts

toListItems :: WriterOptions -> [Html] -> [Html]
toListItems opts items = map (toListItem opts) items ++ [nl opts]

toListItem :: WriterOptions -> Html -> Html
toListItem opts item = nl opts >> H.li item

blockListToHtml :: PandocMonad m
                => WriterOptions -> [Block] -> StateT WriterState m Html
blockListToHtml opts lst =
  (mconcat . intersperse (nl opts) . filter nonempty)
    <$> mapM (blockToHtml opts) lst
  where nonempty (Empty _) = False
        nonempty _         = True

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
    (Str str)      -> return $ strToHtml str
    Space          -> return $ strToHtml " "
    SoftBreak      -> return $ case writerWrapText opts of
                                     WrapNone     -> preEscapedString " "
                                     WrapAuto     -> preEscapedString " "
                                     WrapPreserve -> preEscapedString "\n"
    LineBreak      -> return $ do
                        if html5 then H5.br else H.br
                        strToHtml "\n"

    (Span (id',classes,kvs) ils) ->
                        let spanLikeTag = case classes of
                                [c] -> do
                                  let c' = T.pack c
                                  guard (c' `Set.member` htmlSpanLikeElements)
                                  pure $ customParent (textTag c')
                                _   -> Nothing
                        in case spanLikeTag of
                            Just tag -> tag <$> inlineListToHtml opts ils
                            Nothing -> do
                              h <- inlineListToHtml opts ils
                              addAttrs opts (id',classes',kvs') (H.span h)
                            where
                              styles = ["font-style:normal;"
                                       | "csl-no-emph" `elem` classes]
                                    ++ ["font-weight:normal;"
                                       | "csl-no-strong" `elem` classes]
                                    ++ ["font-variant:normal;"
                                       | "csl-no-smallcaps" `elem` classes]
                              kvs' = if null styles
                                        then kvs
                                        else ("style", concat styles) : kvs
                              classes' = [ c | c <- classes
                                         , c `notElem` [ "csl-no-emph"
                                                       , "csl-no-strong"
                                                       , "csl-no-smallcaps"
                                                       ]
                                         ]

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

                        in if writerHtmlQTags opts
                               then do
                                 modify $ \st -> st{ stQuotes = True }
                                 let (maybeAttr, lst') = case lst of
                                      [Span attr@(_, _, kvs) cs]
                                        | any ((=="cite") . fst) kvs
                                          -> (Just attr, cs)
                                      cs -> (Nothing, cs)
                                 H.q `fmap` inlineListToHtml opts lst'
                                   >>= maybe return (addAttrs opts) maybeAttr
                               else (\x -> leftQuote >> x >> rightQuote)
                                    `fmap` inlineListToHtml opts lst
    (Math t str) -> do
      modify (\st -> st {stMath = True})
      let mathClass = toValue $ ("math " :: String) ++
                      if t == InlineMath then "inline" else "display"
      case writerHTMLMathMethod opts of
           WebTeX url -> do
              let imtag = if html5 then H5.img else H.img
              let s = case t of
                           InlineMath  -> "\\textstyle "
                           DisplayMath -> "\\displaystyle "
              let m = imtag ! A.style "vertical-align:middle"
                            ! A.src (toValue $ url ++ urlEncode (s ++ str))
                            ! A.alt (toValue str)
                            ! A.title (toValue str)
              let brtag = if html5 then H5.br else H.br
              return $ case t of
                        InlineMath  -> m
                        DisplayMath -> brtag >> m >> brtag
           GladTeX ->
              return $
                customParent (textTag "eq") !
                  customAttribute "env"
                    (toValue $ if t == InlineMath
                                  then ("math" :: Text)
                                  else "displaymath") $ strToHtml str
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
           KaTeX _ -> return $ H.span ! A.class_ mathClass $ toHtml $
              case t of
                InlineMath  -> str
                DisplayMath -> str
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
                        link' <- addAttrs opts (ident, classes, kvs) link
                        return $ if null tit
                                    then link'
                                    else link' ! A.title (toValue tit)
    (Image attr txt (s,tit)) -> do
                        let alternate = stringify txt
                        slideVariant <- gets stSlideVariant
                        let isReveal = slideVariant == RevealJsSlides
                        attrs <- imgAttrsToHtml opts attr
                        let attributes =
                              -- reveal.js uses data-src for lazy loading
                              (if isReveal
                                  then customAttribute "data-src" $ toValue s
                                  else A.src $ toValue s) :
                              [A.title $ toValue tit | not (null tit)] ++
                              attrs
                            imageTag = (if html5 then H5.img else H.img
                              , [A.alt $ toValue alternate | not (null txt)] )
                            mediaTag tg fallbackTxt =
                              let linkTxt = if null txt
                                            then fallbackTxt
                                            else alternate
                              in (tg $ H.a ! A.href (toValue s) $ toHtml linkTxt
                                 , [A5.controls ""] )
                            normSrc = maybe s uriPath (parseURIReference s)
                            (tag, specAttrs) = case mediaCategory normSrc of
                              Just "image" -> imageTag
                              Just "video" -> mediaTag H5.video "Video"
                              Just "audio" -> mediaTag H5.audio "Audio"
                              Just _       -> (H5.embed, [])
                              _            -> imageTag
                        return $ foldl (!) tag $ attributes ++ specAttrs
                        -- note:  null title included, as in Markdown.pl
    (Note contents) -> do
                        notes <- gets stNotes
                        let number = length notes + 1
                        let ref = show number
                        htmlContents <- blockListToNote opts ref contents
                        epubVersion <- gets stEPUBVersion
                        -- push contents onto front of notes
                        modify $ \st -> st {stNotes = htmlContents:notes}
                        slideVariant <- gets stSlideVariant
                        let revealSlash = ['/' | slideVariant == RevealJsSlides]
                        let link = H.a ! A.href (toValue $ "#" ++
                                         revealSlash ++
                                         writerIdentifierPrefix opts ++ "fn" ++ ref)
                                       ! A.class_ "footnote-ref"
                                       ! prefixedId opts ("fnref" ++ ref)
                                       $ (if isJust epubVersion
                                             then id
                                             else H.sup)
                                       $ toHtml ref
                        return $ case epubVersion of
                                      Just EPUB3 -> link ! customAttribute "epub:type" "noteref"
                                      _ | html5  -> link ! H5.customAttribute
                                                      "role" "doc-noteref"
                                      _          -> link
    (Cite cits il)-> do contents <- inlineListToHtml opts (walk addRoleToLink il)
                        let citationIds = unwords $ map citationId cits
                        let result = H.span ! A.class_ "citation" $ contents
                        return $ if html5
                                    then result ! customAttribute "data-cites" (toValue citationIds)
                                    else result

addRoleToLink :: Inline -> Inline
addRoleToLink (Link (id',classes,kvs) ils (src,tit)) =
  Link (id',classes,("role","doc-biblioref"):kvs) ils (src,tit)
addRoleToLink x = x

blockListToNote :: PandocMonad m
                => WriterOptions -> String -> [Block]
                -> StateT WriterState m Html
blockListToNote opts ref blocks = do
  html5 <- gets stHtml5
  -- If last block is Para or Plain, include the backlink at the end of
  -- that block. Otherwise, insert a new Plain block with the backlink.
  let kvs = if html5 then [("role","doc-backlink")] else []
  let backlink = [Link ("",["footnote-back"],kvs)
                    [Str "↩"] ("#" ++ "fnref" ++ ref,[])]
  let blocks'  = if null blocks
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
  contents <- blockListToHtml opts blocks'
  let noteItem = H.li ! prefixedId opts ("fn" ++ ref) $ contents
  epubVersion <- gets stEPUBVersion
  let noteItem' = case epubVersion of
                       Just EPUB3 -> noteItem !
                                       customAttribute "epub:type" "footnote"
                       _ | html5  -> noteItem !
                                       customAttribute "role" "doc-endnote"
                       _          -> noteItem
  return $ nl opts >> noteItem'

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
allowsMathEnvironments MathML      = True
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
  , "role"
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

-- See https://en.wikipedia.org/wiki/RDFa, https://www.w3.org/TR/rdfa-primer/
rdfaAttributes :: Set.Set String
rdfaAttributes = Set.fromList
  [ "about"
  , "rel"
  , "rev"
  , "src"
  , "href"
  , "resource"
  , "property"
  , "content"
  , "datatype"
  , "typeof"
  , "vocab"
  , "prefix"
  ]

html4Attributes :: Set.Set String
html4Attributes = Set.fromList
  [ "abbr"
  , "accept"
  , "accept-charset"
  , "accesskey"
  , "action"
  , "align"
  , "alink"
  , "alt"
  , "archive"
  , "axis"
  , "background"
  , "bgcolor"
  , "border"
  , "cellpadding"
  , "cellspacing"
  , "char"
  , "charoff"
  , "charset"
  , "checked"
  , "cite"
  , "class"
  , "classid"
  , "clear"
  , "code"
  , "codebase"
  , "codetype"
  , "color"
  , "cols"
  , "colspan"
  , "compact"
  , "content"
  , "coords"
  , "data"
  , "datetime"
  , "declare"
  , "defer"
  , "dir"
  , "disabled"
  , "enctype"
  , "face"
  , "for"
  , "frame"
  , "frameborder"
  , "headers"
  , "height"
  , "href"
  , "hreflang"
  , "hspace"
  , "http-equiv"
  , "id"
  , "ismap"
  , "label"
  , "lang"
  , "language"
  , "link"
  , "longdesc"
  , "marginheight"
  , "marginwidth"
  , "maxlength"
  , "media"
  , "method"
  , "multiple"
  , "name"
  , "nohref"
  , "noresize"
  , "noshade"
  , "nowrap"
  , "object"
  , "onblur"
  , "onchange"
  , "onclick"
  , "ondblclick"
  , "onfocus"
  , "onkeydown"
  , "onkeypress"
  , "onkeyup"
  , "onload"
  , "onmousedown"
  , "onmousemove"
  , "onmouseout"
  , "onmouseover"
  , "onmouseup"
  , "onreset"
  , "onselect"
  , "onsubmit"
  , "onunload"
  , "profile"
  , "prompt"
  , "readonly"
  , "rel"
  , "rev"
  , "rows"
  , "rowspan"
  , "rules"
  , "scheme"
  , "scope"
  , "scrolling"
  , "selected"
  , "shape"
  , "size"
  , "span"
  , "src"
  , "standby"
  , "start"
  , "style"
  , "summary"
  , "tabindex"
  , "target"
  , "text"
  , "title"
  , "usemap"
  , "valign"
  , "value"
  , "valuetype"
  , "version"
  , "vlink"
  , "vspace"
  , "width"
  ]

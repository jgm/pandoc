{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Writers.HTML
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
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
    ( StateT, MonadState(get), gets, modify, evalStateT )
import Control.Monad ( liftM, when, foldM, unless )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.Char (ord, isSpace, isAscii)
import Data.List (intercalate, intersperse, partition, delete, (\\))
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.URI (URI (..), parseURIReference, escapeURIString)
import Text.Pandoc.URI (urlEncode)
import Numeric (showHex)
import Text.DocLayout (render, literal, Doc)
import Text.Blaze.Internal (MarkupM (Empty), customLeaf, customParent)
import Text.DocTemplates (FromContext (lookupContext), Context (..))
import qualified Text.DocTemplates.Internal as DT
import Text.Blaze.Html hiding (contents)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (formatHtmlBlock, formatHtml4Block,
                 formatHtmlInline, highlight, styleToCss, defaultStyle)
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Slides
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann
import Text.Pandoc.XML (escapeStringForXML, fromEntities, toEntities,
                        html5Attributes, html4Attributes, rdfaAttributes)
import qualified Text.Blaze.XHtml5 as H5
import qualified Text.Blaze.XHtml5.Attributes as A5
import Control.Monad.Except (throwError)
import System.FilePath (takeBaseName)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.XHtml1.Transitional as H
import qualified Text.Blaze.XHtml1.Transitional.Attributes as A
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Translations (Term(Abstract), translateTerm)
import Text.Pandoc.Class.PandocPure (runPure)
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.MIME (mediaCategory)
import Text.Pandoc.Writers.Blaze (layoutMarkup)
import Text.TeXMath
import Text.XML.Light (elChildren, unode, unqual)
import qualified Text.XML.Light as XML
import Text.XML.Light.Output
import Data.String (fromString)

data WriterState = WriterState
    { stNotes        :: [Html]  -- ^ List of notes
    , stEmittedNotes :: Int     -- ^ How many notes we've already pushed out to the HTML
    , stEmittedNoteBlocks :: Int  -- ^ How many @\<div class=footnote>@ blocks we've already pushed out
    , stMath         :: Bool    -- ^ Math is used in document
    , stQuotes       :: Bool    -- ^ <q> tag is used
    , stHighlighting :: Bool    -- ^ Syntax highlighting is used
    , stHtml5        :: Bool    -- ^ Use HTML5
    , stEPUBVersion  :: Maybe EPUBVersion -- ^ EPUB version if for epub
    , stSlideVariant :: HTMLSlideVariant
    , stSlideLevel   :: Int     -- ^ Slide level
    , stInSection    :: Bool    -- ^ Content is in a section (revealjs)
    , stCodeBlockNum :: Int     -- ^ Number of code block
    , stCsl          :: Bool    -- ^ Has CSL references
    , stCslEntrySpacing :: Maybe Int  -- ^ CSL entry spacing
    , stBlockLevel   :: Int     -- ^ Current block depth, excluding section divs
    }

defaultWriterState :: WriterState
defaultWriterState = WriterState {stNotes= [],
                                  stEmittedNotes = 0,
                                  stEmittedNoteBlocks = 0,
                                  stMath = False,
                                  stQuotes = False,
                                  stHighlighting = False,
                                  stHtml5 = False,
                                  stEPUBVersion = Nothing,
                                  stSlideVariant = NoSlides,
                                  stSlideLevel = 1,
                                  stInSection = False,
                                  stCodeBlockNum = 0,
                                  stCsl = False,
                                  stCslEntrySpacing = Nothing,
                                  stBlockLevel = 0}

-- Helpers to render HTML with the appropriate function.

strToHtml :: Text -> Html
strToHtml t
    | T.any isSpecial t =
       let !x = L.foldl' go mempty $ T.groupBy samegroup t
        in x
    | otherwise = toHtml t
  where
    samegroup c d = d == '\xFE0E' || not (isSpecial c || isSpecial d)
    isSpecial '\'' = True
    isSpecial '"' = True
    isSpecial c = needsVariationSelector c
    go h "\'" = h <> preEscapedString "\'"
    go h "\"" = h <> preEscapedString "\""
    go h txt | T.length txt == 1 && T.all needsVariationSelector txt
           = h <> preEscapedString (T.unpack txt <> "\xFE0E")
    go h txt = h <> toHtml txt

-- See #5469: this prevents iOS from substituting emojis.
needsVariationSelector :: Char -> Bool
needsVariationSelector '↩' = True
needsVariationSelector '↔' = True
needsVariationSelector _   = False

-- | Hard linebreak.
nl :: Html
nl = preEscapedString "\n"

-- | Convert Pandoc document to Html 5 string.
writeHtml5String :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHtml5String = writeHtmlString'
                      defaultWriterState{ stHtml5 = True }

-- | Convert Pandoc document to Html 5 structure.
writeHtml5 :: PandocMonad m => WriterOptions -> Pandoc -> m Html
writeHtml5 = writeHtml' defaultWriterState{ stHtml5 = True }

-- | Convert Pandoc document to Html 4 string.
writeHtml4String :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHtml4String opts = writeHtmlString'
                         defaultWriterState{ stHtml5 = False } opts .
                        ensureValidXmlIdentifiers

-- | Convert Pandoc document to Html 4 structure.
writeHtml4 :: PandocMonad m => WriterOptions -> Pandoc -> m Html
writeHtml4 opts = writeHtml' defaultWriterState{ stHtml5 = False } opts .
                    ensureValidXmlIdentifiers

-- | Convert Pandoc document to Html appropriate for an epub version.
writeHtmlStringForEPUB :: PandocMonad m
                       => EPUBVersion -> WriterOptions -> Pandoc
                       -> m Text
writeHtmlStringForEPUB version o = writeHtmlString'
                      defaultWriterState{ stHtml5 = version == EPUB3,
                                          stEPUBVersion = Just version }
                      o{ writerWrapText = WrapNone }
   -- we don't use ensureValidXmlIdentifiers here because we
   -- do that in the EPUB writer

-- | Convert Pandoc document to Reveal JS HTML slide show.
writeRevealJs :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeRevealJs = writeHtmlSlideShow' RevealJsSlides

-- | Convert Pandoc document to S5 HTML slide show.
writeS5 :: PandocMonad m
        => WriterOptions -> Pandoc -> m Text
writeS5 opts = writeHtmlSlideShow' S5Slides opts .
               ensureValidXmlIdentifiers

-- | Convert Pandoc document to Slidy HTML slide show.
writeSlidy :: PandocMonad m
           => WriterOptions -> Pandoc -> m Text
writeSlidy opts = writeHtmlSlideShow' SlidySlides opts .
                  ensureValidXmlIdentifiers

-- | Convert Pandoc document to Slideous HTML slide show.
writeSlideous :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeSlideous opts = writeHtmlSlideShow' SlideousSlides opts .
                     ensureValidXmlIdentifiers

-- | Convert Pandoc document to DZSlides HTML slide show.
writeDZSlides :: PandocMonad m
              => WriterOptions -> Pandoc -> m Text
writeDZSlides opts = writeHtmlSlideShow' DZSlides opts

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
  let colwidth = case writerWrapText opts of
                    WrapAuto -> Just (writerColumns opts)
                    _ -> Nothing
  (if writerPreferAscii opts
      then toEntities
      else id) <$>
    case writerTemplate opts of
       Nothing -> return $
         case colwidth of
           Nothing -> renderHtml' body  -- optimization, skip layout
           Just cols -> render (Just cols) $ layoutMarkup body
       Just tpl -> do
         -- warn if empty lang
         when (isNothing (getField "lang" context :: Maybe Text) &&
               hasVariable "lang" tpl) $
           report NoLangSpecified
         (context' :: Context Text) <-
            -- check for empty pagetitle
            case getField "pagetitle" context of
                 Just (s :: Text) | not (T.null s) -> return context
                 _ | hasVariable "pagetitle" tpl -> do
                       let fallback = T.pack $
                             case lookupContext "sourcefile"
                                       (writerVariables opts) of
                               Nothing    -> "Untitled"
                               Just []    -> "Untitled"
                               Just (x:_) -> takeBaseName $ T.unpack x
                       report $ NoTitleElement fallback
                       return $ resetField "pagetitle" (literal fallback) context
                   | otherwise -> return context
         return $ render colwidth $ renderTemplate tpl
             (defField "body" (layoutMarkup body) context')

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
  lift $ setupTranslations meta
  let slideLevel = fromMaybe (getSlideLevel blocks) $ writerSlideLevel opts
  modify $ \st -> st{ stSlideLevel = slideLevel }
  metadata <- metaToContext opts
              (fmap layoutMarkup . blockListToHtml opts)
              (fmap layoutMarkup . inlineListToHtml opts)
              meta
  let stringifyHTML = escapeStringForXML . stringify
  let authsMeta = map (literal . stringifyHTML) $ docAuthors meta
  let dateMeta  = stringifyHTML $ docDate meta
  let descriptionMeta = literal $ escapeStringForXML $
                          lookupMetaString "description" meta
  slideVariant <- gets stSlideVariant
  abstractTitle <- translateTerm Abstract
  let sects = makeSectionsWithOffsets
                (writerNumberOffset opts) (writerNumberSections opts) Nothing $
              if slideVariant == NoSlides
                 then blocks
                 else prepSlides slideLevel blocks
  toc <- if writerTableOfContents opts && slideVariant /= S5Slides
            then fmap layoutMarkup <$> tableOfContents opts sects
            else return Nothing
  blocks' <- blockListToHtml opts sects
  notes <- do
    -- make the st private just to be safe, since we modify it right afterwards
    st <- get
    if null (stNotes st)
      then return mempty
      else do
        notes <- footnoteSection opts EndOfDocument (stEmittedNotes st + 1) (reverse (stNotes st))
        modify (\st' -> st'{ stNotes = mempty, stEmittedNotes = stEmittedNotes st' + length (stNotes st') })
        return notes
  st <- get
  let html5 = stHtml5 st
  let thebody = blocks' >> notes
  let math = layoutMarkup $ case writerHTMLMathMethod opts of
        MathJax url
          | slideVariant /= RevealJsSlides ->
          -- mathjax is handled via a special plugin in revealjs
            H.script ! A.defer mempty
                    ! A.src (toValue $ toURI html5 url)
                    ! A.type_ "text/javascript"
                    $ case slideVariant of
                            SlideousSlides ->
                              preEscapedString
                              "MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);"
                            _ -> mempty
        KaTeX url -> do
          H.script !
            A.defer mempty !
            A.src (toValue $ toURI html5 $ url <> "katex.min.js") $ mempty
          nl
          let katexFlushLeft =
                case lookupContext "classoption" metadata of
                  Just clsops | "fleqn" `elem` (clsops :: [Doc Text]) -> "true"
                  _ -> "false"
          H.script $ text $ T.unlines [
              "document.addEventListener(\"DOMContentLoaded\", function () {"
            , " var mathElements = document.getElementsByClassName(\"math\");"
            , " var macros = [];"
            , " for (var i = 0; i < mathElements.length; i++) {"
            , "  var texText = mathElements[i].firstChild;"
            , "  if (mathElements[i].tagName == \"SPAN\") {"
            , "   katex.render(texText.data, mathElements[i], {"
            , "    displayMode: mathElements[i].classList.contains('display'),"
            , "    throwOnError: false,"
            , "    macros: macros,"
            , "    fleqn: " <> katexFlushLeft
            , "   });"
            , "}}});"
            ]
          nl
          H.link ! A.rel "stylesheet" !
            A.href (toValue $ toURI html5 url <> "katex.min.css")

        _ -> mempty
  let mCss :: Maybe [Text] = lookupContext "css" metadata
  let context :: Context Text
      context =   (if stHighlighting st
                      then case writerHighlightMethod opts of
                             Skylighting sty ->
                               defField "highlighting-css"
                                 (literal $ T.pack $ styleToCss sty)
                             DefaultHighlighting ->
                               defField "highlighting-css"
                                 (literal $ T.pack $ styleToCss defaultStyle)
                             _  -> id
                      else id) .
                  (if stCsl st
                      then defField "csl-css" True .
                           (case stCslEntrySpacing st of
                              Nothing -> id
                              Just n  ->
                                defField "csl-entry-spacing"
                                  (literal $ tshow n <> "em"))
                      else id) .
                  (if stMath st
                      then defField "math" math
                      else id) .
                  defField "abstract-title" abstractTitle .
                  (case writerHTMLMathMethod opts of
                        MathJax u -> defField "mathjax" True .
                                     defField "mathjaxurl"
                                       (literal $ T.takeWhile (/='?') u)
                        _         -> defField "mathjax" False) .
                  (case writerHTMLMathMethod opts of
                        PlainMath -> defField "displaymath-css" True
                        WebTeX _  -> defField "displaymath-css" True
                        _         -> id) .
                  (if slideVariant == RevealJsSlides
                      then -- set boolean options explicitly, since
                           -- template can't distinguish False/undefined
                         defField "controls" True .
                         defField "controlsTutorial" True .
                         defField "controlsLayout"
                           ("bottom-right" :: Doc Text) .
                         defField "controlsBackArrows" ("faded" :: Doc Text) .
                         defField "progress" True .
                         defField "slideNumber" False .
                         defField "showSlideNumber" ("all" :: Doc Text) .
                         defField "hashOneBasedIndex" False .
                         defField "hash" True .
                         defField "respondToHashChanges" True .
                         defField "history" False .
                         defField "keyboard" True .
                         defField "overview" True .
                         defField "disableLayout" False .
                         defField "center" True .
                         defField "touch" True .
                         defField "loop" False .
                         defField "rtl" False .
                         defField "navigationMode" ("default" :: Doc Text) .
                         defField "shuffle" False .
                         defField "fragments" True .
                         defField "fragmentInURL" True .
                         defField "embedded" False .
                         defField "help" True .
                         defField "pause" True .
                         defField "showNotes" False .
                         defField "autoPlayMedia" ("null" :: Doc Text) .
                         defField "preloadIframes" ("null" :: Doc Text) .
                         defField "autoSlide" ("0" :: Doc Text) .
                         defField "autoSlideStoppable" True .
                         defField "autoSlideMethod" ("null" :: Doc Text) .
                         defField "defaultTiming" ("null" :: Doc Text) .
                         defField "mouseWheel" False .
                         defField "display" ("block" :: Doc Text) .
                         defField "hideInactiveCursor" True .
                         defField "hideCursorTime" ("5000" :: Doc Text) .
                         defField "previewLinks" False .
                         defField "transition" ("slide" :: Doc Text) .
                         defField "transitionSpeed" ("default" :: Doc Text) .
                         defField "backgroundTransition" ("fade" :: Doc Text) .
                         defField "viewDistance" ("3" :: Doc Text) .
                         defField "mobileViewDistance" ("2" :: Doc Text) .
                         defField "scrollProgress" True .
                         defField "scrollActivationWidth" ("0" :: Doc Text) .
                         defField "scrollSnap" ("mandatory" :: Doc Text) .
                         defField "scrollLayout" ("full" :: Doc Text)
                      else id) .
                  defField "document-css" (isNothing mCss && slideVariant == NoSlides) .
                  defField "quotes" (stQuotes st) .
                  -- for backwards compatibility we populate toc
                  -- with the contents of the toc, rather than a
                  -- boolean:
                  maybe id (defField "toc") toc .
                  maybe id (defField "table-of-contents") toc .
                  defField "author-meta" authsMeta .
                  maybe id (defField "date-meta" . literal)
                    (normalizeDate dateMeta) .
                  defField "description-meta" descriptionMeta .
                  defField "pagetitle"
                      (literal . stringifyHTML . docTitle $ meta) .
                  defField "idprefix" (literal $ writerIdentifierPrefix opts) .
                  -- these should maybe be set in pandoc.hs
                  defField "slidy-url"
                    ("https://www.w3.org/Talks/Tools/Slidy2" :: Doc Text) .
                  defField "slideous-url" ("slideous" :: Doc Text) .
                  defField "revealjs-url" ("https://unpkg.com/reveal.js@^5" :: Doc Text) $
                  defField "s5-url" ("s5/default" :: Doc Text) .
                  defField "table-caption-below"
                     (writerTableCaptionPosition opts == CaptionBelow) .
                  defField "html5" (stHtml5 st) $
                  metadata
  return (thebody, context)

-- | Like Text.XHtml's identifier, but adds the writerIdentifierPrefix
prefixedId :: WriterOptions -> Text -> Attribute
prefixedId opts s =
  case s of
    "" -> mempty
    _  -> A.id $ toValue $ writerIdentifierPrefix opts <> s

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
unordList opts = toList H.ul opts . toListItems

ordList :: PandocMonad m
        => WriterOptions -> [Html] -> StateT WriterState m Html
ordList opts = toList H.ol opts . toListItems

defList :: PandocMonad m
        => WriterOptions -> [Html] -> StateT WriterState m Html
defList opts items = toList H.dl opts (items ++ [nl])

listItemToHtml :: PandocMonad m
               => WriterOptions -> [Block] -> StateT WriterState m Html
listItemToHtml opts bls =
  case toTaskListItem bls of
    Just (checked, (Para is:bs)) -> taskListItem checked H.p is bs
    Just (checked, (Plain is:bs)) -> taskListItem checked id is bs
    _ -> blockListToHtml opts bls
  where
    taskListItem checked constr is bs = do
      let checkbox  = if checked
                      then checkbox' ! A.checked ""
                      else checkbox'
          checkbox' = H.input ! A.type_ "checkbox"
      isContents <- inlineListToHtml opts is
      bsContents <- blockListToHtml opts bs
      return $ constr (H.label (checkbox >> isContents)) >>
               (if null bs then mempty else nl) >>
               bsContents

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
                          "/" <> writerIdentifierPrefix opts }
                _ -> opts
  case toTableOfContents opts sects of
    bl@(BulletList (_:_)) -> Just <$> blockToHtml opts' bl
    _                     -> return Nothing

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection ::
  PandocMonad m => WriterOptions -> ReferenceLocation -> Int -> [Html] -> StateT WriterState m Html
footnoteSection opts refLocation startCounter notes = do
  html5 <- gets stHtml5
  slideVariant <- gets stSlideVariant
  let hrtag = if refLocation /= EndOfBlock
                 then (if html5 then H5.hr else H.hr) <> nl
                 else mempty
  idName <- do
    blockCount <- gets stEmittedNoteBlocks
    modify $ \st -> st{ stEmittedNoteBlocks = blockCount + 1 }
    return $
      -- Keep the first note section's id undecorated to maintain a target for
      -- old links which don't expect numbered sections, or for when the notes
      -- are rendered all together at the end of the document.
      if blockCount <= 0
        then "footnotes"
        else "footnotes-" <> show (blockCount + 1)
  let additionalClassName = case refLocation of
        EndOfBlock -> "footnotes-end-of-block"
        EndOfDocument -> "footnotes-end-of-document"
        EndOfSection -> "footnotes-end-of-section"
  let className = "footnotes " <> additionalClassName
  epubVersion <- gets stEPUBVersion
  let container x
        | html5
        , epubVersion == Just EPUB3
                = H5.section ! A.id (fromString idName)
                             ! A.class_ className
                             ! customAttribute "epub:type" "footnotes" $ x
        | html5
        , refLocation == EndOfDocument
        -- Note: we need a section for a new slide in slide formats.
                = H5.section ! prefixedId opts (fromString idName)
                             ! A5.class_ className
                             ! A5.role "doc-endnotes"
                             $ x
        | html5 = H5.aside   ! prefixedId opts (fromString idName)
                             ! A5.class_ className
                             ! A5.role "doc-footnote"
                             $ x
        | slideVariant /= NoSlides = H.div ! A.class_ "footnotes slide" $ x
        | otherwise = H.div ! A.class_ className $ x
  return $
    if null notes
       then mempty
       else do
         nl
         container $ do
           nl
           hrtag
           -- Keep the previous output exactly the same if we don't
           -- have multiple notes sections
           case epubVersion of
             Just _ -> mconcat notes
             Nothing | startCounter == 1 ->
               (H.ol (nl >> mconcat notes)) >> nl
             Nothing -> (H.ol ! A.start (fromString (show startCounter)) $
                         nl >> mconcat notes) >> nl

-- | Parse a mailto link; return Just (name, domain) or Nothing.
parseMailto :: Text -> Maybe (Text, Text)
parseMailto s =
  case T.break (==':') s of
       (xs,T.uncons -> Just (':',addr)) | T.toLower xs == "mailto" -> do
         let (name', rest) = T.span (/='@') addr
         let domain = T.drop 1 rest
         return (name', domain)
       _ -> Prelude.fail "not a mailto: URL"

-- | Obfuscate a "mailto:" link.
obfuscateLink :: PandocMonad m
              => WriterOptions -> Attr -> Html -> Text
              -> StateT WriterState m Html
obfuscateLink opts attr txt s | writerEmailObfuscation opts == NoObfuscation = do
  html5 <- gets stHtml5
  addAttrs opts attr $ H.a ! A.href (toValue $ toURI html5 s) $ txt
obfuscateLink opts attr (TL.toStrict . renderHtml -> txt) s = do
  html5 <- gets stHtml5
  let meth = writerEmailObfuscation opts
  let s' = T.toLower (T.take 7 s) <> T.drop 7 s
  case parseMailto s' of
        (Just (name', domain)) ->
          let domain'  = T.replace "." " dot " domain
              at'      = obfuscateChar '@'
              (linkText, altText) =
                 if txt == T.drop 7 s' -- autolink
                    then ("e", name' <> " at " <> domain')
                    else ("'" <> obfuscateString txt <> "'",
                          txt <> " (" <> name' <> " at " <> domain' <> ")")
              (_, classNames, _) = attr
              classNamesStr = T.concat $ map (" "<>) classNames
          in  case meth of
                ReferenceObfuscation ->
                     -- need to use preEscapedString or &'s are escaped to &amp; in URL
                     return $
                     preEscapedText $ "<a href=\"" <> obfuscateString s'
                     <> "\" class=\"email\">" <> obfuscateString txt <> "</a>"
                JavascriptObfuscation ->
                     return $
                     (H.script ! A.type_ "text/javascript" $
                     preEscapedText ("\n<!--\nh='" <>
                     obfuscateString domain <> "';a='" <> at' <> "';n='" <>
                     obfuscateString name' <> "';e=n+a+h;\n" <>
                     "document.write('<a h'+'ref'+'=\"ma'+'ilto'+':'+e+'\" clas'+'s=\"em' + 'ail" <>
                     classNamesStr <> "\">'+" <>
                     linkText  <> "+'<\\/'+'a'+'>');\n// -->\n")) >>
                     H.noscript (preEscapedText $ obfuscateString altText)
                _ -> throwError $ PandocSomeError $ "Unknown obfuscation method: " <> tshow meth
        _ -> addAttrs opts attr $ H.a ! A.href (toValue $ toURI html5 s)
                                      $ toHtml txt  -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> Text
obfuscateChar char =
  let num    = ord char
      numstr = if even num then show num else "x" <> showHex num ""
  in  "&#" <> T.pack numstr <> ";"

-- | Obfuscate string using entities.
obfuscateString :: Text -> Text
obfuscateString = T.concatMap obfuscateChar . fromEntities

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
addAttrs opts attr h = L.foldl' (!) h <$> attrsToHtml opts attr

toAttrs :: PandocMonad m
        => [(Text, Text)] -> StateT WriterState m [Attribute]
toAttrs kvs = do
  html5 <- gets stHtml5
  mbEpubVersion <- gets stEPUBVersion
  reverse . snd <$> foldM (go html5 mbEpubVersion) (Set.empty, []) kvs
 where
  go html5 mbEpubVersion (keys, attrs) (k,v) = do
    if k `Set.member` keys
       then do
         report $ DuplicateAttribute k v
         return (keys, attrs)
       else return (Set.insert k keys, addAttr html5 mbEpubVersion k v attrs)
  addAttr html5 mbEpubVersion x y
    | T.null x = id  -- see #7546
    | html5
      = if (x `Set.member` (html5Attributes <> rdfaAttributes)
            && x /= "label") -- #10048
             || T.any (== ':') x -- e.g. epub: namespace
             || "data-" `T.isPrefixOf` x
             || "aria-" `T.isPrefixOf` x
           then (customAttribute (textTag x) (toValue y) :)
           else (customAttribute (textTag ("data-" <> x)) (toValue y) :)
    | mbEpubVersion == Just EPUB2
    , not (x `Set.member` (html4Attributes <> rdfaAttributes) ||
      "xml:" `T.isPrefixOf` x)
      = id
    | otherwise
      = (customAttribute (textTag x) (toValue y) :)

attrsToHtml :: PandocMonad m
            => WriterOptions -> Attr -> StateT WriterState m [Attribute]
attrsToHtml opts (id',classes',keyvals) = do
  attrs <- toAttrs keyvals
  let classes'' = nubOrd $ filter (not . T.null) classes'
  return $
    [prefixedId opts id' | not (T.null id')] ++
    [A.class_ (toValue $ T.unwords classes'') | not (null classes'')] ++ attrs

imgAttrsToHtml :: PandocMonad m
               => WriterOptions -> Attr -> StateT WriterState m [Attribute]
imgAttrsToHtml opts attr = do
  attrsToHtml opts (ident,cls, consolidateStyles (kvs' ++ dimensionsToAttrList attr))
  where
    (ident,cls,kvs) = attr
    kvs' = filter isNotDim kvs
    isNotDim ("width", _)  = False
    isNotDim ("height", _) = False
    isNotDim _             = True
    consolidateStyles :: [(Text, Text)] -> [(Text, Text)]
    consolidateStyles xs =
      case partition isStyle xs of
           ([], _)    -> xs
           (ss, rest) -> ("style", T.intercalate ";" $ map snd ss) : rest
    isStyle ("style", _) = True
    isStyle _            = False

dimensionsToAttrList :: Attr -> [(Text, Text)]
dimensionsToAttrList attr = go Width ++ go Height
  where
    go dir = case dimension dir attr of
               (Just (Pixel a)) -> [(tshow dir, tshow a)]
               (Just x)         -> [("style", tshow dir <> ":" <> tshow x)]
               Nothing          -> []

blockToHtmlInner :: PandocMonad m => WriterOptions -> Block -> StateT WriterState m Html
blockToHtmlInner opts (Plain lst) = inlineListToHtml opts lst
blockToHtmlInner opts (Para lst) = do
  slideVariant <- gets stSlideVariant
  case (slideVariant, lst) of
    (RevealJsSlides, [Image attr@(_,classes,_) txt (src,tit)])
      | "r-stretch" `elem` classes -> do
          -- a "stretched" image in reveal.js must be a direct child
          -- of the slide container
          inlineToHtml opts (Image attr txt (src, tit))
    _ -> do
      contents <- inlineListToHtml opts lst
      case contents of
        Empty _ | not (isEnabled Ext_empty_paragraphs opts) -> return mempty
        _ -> return $ H.p contents
blockToHtmlInner opts (LineBlock lns) = do
  htmlLines <- inlineListToHtml opts $ intercalate [LineBreak] lns
  return $ H.div ! A.class_ "line-block" $ htmlLines
blockToHtmlInner opts (Div (ident, "section":dclasses, dkvs)
                   (Header level
                     hattr@(hident,hclasses,hkvs) ils : xs)) = do
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
  let inDiv' zs = RawBlock (Format "html") ("<div class=\""
                       <> fragmentClass <> "\">") :
                   (zs ++ [RawBlock (Format "html") "</div>"])
  let breakOnPauses zs
        | slide = case splitBy isPause zs of
                           []   -> []
                           y:ys -> y ++ concatMap inDiv' ys
        | otherwise = zs
  let (titleBlocks, innerSecs) =
        if titleSlide
           -- title slides have no content of their own
           then let (as, bs) = break isSec xs
                in  (breakOnPauses as, bs)
           else ([], breakOnPauses xs)
  let secttag  = if html5
                    then H5.section
                    else H.div
  titleContents <- blockListToHtml opts titleBlocks
  inSection <- gets stInSection
  innerContents <- do
    modify $ \st -> st{ stInSection = True }
    res <- blockListToHtml opts innerSecs
    modify $ \st -> st{ stInSection = inSection }
    notes <- gets stNotes
    let emitNotes = writerReferenceLocation opts == EndOfSection &&
                     not (null notes)
    if emitNotes
      then do
        st <- get
        renderedNotes <- footnoteSection opts (writerReferenceLocation opts)
                           (stEmittedNotes st + 1) (reverse notes)
        modify (\st' -> st'{ stNotes = mempty,
                             stEmittedNotes = stEmittedNotes st' + length notes })
        return (res <> renderedNotes)
      else return res
  let classes' = ["title-slide" | titleSlide] ++ ["slide" | slide] ++
                  ["section" | (slide || writerSectionDivs opts) &&
                               not html5 ] ++
                  ["level" <> tshow level | slide || writerSectionDivs opts ]
                  <> [d | d <- dclasses,
                               slideVariant /= RevealJsSlides ||
                               d /= "r-fit-text"] -- see #5965
  let attr = (ident, classes', dkvs)
  if titleSlide
     then do
       t <- addAttrs opts attr $
             secttag $ nl <> header' <> nl <> titleContents <> nl
       -- ensure 2D nesting for revealjs, but only for one level;
       -- revealjs doesn't like more than one level of nesting
       return $
         if slideVariant == RevealJsSlides && not inSection &&
              not (null innerSecs)
            then H5.section (nl <> t <> nl <> innerContents)
            else t <> nl <> if null innerSecs
                                    then mempty
                                    else innerContents <> nl
     else if writerSectionDivs opts || slide ||
              (hident /= ident && not (T.null hident || T.null ident)) ||
              (hclasses /= dclasses) || (hkvs /= dkvs)
          then addAttrs opts attr
               $ secttag
               $ nl <> header' <> nl <>
                 if null innerSecs
                    then mempty
                    else innerContents <> nl
          else do
            let attr' = (ident, classes' \\ hclasses, dkvs \\ hkvs)
            t <- addAttrs opts attr' header'
            return $ t <>
                     if null innerSecs
                        then mempty
                        else nl <> innerContents
blockToHtmlInner opts (Div (ident, classes, kvs) [b])
  | Just "1" <- lookup "wrapper" kvs
    -- unwrap "wrapper" div, putting attr on child
  = blockToHtmlInner opts b >>=
      addAttrs opts (ident, classes, [(k,v) | (k,v) <- kvs, k /= "wrapper"])
blockToHtmlInner opts (Div attr@(ident, classes, kvs') bs) = do
  html5 <- gets stHtml5
  slideVariant <- gets stSlideVariant
  let isCslBibBody = ident == "refs" || "csl-bib-body" `elem` classes
  when isCslBibBody $ modify $ \st -> st{ stCsl = True
                                        , stCslEntrySpacing =
                                           lookup "entry-spacing" kvs' >>=
                                           safeRead }
  let isCslBibEntry = "csl-entry" `elem` classes
  let kvs = [(k,v) | (k,v) <- kvs'
                   , k /= "width" || "column" `notElem` classes] ++
            [("style", "width:" <> w <> ";") | "column" `elem` classes
                                             , ("width", w) <- kvs'] ++
            [("role", "list") | isCslBibBody && html5] ++
            [("role", "listitem") | isCslBibEntry && html5]
  let speakerNotes = "notes" `elem` classes
  -- we don't want incremental output inside speaker notes, see #1394
  let (opts', isIncrDiv) =
        if | speakerNotes ->
             (opts{ writerIncremental = False }, False)
           | "incremental" `elem` classes ->
             (opts{ writerIncremental = True }, True)
           | "nonincremental" `elem` classes ->
             (opts{ writerIncremental = False }, True)
           | otherwise ->
             (opts, False)
      -- we remove "incremental" and "nonincremental" if we're in a
      -- slide presentation format.
      classes' = case slideVariant of
        NoSlides -> classes
        _ -> filter (\k -> k /= "incremental" && k /= "nonincremental") classes
  let paraToPlain (Para ils) = Plain ils
      paraToPlain x          = x
  let bs' = if "csl-entry" `elem` classes'
               then walk paraToPlain bs
               else bs
  contents <- if "columns" `elem` classes'
                 then -- we don't use blockListToHtml because it inserts
                      -- a newline between the column divs, which throws
                      -- off widths! see #4028
                      mconcat <$> mapM (blockToHtml opts) bs'
                 else blockListToHtml opts' bs'
  let contents' = nl >> contents >> nl
  let (divtag, classes'') = if html5 && "section" `elem` classes'
                            then (H5.section, filter (/= "section") classes')
                            else (H.div, classes')
  if | isIncrDiv && (ident, classes'', kvs) == nullAttr ->
         -- Unwrap divs that only have (non)increment information
         pure contents
     | speakerNotes ->
         case slideVariant of
              RevealJsSlides -> addAttrs opts' attr $
                          H5.aside contents'
              DZSlides       -> do
                t <- addAttrs opts' attr $
                            H5.div contents'
                return $ t ! A5.role "note"
              NoSlides       -> addAttrs opts' attr $
                          H.div contents'
              _              -> return mempty
     | otherwise ->
          addAttrs opts (ident, classes'', kvs) $
              divtag contents'
blockToHtmlInner opts (RawBlock f str) = do
  ishtml <- isRawHtml f
  if ishtml
     then return $ preEscapedText str
     else if (f == Format "latex" || f == Format "tex") &&
             allowsMathEnvironments (writerHTMLMathMethod opts) &&
             isMathEnvironment str
             then do
               modify (\st -> st {stMath = True})
               blockToHtml opts $ Plain [Math DisplayMath str]
             else do
               report $ BlockNotRendered (RawBlock f str)
               return mempty
blockToHtmlInner _ HorizontalRule = do
  html5 <- gets stHtml5
  return $ if html5 then H5.hr else H.hr
blockToHtmlInner opts (CodeBlock (id',classes,keyvals) rawCode) = do
  html5 <- gets stHtml5
  id'' <- if T.null id'
             then do
               modify $ \st -> st{ stCodeBlockNum = stCodeBlockNum st + 1 }
               codeblocknum <- gets stCodeBlockNum
               return (writerIdentifierPrefix opts <> "cb" <> tshow codeblocknum)
             else return (writerIdentifierPrefix opts <> id')
  let tolhs = isEnabled Ext_literate_haskell opts &&
                any (\c -> T.toLower c == "haskell") classes &&
                any (\c -> T.toLower c == "literate") classes
      classes' = if tolhs
                    then map (\c -> if T.toLower c == "haskell"
                                       then "literatehaskell"
                                       else c) classes
                    else classes
      adjCode  = if tolhs
                    then T.unlines . map ("> " <>) . T.lines $ rawCode
                    else rawCode
      highlighted = highlight (writerSyntaxMap opts)
                      (if html5 then formatHtmlBlock else formatHtml4Block)
                      (id'',classes',keyvals) adjCode
      hlCode   = case writerHighlightMethod opts of
                   Skylighting _ -> highlighted
                   DefaultHighlighting -> highlighted
                   _ -> Left ""
  case hlCode of
         Left msg -> do
           unless (T.null msg) $
             report $ CouldNotHighlight msg
           addAttrs opts (id',classes,keyvals)
             $ H.pre $ H.code $ toHtml adjCode
         Right h -> modify (\st -> st{ stHighlighting = True }) >>
                    -- we set writerIdentifierPrefix to "" since id'' already
                    -- includes it:
                    addAttrs opts{writerIdentifierPrefix = ""} (id'',[],keyvals) h
blockToHtmlInner opts (BlockQuote blocks) = do
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
                                            $ nl >> contents >> nl
     else do
       contents <- blockListToHtml opts blocks
       return $ H.blockquote $ nl >> contents >> nl
blockToHtmlInner opts (Header level (ident,classes,kvs) lst) = do
  contents <- inlineListToHtml opts lst
  let secnum = fromMaybe mempty $ lookup "number" kvs
  let contents' = if writerNumberSections opts && not (T.null secnum)
                     && "unnumbered" `notElem` classes
                     then (H.span ! A.class_ "header-section-number"
                             $ toHtml secnum) >> toHtml ' ' >> contents
                     else contents
  html5 <- gets stHtml5
  let kvs' = if html5
             then kvs
             else [ (k, v) | (k, v) <- kvs
                           , k `elem` (["lang", "dir", "title", "style"
                                      , "align"] ++ intrinsicEventsHTML4)]
  let classes' = if level > 6 then "heading":classes else classes
  addAttrs opts (ident,classes',kvs')
         $ case level of
              1 -> H.h1 contents'
              2 -> H.h2 contents'
              3 -> H.h3 contents'
              4 -> H.h4 contents'
              5 -> H.h5 contents'
              6 -> H.h6 contents'
              _ -> H.p  contents'
blockToHtmlInner opts (BulletList lst) = do
  contents <- mapM (listItemToHtml opts) lst
  (if isJust (mapM toTaskListItem lst) then (! A.class_ "task-list") else id) <$>
    unordList opts contents
blockToHtmlInner opts (OrderedList (startnum, numstyle, _) lst) = do
  contents <- mapM (listItemToHtml opts) lst
  html5 <- gets stHtml5
  let numstyle' = case numstyle of
                       Example -> "decimal"
                       _       -> camelCaseToHyphenated $ tshow numstyle
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
                           else [A.style $ toValue $ "list-style-type: " <>
                                   numstyle']
                   else [])
  l <- ordList opts contents
  return $ L.foldl' (!) l attribs
blockToHtmlInner opts (DefinitionList lst) = do
  contents <- mapM (\(term, defs) ->
                  do term' <- liftM H.dt $ inlineListToHtml opts term
                     defs' <- mapM (liftM (\x -> H.dd (nl >> x >> nl)) .
                                    blockListToHtml opts) defs
                     return $ mconcat $ nl : term' : nl :
                                        intersperse (nl) defs') lst
  defList opts contents
blockToHtmlInner opts (Table attr caption colspecs thead tbody tfoot) =
  tableToHtml opts (Ann.toTable attr caption colspecs thead tbody tfoot)
blockToHtmlInner opts (Figure attrs (Caption _ captBody)  body) = do
  html5 <- gets stHtml5

  figAttrs <- attrsToHtml opts attrs
  contents <- blockListToHtml opts body
  captCont <- blockListToHtml opts captBody
  let figCaption = mconcat $
                    if html5
                    then let fcattr = if captionIsAlt captBody body
                                      then H5.customAttribute
                                           (textTag "aria-hidden")
                                           (toValue @Text "true")
                                      else mempty
                         in [ H5.figcaption ! fcattr $ captCont ]
                    else [ (H.div ! A.class_ "figcaption") captCont ]
  let innards = mconcat $
                if null captBody
                   then [nl, contents, nl]
                   else case writerFigureCaptionPosition opts of
                         CaptionAbove -> [nl, figCaption, nl, contents, nl]
                         CaptionBelow -> [nl, contents, nl, figCaption, nl]
  return $
    if html5
    then foldl (!) H5.figure figAttrs innards
    else foldl (!) H.div (A.class_ "float" : figAttrs) innards
 where
  captionIsAlt capt [Plain [Image (_, _, kv) desc _]] =
    let alt = fromMaybe (stringify desc) $ lookup "alt" kv
    in stringify capt == alt
  captionIsAlt _ _ = False

-- | Convert Pandoc block element to HTML. All the legwork is done by
-- 'blockToHtmlInner', this just takes care of emitting the notes after
-- the block if necessary.
blockToHtml :: PandocMonad m => WriterOptions -> Block -> StateT WriterState m Html
blockToHtml opts block = do
  let isSection = case block of
        Div (_, classes, _) _ | "section" `elem` classes -> True
        _ -> False
  let increaseLevel = not isSection
  when increaseLevel $
    modify (\st -> st{ stBlockLevel = stBlockLevel st + 1 })
  doc <- blockToHtmlInner opts block
  st <- get
  let emitNotes =
        writerReferenceLocation opts == EndOfBlock && stBlockLevel st == 1
  res <- if emitNotes
    then do
      notes <- if null (stNotes st)
        then return mempty
        else footnoteSection opts (writerReferenceLocation opts)
                             (stEmittedNotes st + 1) (reverse (stNotes st))
      modify (\st' -> st'{ stNotes = mempty, stEmittedNotes = stEmittedNotes st' + length (stNotes st') })
      return (doc <> notes)
    else return doc
  when increaseLevel $
    modify (\st' -> st'{ stBlockLevel = stBlockLevel st' - 1 })
  return res

tableToHtml :: PandocMonad m
            => WriterOptions
            -> Ann.Table
            -> StateT WriterState m Html
tableToHtml opts (Ann.Table attr caption colspecs thead tbodies tfoot) = do
  captionDoc <- case caption of
    Caption _ [] -> return mempty
    Caption _ longCapt -> do
      cs <- blockListToHtml opts longCapt
      return $ do
        H.caption cs
        nl
  coltags <- colSpecListToHtml colspecs
  head' <- tableHeadToHtml opts thead
  bodies <- intersperse (nl) <$> mapM (tableBodyToHtml opts) tbodies
  foot' <- tableFootToHtml opts tfoot
  let (ident,classes,kvs) = attr
  -- When widths of columns are < 100%, we need to set width for the whole
  -- table, or some browsers give us skinny columns with lots of space
  -- between:
  let colWidth = \case
        ColWidth d -> d
        ColWidthDefault -> 0
  let totalWidth = sum . map (colWidth . snd) $ colspecs
  let attr' = case lookup "style" kvs of
                Nothing | totalWidth < 1 && totalWidth > 0
                  -> (ident,classes, ("style","width:" <>
                         T.pack (show (round (totalWidth * 100) :: Int))
                         <> "%;"):kvs)
                _ -> attr
  addAttrs opts attr' $ H.table $ do
    nl
    captionDoc
    coltags
    head'
    mconcat bodies
    foot'
    nl

tableBodyToHtml :: PandocMonad m
                => WriterOptions
                -> Ann.TableBody
                -> StateT WriterState m Html
tableBodyToHtml opts (Ann.TableBody attr _rowHeadCols inthead rows) =
  addAttrs opts attr . H.tbody =<< do
    intermediateHead <-
      if null inthead
      then return mempty
      else headerRowsToHtml opts Thead inthead
    bodyRows <- bodyRowsToHtml opts rows
    return $ intermediateHead <> bodyRows

tableHeadToHtml :: PandocMonad m
                => WriterOptions
                -> Ann.TableHead
                -> StateT WriterState m Html
tableHeadToHtml opts (Ann.TableHead attr rows) =
  tablePartToHtml opts Thead attr rows

tableFootToHtml :: PandocMonad m
                => WriterOptions
                -> Ann.TableFoot
                -> StateT WriterState m Html
tableFootToHtml opts (Ann.TableFoot attr rows) =
  tablePartToHtml opts Tfoot attr rows

tablePartToHtml :: PandocMonad m
                => WriterOptions
                -> TablePart
                -> Attr
                -> [Ann.HeaderRow]
                -> StateT WriterState m Html
tablePartToHtml opts tblpart attr rows =
  if null rows || all isEmptyRow rows
  then return mempty
  else do
    let tag' = case tblpart of
                 Thead -> H.thead
                 Tfoot -> H.tfoot
                 Tbody -> H.tbody -- this would be unexpected
    contents <- headerRowsToHtml opts tblpart rows
    tablePartElement <- addAttrs opts attr $ tag' contents
    return $ do
      tablePartElement
      nl
  where
    isEmptyRow (Ann.HeaderRow _attr _rownum cells) = all isEmptyCell cells
    isEmptyCell (Ann.Cell _colspecs _colnum cell) =
      cell == Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) []

-- | The part of a table; header, footer, or body.
data TablePart = Thead | Tfoot | Tbody
  deriving (Eq)

data CellType = HeaderCell | BodyCell

data TableRow = TableRow TablePart Attr Ann.RowNumber Ann.RowHead Ann.RowBody

headerRowsToHtml :: PandocMonad m
                 => WriterOptions
                 -> TablePart
                 -> [Ann.HeaderRow]
                 -> StateT WriterState m Html
headerRowsToHtml opts tablepart =
  rowListToHtml opts . map toTableRow
  where
    toTableRow (Ann.HeaderRow attr rownum rowbody) =
      TableRow tablepart attr rownum [] rowbody

bodyRowsToHtml :: PandocMonad m
               => WriterOptions
               -> [Ann.BodyRow]
               -> StateT WriterState m Html
bodyRowsToHtml opts =
  rowListToHtml opts . zipWith toTableRow [1..]
  where
    toTableRow rownum (Ann.BodyRow attr _rownum rowhead rowbody) =
      TableRow Tbody attr rownum rowhead rowbody


rowListToHtml :: PandocMonad m
              => WriterOptions
              -> [TableRow]
              -> StateT WriterState m Html
rowListToHtml opts rows =
  (\x -> nl *> mconcat x) <$>
     mapM (tableRowToHtml opts) rows

colSpecListToHtml :: PandocMonad m
                  => [ColSpec]
                  -> StateT WriterState m Html
colSpecListToHtml colspecs = do
  html5 <- gets stHtml5
  let hasDefaultWidth (_, ColWidthDefault) = True
      hasDefaultWidth _                    = False

  let percent w = show (truncate (100*w) :: Integer) <> "%"

  let col :: ColWidth -> Html
      col cw = do
        H.col ! case cw of
          ColWidthDefault -> mempty
          ColWidth w -> if html5
                        then A.style (toValue $ "width: " <> percent w)
                        else A.width (toValue $ percent w)
        nl

  return $
    if all hasDefaultWidth colspecs
    then mempty
    else do
      H.colgroup $ do
        nl
        mapM_ (col . snd) colspecs
      nl

tableRowToHtml :: PandocMonad m
               => WriterOptions
               -> TableRow
               -> StateT WriterState m Html
tableRowToHtml opts (TableRow tblpart attr _rownum rowhead rowbody) = do
  let celltype = case tblpart of
                   Thead -> HeaderCell
                   _     -> BodyCell
  headcells <- mapM (cellToHtml opts HeaderCell) rowhead
  bodycells <- mapM (cellToHtml opts celltype) rowbody
  rowHtml <- addAttrs opts attr $ H.tr $ do
    nl
    mconcat headcells
    mconcat bodycells
  return $ do
    rowHtml
    nl

colspanAttrib :: ColSpan -> Attribute
colspanAttrib = \case
  ColSpan 1 -> mempty
  ColSpan n -> A.colspan (toValue n)

rowspanAttrib :: RowSpan -> Attribute
rowspanAttrib = \case
  RowSpan 1 -> mempty
  RowSpan n -> A.rowspan (toValue n)

cellToHtml :: PandocMonad m
           => WriterOptions
           -> CellType
           -> Ann.Cell
           -> StateT WriterState m Html
cellToHtml opts celltype (Ann.Cell (colspec :| _) _colNum cell) =
  let align = fst colspec
  in tableCellToHtml opts celltype align cell

tableCellToHtml :: PandocMonad m
                => WriterOptions
                -> CellType
                -> Alignment
                -> Cell
                -> StateT WriterState m Html
tableCellToHtml opts ctype colAlign (Cell attr align rowspan colspan item) = do
  contents <- blockListToHtml opts item
  html5 <- gets stHtml5
  let (ident, cls, kvs) = attr
  let tag' = case ctype of
        BodyCell   -> H.td
        HeaderCell -> H.th
  let align' = case align of
        AlignDefault -> colAlign
        _            -> align
  let kvs' = case htmlAlignmentToString align' of
               Nothing ->
                 kvs
               Just alignStr ->
                 if html5
                 then htmlAddStyle ("text-align", alignStr) kvs
                 else case break ((== "align") . fst) kvs of
                   (_, []) -> ("align", alignStr) : kvs
                   (xs, _:rest) -> xs ++ ("align", alignStr) : rest
  otherAttribs <- attrsToHtml opts (ident, cls, kvs')
  let attribs = mconcat
              $ colspanAttrib colspan
              : rowspanAttrib rowspan
              : otherAttribs
  return $ do
    tag' ! attribs $ contents
    nl

toListItems :: [Html] -> [Html]
toListItems items = map toListItem items ++ [nl]

toListItem :: Html -> Html
toListItem item = nl *> H.li item

blockListToHtml :: PandocMonad m
                => WriterOptions -> [Block] -> StateT WriterState m Html
blockListToHtml opts lst =
  mconcat . intersperse (nl) . filter nonempty
    <$> mapM (blockToHtml opts) lst
  where nonempty (Empty _) = False
        nonempty _         = True

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: PandocMonad m => WriterOptions -> [Inline] -> StateT WriterState m Html
inlineListToHtml opts lst = mconcat <$> mapM (inlineToHtml opts) lst

-- | Annotates a MathML expression with the tex source
annotateMML :: XML.Element -> Text -> XML.Element
annotateMML e tex = math (unode "semantics" [cs, unode "annotation" (annotAttrs, T.unpack tex)])
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
    Space          -> return $ toHtml ' '
    SoftBreak      -> return $ case writerWrapText opts of
                                     WrapNone     -> toHtml ' '
                                     WrapAuto     -> toHtml ' '
                                     WrapPreserve -> toHtml '\n'
    LineBreak      -> return $ do
                        if html5 then H5.br else H.br
                        toHtml '\n'
    (Span ("",[cls],[]) ils)
        | cls == "csl-block" || cls == "csl-left-margin" ||
          cls == "csl-right-inline" || cls == "csl-indent"
        -> inlineListToHtml opts ils >>= inDiv cls

    (Span (id',classes,kvs) ils) ->
                        let go Nothing c
                             | c `Set.member` htmlSpanLikeElements
                               = Just (customParent (textTag c), [])
                             | c == "smallcaps"
                               = Just (H.span ! A.class_ "smallcaps", [])
                             | c == "underline"
                               = Just (H.u, [])
                             | otherwise = Nothing
                            go (Just (t,cs)) c
                             | c `Set.member` htmlSpanLikeElements
                               = Just (t . customParent (textTag c), cs)
                             | c == "smallcaps"
                               = Just (t . (H.span ! A.class_ "smallcaps"), cs)
                             | c == "underline"
                               = Just (t . H.u, cs)
                             | otherwise
                               = Just (t, c:cs)
                            spanLikeTags = L.foldl' go Nothing
                        in case spanLikeTags classes of
                            Just (tag, cs) -> do
                              h <- inlineListToHtml opts ils
                              addAttrs opts (id',cs,kvs') $ tag h
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
                                        else ("style", T.concat styles) : kvs
                              classes' = [ c | c <- classes
                                         , c `notElem` [ "csl-no-emph"
                                                       , "csl-no-strong"
                                                       , "csl-no-smallcaps"
                                                       ]
                                         ]

    (Emph lst)       -> H.em <$> inlineListToHtml opts lst
    (Underline lst)  -> H.u <$> inlineListToHtml opts lst
    (Strong lst)     -> H.strong <$> inlineListToHtml opts lst
    (Code attr@(ids,cs,kvs) str)
                     -> case hlCode of
                             Left msg -> do
                               unless (T.null msg) $
                                 report $ CouldNotHighlight msg
                               addAttrs opts (ids,cs',kvs) $
                                 fromMaybe H.code sampOrVar $
                                 strToHtml str
                             Right h -> do
                               modify $ \st -> st{ stHighlighting = True }
                               addAttrs opts (ids,[],kvs) $
                                 fromMaybe id sampOrVar h
                        where hlCode = case writerHighlightMethod opts of
                                          Skylighting _ -> highlighted
                                          DefaultHighlighting -> highlighted
                                          _ -> Left ""
                              highlighted =  highlight (writerSyntaxMap opts)
                                                       formatHtmlInline attr str
                              (sampOrVar,cs')
                                | "sample" `elem` cs =
                                      (Just H.samp,"sample" `delete` cs)
                                | "variable" `elem` cs =
                                      (Just H.var,"variable" `delete` cs)
                                | otherwise = (Nothing,cs)
    (Strikeout lst)  -> H.del <$> inlineListToHtml opts lst
    (SmallCaps lst)   -> (H.span ! A.class_ "smallcaps") <$>
                           inlineListToHtml opts lst
    (Superscript lst) -> H.sup <$> inlineListToHtml opts lst
    (Subscript lst)   -> H.sub <$> inlineListToHtml opts lst
    (Quoted quoteType lst) ->
                        let (leftQuote, rightQuote) = case quoteType of
                              SingleQuote -> (toHtml '‘',
                                              toHtml '’')
                              DoubleQuote -> (toHtml '“',
                                              toHtml '”')

                        in if writerHtmlQTags opts
                               then do
                                 modify $ \st -> st{ stQuotes = True }
                                 let (maybeAttr, lst') = case lst of
                                      [Span attr@(_, _, kvs) cs]
                                        | any ((=="cite") . fst) kvs
                                          -> (Just attr, cs)
                                      cs -> (Nothing, cs)
                                 let addAttrsMb = maybe return (addAttrs opts)
                                 inlineListToHtml opts lst' >>=
                                   addAttrsMb maybeAttr . H.q
                               else (\x -> leftQuote >> x >> rightQuote)
                                    `fmap` inlineListToHtml opts lst
    (Math t str) -> do
      modify (\st -> st {stMath = True})
      let mathClass = toValue $ ("math " :: Text) <>
                      if t == InlineMath then "inline" else "display"
      case writerHTMLMathMethod opts of
           WebTeX url -> do
              let imtag = if html5 then H5.img else H.img
              let str' = T.strip str
              let s = case t of
                           InlineMath  -> "\\textstyle "
                           DisplayMath -> "\\displaystyle "
              return $ imtag ! A.style "vertical-align:middle"
                             ! A.src (toValue . (url <>) . urlEncode $ s <> str')
                             ! A.alt (toValue str')
                             ! A.title (toValue str')
                             ! A.class_ mathClass
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
                InlineMath  -> "\\(" <> str <> "\\)"
                DisplayMath -> "\\[" <> str <> "\\]"
           KaTeX _ -> return $ H.span ! A.class_ mathClass $ toHtml $
              case t of
                InlineMath  -> str
                DisplayMath -> str
           PlainMath -> do
              x <- lift (texMathToInlines t str) >>= inlineListToHtml opts
              return $ H.span ! A.class_ mathClass $ x
    (RawInline f str) -> do
      ishtml <- isRawHtml f
      if ishtml
         then return $ preEscapedText str
         else do
           let istex = f == Format "latex" || f == Format "tex"
           let mm = writerHTMLMathMethod opts
           case istex of
             True
               | allowsMathEnvironments mm && isMathEnvironment str
                 -> do
                    modify (\st -> st {stMath = True})
                    inlineToHtml opts $ Math DisplayMath str
               | allowsRef mm && isRef str
                 -> do
                    modify (\st -> st {stMath = True})
                    inlineToHtml opts $ Math InlineMath str
             _ -> do report $ InlineNotRendered inline
                     return mempty
    (Link attr txt (s,_)) | "mailto:" `T.isPrefixOf` s -> do
                        -- We need to remove links from link text, because an
                        -- <a> element is not allowed inside another <a>
                        -- element.
                        linkText <- inlineListToHtml opts (removeLinks txt)
                        obfuscateLink opts attr linkText s
    (Link (ident,classes,kvs) txt (s,tit)) -> do
                        linkText <- inlineListToHtml opts (removeLinks txt)
                        slideVariant <- gets stSlideVariant
                        let s' = case T.uncons s of
                                   Just ('#',xs) -> let prefix = if slideVariant == RevealJsSlides
                                                             then "/"
                                                             else writerIdentifierPrefix opts
                                             in  "#" <> prefix <> xs
                                   _ -> s
                        let link = H.a ! A.href (toValue $ toURI html5 s')
                                       $ linkText
                        link' <- addAttrs opts (ident, classes, kvs) link
                        return $ if T.null tit
                                    then link'
                                    else link' ! A.title (toValue tit)
    (Image attr@(_, _, attrList) txt (s, tit)) -> do
                        epubVersion <- gets stEPUBVersion
                        let alternate = stringify txt
                        slideVariant <- gets stSlideVariant
                        let isReveal = slideVariant == RevealJsSlides
                        attrs <- imgAttrsToHtml opts attr
                        let attributes =
                              -- reveal.js uses data-src for lazy loading
                              (if isReveal
                                  then customAttribute "data-src" $ toValue s
                                  else A.src $ toValue $ toURI html5 s) :
                              [A.title $ toValue tit | not (T.null tit)] ++
                              attrs
                            imageTag = (if html5 then H5.img else H.img
                              , [A.alt $ toValue alternate |
                                  isNothing (lookup "alt" attrList) &&
                                  (isJust epubVersion || not (null txt))] )
                            mediaTag tg fallbackTxt =
                              let linkTxt = if null txt
                                            then fallbackTxt
                                            else alternate
                              in (tg $ H.a ! A.href (toValue $ toURI html5 s)
                                           $ toHtml linkTxt
                                 , [A5.controls ""] )
                            s' = fromMaybe s $ T.stripSuffix ".gz" s
                            category =
                              if "data:" `T.isPrefixOf` s
                                 then Just . T.takeWhile (/= '/') . T.drop 5 $ s
                                 else case parseURIReference (T.unpack s') of
                                        Just u -> mediaCategory $ uriPath u
                                        Nothing -> mediaCategory (T.unpack s)
                            (tag, specAttrs) = case category of
                              Just "image" -> imageTag
                              Just "video" -> mediaTag H5.video "Video"
                              Just "audio" -> mediaTag H5.audio "Audio"
                              Just _       -> (H5.embed, [])
                              _            -> imageTag
                        return $ L.foldl' (!) tag $ attributes ++ specAttrs
                        -- note:  null title included, as in Markdown.pl
    (Note contents) -> do
                        notes <- gets stNotes
                        emittedNotes <- gets stEmittedNotes
                        let number = emittedNotes + length notes + 1
                        let ref = tshow number
                        htmlContents <- blockListToNote opts ref contents
                        epubVersion <- gets stEPUBVersion
                        -- push contents onto front of notes
                        modify $ \st -> st {stNotes = htmlContents:notes}
                        slideVariant <- gets stSlideVariant
                        let revealSlash = T.pack ['/' | slideVariant == RevealJsSlides]
                        let link = H.a ! A.href (toValue $ toURI html5 $ "#" <>
                                         revealSlash <>
                                         writerIdentifierPrefix opts <> "fn" <> ref)
                                       ! A.class_ "footnote-ref"
                                       ! prefixedId opts ("fnref" <> ref)
                                       $ (if isJust epubVersion
                                             then id
                                             else H.sup)
                                       $ toHtml ref
                        return $ case epubVersion of
                                      Just EPUB3 -> link ! customAttribute "epub:type" "noteref" ! customAttribute "role" "doc-noteref"
                                      _ | html5  -> link ! A5.role "doc-noteref"
                                      _          -> link
    (Cite cits il)-> do contents <- inlineListToHtml opts
                                      (if html5
                                          then walk addBibliorefRole il
                                          else il)
                        let citationIds = T.unwords $ map citationId cits
                        let result = H.span ! A.class_ "citation" $ contents
                        return $ if html5
                                    then result ! customAttribute "data-cites" (toValue citationIds)
                                    else result

addBibliorefRole :: Inline -> Inline
addBibliorefRole (Link (id',classes,kvs) ils (src,tit))
   | "#ref-" `T.isPrefixOf` src =
  Link (id',classes,("role","doc-biblioref"):kvs) ils (src,tit)
addBibliorefRole x = x

blockListToNote :: PandocMonad m
                => WriterOptions -> Text -> [Block]
                -> StateT WriterState m Html
blockListToNote opts ref blocks = do
  epubVersion <- gets stEPUBVersion
  html5 <- gets stHtml5
  case epubVersion of
    Nothing -> do -- web page
      -- If last block is Para or Plain, include the backlink at the end of
      -- that block. Otherwise, insert a new Plain block with the backlink.
      let kvs = [("role","doc-backlink") | html5]
      let backlink = [Link ("",["footnote-back"],kvs)
                        [Str "↩"] ("#" <> "fnref" <> ref,"")]
      let blocks'  = if null blocks
                        then []
                        else let lastBlock   = last blocks
                                 otherBlocks = init blocks
                             in  case lastBlock of
                                      Para [Image (_,cls,_) _ (_,tit)]
                                          | "fig:" `T.isPrefixOf` tit
                                            || "r-stretch" `elem` cls
                                                -> otherBlocks ++ [lastBlock,
                                                      Plain backlink]
                                      Para lst  -> otherBlocks ++
                                                     [Para (lst ++ backlink)]
                                      Plain lst -> otherBlocks ++
                                                     [Plain (lst ++ backlink)]
                                      _         -> otherBlocks ++ [lastBlock,
                                                     Plain backlink]
      contents <- blockListToHtml opts blocks'
      let noteItem = H.li ! prefixedId opts ("fn" <> ref) $ contents
      return $ noteItem >> nl
    Just epubv -> do
      let kvs = [("role","doc-backlink") | html5]
      let backlink = Link ("",["footnote-back"],kvs)
                        [Str ref] ("#" <> "fnref" <> ref,"")
      let addBacklinkInlines bs
             | epubv == EPUB3 = bs
             | otherwise =
                 case bs of
                   (Para ils : rest) ->
                     Para (backlink : Str "." : Space : ils) : rest
                   (Plain ils : rest) ->
                     Plain (backlink : Str "." : Space : ils) : rest
                   _ -> Para [backlink , Str "."] : blocks
      contents <- blockListToHtml opts (addBacklinkInlines blocks)
      let noteItem = (if epubv == EPUB3
                         then H5.aside ! customAttribute "epub:type" "footnote" ! customAttribute "role" "doc-footnote"
                         else H.div) ! prefixedId opts ("fn" <> ref)
                      $ nl >> contents >> nl
      return $ noteItem >> nl

inDiv :: PandocMonad m=> Text -> Html -> StateT WriterState m Html
inDiv cls x = do
  html5 <- gets stHtml5
  return $
    (if html5 then H5.div else H.div)
                x ! A.class_ (toValue cls)

isRef :: Text -> Bool
isRef t = "\\ref{" `T.isPrefixOf` t || "\\eqref{" `T.isPrefixOf` t

isMathEnvironment :: Text -> Bool
isMathEnvironment s = "\\begin{" `T.isPrefixOf` s &&
                         envName `elem` mathmlenvs
  where envName = T.takeWhile (/= '}') (T.drop 7 s)
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
                     , "prooftree" -- bussproofs
                     , "smallmatrix"
                     , "split"
                     , "subarray"
                     , "Vmatrix"
                     , "vmatrix" ]

allowsMathEnvironments :: HTMLMathMethod -> Bool
allowsMathEnvironments (MathJax _) = True
allowsMathEnvironments (KaTeX _)   = True
allowsMathEnvironments MathML      = True
allowsMathEnvironments (WebTeX _)  = True
allowsMathEnvironments _           = False

allowsRef :: HTMLMathMethod -> Bool
allowsRef (MathJax _) = True
allowsRef _           = False

-- | List of intrinsic event attributes allowed on all elements in HTML4.
intrinsicEventsHTML4 :: [Text]
intrinsicEventsHTML4 =
  [ "onclick", "ondblclick", "onmousedown", "onmouseup", "onmouseover"
  , "onmouseout", "onmouseout", "onkeypress", "onkeydown", "onkeyup"]


-- | Check to see if Format is valid HTML
isRawHtml :: PandocMonad m => Format -> StateT WriterState m Bool
isRawHtml f = do
  html5 <- gets stHtml5
  return $ f == Format "html" ||
           ((html5 && f == Format "html5") || f == Format "html4") ||
           isSlideVariant f

-- | Check to see if Format matches with an HTML slide variant
isSlideVariant :: Format -> Bool
isSlideVariant f = f `elem` [Format "s5", Format "slidy", Format "slideous",
                             Format "dzslides", Format "revealjs"]

toURI :: Bool -> Text -> Text
toURI isHtml5 t = if isHtml5 then t else escapeURI t
 where
   escapeURI = T.pack . escapeURIString (not . needsEscaping) . T.unpack
   needsEscaping c = isSpace c || T.any (== c) "<>|\"{}[]^`" || not (isAscii c)

hasVariable :: Text -> DT.Template a -> Bool
hasVariable var = checkVar
 where
   matches v' = T.intercalate "." (DT.varParts v') == var
   checkVar (DT.Interpolate v) = matches v
   checkVar (DT.Conditional v t1 t2) = matches v || checkVar t1 || checkVar t2
   checkVar (DT.Iterate v t1 t2) = matches v || checkVar t1 || checkVar t2
   checkVar (DT.Nested t) = checkVar t
   checkVar (DT.Partial _ t) = checkVar t
   checkVar (DT.Concat t1 t2) = checkVar t1 || checkVar t2
   checkVar (DT.Literal _) = False
   checkVar DT.Empty = False

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Writers.JATS
   Copyright   : Copyright (C) 2017-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to JATS XML.
Reference:
https://jats.nlm.nih.gov/publishing/tag-library
-}
module Text.Pandoc.Writers.JATS
  ( writeJATS
  , writeJatsArchiving
  , writeJatsPublishing
  , writeJatsArticleAuthoring
  ) where
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics (everywhere, mkT)
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Time (toGregorian, Day, parseTimeM, defaultTimeLocale, formatTime)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.Logging
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.DocTemplates (Context(..), Val(..))
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML
import Text.TeXMath
import qualified Text.XML.Light as Xml

-- | JATS tag set variant
data JATSTagSet
  = TagSetArchiving         -- ^ Archiving and Interchange Tag Set
  | TagSetPublishing        -- ^ Journal Publishing Tag Set
  | TagSetArticleAuthoring  -- ^ Article Authoring Tag Set
  deriving (Eq)

-- | Internal state used by the writer.
newtype JATSState = JATSState
  { jatsNotes :: [(Int, Doc Text)] }

-- | JATS writer type
type JATS a = StateT JATSState (ReaderT JATSTagSet a)

-- | Convert a @'Pandoc'@ document to JATS (Archiving and Interchange
-- Tag Set.)
writeJatsArchiving :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJatsArchiving = writeJats TagSetArchiving

-- | Convert a @'Pandoc'@ document to JATS (Journal Publishing Tag Set.)
writeJatsPublishing :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJatsPublishing = writeJats TagSetPublishing

-- | Convert a @'Pandoc'@ document to JATS (Archiving and Interchange
-- Tag Set.)
writeJatsArticleAuthoring :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJatsArticleAuthoring = writeJats TagSetArticleAuthoring

-- | Alias for @'writeJatsArchiving'@. This function exists for backwards
-- compatibility, but will be deprecated in the future. Use
-- @'writeJatsArchiving'@ instead.
writeJATS :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJATS = writeJatsArchiving

-- | Convert a @'Pandoc'@ document to JATS.
writeJats :: PandocMonad m => JATSTagSet -> WriterOptions -> Pandoc -> m Text
writeJats tagSet opts d =
  runReaderT (evalStateT (docToJATS opts d)
               (JATSState{ jatsNotes = [] }))
             tagSet

-- | Convert Pandoc document to string in JATS format.
docToJATS :: PandocMonad m => WriterOptions -> Pandoc -> JATS m Text
docToJATS opts (Pandoc meta blocks) = do
  let isBackBlock (Div ("refs",_,_) _) = True
      isBackBlock _                    = False
  let (backblocks, bodyblocks) = partition isBackBlock blocks
  -- The numbering here follows LaTeX's internal numbering
  let startLvl = case writerTopLevelDivision opts of
                   TopLevelPart    -> -1
                   TopLevelChapter -> 0
                   TopLevelSection -> 1
                   TopLevelDefault -> 1
  let fromBlocks = blocksToJATS opts . makeSections False (Just startLvl)
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToContext opts
                 fromBlocks
                 (fmap chomp . inlinesToJATS opts)
                 meta
  main <- fromBlocks bodyblocks
  notes <- reverse . map snd <$> gets jatsNotes
  backs <- fromBlocks backblocks
  tagSet <- ask
  -- In the "Article Authoring" tag set, occurrence of fn-group elements
  -- is restricted to table footers. Footnotes have to be placed inline.
  let fns = if null notes || tagSet == TagSetArticleAuthoring
            then mempty
            else inTagsIndented "fn-group" $ vcat notes
  let back = backs $$ fns
  let date =
        case getField "date" metadata of
          Nothing -> NullVal
          Just (SimpleVal (x :: Doc Text)) ->
             case parseDate (render Nothing x) of
               Nothing  -> NullVal
               Just day ->
                 let (y,m,d) = toGregorian day
                 in  MapVal . Context $ M.fromList
                      [("year" :: Text, SimpleVal $ text $ show y)
                      ,("month", SimpleVal $ text $ show m)
                      ,("day", SimpleVal $ text $ show d)
                      ,("iso-8601", SimpleVal $ text $
                            formatTime defaultTimeLocale "%F" day)
                      ]
          Just x -> x
  let context = defField "body" main
              $ defField "back" back
              $ resetField "date" date
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML -> True
                                        _      -> False) metadata
  return $ render colwidth $
    (if writerPreferAscii opts then fmap toEntities else id) $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Convert a list of Pandoc blocks to JATS.
blocksToJATS :: PandocMonad m => WriterOptions -> [Block] -> JATS m (Doc Text)
blocksToJATS = wrappedBlocksToJATS (const False)

-- | Like @'blocksToJATS'@, but wraps top-level blocks into a @<p>@
-- element if the @needsWrap@ predicate evaluates to @True@.
wrappedBlocksToJATS :: PandocMonad m
                    => (Block -> Bool)
                    -> WriterOptions
                    -> [Block]
                    -> JATS m (Doc Text)
wrappedBlocksToJATS needsWrap opts =
  fmap vcat . mapM wrappedBlockToJATS
  where
    wrappedBlockToJATS b = do
      inner <- blockToJATS opts b
      return $
        if needsWrap b
           then inTags True "p" [("specific-use","wrapper")] inner
           else inner

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of
-- JATS varlistentrys.
deflistItemsToJATS :: PandocMonad m
                      => WriterOptions -> [([Inline],[[Block]])] -> JATS m (Doc Text)
deflistItemsToJATS opts items =
  vcat <$> mapM (uncurry (deflistItemToJATS opts)) items

-- | Convert a term and a list of blocks into a JATS varlistentry.
deflistItemToJATS :: PandocMonad m
                     => WriterOptions -> [Inline] -> [[Block]] -> JATS m (Doc Text)
deflistItemToJATS opts term defs = do
  term' <- inlinesToJATS opts term
  def' <- wrappedBlocksToJATS (not . isPara)
              opts $ concatMap (walk demoteHeaderAndRefs .
                                map plainToPara) defs
  return $ inTagsIndented "def-item" $
      inTagsSimple "term" term' $$
      inTagsIndented "def" def'

-- | Convert a list of lists of blocks to a list of JATS list items.
listItemsToJATS :: PandocMonad m
                => WriterOptions -> Maybe [Text] -> [[Block]] -> JATS m (Doc Text)
listItemsToJATS opts markers items =
  case markers of
       Nothing -> vcat <$> mapM (listItemToJATS opts Nothing) items
       Just ms -> vcat <$> zipWithM (listItemToJATS opts) (map Just ms) items

-- | Convert a list of blocks into a JATS list item.
listItemToJATS :: PandocMonad m
               => WriterOptions -> Maybe Text -> [Block] -> JATS m (Doc Text)
listItemToJATS opts mbmarker item = do
  contents <- wrappedBlocksToJATS (not . isParaOrList) opts
                 (walk demoteHeaderAndRefs item)
  return $ inTagsIndented "list-item" $
           maybe empty (\lbl -> inTagsSimple "label" (text $ T.unpack lbl)) mbmarker
           $$ contents

imageMimeType :: Text -> [(Text, Text)] -> (Text, Text)
imageMimeType src kvs =
  let mbMT = getMimeType (T.unpack src)
      maintype = fromMaybe "image" $
                  lookup "mimetype" kvs `mplus`
                  (T.takeWhile (/='/') <$> mbMT)
      subtype = fromMaybe "" $
                  lookup "mime-subtype" kvs `mplus`
                  ((T.drop 1 . T.dropWhile (/='/')) <$> mbMT)
  in (maintype, subtype)

languageFor :: [Text] -> Text
languageFor classes =
  case langs of
     (l:_) -> escapeStringForXML l
     []    -> ""
    where isLang l    = T.toLower l `elem` map T.toLower languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . T.toLower $ s
          langs       = concatMap langsFrom classes

codeAttr :: Attr -> (Text, [(Text, Text)])
codeAttr (ident,classes,kvs) = (lang, attr)
    where
       attr = [("id",ident) | not (T.null ident)] ++
              [("language",lang) | not (T.null lang)] ++
              [(k,v) | (k,v) <- kvs, k `elem` ["code-type",
                "code-version", "executable",
                "language-version", "orientation",
                    "platforms", "position", "specific-use"]]
       lang  = languageFor classes

-- | Convert a Pandoc block element to JATS.
blockToJATS :: PandocMonad m => WriterOptions -> Block -> JATS m (Doc Text)
blockToJATS _ Null = return empty
blockToJATS opts (Div (id',"section":_,kvs) (Header _lvl _ ils : xs)) = do
  let idAttr = [("id", writerIdentifierPrefix opts <> id') | not (T.null id')]
  let otherAttrs = ["sec-type", "specific-use"]
  let attribs = idAttr ++ [(k,v) | (k,v) <- kvs, k `elem` otherAttrs]
  title' <- inlinesToJATS opts ils
  contents <- blocksToJATS opts xs
  return $ inTags True "sec" attribs $
      inTagsSimple "title" title' $$ contents
-- Bibliography reference:
blockToJATS opts (Div (T.stripPrefix "ref-" -> Just _,_,_) [Para lst]) =
  inlinesToJATS opts lst
blockToJATS opts (Div ("refs",_,_) xs) = do
  contents <- blocksToJATS opts xs
  return $ inTagsIndented "ref-list" contents
blockToJATS opts (Div (ident,[cls],kvs) bs) | cls `elem` ["fig", "caption", "table-wrap"] = do
  contents <- blocksToJATS opts bs
  let attr = [("id", ident) | not (T.null ident)] ++
             [("xml:lang",l) | ("lang",l) <- kvs] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["specific-use",
                 "content-type", "orientation", "position"]]
  return $ inTags True cls attr contents
blockToJATS opts (Div (ident,_,kvs) bs) = do
  contents <- blocksToJATS opts bs
  let attr = [("id", ident) | not (T.null ident)] ++
             [("xml:lang",l) | ("lang",l) <- kvs] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["specific-use",
                 "content-type", "orientation", "position"]]
  return $ inTags True "boxed-text" attr contents
blockToJATS opts (Header _ _ title) = do
  title' <- inlinesToJATS opts title
  return $ inTagsSimple "title" title'
-- No Plain, everything needs to be in a block-level tag
blockToJATS opts (Plain lst) = blockToJATS opts (Para lst)
-- title beginning with fig: indicates that the image is a figure
blockToJATS opts (Para [Image (ident,_,kvs) txt
  (src,T.stripPrefix "fig:" -> Just tit)]) = do
  alt <- inlinesToJATS opts txt
  let (maintype, subtype) = imageMimeType src kvs
  let capt = if null txt
                then empty
                else inTagsSimple "caption" $ inTagsSimple "p" alt
  let attr = [("id", ident) | not (T.null ident)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["fig-type", "orientation",
                                              "position", "specific-use"]]
  let graphicattr = [("mimetype",maintype),
                     ("mime-subtype",subtype),
                     ("xlink:href",src),  -- do we need to URL escape this?
                     ("xlink:title",tit)]
  return $ inTags True "fig" attr $
              capt $$ selfClosingTag "graphic" graphicattr
blockToJATS _ (Para [Image (ident,_,kvs) _ (src, tit)]) = do
  let (maintype, subtype) = imageMimeType src kvs
  let attr = [("id", ident) | not (T.null ident)] ++
             [("mimetype", maintype),
              ("mime-subtype", subtype),
              ("xlink:href", src)] ++
             [("xlink:title", tit) | not (T.null tit)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["baseline-shift",
                        "content-type", "specific-use", "xlink:actuate",
                        "xlink:href", "xlink:role", "xlink:show",
                        "xlink:type"]]
  return $ selfClosingTag "graphic" attr
blockToJATS opts (Para lst) =
  inTagsSimple "p" <$> inlinesToJATS opts lst
blockToJATS opts (LineBlock lns) =
  blockToJATS opts $ linesToPara lns
blockToJATS opts (BlockQuote blocks) = do
  tagSet <- ask
  let blocksToJats' = if tagSet == TagSetArticleAuthoring
                      then wrappedBlocksToJATS (not . isPara)
                      else blocksToJATS
  inTagsIndented "disp-quote" <$> blocksToJats' opts blocks
blockToJATS _ (CodeBlock a str) = return $
  inTags False tag attr (flush (text (T.unpack $ escapeStringForXML str)))
    where (lang, attr) = codeAttr a
          tag          = if T.null lang then "preformat" else "code"
blockToJATS _ (BulletList []) = return empty
blockToJATS opts (BulletList lst) =
  inTags True "list" [("list-type", "bullet")] <$>
  listItemsToJATS opts Nothing lst
blockToJATS _ (OrderedList _ []) = return empty
blockToJATS opts (OrderedList (start, numstyle, delimstyle) items) = do
  tagSet <- ask
  let listType =
        -- The Article Authoring tag set doesn't allow a more specific
        -- @list-type@ attribute than "order".
        if tagSet == TagSetArticleAuthoring
        then "order"
        else case numstyle of
               DefaultStyle -> "order"
               Decimal      -> "order"
               Example      -> "order"
               UpperAlpha   -> "alpha-upper"
               LowerAlpha   -> "alpha-lower"
               UpperRoman   -> "roman-upper"
               LowerRoman   -> "roman-lower"
  let simpleList = start == 1 && (delimstyle == DefaultDelim ||
                                  delimstyle == Period)
  let markers = if simpleList
                   then Nothing
                   else Just $
                          orderedListMarkers (start, numstyle, delimstyle)
  inTags True "list" [("list-type", listType)] <$>
    listItemsToJATS opts markers items
blockToJATS opts (DefinitionList lst) =
  inTags True "def-list" [] <$> deflistItemsToJATS opts lst
blockToJATS _ b@(RawBlock f str)
  | f == "jats"    = return $ text $ T.unpack str -- raw XML block
  | otherwise      = do
      report $ BlockNotRendered b
      return empty
blockToJATS _ HorizontalRule = return empty -- not semantic
blockToJATS opts (Table _ blkCapt specs th tb tf) =
  case toLegacyTable blkCapt specs th tb tf of
    ([], aligns, widths, headers, rows) -> captionlessTable aligns widths headers rows
    (caption, aligns, widths, headers, rows) -> do
      captionDoc <- inTagsIndented "caption" <$> blockToJATS opts (Para caption)
      tbl <- captionlessTable aligns widths headers rows
      return $ inTags True "table-wrap" [] $ captionDoc $$ tbl
  where
    captionlessTable aligns widths headers rows = do
      let percent w = tshow (truncate (100*w) :: Integer) <> "*"
      let coltags = vcat $ zipWith (\w al -> selfClosingTag "col"
                           ([("width", percent w) | w > 0] ++
                            [("align", alignmentToText al)])) widths aligns
      thead <- if all null headers
                  then return empty
                  else inTagsIndented "thead" <$> tableRowToJATS opts True headers
      tbody <- (inTagsIndented "tbody" . vcat) <$>
                    mapM (tableRowToJATS opts False) rows
      return $ inTags True "table" [] $ coltags $$ thead $$ tbody

alignmentToText :: Alignment -> Text
alignmentToText alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToJATS :: PandocMonad m
                  => WriterOptions
                  -> Bool
                  -> [[Block]]
                  -> JATS m (Doc Text)
tableRowToJATS opts isHeader cols =
  (inTagsIndented "tr" . vcat) <$> mapM (tableItemToJATS opts isHeader) cols

tableItemToJATS :: PandocMonad m
                   => WriterOptions
                   -> Bool
                   -> [Block]
                   -> JATS m (Doc Text)
tableItemToJATS opts isHeader [Plain item] =
  inTags False (if isHeader then "th" else "td") [] <$>
    inlinesToJATS opts item
tableItemToJATS opts isHeader item =
  (inTags False (if isHeader then "th" else "td") [] . vcat) <$>
    mapM (blockToJATS opts) item

-- | Convert a list of inline elements to JATS.
inlinesToJATS :: PandocMonad m => WriterOptions -> [Inline] -> JATS m (Doc Text)
inlinesToJATS opts lst = hcat <$> mapM (inlineToJATS opts) (fixCitations lst)
  where
   fixCitations [] = []
   fixCitations (x:xs) | needsFixing x =
     x : Str (stringify ys) : fixCitations zs
     where
       needsFixing (RawInline (Format "jats") z) =
           "<pub-id pub-id-type=" `T.isPrefixOf` z
       needsFixing _           = False
       isRawInline RawInline{} = True
       isRawInline _           = False
       (ys,zs)                 = break isRawInline xs
   fixCitations (x:xs) = x : fixCitations xs

-- | Convert an inline element to JATS.
inlineToJATS :: PandocMonad m => WriterOptions -> Inline -> JATS m (Doc Text)
inlineToJATS _ (Str str) = return $ text $ T.unpack $ escapeStringForXML str
inlineToJATS opts (Emph lst) =
  inTagsSimple "italic" <$> inlinesToJATS opts lst
inlineToJATS opts (Underline lst) =
  inTagsSimple "underline" <$> inlinesToJATS opts lst
inlineToJATS opts (Strong lst) =
  inTagsSimple "bold" <$> inlinesToJATS opts lst
inlineToJATS opts (Strikeout lst) =
  inTagsSimple "strike" <$> inlinesToJATS opts lst
inlineToJATS opts (Superscript lst) =
  inTagsSimple "sup" <$> inlinesToJATS opts lst
inlineToJATS opts (Subscript lst) =
  inTagsSimple "sub" <$> inlinesToJATS opts lst
inlineToJATS opts (SmallCaps lst) =
  inTagsSimple "sc" <$> inlinesToJATS opts lst
inlineToJATS opts (Quoted SingleQuote lst) = do
  contents <- inlinesToJATS opts lst
  return $ char '‘' <> contents <> char '’'
inlineToJATS opts (Quoted DoubleQuote lst) = do
  contents <- inlinesToJATS opts lst
  return $ char '“' <> contents <> char '”'
inlineToJATS _ (Code a str) =
  return $ inTags False tag attr $ literal (escapeStringForXML str)
    where (lang, attr) = codeAttr a
          tag          = if T.null lang then "monospace" else "code"
inlineToJATS _ il@(RawInline f x)
  | f == "jats" = return $ literal x
  | otherwise   = do
      report $ InlineNotRendered il
      return empty
inlineToJATS _ LineBreak = return cr -- not allowed as child of p
-- see https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/break.html
inlineToJATS _ Space = return space
inlineToJATS opts SoftBreak
  | writerWrapText opts == WrapPreserve = return cr
  | otherwise = return space
inlineToJATS opts (Note contents) = do
  tagSet <- ask
  -- Footnotes must occur inline when using the Article Authoring tag set.
  if tagSet == TagSetArticleAuthoring
    then inTagsIndented "fn" <$> wrappedBlocksToJATS (not . isPara) opts contents
    else do
      notes <- gets jatsNotes
      let notenum = case notes of
                      (n, _):_ -> n + 1
                      []       -> 1
      thenote <- inTags True "fn" [("id","fn" <> tshow notenum)]
                    <$> wrappedBlocksToJATS (not . isPara) opts
                         (walk demoteHeaderAndRefs contents)
      modify $ \st -> st{ jatsNotes = (notenum, thenote) : notes }
      return $ inTags False "xref" [("ref-type", "fn"),
                                    ("rid", "fn" <> tshow notenum)]
             $ text (show notenum)
inlineToJATS opts (Cite _ lst) =
  -- TODO revisit this after examining the jats.csl pipeline
  inlinesToJATS opts lst
inlineToJATS opts (Span ("",_,[]) ils) = inlinesToJATS opts ils
inlineToJATS opts (Span (ident,_,kvs) ils) = do
  contents <- inlinesToJATS opts ils
  let attr = [("id",ident) | not (T.null ident)] ++
             [("xml:lang",l) | ("lang",l) <- kvs] ++
             [(k,v) | (k,v) <- kvs
                    ,  k `elem` ["content-type", "rationale",
                                 "rid", "specific-use"]]
  return $ selfClosingTag "milestone-start" attr <> contents <>
           selfClosingTag "milestone-end" []
inlineToJATS _ (Math t str) = do
  let addPref (Xml.Attr q v)
         | Xml.qName q == "xmlns" = Xml.Attr q{ Xml.qName = "xmlns:mml" } v
         | otherwise = Xml.Attr q v
  let fixNS' e = e{ Xml.elName =
                         (Xml.elName e){ Xml.qPrefix = Just "mml" } }
  let fixNS = everywhere (mkT fixNS') .
              (\e -> e{ Xml.elAttribs = map addPref (Xml.elAttribs e) })
  let conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
  res <- convertMath writeMathML t str
  let tagtype = case t of
                     DisplayMath -> "disp-formula"
                     InlineMath  -> "inline-formula"

  let rawtex = text "<![CDATA[" <> literal str <> text "]]>"
  let texMath = inTagsSimple "tex-math" rawtex

  tagSet <- ask
  return . inTagsSimple tagtype $
    case res of
      Right r  -> let mathMl = text (Xml.ppcElement conf $ fixNS r)
                  -- tex-math is unsupported in Article Authoring tag set
                  in if tagSet == TagSetArticleAuthoring
                     then mathMl
                     else inTagsSimple "alternatives" $
                          cr <> texMath $$ mathMl
      Left _   -> if tagSet /= TagSetArticleAuthoring
                  then texMath
                  else rawtex
inlineToJATS _ (Link _attr [Str t] (T.stripPrefix "mailto:" -> Just email, _))
  | escapeURI t == email =
  return $ inTagsSimple "email" $ literal (escapeStringForXML email)
inlineToJATS opts (Link (ident,_,kvs) txt (T.uncons -> Just ('#', src), _)) = do
  let attr = [("id", ident) | not (T.null ident)] ++
             [("alt", stringify txt) | not (null txt)] ++
             [("rid", src)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["ref-type", "specific-use"]]
  if null txt
     then return $ selfClosingTag "xref" attr
     else do
        contents <- inlinesToJATS opts txt
        return $ inTags False "xref" attr contents
inlineToJATS opts (Link (ident,_,kvs) txt (src, tit)) = do
  let attr = [("id", ident) | not (T.null ident)] ++
             [("ext-link-type", "uri"),
              ("xlink:href", src)] ++
             [("xlink:title", tit) | not (T.null tit)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["assigning-authority",
                                              "specific-use", "xlink:actuate",
                                              "xlink:role", "xlink:show",
                                              "xlink:type"]]
  contents <- inlinesToJATS opts txt
  return $ inTags False "ext-link" attr contents
inlineToJATS _ (Image (ident,_,kvs) _ (src, tit)) = do
  let mbMT = getMimeType (T.unpack src)
  let maintype = fromMaybe "image" $
                  lookup "mimetype" kvs `mplus`
                  (T.takeWhile (/='/') <$> mbMT)
  let subtype = fromMaybe "" $
                  lookup "mime-subtype" kvs `mplus`
                  ((T.drop 1 . T.dropWhile (/='/')) <$> mbMT)
  let attr = [("id", ident) | not (T.null ident)] ++
             [("mimetype", maintype),
              ("mime-subtype", subtype),
              ("xlink:href", src)] ++
             [("xlink:title", tit) | not (T.null tit)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["baseline-shift",
                        "content-type", "specific-use", "xlink:actuate",
                        "xlink:href", "xlink:role", "xlink:show",
                        "xlink:type"]]
  return $ selfClosingTag "inline-graphic" attr

isParaOrList :: Block -> Bool
isParaOrList Para{}           = True
isParaOrList Plain{}          = True
isParaOrList BulletList{}     = True
isParaOrList OrderedList{}    = True
isParaOrList DefinitionList{} = True
isParaOrList _                = False

isPara :: Block -> Bool
isPara Para{}  = True
isPara Plain{} = True
isPara _       = False

demoteHeaderAndRefs :: Block -> Block
demoteHeaderAndRefs (Header _ _ ils) = Para ils
demoteHeaderAndRefs (Div ("refs",cls,kvs) bs) =
                       Div ("",cls,kvs) bs
demoteHeaderAndRefs x = x

parseDate :: Text -> Maybe Day
parseDate s = msum (map (\fs -> parsetimeWith fs $ T.unpack s) formats) :: Maybe Day
  where parsetimeWith = parseTimeM True defaultTimeLocale
        formats = ["%x","%m/%d/%Y", "%D","%F", "%d %b %Y",
                    "%e %B %Y", "%b. %e, %Y", "%B %e, %Y",
                    "%Y%m%d", "%Y%m", "%Y"]

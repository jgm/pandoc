{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.OpenDocument
   Copyright   : Copyright (C) 2008-2020 Andrea Rossato and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Andrea Rossato <andrea.rossato@ing.unitn.it>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to OpenDocument XML.
-}
module Text.Pandoc.Writers.OpenDocument ( writeOpenDocument ) where
import Control.Arrow ((***), (>>>))
import Control.Monad.State.Strict hiding (when)
import Data.Char (chr)
import Data.List (sortOn, sortBy, foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report, translateTerm,
                                      setTranslations, toLang)
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared (linesToPara, tshow)
import Text.Pandoc.Templates (renderTemplate)
import qualified Text.Pandoc.Translations as Term (Term(Figure, Table))
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML
import Text.Printf (printf)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

--
-- OpenDocument writer
--

type OD m = StateT WriterState m

data WriterState =
    WriterState { stNotes          :: [Doc Text]
                , stTableStyles    :: [Doc Text]
                , stParaStyles     :: [Doc Text]
                , stListStyles     :: [(Int, [Doc Text])]
                , stTextStyles     :: Map.Map (Set.Set TextStyle)
                                        (Text, Doc Text)
                , stTextStyleAttr  :: Set.Set TextStyle
                , stIndentPara     :: Int
                , stInDefinition   :: Bool
                , stTight          :: Bool
                , stFirstPara      :: Bool
                , stImageId        :: Int
                , stTableCaptionId :: Int
                , stImageCaptionId :: Int
                }

defaultWriterState :: WriterState
defaultWriterState =
    WriterState { stNotes          = []
                , stTableStyles    = []
                , stParaStyles     = []
                , stListStyles     = []
                , stTextStyles     = Map.empty
                , stTextStyleAttr  = Set.empty
                , stIndentPara     = 0
                , stInDefinition   = False
                , stTight          = False
                , stFirstPara      = False
                , stImageId        = 1
                , stTableCaptionId = 1
                , stImageCaptionId = 1
                }

when :: Bool -> Doc Text -> Doc Text
when p a = if p then a else empty

addTableStyle :: PandocMonad m => Doc Text -> OD m ()
addTableStyle i = modify $ \s -> s { stTableStyles = i : stTableStyles s }

addNote :: PandocMonad m => Doc Text -> OD m ()
addNote i = modify $ \s -> s { stNotes = i : stNotes s }

addParaStyle :: PandocMonad m => Doc Text -> OD m ()
addParaStyle i = modify $ \s -> s { stParaStyles = i : stParaStyles s }

addTextStyle :: PandocMonad m
             => Set.Set TextStyle -> (Text, Doc Text) -> OD m ()
addTextStyle attrs i = modify $ \s ->
  s { stTextStyles = Map.insert attrs i (stTextStyles s) }

addTextStyleAttr :: PandocMonad m => TextStyle -> OD m ()
addTextStyleAttr t = modify $ \s ->
  s { stTextStyleAttr = Set.insert t (stTextStyleAttr s) }

increaseIndent :: PandocMonad m => OD m ()
increaseIndent = modify $ \s -> s { stIndentPara = 1 + stIndentPara s }

resetIndent :: PandocMonad m => OD m ()
resetIndent = modify $ \s -> s { stIndentPara = stIndentPara s - 1 }

inTightList :: PandocMonad m => OD m a -> OD m a
inTightList  f = modify (\s -> s { stTight = True  }) >> f >>= \r ->
                 modify (\s -> s { stTight = False }) >> return r

setInDefinitionList :: PandocMonad m => Bool -> OD m ()
setInDefinitionList b = modify $  \s -> s { stInDefinition = b }

setFirstPara :: PandocMonad m => OD m ()
setFirstPara =  modify $  \s -> s { stFirstPara = True }

inParagraphTags :: PandocMonad m => Doc Text -> OD m (Doc Text)
inParagraphTags d = do
  b <- gets stFirstPara
  a <- if b
       then do modify $ \st -> st { stFirstPara = False }
               return [("text:style-name", "First_20_paragraph")]
       else    return   [("text:style-name", "Text_20_body")]
  return $ inTags False "text:p" a d

inParagraphTagsWithStyle :: Text -> Doc Text -> Doc Text
inParagraphTagsWithStyle sty = inTags False "text:p" [("text:style-name", sty)]

inSpanTags :: Text -> Doc Text -> Doc Text
inSpanTags s = inTags False "text:span" [("text:style-name",s)]

withTextStyle :: PandocMonad m => TextStyle -> OD m a -> OD m a
withTextStyle s f = do
  oldTextStyleAttr <- gets stTextStyleAttr
  addTextStyleAttr s
  res <- f
  modify $ \st -> st{ stTextStyleAttr = oldTextStyleAttr }
  return res

inTextStyle :: PandocMonad m => Doc Text -> OD m (Doc Text)
inTextStyle d = do
  at <- gets stTextStyleAttr
  if Set.null at
     then return d
     else do
       styles <- gets stTextStyles
       case Map.lookup at styles of
            Just (styleName, _) -> return $
              inTags False "text:span" [("text:style-name",styleName)] d
            Nothing -> do
              let styleName = "T" <> tshow (Map.size styles + 1)
              addTextStyle at (styleName,
                     inTags False "style:style"
                       [("style:name", styleName)
                       ,("style:family", "text")]
                       $ selfClosingTag "style:text-properties"
                          (sortOn fst . Map.toList
                                $ foldl' textStyleAttr mempty (Set.toList at)))
              return $ inTags False
                  "text:span" [("text:style-name",styleName)] d

formulaStyles :: [Doc Text]
formulaStyles = [formulaStyle InlineMath, formulaStyle DisplayMath]

formulaStyle :: MathType -> Doc Text
formulaStyle mt = inTags False "style:style"
  [("style:name", if mt == InlineMath then "fr1" else "fr2")
  ,("style:family", "graphic")
  ,("style:parent-style-name", "Formula")]
  $ selfClosingTag "style:graphic-properties" $ if mt == InlineMath then
                                                  [("style:vertical-pos", "middle")
                                                  ,("style:vertical-rel", "text")]
                                                else
                                                  [("style:vertical-pos",   "middle")
                                                  ,("style:vertical-rel",   "paragraph-content")
                                                  ,("style:horizontal-pos", "center")
                                                  ,("style:horizontal-rel", "paragraph-content")
                                                  ,("style:wrap",           "none")]

inHeaderTags :: PandocMonad m => Int -> Text -> Doc Text -> OD m (Doc Text)
inHeaderTags i ident d =
  return $ inTags False "text:h" [ ("text:style-name", "Heading_20_" <> tshow i)
                                 , ("text:outline-level", tshow i)]
         $ if T.null ident
              then d
              else selfClosingTag "text:bookmark-start" [ ("text:name", ident) ]
                   <> d <>
                   selfClosingTag "text:bookmark-end" [ ("text:name", ident) ]

inQuotes :: QuoteType -> Doc Text -> Doc Text
inQuotes SingleQuote s = char '\8216' <> s <> char '\8217'
inQuotes DoubleQuote s = char '\8220' <> s <> char '\8221'

handleSpaces :: Text -> Doc Text
handleSpaces s = case T.uncons s of
  Just (' ', _) -> genTag s
  Just ('\t',x) -> selfClosingTag "text:tab" [] <> rm x
  _             -> rm s
  where
    genTag = T.span (==' ') >>> tag . T.length *** rm >>> uncurry (<>)
    tag n  = when (n /= 0) $ selfClosingTag "text:s" [("text:c", tshow n)]
    rm t   = case T.uncons t of
      Just ( ' ',xs) -> char ' ' <> genTag xs
      Just ('\t',xs) -> selfClosingTag "text:tab" [] <> genTag xs
      Just (   x,xs) -> char x <> rm xs
      Nothing        -> empty

-- | Convert Pandoc document to string in OpenDocument format.
writeOpenDocument :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeOpenDocument opts (Pandoc meta blocks) = do
  let defLang = Lang "en" "US" "" []
  lang <- case lookupMetaString "lang" meta of
            "" -> pure defLang
            s  -> fromMaybe defLang <$> toLang (Just s)
  setTranslations lang
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let meta' = case lookupMetaBlocks "abstract" meta of
                [] -> meta
                xs -> B.setMeta "abstract"
                        (B.divWith ("",[],[("custom-style","Abstract")])
                          (B.fromList xs))
                        meta
  ((body, metadata),s) <- flip runStateT
        defaultWriterState $ do
           m <- metaToContext opts
                  (blocksToOpenDocument opts)
                  (fmap chomp . inlinesToOpenDocument opts)
                  meta'
           b <- blocksToOpenDocument opts blocks
           return (b, m)
  let styles   = stTableStyles s ++ stParaStyles s ++ formulaStyles ++
                     map snd (sortBy (flip (comparing fst)) (
                        Map.elems (stTextStyles s)))
      listStyle (n,l) = inTags True "text:list-style"
                          [("style:name", "L" <> tshow n)] (vcat l)
  let listStyles  = map listStyle (stListStyles s)
  let automaticStyles = vcat $ reverse $ styles ++ listStyles
  let context = defField "body" body
              . defField "toc" (writerTableOfContents opts)
              . defField "automatic-styles" automaticStyles
              $ metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> body
       Just tpl -> renderTemplate tpl context

withParagraphStyle :: PandocMonad m
                   => WriterOptions -> Text -> [Block] -> OD m (Doc Text)
withParagraphStyle  o s (b:bs)
    | Para l <- b = go =<< inParagraphTagsWithStyle s <$> inlinesToOpenDocument o l
    | otherwise   = go =<< blockToOpenDocument o b
    where go i = (<>) i <$>  withParagraphStyle o s bs
withParagraphStyle _ _ [] = return empty

inPreformattedTags :: PandocMonad m => Text -> OD m (Doc Text)
inPreformattedTags s = do
  n <- paraStyle [("style:parent-style-name","Preformatted_20_Text")]
  return . inParagraphTagsWithStyle ("P" <> tshow n) . handleSpaces $ s

orderedListToOpenDocument :: PandocMonad m
                          => WriterOptions -> Int -> [[Block]] -> OD m (Doc Text)
orderedListToOpenDocument o pn bs =
    vcat . map (inTagsIndented "text:list-item") <$>
    mapM (orderedItemToOpenDocument o pn . map plainToPara) bs

orderedItemToOpenDocument :: PandocMonad m
                          => WriterOptions -> Int -> [Block] -> OD m (Doc Text)
orderedItemToOpenDocument  o n bs = vcat <$> mapM go bs
 where go (OrderedList a l) = newLevel a l
       go (Para          l) = inParagraphTagsWithStyle ("P" <> tshow n) <$>
                                inlinesToOpenDocument o l
       go b                 = blockToOpenDocument o b
       newLevel a l = do
         nn <- length <$> gets stParaStyles
         ls <- head   <$> gets stListStyles
         modify $ \s -> s { stListStyles = orderedListLevelStyle a ls :
                                 drop 1 (stListStyles s) }
         inTagsIndented "text:list" <$> orderedListToOpenDocument o nn l

isTightList :: [[Block]] -> Bool
isTightList []          = False
isTightList (b:_)
    | Plain {} : _ <- b = True
    | otherwise         = False

newOrderedListStyle :: PandocMonad m
                    => Bool -> ListAttributes -> OD m (Int,Int)
newOrderedListStyle b a = do
  ln <- (+) 1 . length  <$> gets stListStyles
  let nbs = orderedListLevelStyle a (ln, [])
  pn <- if b then inTightList (paraListStyle ln) else paraListStyle ln
  modify $ \s -> s { stListStyles = nbs : stListStyles s }
  return (ln,pn)

bulletListToOpenDocument :: PandocMonad m
                         => WriterOptions -> [[Block]] -> OD m (Doc Text)
bulletListToOpenDocument o b = do
  ln <- (+) 1 . length <$> gets stListStyles
  (pn,ns) <- if isTightList b then inTightList (bulletListStyle ln) else bulletListStyle ln
  modify $ \s -> s { stListStyles = ns : stListStyles s }
  is <- listItemsToOpenDocument ("P" <> tshow pn) o b
  return $ inTags True "text:list" [("text:style-name", "L" <> tshow ln)] is

listItemsToOpenDocument :: PandocMonad m
                        => Text -> WriterOptions -> [[Block]] -> OD m (Doc Text)
listItemsToOpenDocument s o is =
    vcat . map (inTagsIndented "text:list-item") <$> mapM (withParagraphStyle o s . map plainToPara) is

deflistItemToOpenDocument :: PandocMonad m
                          => WriterOptions -> ([Inline],[[Block]]) -> OD m (Doc Text)
deflistItemToOpenDocument o (t,d) = do
  let ts = if isTightList d
           then "Definition_20_Term_20_Tight"       else "Definition_20_Term"
      ds = if isTightList d
           then "Definition_20_Definition_20_Tight" else "Definition_20_Definition"
  t' <- withParagraphStyle o ts [Para t]
  d' <- liftM vcat $ mapM (withParagraphStyle o ds . map plainToPara) d
  return $ t' $$ d'

inBlockQuote :: PandocMonad m
             => WriterOptions -> Int -> [Block] -> OD m (Doc Text)
inBlockQuote  o i (b:bs)
    | BlockQuote l <- b = do increaseIndent
                             ni <- paraStyle
                                   [("style:parent-style-name","Quotations")]
                             go =<< inBlockQuote o ni (map plainToPara l)
    | Para       l <- b = go =<< inParagraphTagsWithStyle ("P" <> tshow i) <$> inlinesToOpenDocument o l
    | otherwise         = go =<< blockToOpenDocument o b
    where go  block  = ($$) block <$> inBlockQuote o i bs
inBlockQuote     _ _ [] =  resetIndent >> return empty

-- | Convert a list of Pandoc blocks to OpenDocument.
blocksToOpenDocument :: PandocMonad m => WriterOptions -> [Block] -> OD m (Doc Text)
blocksToOpenDocument o b = vcat <$> mapM (blockToOpenDocument o) b

-- | Convert a Pandoc block element to OpenDocument.
blockToOpenDocument :: PandocMonad m => WriterOptions -> Block -> OD m (Doc Text)
blockToOpenDocument o bs
    | Plain          b <- bs = if null b
                                  then return empty
                                  else inParagraphTags =<< inlinesToOpenDocument o b
    | Para [Image attr c (s,T.stripPrefix "fig:" -> Just t)] <- bs
                             = figure attr c s t
    | Para           b <- bs = if null b &&
                                    not (isEnabled Ext_empty_paragraphs o)
                                  then return empty
                                  else inParagraphTags =<< inlinesToOpenDocument o b
    | LineBlock      b <- bs = blockToOpenDocument o $ linesToPara b
    | Div attr xs      <- bs = do
        let (_,_,kvs) = attr
        withLangFromAttr attr $
          case lookup "custom-style" kvs of
            Just sty -> withParagraphStyle o sty xs
            _        -> blocksToOpenDocument o xs
    | Header     i (ident,_,_) b
                       <- bs = setFirstPara >> (inHeaderTags i ident
                                  =<< inlinesToOpenDocument o b)
    | BlockQuote     b <- bs = setFirstPara >> mkBlockQuote b
    | DefinitionList b <- bs = setFirstPara >> defList b
    | BulletList     b <- bs = setFirstPara >> bulletListToOpenDocument o b
    | OrderedList  a b <- bs = setFirstPara >> orderedList a b
    | CodeBlock    _ s <- bs = setFirstPara >> preformatted s
    | Table _ bc s th tb tf
                       <- bs = let (c, a, w, h, r) = toLegacyTable bc s th tb tf
                               in setFirstPara >> table c a w h r
    | HorizontalRule   <- bs = setFirstPara >> return (selfClosingTag "text:p"
                                [ ("text:style-name", "Horizontal_20_Line") ])
    | RawBlock f     s <- bs = if f == Format "opendocument"
                                  then return $ text $ T.unpack s
                                  else do
                                    report $ BlockNotRendered bs
                                    return empty
    | Null             <- bs = return empty
    | otherwise              = return empty
    where
      defList       b = do setInDefinitionList True
                           r <- vcat  <$> mapM (deflistItemToOpenDocument o) b
                           setInDefinitionList False
                           return r
      preformatted  s = (flush . vcat) <$> mapM (inPreformattedTags . escapeStringForXML) (T.lines s)
      mkBlockQuote  b = do increaseIndent
                           i <- paraStyle
                                 [("style:parent-style-name","Quotations")]
                           inBlockQuote o i (map plainToPara b)
      orderedList a b = do (ln,pn) <- newOrderedListStyle (isTightList b) a
                           inTags True "text:list" [ ("text:style-name", "L" <> tshow ln)]
                                      <$> orderedListToOpenDocument o pn b
      table c a w h r = do
        tn <- length <$> gets stTableStyles
        pn <- length <$> gets stParaStyles
        let  genIds      = map chr [65..]
             name        = "Table" <> tshow (tn + 1)
             columnIds   = zip genIds w
             mkColumn  n = selfClosingTag "table:table-column" [("table:style-name", name <> "." <> T.singleton (fst n))]
             columns     = map mkColumn columnIds
             paraHStyles = paraTableStyles "Heading"  pn a
             paraStyles  = paraTableStyles "Contents" (pn + length (newPara paraHStyles)) a
             newPara     = map snd . filter (not . isEmpty . snd)
        addTableStyle $ tableStyle tn columnIds
        mapM_ addParaStyle . newPara $ paraHStyles ++ paraStyles
        captionDoc <- if null c
                      then return empty
                      else inlinesToOpenDocument o c >>=
                             if isEnabled Ext_native_numbering o
                                then numberedTableCaption
                                else unNumberedCaption "TableCaption"
        th <- if all null h
                 then return empty
                 else colHeadsToOpenDocument o (map fst paraHStyles) h
        tr <- mapM (tableRowToOpenDocument o (map fst paraStyles)) r
        let tableDoc = inTags True "table:table" [
                            ("table:name"      , name)
                          , ("table:style-name", name)
                          ] (vcat columns $$ th $$ vcat tr)
        return $ captionDoc $$ tableDoc
      figure attr caption source title | null caption =
        withParagraphStyle o "Figure" [Para [Image attr caption (source,title)]]
                                  | otherwise    = do
        imageDoc <- withParagraphStyle o "FigureWithCaption" [Para [Image attr caption (source,title)]]
        captionDoc <- inlinesToOpenDocument o caption >>=
                         if isEnabled Ext_native_numbering o
                            then numberedFigureCaption
                            else unNumberedCaption "FigureCaption"
        return $ imageDoc $$ captionDoc


numberedTableCaption :: PandocMonad m => Doc Text -> OD m (Doc Text)
numberedTableCaption caption = do
    id' <- gets stTableCaptionId
    modify (\st -> st{ stTableCaptionId = id' + 1 })
    capterm <- translateTerm Term.Table
    return $ numberedCaption "TableCaption" capterm "Table" id' caption

numberedFigureCaption :: PandocMonad m => Doc Text -> OD m (Doc Text)
numberedFigureCaption caption = do
    id' <- gets stImageCaptionId
    modify (\st -> st{ stImageCaptionId = id' + 1 })
    capterm <- translateTerm Term.Figure
    return $ numberedCaption "FigureCaption" capterm "Illustration" id' caption

numberedCaption :: Text -> Text -> Text -> Int -> Doc Text -> Doc Text
numberedCaption style term name num caption =
    let t = text $ T.unpack term
        r = num - 1
        s = inTags False "text:sequence" [ ("text:ref-name", "ref" <> name <> tshow r),
                                           ("text:name", name),
                                           ("text:formula", "ooow:" <> name <> "+1"),
                                           ("style:num-format", "1") ] $ text $ show num
        c = text ": "
    in inParagraphTagsWithStyle style $ hcat [ t, text " ", s, c, caption ]

unNumberedCaption :: Monad m => Text -> Doc Text -> OD m (Doc Text)
unNumberedCaption style caption = return $ inParagraphTagsWithStyle style caption

colHeadsToOpenDocument :: PandocMonad m
                       => WriterOptions -> [Text] -> [[Block]]
                       -> OD m (Doc Text)
colHeadsToOpenDocument o ns hs =
    inTagsIndented "table:table-header-rows" . inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o "TableHeaderRowCell") (zip ns hs)

tableRowToOpenDocument :: PandocMonad m
                       => WriterOptions -> [Text] -> [[Block]]
                       -> OD m (Doc Text)
tableRowToOpenDocument o ns cs =
    inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o "TableRowCell") (zip ns cs)

tableItemToOpenDocument :: PandocMonad m
                        => WriterOptions -> Text -> (Text,[Block])
                        -> OD m (Doc Text)
tableItemToOpenDocument o s (n,i) =
  let a = [ ("table:style-name" , s )
          , ("office:value-type", "string"     )
          ]
  in  inTags True "table:table-cell" a <$>
      withParagraphStyle o n (map plainToPara i)

-- | Convert a list of inline elements to OpenDocument.
inlinesToOpenDocument :: PandocMonad m => WriterOptions -> [Inline] -> OD m (Doc Text)
inlinesToOpenDocument o l = hcat <$> toChunks o l

toChunks :: PandocMonad m => WriterOptions -> [Inline] -> OD m [Doc Text]
toChunks _ [] = return []
toChunks o (x : xs)
  | isChunkable x = do
        contents <- (inTextStyle . hcat) =<<
                     mapM (inlineToOpenDocument o) (x:ys)
        rest <- toChunks o zs
        return (contents : rest)
  | otherwise     = do
        contents <- inlineToOpenDocument o x
        rest <- toChunks o xs
        return (contents : rest)
  where (ys, zs) = span isChunkable xs

isChunkable :: Inline -> Bool
isChunkable (Str _)   = True
isChunkable Space     = True
isChunkable SoftBreak = True
isChunkable _         = False

-- | Convert an inline element to OpenDocument.
inlineToOpenDocument :: PandocMonad m => WriterOptions -> Inline -> OD m (Doc Text)
inlineToOpenDocument o ils
  = case ils of
    Space         -> return space
    SoftBreak
     | writerWrapText o == WrapPreserve
                  -> return $ preformatted "\n"
     | otherwise  ->return space
    Span attr xs  -> withLangFromAttr attr (inlinesToOpenDocument o xs)
    LineBreak     -> return $ selfClosingTag "text:line-break" []
    Str         s -> return $ handleSpaces $ escapeStringForXML s
    Emph        l -> withTextStyle Italic $ inlinesToOpenDocument o l
    Underline   l -> withTextStyle Under  $ inlinesToOpenDocument o l
    Strong      l -> withTextStyle Bold   $ inlinesToOpenDocument o l
    Strikeout   l -> withTextStyle Strike $ inlinesToOpenDocument o l
    Superscript l -> withTextStyle Sup    $ inlinesToOpenDocument o l
    Subscript   l -> withTextStyle Sub    $ inlinesToOpenDocument o l
    SmallCaps   l -> withTextStyle SmallC $ inlinesToOpenDocument o l
    Quoted    t l -> inQuotes t <$> inlinesToOpenDocument o l
    Code      _ s -> inlinedCode $ preformatted s
    Math      t s -> lift (texMathToInlines t s) >>=
                         inlinesToOpenDocument o
    Cite      _ l -> inlinesToOpenDocument o l
    RawInline f s -> if f == Format "opendocument"
                       then return $ text $ T.unpack s
                       else do
                         report $ InlineNotRendered ils
                         return empty
    Link _ l (s,t) ->  mkLink s t <$> inlinesToOpenDocument o l
    Image attr _ (s,t) -> mkImg attr s t
    Note        l  -> mkNote l
    where
      preformatted s = handleSpaces $ escapeStringForXML s
      inlinedCode s = return $ inTags False "text:span" [("text:style-name", "Source_Text")] s
      mkLink   s t = inTags False "text:a" [ ("xlink:type" , "simple")
                                           , ("xlink:href" , s       )
                                           , ("office:name", t       )
                                           ] . inSpanTags "Definition"
      mkImg (_, _, kvs) s _ = do
               id' <- gets stImageId
               modify (\st -> st{ stImageId = id' + 1 })
               let getDims [] = []
                   getDims (("width", w) :xs) = ("svg:width", w)  : getDims xs
                   getDims (("rel-width", w):xs) = ("style:rel-width", w) : getDims xs
                   getDims (("height", h):xs) = ("svg:height", h) : getDims xs
                   getDims (("rel-height", w):xs) = ("style:rel-height", w) : getDims xs
                   getDims (_:xs) =                             getDims xs
               return $ inTags False "draw:frame"
                        (("draw:name", "img" <> tshow id') : getDims kvs) $
                     selfClosingTag "draw:image" [ ("xlink:href"   , s       )
                                                 , ("xlink:type"   , "simple")
                                                 , ("xlink:show"   , "embed" )
                                                 , ("xlink:actuate", "onLoad")]
      mkNote     l = do
        n <- length <$> gets stNotes
        let footNote t = inTags False "text:note"
                         [ ("text:id"        , "ftn" <> tshow n)
                         , ("text:note-class", "footnote"     )] $
                         inTagsSimple "text:note-citation" (text . show $ n + 1) <>
                         inTagsSimple "text:note-body" t
        nn <- footNote <$> withParagraphStyle o "Footnote" l
        addNote nn
        return nn

bulletListStyle :: PandocMonad m => Int -> OD m (Int,(Int,[Doc Text]))
bulletListStyle l = do
  let doStyles  i = inTags True "text:list-level-style-bullet"
                    [ ("text:level"      , tshow (i + 1))
                    , ("text:style-name" , "Bullet_20_Symbols"  )
                    , ("style:num-suffix", "."                  )
                    , ("text:bullet-char", T.singleton (bulletList !! i))
                    ] (listLevelStyle (1 + i))
      bulletList  = map chr $ cycle [8226,9702,9642]
      listElStyle = map doStyles [0..9]
  pn <- paraListStyle l
  return (pn, (l, listElStyle))

orderedListLevelStyle :: ListAttributes -> (Int, [Doc Text]) -> (Int,[Doc Text])
orderedListLevelStyle (s,n, d) (l,ls) =
    let suffix    = case d of
                      OneParen  -> [("style:num-suffix", ")")]
                      TwoParens -> [("style:num-prefix", "(")
                                   ,("style:num-suffix", ")")]
                      _         -> [("style:num-suffix", ".")]
        format    = case n of
                      UpperAlpha -> "A"
                      LowerAlpha -> "a"
                      UpperRoman -> "I"
                      LowerRoman -> "i"
                      _          -> "1"
        listStyle = inTags True "text:list-level-style-number"
                    ([ ("text:level"      , tshow $ 1 + length ls )
                     , ("text:style-name" , "Numbering_20_Symbols")
                     , ("style:num-format", format                )
                     , ("text:start-value", tshow s               )
                     ] ++ suffix) (listLevelStyle (1 + length ls))
    in  (l, ls ++ [listStyle])

listLevelStyle :: Int -> Doc Text
listLevelStyle i =
    let indent = tshow (0.25 + (0.25 * fromIntegral i :: Double)) in
    inTags True "style:list-level-properties"
                       [ ("text:list-level-position-and-space-mode",
                          "label-alignment")
                       , ("fo:text-align", "right")
                       ] $
       selfClosingTag "style:list-level-label-alignment"
                      [ ("text:label-followed-by", "listtab")
                      , ("text:list-tab-stop-position", indent <> "in")
                      , ("fo:text-indent", "-0.25in")
                      , ("fo:margin-left", indent <> "in")
                      ]

tableStyle :: Int -> [(Char,Double)] -> Doc Text
tableStyle num wcs =
    let tableId        = "Table" <> tshow (num + 1)
        table          = inTags True "style:style"
                         [("style:name", tableId)
                         ,("style:family", "table")] $
                         selfClosingTag "style:table-properties"
                         [("table:align"    , "center")]
        colStyle (c,0) = selfClosingTag "style:style"
                         [ ("style:name"  , tableId <> "." <> T.singleton c)
                         , ("style:family", "table-column"       )]
        colStyle (c,w) = inTags True "style:style"
                         [ ("style:name"  , tableId <> "." <> T.singleton c)
                         , ("style:family", "table-column"       )] $
                         selfClosingTag "style:table-column-properties"
                         [("style:rel-column-width", T.pack $ printf "%d*" (floor $ w * 65535 :: Integer))]
        headerRowCellStyle = inTags True "style:style"
                         [ ("style:name"  , "TableHeaderRowCell")
                         , ("style:family", "table-cell"    )] $
                         selfClosingTag "style:table-cell-properties"
                         [ ("fo:border", "none")]
        rowCellStyle = inTags True "style:style"
                         [ ("style:name"  , "TableRowCell")
                         , ("style:family", "table-cell"    )] $
                         selfClosingTag "style:table-cell-properties"
                         [ ("fo:border", "none")]
        cellStyles = if num == 0
                     then headerRowCellStyle $$ rowCellStyle
                     else empty
        columnStyles   = map colStyle wcs
    in cellStyles $$ table $$ vcat columnStyles

paraStyle :: PandocMonad m => [(Text,Text)] -> OD m Int
paraStyle attrs = do
  pn <- (+)   1 . length       <$> gets stParaStyles
  i  <- (*) (0.5 :: Double) . fromIntegral <$> gets stIndentPara
  b  <- gets stInDefinition
  t  <- gets stTight
  let styleAttr = [ ("style:name"             , "P" <> tshow pn)
                  , ("style:family"           , "paragraph"   )]
      indentVal = flip (<>) "in" . tshow $ if b then max 0.5 i else i
      tight     = if t then [ ("fo:margin-top"          , "0in"    )
                            , ("fo:margin-bottom"       , "0in"    )]
                       else []
      indent    = if i /= 0 || b
                      then [ ("fo:margin-left"         , indentVal)
                           , ("fo:margin-right"        , "0in"    )
                           , ("fo:text-indent"         , "0in"    )
                           , ("style:auto-text-indent" , "false"  )]
                      else []
      attributes = indent <> tight
      paraProps = if null attributes
                     then mempty
                     else selfClosingTag
                             "style:paragraph-properties" attributes
  addParaStyle $ inTags True "style:style" (styleAttr <> attrs) paraProps
  return pn

paraListStyle :: PandocMonad m => Int -> OD m Int
paraListStyle l = paraStyle
  [("style:parent-style-name","Text_20_body")
  ,("style:list-style-name", "L" <> tshow l)]

paraTableStyles :: Text -> Int -> [Alignment] -> [(Text, Doc Text)]
paraTableStyles _ _ [] = []
paraTableStyles t s (a:xs)
    | AlignRight  <- a = (         pName s, res s "end"   ) : paraTableStyles t (s + 1) xs
    | AlignCenter <- a = (         pName s, res s "center") : paraTableStyles t (s + 1) xs
    | otherwise        = ("Table_20_" <> t, empty         ) : paraTableStyles t  s      xs
    where pName sn = "P" <> tshow (sn + 1)
          res sn x = inTags True "style:style"
                     [ ("style:name"             , pName sn        )
                     , ("style:family"           , "paragraph"     )
                     , ("style:parent-style-name", "Table_20_" <> t)] $
                     selfClosingTag "style:paragraph-properties"
                     [ ("fo:text-align", x)
                     , ("style:justify-single-word", "false")]

data TextStyle = Italic
               | Bold
               | Under
               | Strike
               | Sub
               | Sup
               | SmallC
               | Pre
               | Language Lang
               deriving ( Eq,Ord )

textStyleAttr :: Map.Map Text Text
              -> TextStyle
              -> Map.Map Text Text
textStyleAttr m s
    | Italic <- s = Map.insert "fo:font-style" "italic" .
                    Map.insert "style:font-style-asian" "italic" .
                    Map.insert "style:font-style-complex" "italic" $ m
    | Bold   <- s = Map.insert "fo:font-weight" "bold" .
                    Map.insert "style:font-weight-asian" "bold" .
                    Map.insert "style:font-weight-complex" "bold" $ m
    | Under  <- s = Map.insert "style:text-underline-style" "solid" .
                    Map.insert "style:text-underline-width" "auto" .
                    Map.insert "style:text-underline-color" "font-color" $ m
    | Strike <- s = Map.insert "style:text-line-through-style" "solid" m
    | Sub    <- s = Map.insert "style:text-position" "sub 58%" m
    | Sup    <- s = Map.insert "style:text-position" "super 58%" m
    | SmallC <- s = Map.insert "fo:font-variant" "small-caps" m
    | Pre    <- s = Map.insert "style:font-name" "Courier New" .
                    Map.insert "style:font-name-asian" "Courier New" .
                    Map.insert "style:font-name-complex" "Courier New" $ m
    | Language lang <- s
                  = Map.insert "fo:language" (langLanguage lang) .
                    Map.insert "fo:country" (langRegion lang) $ m
    | otherwise   = m

withLangFromAttr :: PandocMonad m => Attr -> OD m a -> OD m a
withLangFromAttr (_,_,kvs) action =
  case lookup "lang" kvs of
       Nothing -> action
       Just l  ->
         case parseBCP47 l of
              Right lang -> withTextStyle (Language lang) action
              Left _ -> do
                report $ InvalidLang l
                action

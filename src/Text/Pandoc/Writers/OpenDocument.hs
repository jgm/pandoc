{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-
Copyright (C) 2008-2017 Andrea Rossato <andrea.rossato@ing.unitn.it>
                        and John MacFarlane.

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
   Module      : Text.Pandoc.Writers.OpenDocument
   Copyright   : Copyright (C) 2008-2017 Andrea Rossato and John MacFarlane
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
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared (linesToPara)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML
import Text.Pandoc.BCP47 (parseBCP47, Lang(..))
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
    WriterState { stNotes         :: [Doc]
                , stTableStyles   :: [Doc]
                , stParaStyles    :: [Doc]
                , stListStyles    :: [(Int, [Doc])]
                , stTextStyles    :: Map.Map (Set.Set TextStyle) (String, Doc)
                , stTextStyleAttr :: Set.Set TextStyle
                , stIndentPara    :: Int
                , stInDefinition  :: Bool
                , stTight         :: Bool
                , stFirstPara     :: Bool
                , stImageId       :: Int
                }

defaultWriterState :: WriterState
defaultWriterState =
    WriterState { stNotes         = []
                , stTableStyles   = []
                , stParaStyles    = []
                , stListStyles    = []
                , stTextStyles    = Map.empty
                , stTextStyleAttr = Set.empty
                , stIndentPara    = 0
                , stInDefinition  = False
                , stTight         = False
                , stFirstPara     = False
                , stImageId       = 1
                }

when :: Bool -> Doc -> Doc
when p a = if p then a else empty

addTableStyle :: PandocMonad m => Doc -> OD m ()
addTableStyle i = modify $ \s -> s { stTableStyles = i : stTableStyles s }

addNote :: PandocMonad m => Doc -> OD m ()
addNote i = modify $ \s -> s { stNotes = i : stNotes s }

addParaStyle :: PandocMonad m => Doc -> OD m ()
addParaStyle i = modify $ \s -> s { stParaStyles = i : stParaStyles s }

addTextStyle :: PandocMonad m => Set.Set TextStyle -> (String, Doc) -> OD m ()
addTextStyle attrs i = modify $ \s ->
  s { stTextStyles = Map.insert attrs i (stTextStyles s) }

addTextStyleAttr :: PandocMonad m => TextStyle -> OD m ()
addTextStyleAttr t = modify $ \s ->
  s { stTextStyleAttr = Set.insert t (stTextStyleAttr s) }

increaseIndent :: PandocMonad m => OD m ()
increaseIndent = modify $ \s -> s { stIndentPara = 1 + stIndentPara s }

resetIndent :: PandocMonad m => OD m ()
resetIndent = modify $ \s -> s { stIndentPara = (stIndentPara s) - 1 }

inTightList :: PandocMonad m => OD m a -> OD m a
inTightList  f = modify (\s -> s { stTight = True  }) >> f >>= \r ->
                 modify (\s -> s { stTight = False }) >> return r

setInDefinitionList :: PandocMonad m => Bool -> OD m ()
setInDefinitionList b = modify $  \s -> s { stInDefinition = b }

setFirstPara :: PandocMonad m => OD m ()
setFirstPara =  modify $  \s -> s { stFirstPara = True }

inParagraphTags :: PandocMonad m => Doc -> OD m Doc
inParagraphTags d | isEmpty d = return empty
inParagraphTags d = do
  b <- gets stFirstPara
  a <- if b
       then do modify $ \st -> st { stFirstPara = False }
               return $ [("text:style-name", "First_20_paragraph")]
       else    return   [("text:style-name", "Text_20_body")]
  return $ inTags False "text:p" a d

inParagraphTagsWithStyle :: String -> Doc -> Doc
inParagraphTagsWithStyle sty = inTags False "text:p" [("text:style-name", sty)]

inSpanTags :: String -> Doc -> Doc
inSpanTags s = inTags False "text:span" [("text:style-name",s)]

withTextStyle :: PandocMonad m => TextStyle -> OD m a -> OD m a
withTextStyle s f = do
  oldTextStyleAttr <- gets stTextStyleAttr
  addTextStyleAttr s
  res <- f
  modify $ \st -> st{ stTextStyleAttr = oldTextStyleAttr }
  return res

inTextStyle :: PandocMonad m => Doc -> OD m Doc
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
              let styleName = "T" ++ show (Map.size styles + 1)
              addTextStyle at (styleName,
                     inTags False "style:style"
                       [("style:name", styleName)
                       ,("style:family", "text")]
                       $ selfClosingTag "style:text-properties"
                          (concatMap textStyleAttr (Set.toList at)))
              return $ inTags False
                  "text:span" [("text:style-name",styleName)] d

inHeaderTags :: PandocMonad m => Int -> Doc -> OD m Doc
inHeaderTags i d =
  return $ inTags False "text:h" [ ("text:style-name", "Heading_20_" ++ show i)
                                 , ("text:outline-level", show i)] d

inQuotes :: QuoteType -> Doc -> Doc
inQuotes SingleQuote s = char '\8216' <> s <> char '\8217'
inQuotes DoubleQuote s = char '\8220' <> s <> char '\8221'

handleSpaces :: String -> Doc
handleSpaces s
    | ( ' ':_) <- s = genTag s
    | ('\t':x) <- s = selfClosingTag "text:tab" [] <> rm x
    | otherwise     = rm s
    where
        genTag       = span (==' ') >>> tag . length *** rm >>> uncurry (<>)
        tag n        = when (n /= 0) $ selfClosingTag "text:s" [("text:c", show n)]
        rm ( ' ':xs) = char ' ' <> genTag xs
        rm ('\t':xs) = selfClosingTag "text:tab" [] <> genTag xs
        rm (   x:xs) = char  x  <> rm xs
        rm        [] = empty

-- | Convert Pandoc document to string in OpenDocument format.
writeOpenDocument :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeOpenDocument opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' :: Doc -> Text
      render' = render colwidth
  ((body, metadata),s) <- flip runStateT
        defaultWriterState $ do
           m <- metaToJSON opts
                  (fmap render' . blocksToOpenDocument opts)
                  (fmap render' . inlinesToOpenDocument opts)
                  meta
           b <- render' `fmap` blocksToOpenDocument opts blocks
           return (b, m)
  let styles   = stTableStyles s ++ stParaStyles s ++
                     map snd (reverse $ sortBy (comparing fst) $
                        Map.elems (stTextStyles s))
      listStyle (n,l) = inTags True "text:list-style"
                          [("style:name", "L" ++ show n)] (vcat l)
  let listStyles  = map listStyle (stListStyles s)
  let automaticStyles = vcat $ reverse $ styles ++ listStyles
  let context = defField "body" body
              $ defField "toc" (writerTableOfContents opts)
              $ defField "automatic-styles" (render' automaticStyles)
              $ metadata
  case writerTemplate opts of
       Nothing  -> return body
       Just tpl -> renderTemplate' tpl context

withParagraphStyle :: PandocMonad m
                   => WriterOptions -> String -> [Block] -> OD m Doc
withParagraphStyle  o s (b:bs)
    | Para l <- b = go =<< inParagraphTagsWithStyle s <$> inlinesToOpenDocument o l
    | otherwise   = go =<< blockToOpenDocument o b
    where go i = (<>) i <$>  withParagraphStyle o s bs
withParagraphStyle _ _ [] = return empty

inPreformattedTags :: PandocMonad m => String -> OD m Doc
inPreformattedTags s = do
  n <- paraStyle [("style:parent-style-name","Preformatted_20_Text")]
  return . inParagraphTagsWithStyle ("P" ++ show n) . handleSpaces $ s

orderedListToOpenDocument :: PandocMonad m
                          => WriterOptions -> Int -> [[Block]] -> OD m Doc
orderedListToOpenDocument o pn bs =
    vcat . map (inTagsIndented "text:list-item") <$>
    mapM (orderedItemToOpenDocument o pn . map plainToPara) bs

orderedItemToOpenDocument :: PandocMonad m
                          => WriterOptions -> Int -> [Block] -> OD m Doc
orderedItemToOpenDocument  o n bs = vcat <$> mapM go bs
 where go (OrderedList a l) = newLevel a l
       go (Para          l) = inParagraphTagsWithStyle ("P" ++ show n) <$>
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
                         => WriterOptions -> [[Block]] -> OD m Doc
bulletListToOpenDocument o b = do
  ln <- (+) 1 . length <$> gets stListStyles
  (pn,ns) <- if isTightList b then inTightList (bulletListStyle ln) else bulletListStyle ln
  modify $ \s -> s { stListStyles = ns : stListStyles s }
  is <- listItemsToOpenDocument ("P" ++ show pn) o b
  return $ inTags True "text:list" [("text:style-name", "L" ++ show ln)] is

listItemsToOpenDocument :: PandocMonad m
                        => String -> WriterOptions -> [[Block]] -> OD m Doc
listItemsToOpenDocument s o is =
    vcat . map (inTagsIndented "text:list-item") <$> mapM (withParagraphStyle o s . map plainToPara) is

deflistItemToOpenDocument :: PandocMonad m
                          => WriterOptions -> ([Inline],[[Block]]) -> OD m Doc
deflistItemToOpenDocument o (t,d) = do
  let ts = if isTightList d
           then "Definition_20_Term_20_Tight"       else "Definition_20_Term"
      ds = if isTightList d
           then "Definition_20_Definition_20_Tight" else "Definition_20_Definition"
  t' <- withParagraphStyle o ts [Para t]
  d' <- liftM vcat $ mapM (withParagraphStyle o ds . (map plainToPara)) d
  return $ t' $$ d'

inBlockQuote :: PandocMonad m
             => WriterOptions -> Int -> [Block] -> OD m Doc
inBlockQuote  o i (b:bs)
    | BlockQuote l <- b = do increaseIndent
                             ni <- paraStyle
                                   [("style:parent-style-name","Quotations")]
                             go =<< inBlockQuote o ni (map plainToPara l)
    | Para       l <- b = do go =<< inParagraphTagsWithStyle ("P" ++ show  i) <$> inlinesToOpenDocument o l
    | otherwise         = do go =<< blockToOpenDocument o b
    where go  block  = ($$) block <$> inBlockQuote o i bs
inBlockQuote     _ _ [] =  resetIndent >> return empty

-- | Convert a list of Pandoc blocks to OpenDocument.
blocksToOpenDocument :: PandocMonad m => WriterOptions -> [Block] -> OD m Doc
blocksToOpenDocument o b = vcat <$> mapM (blockToOpenDocument o) b

-- | Convert a Pandoc block element to OpenDocument.
blockToOpenDocument :: PandocMonad m => WriterOptions -> Block -> OD m Doc
blockToOpenDocument o bs
    | Plain          b <- bs = if null b
                                  then return empty
                                  else inParagraphTags =<< inlinesToOpenDocument o b
    | Para [Image attr c (s,'f':'i':'g':':':t)] <- bs
                             = figure attr c s t
    | Para           b <- bs = if null b
                                  then return empty
                                  else inParagraphTags =<< inlinesToOpenDocument o b
    | LineBlock      b <- bs = blockToOpenDocument o $ linesToPara b
    | Div attr xs      <- bs = withLangFromAttr attr
                                  (blocksToOpenDocument o xs)
    | Header     i _ b <- bs = setFirstPara >>
                               (inHeaderTags  i =<< inlinesToOpenDocument o b)
    | BlockQuote     b <- bs = setFirstPara >> mkBlockQuote b
    | DefinitionList b <- bs = setFirstPara >> defList b
    | BulletList     b <- bs = setFirstPara >> bulletListToOpenDocument o b
    | OrderedList  a b <- bs = setFirstPara >> orderedList a b
    | CodeBlock    _ s <- bs = setFirstPara >> preformatted s
    | Table  c a w h r <- bs = setFirstPara >> table c a w h r
    | HorizontalRule   <- bs = setFirstPara >> return (selfClosingTag "text:p"
                                [ ("text:style-name", "Horizontal_20_Line") ])
    | RawBlock f     s <- bs = if f == Format "opendocument"
                                  then return $ text s
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
      preformatted  s = (flush . vcat) <$> mapM (inPreformattedTags . escapeStringForXML) (lines s)
      mkBlockQuote  b = do increaseIndent
                           i <- paraStyle
                                 [("style:parent-style-name","Quotations")]
                           inBlockQuote o i (map plainToPara b)
      orderedList a b = do (ln,pn) <- newOrderedListStyle (isTightList b) a
                           inTags True "text:list" [ ("text:style-name", "L" ++ show ln)]
                                      <$> orderedListToOpenDocument o pn b
      table c a w h r = do
        tn <- length <$> gets stTableStyles
        pn <- length <$> gets stParaStyles
        let  genIds      = map chr [65..]
             name        = "Table" ++ show (tn + 1)
             columnIds   = zip genIds w
             mkColumn  n = selfClosingTag "table:table-column" [("table:style-name", name ++ "." ++ [fst n])]
             columns     = map mkColumn columnIds
             paraHStyles = paraTableStyles "Heading"  pn a
             paraStyles  = paraTableStyles "Contents" (pn + length (newPara paraHStyles)) a
             newPara     = map snd . filter (not . isEmpty . snd)
        addTableStyle $ tableStyle tn columnIds
        mapM_ addParaStyle . newPara $ paraHStyles ++ paraStyles
        captionDoc <- if null c
                      then return empty
                      else withParagraphStyle o "Table" [Para c]
        th <- if all null h
                 then return empty
                 else colHeadsToOpenDocument o name (map fst paraHStyles) h
        tr <- mapM (tableRowToOpenDocument o name (map fst paraStyles)) r
        return $ inTags True "table:table" [ ("table:name"      , name)
                                           , ("table:style-name", name)
                                           ] (vcat columns $$ th $$ vcat tr) $$ captionDoc
      figure attr caption source title | null caption =
        withParagraphStyle o "Figure" [Para [Image attr caption (source,title)]]
                                  | otherwise    = do
        imageDoc <- withParagraphStyle o "FigureWithCaption" [Para [Image attr caption (source,title)]]
        captionDoc <- withParagraphStyle o "FigureCaption" [Para caption]
        return $ imageDoc $$ captionDoc

colHeadsToOpenDocument :: PandocMonad m
                       => WriterOptions -> String -> [String] -> [[Block]]
                       -> OD m Doc
colHeadsToOpenDocument o tn ns hs =
    inTagsIndented "table:table-header-rows" . inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o tn) (zip ns hs)

tableRowToOpenDocument :: PandocMonad m
                       => WriterOptions -> String -> [String] -> [[Block]]
                       -> OD m Doc
tableRowToOpenDocument o tn ns cs =
    inTagsIndented "table:table-row" . vcat <$>
    mapM (tableItemToOpenDocument o tn) (zip ns cs)

tableItemToOpenDocument :: PandocMonad m
                        => WriterOptions -> String -> (String,[Block])
                        -> OD m Doc
tableItemToOpenDocument o tn (n,i) =
  let a = [ ("table:style-name" , tn ++ ".A1" )
          , ("office:value-type", "string"     )
          ]
  in  inTags True "table:table-cell" a <$>
      withParagraphStyle o n (map plainToPara i)

-- | Convert a list of inline elements to OpenDocument.
inlinesToOpenDocument :: PandocMonad m => WriterOptions -> [Inline] -> OD m Doc
inlinesToOpenDocument o l = hcat <$> toChunks o l

toChunks :: PandocMonad m => WriterOptions -> [Inline] -> OD m [Doc]
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
inlineToOpenDocument :: PandocMonad m => WriterOptions -> Inline -> OD m Doc
inlineToOpenDocument o ils
  = case ils of
    Space         -> return space
    SoftBreak
     | writerWrapText o == WrapPreserve
                  -> return $ preformatted "\n"
     | otherwise  -> return $ space
    Span attr xs  -> withLangFromAttr attr (inlinesToOpenDocument o xs)
    LineBreak     -> return $ selfClosingTag "text:line-break" []
    Str         s -> return $ handleSpaces $ escapeStringForXML s
    Emph        l -> withTextStyle Italic $ inlinesToOpenDocument o l
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
                       then return $ text s
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
                   getDims (("height", h):xs) = ("svg:height", h) : getDims xs
                   getDims (_:xs) =                             getDims xs
               return $ inTags False "draw:frame"
                        (("draw:name", "img" ++ show id') : getDims kvs) $
                     selfClosingTag "draw:image" [ ("xlink:href"   , s       )
                                                 , ("xlink:type"   , "simple")
                                                 , ("xlink:show"   , "embed" )
                                                 , ("xlink:actuate", "onLoad")]
      mkNote     l = do
        n <- length <$> gets stNotes
        let footNote t = inTags False "text:note"
                         [ ("text:id"        , "ftn" ++ show n)
                         , ("text:note-class", "footnote"     )] $
                         inTagsSimple "text:note-citation" (text . show $ n + 1) <>
                         inTagsSimple "text:note-body" t
        nn <- footNote <$> withParagraphStyle o "Footnote" l
        addNote nn
        return nn

bulletListStyle :: PandocMonad m => Int -> OD m (Int,(Int,[Doc]))
bulletListStyle l = do
  let doStyles  i = inTags True "text:list-level-style-bullet"
                    [ ("text:level"      , show (i + 1)       )
                    , ("text:style-name" , "Bullet_20_Symbols")
                    , ("style:num-suffix", "."                )
                    , ("text:bullet-char", [bulletList !! i]  )
                    ] (listLevelStyle (1 + i))
      bulletList  = map chr $ cycle [8226,9702,9642]
      listElStyle = map doStyles [0..9]
  pn <- paraListStyle l
  return (pn, (l, listElStyle))

orderedListLevelStyle :: ListAttributes -> (Int, [Doc]) -> (Int,[Doc])
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
                    ([ ("text:level"      , show $ 1 + length ls  )
                     , ("text:style-name" , "Numbering_20_Symbols")
                     , ("style:num-format", format                )
                     , ("text:start-value", show s                )
                     ] ++ suffix) (listLevelStyle (1 + length ls))
    in  (l, ls ++ [listStyle])

listLevelStyle :: Int -> Doc
listLevelStyle i =
    let indent = show (0.4 * fromIntegral (i - 1) :: Double) in
    selfClosingTag "style:list-level-properties"
                       [ ("text:space-before"   , indent ++ "in")
                       , ("text:min-label-width",       "0.4in")]

tableStyle :: Int -> [(Char,Double)] -> Doc
tableStyle num wcs =
    let tableId        = "Table" ++ show (num + 1)
        table          = inTags True "style:style"
                         [("style:name", tableId)
                         ,("style:family", "table")] $
                         selfClosingTag "style:table-properties"
                         [("table:align"    , "center")]
        colStyle (c,0) = selfClosingTag "style:style"
                         [ ("style:name"  , tableId ++ "." ++ [c])
                         , ("style:family", "table-column"       )]
        colStyle (c,w) = inTags True "style:style"
                         [ ("style:name"  , tableId ++ "." ++ [c])
                         , ("style:family", "table-column"       )] $
                         selfClosingTag "style:table-column-properties"
                         [("style:rel-column-width", printf "%d*" $ (floor $ w * 65535 :: Integer))]
        cellStyle      = inTags True "style:style"
                         [ ("style:name"  , tableId ++ ".A1")
                         , ("style:family", "table-cell"    )] $
                         selfClosingTag "style:table-cell-properties"
                         [ ("fo:border", "none")]
        columnStyles   = map colStyle wcs
    in  table $$ vcat columnStyles $$ cellStyle

paraStyle :: PandocMonad m => [(String,String)] -> OD m Int
paraStyle attrs = do
  pn <- (+)   1 . length       <$> gets stParaStyles
  i  <- (*) (0.5 :: Double) . fromIntegral <$> gets stIndentPara
  b  <- gets stInDefinition
  t  <- gets stTight
  let styleAttr = [ ("style:name"             , "P" ++ show pn)
                  , ("style:family"           , "paragraph"   )]
      indentVal = flip (++) "in" . show $ if b then (max 0.5 i) else i
      tight     = if t then [ ("fo:margin-top"          , "0in"    )
                            , ("fo:margin-bottom"       , "0in"    )]
                       else []
      indent    = if (i /= 0 || b)
                      then [ ("fo:margin-left"         , indentVal)
                           , ("fo:margin-right"        , "0in"    )
                           , ("fo:text-indent"         , "0in"    )
                           , ("style:auto-text-indent" , "false"  )]
                      else []
      attributes = indent ++ tight
      paraProps = when (not $ null attributes) $
                    selfClosingTag "style:paragraph-properties" attributes
  addParaStyle $ inTags True "style:style" (styleAttr ++ attrs) paraProps
  return pn

paraListStyle :: PandocMonad m => Int -> OD m Int
paraListStyle l = paraStyle
  [("style:parent-style-name","Text_20_body")
  ,("style:list-style-name", "L" ++ show l )]

paraTableStyles :: String -> Int -> [Alignment] -> [(String, Doc)]
paraTableStyles _ _ [] = []
paraTableStyles t s (a:xs)
    | AlignRight  <- a = (         pName s, res s "end"   ) : paraTableStyles t (s + 1) xs
    | AlignCenter <- a = (         pName s, res s "center") : paraTableStyles t (s + 1) xs
    | otherwise        = ("Table_20_" ++ t, empty         ) : paraTableStyles t  s      xs
    where pName sn = "P" ++ show (sn + 1)
          res sn x = inTags True "style:style"
                     [ ("style:name"             , pName sn        )
                     , ("style:family"           , "paragraph"     )
                     , ("style:parent-style-name", "Table_20_" ++ t)] $
                     selfClosingTag "style:paragraph-properties"
                     [ ("fo:text-align", x)
                     , ("style:justify-single-word", "false")]

data TextStyle = Italic
               | Bold
               | Strike
               | Sub
               | Sup
               | SmallC
               | Pre
               | Language Lang
               deriving ( Eq,Ord )

textStyleAttr :: TextStyle -> [(String,String)]
textStyleAttr s
    | Italic <- s = [("fo:font-style"                ,"italic"    )
                    ,("style:font-style-asian"       ,"italic"    )
                    ,("style:font-style-complex"     ,"italic"    )]
    | Bold   <- s = [("fo:font-weight"               ,"bold"      )
                    ,("style:font-weight-asian"      ,"bold"      )
                    ,("style:font-weight-complex"    ,"bold"      )]
    | Strike <- s = [("style:text-line-through-style", "solid"    )]
    | Sub    <- s = [("style:text-position"          ,"sub 58%"   )]
    | Sup    <- s = [("style:text-position"          ,"super 58%" )]
    | SmallC <- s = [("fo:font-variant"              ,"small-caps")]
    | Pre    <- s = [("style:font-name"              ,"Courier New")
                    ,("style:font-name-asian"        ,"Courier New")
                    ,("style:font-name-complex"      ,"Courier New")]
    | Language lang <- s
                  = [("fo:language"                  ,langLanguage lang)
                    ,("fo:country"                   ,langRegion lang)]
    | otherwise   = []

withLangFromAttr :: PandocMonad m => Attr -> OD m a -> OD m a
withLangFromAttr (_,_,kvs) action =
  case lookup "lang" kvs of
       Nothing -> action
       Just l  -> do
         case parseBCP47 l of
              Right lang -> withTextStyle (Language lang) action
              Left _ -> do
                report $ InvalidLang l
                action

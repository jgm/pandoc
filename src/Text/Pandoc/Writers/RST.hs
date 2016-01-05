{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.RST
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to reStructuredText.

reStructuredText:  <http://docutils.sourceforge.net/rst.html>
-}
module Text.Pandoc.Writers.RST ( writeRST ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.ImageSize
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Builder (deleteMeta)
import Data.Maybe (fromMaybe)
import Data.List ( isPrefixOf, stripPrefix, intersperse, transpose )
import Network.URI (isURI)
import Text.Pandoc.Pretty
import Control.Monad.State
import Data.Char (isSpace, toLower)

type Refs = [([Inline], Target)]

data WriterState =
  WriterState { stNotes       :: [[Block]]
              , stLinks       :: Refs
              , stImages      :: [([Inline], (Attr, String, String, Maybe String))]
              , stHasMath     :: Bool
              , stHasRawTeX   :: Bool
              , stOptions     :: WriterOptions
              , stTopLevel    :: Bool
              }

-- | Convert Pandoc to RST.
writeRST :: WriterOptions -> Pandoc -> String
writeRST opts document =
  let st = WriterState { stNotes = [], stLinks = [],
                         stImages = [], stHasMath = False,
                         stHasRawTeX = False, stOptions = opts,
                         stTopLevel = True}
  in evalState (pandocToRST document) st

-- | Return RST representation of document.
pandocToRST :: Pandoc -> State WriterState String
pandocToRST (Pandoc meta blocks) = do
  opts <- liftM stOptions get
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let subtit = case lookupMeta "subtitle" meta of
                    Just (MetaBlocks [Plain xs]) -> xs
                    _ -> []
  title <- titleToRST (docTitle meta) subtit
  metadata <- metaToJSON opts
                (fmap (render colwidth) . blockListToRST)
                (fmap (trimr . render colwidth) . inlineListToRST)
                $ deleteMeta "title" $ deleteMeta "subtitle" meta
  body <- blockListToRST' True $ if writerStandalone opts
                                    then normalizeHeadings 1 blocks
                                    else blocks
  notes <- liftM (reverse . stNotes) get >>= notesToRST
  -- note that the notes may contain refs, so we do them first
  refs <- liftM (reverse . stLinks) get >>= refsToRST
  pics <- liftM (reverse . stImages) get >>= pictRefsToRST
  hasMath <- liftM stHasMath get
  rawTeX <- liftM stHasRawTeX get
  let main = render colwidth $ foldl ($+$) empty $ [body, notes, refs, pics]
  let context = defField "body" main
              $ defField "toc" (writerTableOfContents opts)
              $ defField "toc-depth" (show $ writerTOCDepth opts)
              $ defField "math" hasMath
              $ defField "title" (render Nothing title :: String)
              $ defField "math" hasMath
              $ defField "rawtex" rawTeX
              $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main
  where
    normalizeHeadings lev (Header l a i:bs) =
      Header lev a i:normalizeHeadings (lev+1) cont ++ normalizeHeadings lev bs'
      where (cont,bs') = break (headerLtEq l) bs
            headerLtEq level (Header l' _ _) = l' <= level
            headerLtEq _ _ = False
    normalizeHeadings lev (b:bs) = b:normalizeHeadings lev bs
    normalizeHeadings _   []     = []

-- | Return RST representation of reference key table.
refsToRST :: Refs -> State WriterState Doc
refsToRST refs = mapM keyToRST refs >>= return . vcat

-- | Return RST representation of a reference key.
keyToRST :: ([Inline], (String, String))
         -> State WriterState Doc
keyToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  let label'' = if ':' `elem` ((render Nothing label') :: String)
                   then char '`' <> label' <> char '`'
                   else label'
  return $ nowrap $ ".. _" <> label'' <> ": " <> text src

-- | Return RST representation of notes.
notesToRST :: [[Block]] -> State WriterState Doc
notesToRST notes =
  mapM (\(num, note) -> noteToRST num note) (zip [1..] notes) >>=
  return . vsep

-- | Return RST representation of a note.
noteToRST :: Int -> [Block] -> State WriterState Doc
noteToRST num note = do
  contents <- blockListToRST note
  let marker = ".. [" <> text (show num) <> "]"
  return $ nowrap $ marker $$ nest 3 contents

-- | Return RST representation of picture reference table.
pictRefsToRST :: [([Inline], (Attr, String, String, Maybe String))]
              -> State WriterState Doc
pictRefsToRST refs = mapM pictToRST refs >>= return . vcat

-- | Return RST representation of a picture substitution reference.
pictToRST :: ([Inline], (Attr, String, String, Maybe String))
          -> State WriterState Doc
pictToRST (label, (attr, src, _, mbtarget)) = do
  label' <- inlineListToRST label
  dims   <- imageDimsToRST attr
  let (_, cls, _) = attr
      classes = if null cls
                   then empty
                   else ":class: " <> text (unwords cls)
  return $ nowrap
         $ ".. |" <> label' <> "| image:: " <> text src $$ hang 3 empty (classes $$ dims)
         $$ case mbtarget of
                 Nothing -> empty
                 Just t  -> "   :target: " <> text t

-- | Escape special characters for RST.
escapeString :: String -> String
escapeString = escapeStringUsing (backslashEscapes "`\\|*_")

titleToRST :: [Inline] -> [Inline] -> State WriterState Doc
titleToRST [] _ = return empty
titleToRST tit subtit = do
  title <- inlineListToRST tit
  subtitle <- inlineListToRST subtit
  return $ bordered title '=' $$ bordered subtitle '-'

bordered :: Doc -> Char -> Doc
bordered contents c =
  if len > 0
     then border $$ contents $$ border
     else empty
   where len = offset contents
         border = text (replicate len c)

-- | Convert Pandoc block element to RST.
blockToRST :: Block         -- ^ Block element
           -> State WriterState Doc
blockToRST Null = return empty
blockToRST (Div attr bs) = do
  contents <- blockListToRST bs
  let startTag = ".. raw:: html" $+$ nest 3 (tagWithAttrs "div" attr)
  let endTag = ".. raw:: html" $+$ nest 3 "</div>"
  return $ blankline <> startTag $+$ contents $+$ endTag $$ blankline
blockToRST (Plain inlines) = inlineListToRST inlines
-- title beginning with fig: indicates that the image is a figure
blockToRST (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- inlineListToRST txt
  dims <- imageDimsToRST attr
  let fig = "figure:: " <> text src
      alt = ":alt: " <> if null tit then capt else text tit
      (_,cls,_) = attr
      classes = if null cls
                   then empty
                   else ":figclass: " <> text (unwords cls)
  return $ hang 3 ".. " (fig $$ alt $$ classes $$ dims $+$ capt) $$ blankline
blockToRST (Para inlines)
  | LineBreak `elem` inlines = do -- use line block if LineBreaks
      lns <- mapM inlineListToRST $ splitBy (==LineBreak) inlines
      return $ (vcat $ map (hang 2 (text "| ")) lns) <> blankline
  | otherwise = do
      contents <- inlineListToRST inlines
      return $ contents <> blankline
blockToRST (RawBlock f@(Format f') str)
  | f == "rst" = return $ text str
  | otherwise  = return $ blankline <> ".. raw:: " <>
                    text (map toLower f') $+$
                    (nest 3 $ text str) $$ blankline
blockToRST HorizontalRule =
  return $ blankline $$ "--------------" $$ blankline
blockToRST (Header level (name,classes,_) inlines) = do
  contents <- inlineListToRST inlines
  isTopLevel <- gets stTopLevel
  if isTopLevel
    then do
          let headerChar = if level > 5 then ' ' else "=-~^'" !! (level - 1)
          let border = text $ replicate (offset contents) headerChar
          return $ nowrap $ contents $$ border $$ blankline
    else do
          let rub     = "rubric:: " <> contents
          let name' | null name    = empty
                    | otherwise    = ":name: " <> text name
          let cls   | null classes = empty
                    | otherwise    = ":class: " <> text (unwords classes)
          return $ nowrap $ hang 3 ".. " (rub $$ name' $$ cls) $$ blankline
blockToRST (CodeBlock (_,classes,kvs) str) = do
  opts <- stOptions <$> get
  let tabstop = writerTabStop opts
  let startnum = maybe "" (\x -> " " <> text x) $ lookup "startFrom" kvs
  let numberlines = if "numberLines" `elem` classes
                       then "   :number-lines:" <> startnum
                       else empty
  if "haskell" `elem` classes && "literate" `elem` classes &&
                  isEnabled Ext_literate_haskell opts
     then return $ prefixed "> " (text str) $$ blankline
     else return $
          (case [c | c <- classes,
                     c `notElem` ["sourceCode","literate","numberLines"]] of
             []       -> "::"
             (lang:_) -> (".. code:: " <> text lang) $$ numberlines)
          $+$ nest tabstop (text str) $$ blankline
blockToRST (BlockQuote blocks) = do
  tabstop <- get >>= (return . writerTabStop . stOptions)
  contents <- blockListToRST blocks
  return $ (nest tabstop contents) <> blankline
blockToRST (Table caption _ widths headers rows) =  do
  caption' <- inlineListToRST caption
  let caption'' = if null caption
                     then empty
                     else blankline <> text "Table: " <> caption'
  headers' <- mapM blockListToRST headers
  rawRows <- mapM (mapM blockListToRST) rows
  -- let isSimpleCell [Plain _] = True
  --     isSimpleCell [Para _]  = True
  --     isSimpleCell []        = True
  --     isSimpleCell _         = False
  -- let isSimple = all (==0) widths && all (all isSimpleCell) rows
  let numChars = maximum . map offset
  opts <- get >>= return . stOptions
  let widthsInChars =
       if all (== 0) widths
          then map ((+2) . numChars) $ transpose (headers' : rawRows)
          else map (floor . (fromIntegral (writerColumns opts) *)) widths
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where h      = maximum (1 : map height blocks)
              sep'   = lblock 3 $ vcat (map text $ replicate h " | ")
              beg    = lblock 2 $ vcat (map text $ replicate h "| ")
              end    = lblock 2 $ vcat (map text $ replicate h " |")
              middle = hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow headers'
  let rows' = map makeRow rawRows
  let border ch = char '+' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '+'
  let body = vcat $ intersperse (border '-') rows'
  let head'' = if all null headers
                  then empty
                  else head' $$ border '='
  return $ border '-' $$ head'' $$ body $$ border '-' $$ caption'' $$ blankline
blockToRST (BulletList items) = do
  contents <- mapM bulletListItemToRST items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ chomp (vcat contents) $$ blankline
blockToRST (OrderedList (start, style', delim) items) = do
  let markers = if start == 1 && style' == DefaultStyle && delim == DefaultDelim
                   then take (length items) $ repeat "#."
                   else take (length items) $ orderedListMarkers
                                              (start, style', delim)
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- mapM (\(item, num) -> orderedListItemToRST item num) $
              zip markers' items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ chomp (vcat contents) $$ blankline
blockToRST (DefinitionList items) = do
  contents <- mapM definitionListItemToRST items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ chomp (vcat contents) $$ blankline

-- | Convert bullet list item (list of blocks) to RST.
bulletListItemToRST :: [Block] -> State WriterState Doc
bulletListItemToRST items = do
  contents <- blockListToRST items
  return $ hang 3 "-  " $ contents <> cr

-- | Convert ordered list item (a list of blocks) to RST.
orderedListItemToRST :: String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> State WriterState Doc
orderedListItemToRST marker items = do
  contents <- blockListToRST items
  let marker' = marker ++ " "
  return $ hang (length marker') (text marker') $ contents <> cr

-- | Convert defintion list item (label, list of blocks) to RST.
definitionListItemToRST :: ([Inline], [[Block]]) -> State WriterState Doc
definitionListItemToRST (label, defs) = do
  label' <- inlineListToRST label
  contents <- liftM vcat $ mapM blockListToRST defs
  tabstop <- get >>= (return . writerTabStop . stOptions)
  return $ label' $$ nest tabstop (nestle contents <> cr)

-- | Convert list of Pandoc block elements to RST.
blockListToRST' :: Bool
                -> [Block]       -- ^ List of block elements
                -> State WriterState Doc
blockListToRST' topLevel blocks = do
  tl <- gets stTopLevel
  modify (\s->s{stTopLevel=topLevel})
  res <- vcat `fmap` mapM blockToRST blocks
  modify (\s->s{stTopLevel=tl})
  return res

blockListToRST :: [Block]       -- ^ List of block elements
               -> State WriterState Doc
blockListToRST = blockListToRST' False

-- | Convert list of Pandoc inline elements to RST.
inlineListToRST :: [Inline] -> State WriterState Doc
inlineListToRST lst =
  mapM inlineToRST (removeSpaceAfterDisplayMath $ insertBS lst) >>=
    return . hcat
  where -- remove spaces after displaymath, as they screw up indentation:
        removeSpaceAfterDisplayMath (Math DisplayMath x : zs) =
              Math DisplayMath x : dropWhile (==Space) zs
        removeSpaceAfterDisplayMath (x:xs) = x : removeSpaceAfterDisplayMath xs
        removeSpaceAfterDisplayMath [] = []
        insertBS :: [Inline] -> [Inline] -- insert '\ ' where needed
        insertBS (x:y:z:zs)
          | isComplex y && (surroundComplex x z) =
              x : y : insertBS (z : zs)
        insertBS (x:y:zs)
          | isComplex x && not (okAfterComplex y) =
              x : RawInline "rst" "\\ " : insertBS (y : zs)
          | isComplex y && not (okBeforeComplex x) =
              x : RawInline "rst" "\\ " : insertBS (y : zs)
          | otherwise =
              x : insertBS (y : zs)
        insertBS (x:ys) = x : insertBS ys
        insertBS [] = []
        surroundComplex :: Inline -> Inline -> Bool
        surroundComplex (Str s@(_:_)) (Str s'@(_:_)) =
          case (last s, head s') of
             ('\'','\'') -> True
             ('"','"')   -> True
             ('<','>')   -> True
             ('[',']')   -> True
             ('{','}')   -> True
             _           -> False
        surroundComplex _ _ = False
        okAfterComplex :: Inline -> Bool
        okAfterComplex Space = True
        okAfterComplex SoftBreak = True
        okAfterComplex LineBreak = True
        okAfterComplex (Str (c:_)) = isSpace c || c `elem` ("-.,:;!?\\/'\")]}>–—" :: String)
        okAfterComplex _ = False
        okBeforeComplex :: Inline -> Bool
        okBeforeComplex Space = True
        okBeforeComplex SoftBreak = True
        okBeforeComplex LineBreak = True
        okBeforeComplex (Str (c:_)) = isSpace c || c `elem` ("-:/'\"<([{–—" :: String)
        okBeforeComplex _ = False
        isComplex :: Inline -> Bool
        isComplex (Emph _) = True
        isComplex (Strong _) = True
        isComplex (SmallCaps _) = True
        isComplex (Strikeout _) = True
        isComplex (Superscript _) = True
        isComplex (Subscript _) = True
        isComplex (Link _ _ _) = True
        isComplex (Image _ _ _) = True
        isComplex (Code _ _) = True
        isComplex (Math _ _) = True
        isComplex (Cite _ (x:_)) = isComplex x
        isComplex (Span _ (x:_)) = isComplex x
        isComplex _ = False

-- | Convert Pandoc inline element to RST.
inlineToRST :: Inline -> State WriterState Doc
inlineToRST (Span _ ils) = inlineListToRST ils
inlineToRST (Emph lst) = do
  contents <- inlineListToRST lst
  return $ "*" <> contents <> "*"
inlineToRST (Strong lst) = do
  contents <- inlineListToRST lst
  return $ "**" <> contents <> "**"
inlineToRST (Strikeout lst) = do
  contents <- inlineListToRST lst
  return $ "[STRIKEOUT:" <> contents <> "]"
inlineToRST (Superscript lst) = do
  contents <- inlineListToRST lst
  return $ ":sup:`" <> contents <> "`"
inlineToRST (Subscript lst) = do
  contents <- inlineListToRST lst
  return $ ":sub:`" <> contents <> "`"
inlineToRST (SmallCaps lst) = inlineListToRST lst
inlineToRST (Quoted SingleQuote lst) = do
  contents <- inlineListToRST lst
  return $ "‘" <> contents <> "’"
inlineToRST (Quoted DoubleQuote lst) = do
  contents <- inlineListToRST lst
  return $ "“" <> contents <> "”"
inlineToRST (Cite _  lst) =
  inlineListToRST lst
inlineToRST (Code _ str) = return $ "``" <> text str <> "``"
inlineToRST (Str str) = return $ text $ escapeString str
inlineToRST (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then ":math:`" <> text str <> "`"
              else if '\n' `elem` str
                   then blankline $$ ".. math::" $$
                        blankline $$ nest 3 (text str) $$ blankline
                   else blankline $$ (".. math:: " <> text str) $$ blankline
inlineToRST (RawInline f x)
  | f == "rst" = return $ text x
  | f == "latex" || f == "tex" = do
      modify $ \st -> st{ stHasRawTeX = True }
      return $ ":raw-latex:`" <> text x <> "`"
  | otherwise  = return empty
inlineToRST (LineBreak) = return cr -- there's no line break in RST (see Para)
inlineToRST Space = return space
inlineToRST SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
        WrapPreserve  -> return cr
        WrapAuto      -> return space
        WrapNone      -> return space
-- autolink
inlineToRST (Link _ [Str str] (src, _))
  | isURI src &&
    if "mailto:" `isPrefixOf` src
       then src == escapeURI ("mailto:" ++ str)
       else src == escapeURI str = do
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  return $ text srcSuffix
inlineToRST (Link _ [Image attr alt (imgsrc,imgtit)] (src, _tit)) = do
  label <- registerImage attr alt (imgsrc,imgtit) (Just src)
  return $ "|" <> label <> "|"
inlineToRST (Link _ txt (src, tit)) = do
  useReferenceLinks <- get >>= return . writerReferenceLinks . stOptions
  linktext <- inlineListToRST $ normalizeSpaces txt
  if useReferenceLinks
    then do refs <- get >>= return . stLinks
            case lookup txt refs of
                 Just (src',tit') ->
                   if src == src' && tit == tit'
                      then return $ "`" <> linktext <> "`_"
                      else do -- duplicate label, use non-reference link
                        return $ "`" <> linktext <> " <" <> text src <> ">`__"
                 Nothing -> do
                   modify $ \st -> st { stLinks = (txt,(src,tit)):refs }
                   return $ "`" <> linktext <> "`_"
    else return $ "`" <> linktext <> " <" <> text src <> ">`__"
inlineToRST (Image attr alternate (source, tit)) = do
  label <- registerImage attr alternate (source,tit) Nothing
  return $ "|" <> label <> "|"
inlineToRST (Note contents) = do
  -- add to notes in state
  notes <- gets stNotes
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ (length notes) + 1
  return $ " [" <> text ref <> "]_"

registerImage :: Attr -> [Inline] -> Target -> Maybe String -> State WriterState Doc
registerImage attr alt (src,tit) mbtarget = do
  pics <- get >>= return . stImages
  txt <- case lookup alt pics of
               Just (a,s,t,mbt) | (a,s,t,mbt) == (attr,src,tit,mbtarget)
                 -> return alt
               _ -> do
                 let alt' = if null alt || alt == [Str ""]
                               then [Str $ "image" ++ show (length pics)]
                               else alt
                 modify $ \st -> st { stImages =
                        (alt', (attr,src,tit, mbtarget)):stImages st }
                 return alt'
  inlineListToRST txt

imageDimsToRST :: Attr -> State WriterState Doc
imageDimsToRST attr = do
  let (ident, _, _) = attr
      name = if null ident
                then empty
                else ":name: " <> text ident
      showDim dir = let cols d = ":" <> text (show dir) <> ": " <> text (show d)
                    in  case (dimension dir attr) of
                          Just (Percent a) ->
                            case dir of
                              Height -> empty
                              Width  -> cols (Percent a)
                          Just dim -> cols dim
                          Nothing  -> empty
  return $ cr <> name $$ showDim Width $$ showDim Height

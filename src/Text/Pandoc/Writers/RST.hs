{-# LANGUAGE OverloadedStrings #-}
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
   Module      : Text.Pandoc.Writers.RST
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to reStructuredText.

reStructuredText:  <http://docutils.sourceforge.net/rst.html>
-}
module Text.Pandoc.Writers.RST ( writeRST ) where
import Control.Monad.State.Strict
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text, stripEnd)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Shared

type Refs = [([Inline], Target)]

data WriterState =
  WriterState { stNotes       :: [[Block]]
              , stLinks       :: Refs
              , stImages      :: [([Inline], (Attr, String, String, Maybe String))]
              , stHasMath     :: Bool
              , stHasRawTeX   :: Bool
              , stOptions     :: WriterOptions
              , stTopLevel    :: Bool
              , stLastNested  :: Bool
              }

type RST = StateT WriterState

-- | Convert Pandoc to RST.
writeRST :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeRST opts document = do
  let st = WriterState { stNotes = [], stLinks = [],
                         stImages = [], stHasMath = False,
                         stHasRawTeX = False, stOptions = opts,
                         stTopLevel = True, stLastNested = False}
  evalStateT (pandocToRST document) st

-- | Return RST representation of document.
pandocToRST :: PandocMonad m => Pandoc -> RST m Text
pandocToRST (Pandoc meta blocks) = do
  opts <- gets stOptions
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' :: Doc -> Text
      render' = render colwidth
  let subtit = case lookupMeta "subtitle" meta of
                    Just (MetaBlocks [Plain xs]) -> xs
                    _                            -> []
  title <- titleToRST (docTitle meta) subtit
  metadata <- metaToJSON opts
                (fmap render' . blockListToRST)
                (fmap (stripEnd . render') . inlineListToRST)
                $ B.deleteMeta "title" $ B.deleteMeta "subtitle" meta
  body <- blockListToRST' True $ case writerTemplate opts of
                                      Just _  -> normalizeHeadings 1 blocks
                                      Nothing -> blocks
  notes <- gets (reverse . stNotes) >>= notesToRST
  -- note that the notes may contain refs, so we do them first
  refs <- gets (reverse . stLinks) >>= refsToRST
  pics <- gets (reverse . stImages) >>= pictRefsToRST
  hasMath <- gets stHasMath
  rawTeX <- gets stHasRawTeX
  let main = render' $ foldl ($+$) empty $ [body, notes, refs, pics]
  let context = defField "body" main
              $ defField "toc" (writerTableOfContents opts)
              $ defField "toc-depth" (show $ writerTOCDepth opts)
              $ defField "math" hasMath
              $ defField "title" (render Nothing title :: String)
              $ defField "math" hasMath
              $ defField "rawtex" rawTeX
              $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context
  where
    normalizeHeadings lev (Header l a i:bs) =
      Header lev a i:normalizeHeadings (lev+1) cont ++ normalizeHeadings lev bs'
      where (cont,bs') = break (headerLtEq l) bs
            headerLtEq level (Header l' _ _) = l' <= level
            headerLtEq _ _                   = False
    normalizeHeadings lev (b:bs) = b:normalizeHeadings lev bs
    normalizeHeadings _   []     = []

-- | Return RST representation of reference key table.
refsToRST :: PandocMonad m => Refs -> RST m Doc
refsToRST refs = mapM keyToRST refs >>= return . vcat

-- | Return RST representation of a reference key.
keyToRST :: PandocMonad m => ([Inline], (String, String)) -> RST m Doc
keyToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  let label'' = if ':' `elem` ((render Nothing label') :: String)
                   then char '`' <> label' <> char '`'
                   else label'
  return $ nowrap $ ".. _" <> label'' <> ": " <> text src

-- | Return RST representation of notes.
notesToRST :: PandocMonad m => [[Block]] -> RST m Doc
notesToRST notes =
  mapM (\(num, note) -> noteToRST num note) (zip [1..] notes) >>=
  return . vsep

-- | Return RST representation of a note.
noteToRST :: PandocMonad m => Int -> [Block] -> RST m Doc
noteToRST num note = do
  contents <- blockListToRST note
  let marker = ".. [" <> text (show num) <> "]"
  return $ nowrap $ marker $$ nest 3 contents

-- | Return RST representation of picture reference table.
pictRefsToRST :: PandocMonad m
              => [([Inline], (Attr, String, String, Maybe String))]
              -> RST m Doc
pictRefsToRST refs = mapM pictToRST refs >>= return . vcat

-- | Return RST representation of a picture substitution reference.
pictToRST :: PandocMonad m
          => ([Inline], (Attr, String, String, Maybe String))
          -> RST m Doc
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
escapeString :: WriterOptions -> String -> String
escapeString _  [] = []
escapeString opts (c:cs) =
  case c of
       _ | c `elem` ['\\','`','*','_','|'] -> '\\':c:escapeString opts cs
       '\'' | isEnabled Ext_smart opts -> '\\':'\'':escapeString opts cs
       '"' | isEnabled Ext_smart opts -> '\\':'"':escapeString opts cs
       '-' | isEnabled Ext_smart opts ->
              case cs of
                   '-':_ -> '\\':'-':escapeString opts cs
                   _     -> '-':escapeString opts cs
       '.' | isEnabled Ext_smart opts ->
              case cs of
                   '.':'.':rest -> '\\':'.':'.':'.':escapeString opts rest
                   _            -> '.':escapeString opts cs
       _ -> c : escapeString opts cs

titleToRST :: PandocMonad m => [Inline] -> [Inline] -> RST m Doc
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
blockToRST :: PandocMonad m
           => Block         -- ^ Block element
           -> RST m Doc
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
      linesToLineBlock $ splitBy (==LineBreak) inlines
  | otherwise = do
      contents <- inlineListToRST inlines
      return $ contents <> blankline
blockToRST (LineBlock lns) =
  linesToLineBlock lns
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
  opts <- gets stOptions
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
  tabstop <- gets $ writerTabStop . stOptions
  contents <- blockListToRST blocks
  return $ (nest tabstop contents) <> blankline
blockToRST (Table caption aligns widths headers rows) = do
  caption' <- inlineListToRST caption
  let blocksToDoc opts bs = do
         oldOpts <- gets stOptions
         modify $ \st -> st{ stOptions = opts }
         result <- blockListToRST bs
         modify $ \st -> st{ stOptions = oldOpts }
         return result
  opts <- gets stOptions
  tbl <- gridTable opts blocksToDoc (all null headers)
            (map (const AlignDefault) aligns) widths
            headers rows
  return $ if null caption
              then tbl $$ blankline
              else (".. table:: " <> caption') $$ blankline $$ nest 3 tbl $$
                   blankline
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
bulletListItemToRST :: PandocMonad m => [Block] -> RST m Doc
bulletListItemToRST items = do
  contents <- blockListToRST items
  return $ hang 3 "-  " $ contents <> cr

-- | Convert ordered list item (a list of blocks) to RST.
orderedListItemToRST :: PandocMonad m
                     => String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> RST m Doc
orderedListItemToRST marker items = do
  contents <- blockListToRST items
  let marker' = marker ++ " "
  return $ hang (length marker') (text marker') $ contents <> cr

-- | Convert defintion list item (label, list of blocks) to RST.
definitionListItemToRST :: PandocMonad m => ([Inline], [[Block]]) -> RST m Doc
definitionListItemToRST (label, defs) = do
  label' <- inlineListToRST label
  contents <- liftM vcat $ mapM blockListToRST defs
  tabstop <- gets $ writerTabStop . stOptions
  return $ label' $$ nest tabstop (nestle contents <> cr)

-- | Format a list of lines as line block.
linesToLineBlock :: PandocMonad m => [[Inline]] -> RST m Doc
linesToLineBlock inlineLines = do
  lns <- mapM inlineListToRST inlineLines
  return $ (vcat $ map (hang 2 (text "| ")) lns) <> blankline

-- | Convert list of Pandoc block elements to RST.
blockListToRST' :: PandocMonad m
                => Bool
                -> [Block]       -- ^ List of block elements
                -> RST m Doc
blockListToRST' topLevel blocks = do
  tl <- gets stTopLevel
  modify (\s->s{stTopLevel=topLevel, stLastNested=False})
  res <- vcat `fmap` mapM blockToRST' blocks
  modify (\s->s{stTopLevel=tl})
  return res

blockToRST' :: PandocMonad m => Block -> RST m Doc
blockToRST' (x@BlockQuote{}) = do
  lastNested <- gets stLastNested
  res <- blockToRST x
  modify (\s -> s{stLastNested = True})
  return $ if lastNested
              then ".." $+$ res
              else res
blockToRST' x = do
  modify (\s -> s{stLastNested =
    case x of
         Para [Image _ _ (_,'f':'i':'g':':':_)] -> True
         Para{} -> False
         Plain{} -> False
         Header{} -> False
         LineBlock{} -> False
         HorizontalRule -> False
         _ -> True
    })
  blockToRST x

blockListToRST :: PandocMonad m
               => [Block]       -- ^ List of block elements
               -> RST m Doc
blockListToRST = blockListToRST' False

-- | Convert list of Pandoc inline elements to RST.
inlineListToRST :: PandocMonad m => [Inline] -> RST m Doc
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
        isComplex (Emph _)        = True
        isComplex (Strong _)      = True
        isComplex (SmallCaps _)   = True
        isComplex (Strikeout _)   = True
        isComplex (Superscript _) = True
        isComplex (Subscript _)   = True
        isComplex (Link _ _ _)    = True
        isComplex (Image _ _ _)   = True
        isComplex (Code _ _)      = True
        isComplex (Math _ _)      = True
        isComplex (Cite _ (x:_))  = isComplex x
        isComplex (Span _ (x:_))  = isComplex x
        isComplex _               = False

-- | Convert Pandoc inline element to RST.
inlineToRST :: PandocMonad m => Inline -> RST m Doc
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
  opts <- gets stOptions
  if isEnabled Ext_smart opts
     then return $ "'" <> contents <> "'"
     else return $ "‘" <> contents <> "’"
inlineToRST (Quoted DoubleQuote lst) = do
  contents <- inlineListToRST lst
  opts <- gets stOptions
  if isEnabled Ext_smart opts
     then return $ "\"" <> contents <> "\""
     else return $ "“" <> contents <> "”"
inlineToRST (Cite _  lst) =
  inlineListToRST lst
inlineToRST (Code _ str) =
  -- we trim the string because the delimiters must adjoin a
  -- non-space character; see #3496
  return $ "``" <> text (trim str) <> "``"
inlineToRST (Str str) = do
  opts <- gets stOptions
  return $ text $
    (if isEnabled Ext_smart opts
        then unsmartify opts
        else id) $ escapeString opts str
inlineToRST (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then ":math:`" <> text str <> "`"
              else if '\n' `elem` str
                   then blankline $$ ".. math::" $$
                        blankline $$ nest 3 (text str) $$ blankline
                   else blankline $$ (".. math:: " <> text str) $$ blankline
inlineToRST il@(RawInline f x)
  | f == "rst" = return $ text x
  | f == "latex" || f == "tex" = do
      modify $ \st -> st{ stHasRawTeX = True }
      return $ ":raw-latex:`" <> text x <> "`"
  | otherwise  = empty <$ report (InlineNotRendered il)
inlineToRST (LineBreak) = return cr -- there's no line break in RST (see Para)
inlineToRST Space = return space
inlineToRST SoftBreak = do
  wrapText <- gets $ writerWrapText . stOptions
  case wrapText of
        WrapPreserve -> return cr
        WrapAuto     -> return space
        WrapNone     -> return space
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
  useReferenceLinks <- gets $ writerReferenceLinks . stOptions
  linktext <- inlineListToRST $ B.toList . B.trimInlines . B.fromList $ txt
  if useReferenceLinks
    then do refs <- gets stLinks
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

registerImage :: PandocMonad m => Attr -> [Inline] -> Target -> Maybe String -> RST m Doc
registerImage attr alt (src,tit) mbtarget = do
  pics <- gets stImages
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

imageDimsToRST :: PandocMonad m => Attr -> RST m Doc
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

{-
Copyright (C) 2007-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Ms
   Copyright   : Copyright (C) 2007-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to groff man page format.

TODO:

[ ] warning for non-rendered raw content
[ ] is there a better way to do strikeout?
[ ] options for hyperlink rendering (currently footnote)
[ ] can we get prettier output using .B, etc. instead of
    the inline forms?
[ ] tight/loose list distinction
[ ] internal hyperlinks (this seems to be possible since
    they exist in the groff manual PDF version)
[ ] better handling of accented characters and other non-ascii
    characters (e.g. curly quotes).
    Note:  recent versions of groff (more recent than standard
    on many systems) include a -k option which runs preconv.
    preconv basically converts non-ascii characters
    to \[uXXXX] entities.  Since we can't assume that the local
    groff has the -k option, we could have any invocation of
    groff in Text.Pandoc.PDF filter the input through a Haskell
    function that does what preconv does.
    On the other hand:  only recent groffs have -Tpdf.  so
    if we want compatibility with older groffs, we need to to
    -Tps and pipe through ps2pdf (can we assume it's available?).
    A big advantage of gropdf:  it supports the tag
    \X'pdf: pdfpic file alignment width height line-length'
    and also seems to support bookmarks.
    See also the pdfroff shell script that comes with more
    recent versions of groff.
[ ] add via groff option to PDF module
[ ] better handling of images, perhaps converting to eps when
    going to PDF?
[ ] better template, with configurable page number, table of contents,
    columns, etc.
-}

module Text.Pandoc.Writers.Ms ( writeMs ) where
import Text.Pandoc.Definition
import Text.Pandoc.Templates
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Writers.Math
import Text.Printf ( printf )
import qualified Data.Map as Map
import Data.List ( stripPrefix, intersperse, intercalate, sort )
import Data.Maybe (fromMaybe)
import Text.Pandoc.Pretty
import Text.Pandoc.Builder (deleteMeta)
import Text.Pandoc.Class (PandocMonad)
import Control.Monad.State
import Data.Char ( isDigit, isLower, isUpper, toUpper )
import Text.TeXMath (writeEqn)

data WriterState = WriterState { stHasInlineMath :: Bool
                               , stNotes         :: [Note]
                               , stInNote        :: Bool
                               , stSmallCaps     :: Bool
                               , stFontFeatures  :: Map.Map Char Bool
                               }

defaultWriterState :: WriterState
defaultWriterState = WriterState{ stHasInlineMath = False
                                , stNotes         = []
                                , stInNote        = False
                                , stSmallCaps     = False
                                , stFontFeatures  = Map.fromList [
                                                       ('I',False)
                                                     , ('B',False)
                                                     , ('C',False)
                                                     ]
                                }

type Note = [Block]

type MS = StateT WriterState

-- | Convert Pandoc to Ms.
writeMs :: PandocMonad m => WriterOptions -> Pandoc -> m String
writeMs opts document =
  evalStateT (pandocToMs opts document) defaultWriterState

-- | Return groff man representation of document.
pandocToMs :: PandocMonad m => WriterOptions -> Pandoc -> MS m String
pandocToMs opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' = render colwidth
  titleText <- inlineListToMs' opts $ docTitle meta
  let title' = render' titleText
  let setFieldsFromTitle =
       case break (== ' ') title' of
           (cmdName, rest) -> case reverse cmdName of
                                   (')':d:'(':xs) | isDigit d ->
                                     defField "title" (reverse xs) .
                                     defField "section" [d] .
                                     case splitBy (=='|') rest of
                                          (ft:hds) ->
                                            defField "footer" (trim ft) .
                                            defField "header"
                                               (trim $ concat hds)
                                          [] -> id
                                   _  -> defField "title" title'
  metadata <- metaToJSON opts
              (fmap (render colwidth) . blockListToMs opts)
              (fmap (render colwidth) . inlineListToMs' opts)
              $ deleteMeta "title" meta
  body <- blockListToMs opts blocks
  let main = render' body
  hasInlineMath <- gets stHasInlineMath
  let context = defField "body" main
              $ setFieldsFromTitle
              $ defField "has-inline-math" hasInlineMath
              $ defField "hyphenate" True
              $ defField "pandoc-version" pandocVersion
              $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> return $ renderTemplate' tpl context

-- | Association list of characters to escape.
manEscapes :: Map.Map Char String
manEscapes = Map.fromList $
              [ ('\160', "\\ ")
              , ('\'', "\\[aq]")
              , ('’', "'")
              , ('"', "\\\"")
              , ('\x2014', "\\[em]")
              , ('\x2013', "\\[en]")
              , ('\x2026', "\\&...")
              , ('|', "\\[u007C]")  -- because we use | for inline math
              , ('-', "\\-")
              , ('@', "\\@")
              , ('\\', "\\\\")
              ]

escapeChar :: Char -> String
escapeChar c = case Map.lookup c manEscapes of
                    Just s -> s
                    Nothing -> [c]

-- | Escape | character, used to mark inline math, inside math.
escapeBar :: String -> String
escapeBar = concatMap go
  where go '|' = "\\[u007C]"
        go c   = [c]

-- | Escape special characters for Ms.
escapeString :: String -> String
escapeString = concatMap escapeChar

toSmallCaps :: String -> String
toSmallCaps [] = []
toSmallCaps (c:cs)
  | isLower c = let (lowers,rest) = span isLower (c:cs)
                in  "\\s-2" ++ escapeString (map toUpper lowers) ++
                    "\\s0" ++ toSmallCaps rest
  | isUpper c = let (uppers,rest) = span isUpper (c:cs)
                in  escapeString uppers ++ toSmallCaps rest
  | otherwise = escapeChar c ++ toSmallCaps cs

-- | Escape a literal (code) section for Ms.
escapeCode :: String -> String
escapeCode = concat . intersperse "\n" . map escapeLine . lines
  where escapeCodeChar ' ' = "\\ "
        escapeCodeChar '\t' = "\\\t"
        escapeCodeChar c = escapeChar c
        escapeLine codeline =
          case concatMap escapeCodeChar codeline of
            a@('.':_) -> "\\&" ++ a
            b       -> b

-- We split inline lists into sentences, and print one sentence per
-- line.  groff/troff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

-- | Returns the first sentence in a list of inlines, and the rest.
breakSentence :: [Inline] -> ([Inline], [Inline])
breakSentence [] = ([],[])
breakSentence xs =
  let isSentenceEndInline (Str ys@(_:_)) | last ys == '.' = True
      isSentenceEndInline (Str ys@(_:_)) | last ys == '?' = True
      isSentenceEndInline (LineBreak) = True
      isSentenceEndInline _         = False
      (as, bs) = break isSentenceEndInline xs
  in  case bs of
           []             -> (as, [])
           [c]            -> (as ++ [c], [])
           (c:Space:cs)   -> (as ++ [c], cs)
           (c:SoftBreak:cs) -> (as ++ [c], cs)
           (Str ".":Str (')':ys):cs) -> (as ++ [Str ".", Str (')':ys)], cs)
           (x@(Str ('.':')':_)):cs) -> (as ++ [x], cs)
           (LineBreak:x@(Str ('.':_)):cs) -> (as ++[LineBreak], x:cs)
           (c:cs)         -> (as ++ [c] ++ ds, es)
              where (ds, es) = breakSentence cs

-- | Split a list of inlines into sentences.
splitSentences :: [Inline] -> [[Inline]]
splitSentences xs =
  let (sent, rest) = breakSentence xs
  in  if null rest then [sent] else sent : splitSentences rest

blockToMs :: PandocMonad m
          => WriterOptions -- ^ Options
          -> Block         -- ^ Block element
          -> MS m Doc
blockToMs _ Null = return empty
blockToMs opts (Div _ bs) = blockListToMs opts bs
blockToMs opts (Plain inlines) =
  liftM vcat $ mapM (inlineListToMs' opts) $ splitSentences inlines
blockToMs opts (Para inlines) = do
  contents <- liftM vcat $ mapM (inlineListToMs' opts) $
    splitSentences inlines
  return $ text ".LP" $$ contents
blockToMs _ (RawBlock f str)
  | f == Format "man" = return $ text str
  | otherwise         = return empty
blockToMs _ HorizontalRule = return $ text ".PP" $$ text "   *   *   *   *   *"
blockToMs opts (Header level _ inlines) = do
  contents <- inlineListToMs' opts inlines
  let heading = if writerNumberSections opts
                   then ".NH"
                   else ".SH"
  return $ text heading <> space <> text (show level) $$ contents
blockToMs _ (CodeBlock _ str) = return $
  text ".IP" $$
  text ".nf" $$
  text "\\f[C]" $$
  text (escapeCode str) $$
  text "\\f[]" $$
  text ".fi"
blockToMs opts (LineBlock ls) = do
  blockToMs opts $ Para $ intercalate [LineBreak] ls
blockToMs opts (BlockQuote blocks) = do
  contents <- blockListToMs opts blocks
  return $ text ".RS" $$ contents $$ text ".RE"
blockToMs opts (Table caption alignments widths headers rows) =
  let aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMs' opts caption
  let iwidths = if all (== 0) widths
                   then repeat ""
                   else map (printf "w(%0.1fn)" . (70 *)) widths
  -- 78n default width - 8n indent = 70n
  let coldescriptions = text $ intercalate " "
                        (zipWith (\align width -> aligncode align ++ width)
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMs opts) headers
  let makeRow cols = text "T{" $$
                     (vcat $ intersperse (text "T}@T{") cols) $$
                     text "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (blockListToMs opts) row
                         return $ makeRow cols) rows
  return $ text ".PP" $$ caption' $$
           text ".TS" $$ text "tab(@);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ text ".TE"

blockToMs opts (BulletList items) = do
  contents <- mapM (bulletListItemToMs opts) items
  return (vcat contents)
blockToMs opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 1 + (maximum $ map length markers)
  contents <- mapM (\(num, item) -> orderedListItemToMs opts num indent item) $
              zip markers items
  return (vcat contents)
blockToMs opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMs opts) items
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to man.
bulletListItemToMs :: PandocMonad m => WriterOptions -> [Block] -> MS m Doc
bulletListItemToMs _ [] = return empty
bulletListItemToMs opts ((Para first):rest) =
  bulletListItemToMs opts ((Plain first):rest)
bulletListItemToMs opts ((Plain first):rest) = do
  first' <- blockToMs opts (Plain first)
  rest' <- blockListToMs opts rest
  let first'' = text ".IP \\[bu] 2" $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 2" $$ rest' $$ text ".RE"
  return (first'' $$ rest'')
bulletListItemToMs opts (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  return $ text "\\[bu] .RS 2" $$ first' $$ rest' $$ text ".RE"

-- | Convert ordered list item (a list of blocks) to man.
orderedListItemToMs :: PandocMonad m
                    => WriterOptions -- ^ options
                    -> String   -- ^ order marker for list item
                    -> Int      -- ^ number of spaces to indent
                    -> [Block]  -- ^ list item (list of blocks)
                    -> MS m Doc
orderedListItemToMs _ _ _ [] = return empty
orderedListItemToMs opts num indent ((Para first):rest) =
  orderedListItemToMs opts num indent ((Plain first):rest)
orderedListItemToMs opts num indent (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = text (".IP \"" ++ num' ++ "\" " ++ show indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 4" $$ rest' $$ text ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to man.
definitionListItemToMs :: PandocMonad m
                       => WriterOptions
                       -> ([Inline],[[Block]])
                       -> MS m Doc
definitionListItemToMs opts (label, defs) = do
  labelText <- inlineListToMs' opts label
  contents <- if null defs
                 then return empty
                 else liftM vcat $ forM defs $ \blocks -> do
                        let (first, rest) = case blocks of
                              ((Para x):y) -> (Plain x,y)
                              (x:y)        -> (x,y)
                              []           -> error "blocks is null"
                        rest' <- liftM vcat $
                                  mapM (\item -> blockToMs opts item) rest
                        first' <- blockToMs opts first
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ nowrap (text ".IP \"" <> labelText <> text "\"") $$ contents

-- | Convert list of Pandoc block elements to man.
blockListToMs :: PandocMonad m
              => WriterOptions -- ^ Options
              -> [Block]       -- ^ List of block elements
              -> MS m Doc
blockListToMs opts blocks =
  mapM (blockToMs opts) blocks >>= (return . vcat)

-- | Convert list of Pandoc inline elements to ms.
inlineListToMs :: PandocMonad m => WriterOptions -> [Inline] -> MS m Doc
-- if list starts with ., insert a zero-width character \& so it
-- won't be interpreted as markup if it falls at the beginning of a line.
inlineListToMs opts lst@(Str ('.':_) : _) = mapM (inlineToMs opts) lst >>=
  (return . (text "\\&" <>)  . hcat)
inlineListToMs opts lst = hcat <$> mapM (inlineToMs opts) lst

-- This version to be used when there is no further inline content;
-- forces a note at the end.
inlineListToMs' :: PandocMonad m => WriterOptions -> [Inline] -> MS m Doc
inlineListToMs' opts lst = do
  x <- hcat <$> mapM (inlineToMs opts) lst
  y <- handleNotes opts empty
  return $ x <> y

-- | Convert Pandoc inline element to man.
inlineToMs :: PandocMonad m => WriterOptions -> Inline -> MS m Doc
inlineToMs opts (Span _ ils) = inlineListToMs opts ils
inlineToMs opts (Emph lst) =
  withFontFeature 'I' (inlineListToMs opts lst)
inlineToMs opts (Strong lst) =
  withFontFeature 'B' (inlineListToMs opts lst)
inlineToMs opts (Strikeout lst) = do
  contents <- inlineListToMs opts lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
inlineToMs opts (Superscript lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\*{" <> contents <> text "\\*}"
inlineToMs opts (Subscript lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\*<" <> contents <> text "\\*>"
inlineToMs opts (SmallCaps lst) = do
  -- see https://lists.gnu.org/archive/html/groff/2015-01/msg00016.html
  modify $ \st -> st{ stSmallCaps = not (stSmallCaps st) }
  res <- inlineListToMs opts lst
  modify $ \st -> st{ stSmallCaps = not (stSmallCaps st) }
  return res
inlineToMs opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMs opts lst
  return $ char '`' <> contents <> char '\''
inlineToMs opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMs opts lst
  return $ text "\\[lq]" <> contents <> text "\\[rq]"
inlineToMs opts (Cite _ lst) =
  inlineListToMs opts lst
inlineToMs _ (Code _ str) =
  withFontFeature 'C' (return $ text $ escapeCode str)
inlineToMs _ (Str str) = do
  smallcaps <- gets stSmallCaps
  if smallcaps
     then return $ text $ toSmallCaps str
     else return $ text $ escapeString str
inlineToMs opts (Math InlineMath str) = do
  modify $ \st -> st{ stHasInlineMath = True }
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> inlineToMs opts il
       Right r -> return $ text "|" <> text (escapeBar r) <> text "|"
inlineToMs opts (Math DisplayMath str) = do
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> do
         contents <- inlineToMs opts il
         return $ cr <> text ".RS" $$ contents $$ text ".RE"
       Right r -> return $
            cr <> text ".EQ" $$ text (escapeBar r) $$ text ".EN"
inlineToMs _ (RawInline f str)
  | f == Format "man" = return $ text str
  | otherwise         = return empty
inlineToMs _ (LineBreak) = return $ cr <> text ".br" <> cr
inlineToMs opts SoftBreak = handleNotes opts cr
inlineToMs opts Space = handleNotes opts space
inlineToMs opts (Link _ txt (src, _)) = do
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  inNote <- gets stInNote
  case txt of
       [Str s]
         | escapeURI s == srcSuffix ->
             return $ text (escapeString srcSuffix)
       _ | inNote -> do
         -- avoid a note in a note!
         contents <- inlineListToMs opts txt
         return $ contents <> space <> char '(' <>
                       text (escapeString src) <> char ')'
         | otherwise -> do
         let linknote = [Plain [Str src]]
         inlineListToMs opts (txt ++ [Note linknote])
inlineToMs opts (Image attr alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) ||
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMs opts (Link attr txt (source, tit))
  return $ char '[' <> text "IMAGE: " <> linkPart <> char ']'
inlineToMs _ (Note contents) = do
  modify $ \st -> st{ stNotes = contents : stNotes st }
  return $ text "\\**"

handleNotes :: PandocMonad m => WriterOptions -> Doc -> MS m Doc
handleNotes opts fallback = do
  notes <- gets stNotes
  if null notes
     then return fallback
     else do
       modify $ \st -> st{ stNotes = [], stInNote = True }
       res <- vcat <$> mapM (handleNote opts) notes
       modify $ \st -> st{ stInNote = False }
       return res

handleNote :: PandocMonad m => WriterOptions -> Note -> MS m Doc
handleNote opts bs = do
  -- don't start with Paragraph or we'll get a spurious blank
  -- line after the note ref:
  let bs' = case bs of
                 (Para ils : rest) -> Plain ils : rest
                 _ -> bs
  contents <- blockListToMs opts bs'
  return $ cr <> text ".FS" $$ contents $$ text ".FE" <> cr

fontChange :: PandocMonad m => MS m Doc
fontChange = do
  features <- gets stFontFeatures
  let filling = sort [c | (c,True) <- Map.toList features]
  return $ text $ "\\f[" ++ filling ++ "]"

withFontFeature :: PandocMonad m => Char -> MS m Doc -> MS m Doc
withFontFeature c action = do
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  begin <- fontChange
  d <- action
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  end <- fontChange
  return $ begin <> d <> end

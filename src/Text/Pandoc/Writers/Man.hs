{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.Man
   Copyright   : Copyright (C) 2007-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to roff man page format.

-}
module Text.Pandoc.Writers.Man ( writeMan ) where
import Control.Monad.State.Strict
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder (deleteMeta)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Writers.Roff
import Text.Printf (printf)

-- | Convert Pandoc to Man.
writeMan :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMan opts document =
  evalStateT (pandocToMan opts document) defaultWriterState

-- | Return roff man representation of document.
pandocToMan :: PandocMonad m => WriterOptions -> Pandoc -> StateT WriterState m Text
pandocToMan opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  titleText <- inlineListToMan opts $ docTitle meta
  let title' = render Nothing titleText
  let setFieldsFromTitle =
       case T.break (== ' ') title' of
           (cmdName, rest) -> case T.break (=='(') cmdName of
                                   (xs, ys) | "(" `T.isPrefixOf` ys
                                                && ")" `T.isSuffixOf` ys ->
                                     defField "title" xs .
                                     defField "section" (T.init $ T.drop 1 ys) .
                                     case T.splitOn "|" rest of
                                          (ft:hds) ->
                                            defField "footer" (T.strip ft) .
                                            defField "header"
                                               (T.strip $ mconcat hds)
                                          [] -> id
                                   _  -> defField "title" title'
  metadata <- metaToContext opts
              (blockListToMan opts)
              (fmap chomp . inlineListToMan opts)
              $ deleteMeta "title" meta
  body <- blockListToMan opts blocks
  notes <- gets stNotes
  notes' <- notesToMan opts (reverse notes)
  let main = body $$ notes' $$ text ""
  hasTables <- gets stHasTables
  let context = defField "body" main
              $ setFieldsFromTitle
              $ defField "has-tables" hasTables
              $ defField "hyphenate" True
              $ defField "pandoc-version" pandocVersion metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

escString :: WriterOptions -> Text -> Text
escString _ = escapeString AsciiOnly -- for better portability

-- | Return man representation of notes.
notesToMan :: PandocMonad m => WriterOptions -> [[Block]] -> StateT WriterState m (Doc Text)
notesToMan opts notes =
  if null notes
     then return empty
     else (text ".SH NOTES" $$) . vcat <$> zipWithM (noteToMan opts) [1..] notes

-- | Return man representation of a note.
noteToMan :: PandocMonad m => WriterOptions -> Int -> [Block] -> StateT WriterState m (Doc Text)
noteToMan opts num note = do
  contents <- blockListToMan opts note
  let marker = cr <> text ".SS " <> brackets (text (show num))
  return $ marker $$ contents

-- We split inline lists into sentences, and print one sentence per
-- line.  roff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

-- | Convert Pandoc block element to man.
blockToMan :: PandocMonad m
           => WriterOptions -- ^ Options
           -> Block         -- ^ Block element
           -> StateT WriterState m (Doc Text)
blockToMan _ Null = return empty
blockToMan opts (Div _ bs) = blockListToMan opts bs
blockToMan opts (Plain inlines) =
  liftM vcat $ mapM (inlineListToMan opts) $ splitSentences inlines
blockToMan opts (Para inlines) = do
  contents <- liftM vcat $ mapM (inlineListToMan opts) $
    splitSentences inlines
  return $ text ".PP" $$ contents
blockToMan opts (LineBlock lns) =
  blockToMan opts $ linesToPara lns
blockToMan _ b@(RawBlock f str)
  | f == Format "man" = return $ literal str
  | otherwise         = do
      report $ BlockNotRendered b
      return empty
blockToMan _ HorizontalRule = return $ literal ".PP" $$ literal "   *   *   *   *   *"
blockToMan opts (Header level _ inlines) = do
  contents <- inlineListToMan opts inlines
  let heading = case level of
                  1 -> ".SH "
                  _ -> ".SS "
  return $ nowrap $ literal heading <> contents
blockToMan opts (CodeBlock _ str) = return $
  literal ".IP" $$
  literal ".nf" $$
  literal "\\f[C]" $$
  ((case T.uncons str of
    Just ('.',_) -> literal "\\&"
    _            -> mempty) <>
   literal (escString opts str)) $$
  literal "\\f[R]" $$
  literal ".fi"
blockToMan opts (BlockQuote blocks) = do
  contents <- blockListToMan opts blocks
  return $ literal ".RS" $$ contents $$ literal ".RE"
blockToMan opts (Table _ blkCapt specs thead tbody tfoot) =
  let (caption, alignments, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
      aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMan opts caption
  modify $ \st -> st{ stHasTables = True }
  let iwidths = if all (== 0) widths
                   then repeat ""
                   else map (T.pack . printf "w(%0.1fn)" . (70 *)) widths
  -- 78n default width - 8n indent = 70n
  let coldescriptions = literal $ T.unwords
                        (zipWith (\align width -> aligncode align <> width)
                        alignments iwidths) <> "."
  colheadings <- mapM (blockListToMan opts) headers
  let makeRow cols = literal "T{" $$
                     vcat (intersperse (literal "T}@T{") cols) $$
                     literal "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (blockListToMan opts) row
                         return $ makeRow cols) rows
  return $ literal ".PP" $$ caption' $$
           literal ".TS" $$ literal "tab(@);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ literal ".TE"

blockToMan opts (BulletList items) = do
  contents <- mapM (bulletListItemToMan opts) items
  return (vcat contents)
blockToMan opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 1 +
                     maximum (map T.length markers)
  contents <- mapM (\(num, item) -> orderedListItemToMan opts num indent item) $
              zip markers items
  return (vcat contents)
blockToMan opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMan opts) items
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to man.
bulletListItemToMan :: PandocMonad m => WriterOptions -> [Block] -> StateT WriterState m (Doc Text)
bulletListItemToMan _ [] = return empty
bulletListItemToMan opts (Para first:rest) =
  bulletListItemToMan opts (Plain first:rest)
bulletListItemToMan opts (Plain first:rest) = do
  first' <- blockToMan opts (Plain first)
  rest' <- blockListToMan opts rest
  let first'' = literal ".IP \\[bu] 2" $$ first'
  let rest''  = if null rest
                   then empty
                   else literal ".RS 2" $$ rest' $$ literal ".RE"
  return (first'' $$ rest'')
bulletListItemToMan opts (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  return $ literal "\\[bu] .RS 2" $$ first' $$ rest' $$ literal ".RE"

-- | Convert ordered list item (a list of blocks) to man.
orderedListItemToMan :: PandocMonad m
                     => WriterOptions -- ^ options
                     -> Text   -- ^ order marker for list item
                     -> Int      -- ^ number of spaces to indent
                     -> [Block]  -- ^ list item (list of blocks)
                     -> StateT WriterState m (Doc Text)
orderedListItemToMan _ _ _ [] = return empty
orderedListItemToMan opts num indent (Para first:rest) =
  orderedListItemToMan opts num indent (Plain first:rest)
orderedListItemToMan opts num indent (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = literal (".IP \"" <> T.pack num' <> "\" " <> tshow indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else literal ".RS 4" $$ rest' $$ literal ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to man.
definitionListItemToMan :: PandocMonad m
                        => WriterOptions
                        -> ([Inline],[[Block]])
                        -> StateT WriterState m (Doc Text)
definitionListItemToMan opts (label, defs) = do
  -- in most man pages, option and other code in option lists is boldface,
  -- but not other things, so we try to reproduce this style:
  labelText <- inlineListToMan opts $ makeCodeBold label
  contents <- if null defs
                 then return empty
                 else liftM vcat $ forM defs $ \blocks ->
                        case blocks of
                          (x:xs) -> do
                            first' <- blockToMan opts $
                                      case x of
                                           Para y -> Plain y
                                           _      -> x
                            rest' <- liftM vcat $ mapM
                                        (\item -> blockToMan opts item) xs
                            return $ first' $$
                                     if null xs
                                        then empty
                                        else literal ".RS" $$ rest' $$ literal ".RE"
                          [] -> return empty
  return $ literal ".TP" $$ nowrap labelText $$ contents

makeCodeBold :: [Inline] -> [Inline]
makeCodeBold = walk go
  where go x@Code{} = Strong [x]
        go x        = x

-- | Convert list of Pandoc block elements to man.
blockListToMan :: PandocMonad m
               => WriterOptions -- ^ Options
               -> [Block]       -- ^ List of block elements
               -> StateT WriterState m (Doc Text)
blockListToMan opts blocks =
  vcat <$> mapM (blockToMan opts) blocks

-- | Convert list of Pandoc inline elements to man.
inlineListToMan :: PandocMonad m => WriterOptions -> [Inline] -> StateT WriterState m (Doc Text)
inlineListToMan opts lst = hcat <$> mapM (inlineToMan opts) lst

-- | Convert Pandoc inline element to man.
inlineToMan :: PandocMonad m => WriterOptions -> Inline -> StateT WriterState m (Doc Text)
inlineToMan opts (Span _ ils) = inlineListToMan opts ils
inlineToMan opts (Emph lst) =
  withFontFeature 'I' (inlineListToMan opts lst)
-- Underline is not supported, so treat the same as Emph
inlineToMan opts (Underline lst) =
  withFontFeature 'I' (inlineListToMan opts lst)
inlineToMan opts (Strong lst) =
  withFontFeature 'B' (inlineListToMan opts lst)
inlineToMan opts (Strikeout lst) = do
  contents <- inlineListToMan opts lst
  return $ literal "[STRIKEOUT:" <> contents <> char ']'
inlineToMan opts (Superscript lst) = do
  contents <- inlineListToMan opts lst
  return $ char '^' <> contents <> char '^'
inlineToMan opts (Subscript lst) = do
  contents <- inlineListToMan opts lst
  return $ char '~' <> contents <> char '~'
inlineToMan opts (SmallCaps lst) = inlineListToMan opts lst -- not supported
inlineToMan opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ char '`' <> contents <> char '\''
inlineToMan opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMan opts lst
  return $ literal "\\[lq]" <> contents <> literal "\\[rq]"
inlineToMan opts (Cite _ lst) =
  inlineListToMan opts lst
inlineToMan opts (Code _ str) =
  withFontFeature 'C' (return (literal $ escString opts str))
inlineToMan opts (Str str@(T.uncons -> Just ('.',_))) =
  return $ afterBreak "\\&" <> literal (escString opts str)
inlineToMan opts (Str str) = return $ literal $ escString opts str
inlineToMan opts (Math InlineMath str) =
  lift (texMathToInlines InlineMath str) >>= inlineListToMan opts
inlineToMan opts (Math DisplayMath str) = do
  contents <- lift (texMathToInlines DisplayMath str) >>= inlineListToMan opts
  return $ cr <> literal ".RS" $$ contents $$ literal ".RE"
inlineToMan _ il@(RawInline f str)
  | f == Format "man" = return $ literal str
  | otherwise         = do
      report $ InlineNotRendered il
      return empty
inlineToMan _ LineBreak = return $
  cr <> literal ".PD 0" $$ literal ".P" $$ literal ".PD" <> cr
inlineToMan _ SoftBreak = return space
inlineToMan _ Space = return space
inlineToMan opts (Link _ txt (src, _))
  | not (isURI src) = inlineListToMan opts txt -- skip relative links
  | otherwise       = do
  linktext <- inlineListToMan opts txt
  let srcSuffix = fromMaybe src (T.stripPrefix "mailto:" src)
  return $ case txt of
           [Str s]
             | escapeURI s == srcSuffix ->
                                 char '<' <> literal srcSuffix <> char '>'
           _                  -> linktext <> literal " (" <> literal src <> char ')'
inlineToMan opts (Image attr alternate (source, tit)) = do
  let txt = if null alternate || (alternate == [Str ""]) ||
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMan opts (Link attr txt (source, tit))
  return $ char '[' <> literal "IMAGE: " <> linkPart <> char ']'
inlineToMan _ (Note contents) = do
  -- add to notes in state
  modify $ \st -> st{ stNotes = contents : stNotes st }
  notes <- gets stNotes
  let ref = tshow (length notes)
  return $ char '[' <> literal ref <> char ']'

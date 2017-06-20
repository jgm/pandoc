{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2007-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Man
   Copyright   : Copyright (C) 2007-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to groff man page format.

-}
module Text.Pandoc.Writers.Man ( writeMan) where
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict
import Data.List (intercalate, intersperse, stripPrefix, sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder (deleteMeta)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Printf (printf)

type Notes = [[Block]]
data WriterState = WriterState { stNotes        :: Notes
                               , stFontFeatures :: Map.Map Char Bool
                               , stHasTables    :: Bool }

defaultWriterState :: WriterState
defaultWriterState = WriterState { stNotes = []
                                 , stFontFeatures  = Map.fromList [
                                                       ('I',False)
                                                     , ('B',False)
                                                     , ('C',False)
                                                     ]
                                 , stHasTables  = False }

-- | Convert Pandoc to Man.
writeMan :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMan opts document =
  evalStateT (pandocToMan opts document) defaultWriterState

-- | Return groff man representation of document.
pandocToMan :: PandocMonad m => WriterOptions -> Pandoc -> StateT WriterState m Text
pandocToMan opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' :: Doc -> Text
      render' = render colwidth
  titleText <- inlineListToMan opts $ docTitle meta
  let title' = render' titleText
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
  metadata <- metaToJSON opts
              (fmap render' . blockListToMan opts)
              (fmap render' . inlineListToMan opts)
              $ deleteMeta "title" meta
  body <- blockListToMan opts blocks
  notes <- gets stNotes
  notes' <- notesToMan opts (reverse notes)
  let main = render' $ body $$ notes' $$ text ""
  hasTables <- gets stHasTables
  let context = defField "body" main
              $ setFieldsFromTitle
              $ defField "has-tables" hasTables
              $ defField "hyphenate" True
              $ defField "pandoc-version" pandocVersion
              $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Return man representation of notes.
notesToMan :: PandocMonad m => WriterOptions -> [[Block]] -> StateT WriterState m Doc
notesToMan opts notes =
  if null notes
     then return empty
     else mapM (\(num, note) -> noteToMan opts num note) (zip [1..] notes) >>=
          return . (text ".SH NOTES" $$) . vcat

-- | Return man representation of a note.
noteToMan :: PandocMonad m => WriterOptions -> Int -> [Block] -> StateT WriterState m Doc
noteToMan opts num note = do
  contents <- blockListToMan opts note
  let marker = cr <> text ".SS " <> brackets (text (show num))
  return $ marker $$ contents

-- | Association list of characters to escape.
manEscapes :: [(Char, String)]
manEscapes = [ ('\160', "\\ ")
             , ('\'', "\\[aq]")
             , ('’', "'")
             , ('\x2014', "\\[em]")
             , ('\x2013', "\\[en]")
             , ('\x2026', "\\&...")
             ] ++ backslashEscapes "-@\\"

-- | Escape special characters for Man.
escapeString :: String -> String
escapeString = escapeStringUsing manEscapes

-- | Escape a literal (code) section for Man.
escapeCode :: String -> String
escapeCode = concat . intersperse "\n" . map escapeLine . lines  where
  escapeLine codeline =
    case escapeStringUsing (manEscapes ++ backslashEscapes "\t ") codeline of
      a@('.':_) -> "\\&" ++ a
      b         -> b

-- We split inline lists into sentences, and print one sentence per
-- line.  groff/troff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

-- | Returns the first sentence in a list of inlines, and the rest.
breakSentence :: [Inline] -> ([Inline], [Inline])
breakSentence [] = ([],[])
breakSentence xs =
  let isSentenceEndInline (Str ys@(_:_)) | last ys == '.' = True
      isSentenceEndInline (Str ys@(_:_)) | last ys == '?' = True
      isSentenceEndInline (LineBreak)    = True
      isSentenceEndInline _              = False
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

-- | Convert Pandoc block element to man.
blockToMan :: PandocMonad m
           => WriterOptions -- ^ Options
           -> Block         -- ^ Block element
           -> StateT WriterState m Doc
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
  | f == Format "man" = return $ text str
  | otherwise         = do
      report $ BlockNotRendered b
      return empty
blockToMan _ HorizontalRule = return $ text ".PP" $$ text "   *   *   *   *   *"
blockToMan opts (Header level _ inlines) = do
  contents <- inlineListToMan opts inlines
  let heading = case level of
                  1 -> ".SH "
                  _ -> ".SS "
  return $ text heading <> contents
blockToMan _ (CodeBlock _ str) = return $
  text ".IP" $$
  text ".nf" $$
  text "\\f[C]" $$
  text (escapeCode str) $$
  text "\\f[]" $$
  text ".fi"
blockToMan opts (BlockQuote blocks) = do
  contents <- blockListToMan opts blocks
  return $ text ".RS" $$ contents $$ text ".RE"
blockToMan opts (Table caption alignments widths headers rows) =
  let aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMan opts caption
  modify $ \st -> st{ stHasTables = True }
  let iwidths = if all (== 0) widths
                   then repeat ""
                   else map (printf "w(%0.1fn)" . (70 *)) widths
  -- 78n default width - 8n indent = 70n
  let coldescriptions = text $ intercalate " "
                        (zipWith (\align width -> aligncode align ++ width)
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMan opts) headers
  let makeRow cols = text "T{" $$
                     (vcat $ intersperse (text "T}@T{") cols) $$
                     text "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (blockListToMan opts) row
                         return $ makeRow cols) rows
  return $ text ".PP" $$ caption' $$
           text ".TS" $$ text "tab(@);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ text ".TE"

blockToMan opts (BulletList items) = do
  contents <- mapM (bulletListItemToMan opts) items
  return (vcat contents)
blockToMan opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 1 + (maximum $ map length markers)
  contents <- mapM (\(num, item) -> orderedListItemToMan opts num indent item) $
              zip markers items
  return (vcat contents)
blockToMan opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMan opts) items
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to man.
bulletListItemToMan :: PandocMonad m => WriterOptions -> [Block] -> StateT WriterState m Doc
bulletListItemToMan _ [] = return empty
bulletListItemToMan opts ((Para first):rest) =
  bulletListItemToMan opts ((Plain first):rest)
bulletListItemToMan opts ((Plain first):rest) = do
  first' <- blockToMan opts (Plain first)
  rest' <- blockListToMan opts rest
  let first'' = text ".IP \\[bu] 2" $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 2" $$ rest' $$ text ".RE"
  return (first'' $$ rest'')
bulletListItemToMan opts (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  return $ text "\\[bu] .RS 2" $$ first' $$ rest' $$ text ".RE"

-- | Convert ordered list item (a list of blocks) to man.
orderedListItemToMan :: PandocMonad m
                     => WriterOptions -- ^ options
                     -> String   -- ^ order marker for list item
                     -> Int      -- ^ number of spaces to indent
                     -> [Block]  -- ^ list item (list of blocks)
                     -> StateT WriterState m Doc
orderedListItemToMan _ _ _ [] = return empty
orderedListItemToMan opts num indent ((Para first):rest) =
  orderedListItemToMan opts num indent ((Plain first):rest)
orderedListItemToMan opts num indent (first:rest) = do
  first' <- blockToMan opts first
  rest' <- blockListToMan opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = text (".IP \"" ++ num' ++ "\" " ++ show indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 4" $$ rest' $$ text ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to man.
definitionListItemToMan :: PandocMonad m
                        => WriterOptions
                        -> ([Inline],[[Block]])
                        -> StateT WriterState m Doc
definitionListItemToMan opts (label, defs) = do
  labelText <- inlineListToMan opts label
  contents <- if null defs
                 then return empty
                 else liftM vcat $ forM defs $ \blocks -> do
                        (first, rest) <- case blocks of
                          ((Para x):y) -> return (Plain x,y)
                          (x:y)        -> return (x,y)
                          []           -> throwError $ PandocSomeError "blocks is null"
                        rest' <- liftM vcat $
                                  mapM (\item -> blockToMan opts item) rest
                        first' <- blockToMan opts first
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ text ".TP" $$ nowrap (text ".B " <> labelText) $$ contents

-- | Convert list of Pandoc block elements to man.
blockListToMan :: PandocMonad m
               => WriterOptions -- ^ Options
               -> [Block]       -- ^ List of block elements
               -> StateT WriterState m Doc
blockListToMan opts blocks =
  mapM (blockToMan opts) blocks >>= (return . vcat)

-- | Convert list of Pandoc inline elements to man.
inlineListToMan :: PandocMonad m => WriterOptions -> [Inline] -> StateT WriterState m Doc
inlineListToMan opts lst = mapM (inlineToMan opts) lst >>= (return . hcat)

-- | Convert Pandoc inline element to man.
inlineToMan :: PandocMonad m => WriterOptions -> Inline -> StateT WriterState m Doc
inlineToMan opts (Span _ ils) = inlineListToMan opts ils
inlineToMan opts (Emph lst) = do
  withFontFeature 'I' (inlineListToMan opts lst)
inlineToMan opts (Strong lst) = do
  withFontFeature 'B' (inlineListToMan opts lst)
inlineToMan opts (Strikeout lst) = do
  contents <- inlineListToMan opts lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
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
  return $ text "\\[lq]" <> contents <> text "\\[rq]"
inlineToMan opts (Cite _ lst) =
  inlineListToMan opts lst
inlineToMan _ (Code _ str) =
  withFontFeature 'C' (return (text $ escapeCode str))
inlineToMan _ (Str str@('.':_)) =
  return $ afterBreak "\\&" <> text (escapeString str)
inlineToMan _ (Str str) = return $ text $ escapeString str
inlineToMan opts (Math InlineMath str) =
  lift (texMathToInlines InlineMath str) >>= inlineListToMan opts
inlineToMan opts (Math DisplayMath str) = do
  contents <- lift (texMathToInlines DisplayMath str) >>= inlineListToMan opts
  return $ cr <> text ".RS" $$ contents $$ text ".RE"
inlineToMan _ il@(RawInline f str)
  | f == Format "man" = return $ text str
  | otherwise         = do
      report $ InlineNotRendered il
      return empty
inlineToMan _ LineBreak = return $
  cr <> text ".PD 0" $$ text ".P" $$ text ".PD" <> cr
inlineToMan _ SoftBreak = return space
inlineToMan _ Space = return space
inlineToMan opts (Link _ txt (src, _)) = do
  linktext <- inlineListToMan opts txt
  let srcSuffix = fromMaybe src (stripPrefix "mailto:" src)
  return $ case txt of
           [Str s]
             | escapeURI s == srcSuffix ->
                                 char '<' <> text srcSuffix <> char '>'
           _                  -> linktext <> text " (" <> text src <> char ')'
inlineToMan opts (Image attr alternate (source, tit)) = do
  let txt = if (null alternate) || (alternate == [Str ""]) ||
               (alternate == [Str source]) -- to prevent autolinks
               then [Str "image"]
               else alternate
  linkPart <- inlineToMan opts (Link attr txt (source, tit))
  return $ char '[' <> text "IMAGE: " <> linkPart <> char ']'
inlineToMan _ (Note contents) = do
  -- add to notes in state
  modify $ \st -> st{ stNotes = contents : stNotes st }
  notes <- gets stNotes
  let ref = show $ (length notes)
  return $ char '[' <> text ref <> char ']'

fontChange :: PandocMonad m => StateT WriterState m Doc
fontChange = do
  features <- gets stFontFeatures
  let filling = sort [c | (c,True) <- Map.toList features]
  return $ text $ "\\f[" ++ filling ++ "]"

withFontFeature :: PandocMonad m
                => Char
                -> StateT WriterState m Doc
                -> StateT WriterState m Doc
withFontFeature c action = do
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  begin <- fontChange
  d <- action
  modify $ \st -> st{ stFontFeatures = Map.adjust not c $ stFontFeatures st }
  end <- fontChange
  return $ begin <> d <> end

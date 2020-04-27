{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Writers.Haddock
   Copyright   : Copyright (C) 2014-2015,2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to haddock markup.

Haddock:  <http://www.haskell.org/haddock/doc/html/>
-}
module Text.Pandoc.Writers.Haddock (writeHaddock) where
import Control.Monad.State.Strict
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared

type Notes = [[Block]]
data WriterState = WriterState { stNotes :: Notes }
instance Default WriterState
  where def = WriterState{ stNotes = [] }

-- | Convert Pandoc to Haddock.
writeHaddock :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHaddock opts document =
  evalStateT (pandocToHaddock opts{
                  writerWrapText = writerWrapText opts } document) def

-- | Return haddock representation of document.
pandocToHaddock :: PandocMonad m
                => WriterOptions -> Pandoc -> StateT WriterState m Text
pandocToHaddock opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  body <- blockListToHaddock opts blocks
  st <- get
  notes' <- notesToHaddock opts (reverse $ stNotes st)
  let main = body <> (if isEmpty notes' then empty else blankline <> notes')
  metadata <- metaToContext opts
               (blockListToHaddock opts)
               (fmap chomp . inlineListToHaddock opts)
               meta
  let context  = defField "body" main metadata
  return $ render colwidth $
    case writerTemplate opts of
          Nothing  -> main
          Just tpl -> renderTemplate tpl context

-- | Return haddock representation of notes.
notesToHaddock :: PandocMonad m
               => WriterOptions -> [[Block]] -> StateT WriterState m (Doc Text)
notesToHaddock opts notes =
  if null notes
     then return empty
     else do
       contents <- blockToHaddock opts $ OrderedList (1,DefaultStyle,DefaultDelim) notes
       return $ text "#notes#" <> blankline <> contents

-- | Escape special characters for Haddock.
escapeString :: Text -> Text
escapeString = escapeStringUsing haddockEscapes
  where haddockEscapes = backslashEscapes "\\/'`\"@<"

-- | Convert Pandoc block element to haddock.
blockToHaddock :: PandocMonad m
               => WriterOptions -- ^ Options
               -> Block         -- ^ Block element
               -> StateT WriterState m (Doc Text)
blockToHaddock _ Null = return empty
blockToHaddock opts (Div _ ils) = do
  contents <- blockListToHaddock opts ils
  return $ contents <> blankline
blockToHaddock opts (Plain inlines) = do
  contents <- inlineListToHaddock opts inlines
  return $ contents <> cr
-- title beginning with fig: indicates figure
blockToHaddock opts (Para [Image attr alt (src,tgt)])
  | Just tit <- T.stripPrefix "fig:" tgt
  = blockToHaddock opts (Para [Image attr alt (src,tit)])
blockToHaddock opts (Para inlines) =
  -- TODO:  if it contains linebreaks, we need to use a @...@ block
  (<> blankline) `fmap` blockToHaddock opts (Plain inlines)
blockToHaddock opts (LineBlock lns) =
  blockToHaddock opts $ linesToPara lns
blockToHaddock _ b@(RawBlock f str)
  | f == "haddock" =
      return $ literal str <> text "\n"
  | otherwise = do
    report $ BlockNotRendered b
    return empty
blockToHaddock opts HorizontalRule =
  return $ blankline <> text (replicate (writerColumns opts) '_') <> blankline
blockToHaddock opts (Header level (ident,_,_) inlines) = do
  contents <- inlineListToHaddock opts inlines
  let attr' = if T.null ident
                 then empty
                 else cr <> text "#" <> literal ident <> text "#"
  return $ nowrap (text (replicate level '=') <> space <> contents)
                 <> attr' <> blankline
blockToHaddock _ (CodeBlock (_,_,_) str) =
  return $ prefixed "> " (literal str) <> blankline
-- Nothing in haddock corresponds to block quotes:
blockToHaddock opts (BlockQuote blocks) =
  blockListToHaddock opts blocks
blockToHaddock opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  caption' <- inlineListToHaddock opts caption
  let caption'' = if null caption
                     then empty
                     else blankline <> caption' <> blankline
  tbl <- gridTable opts blockListToHaddock
              (all null headers) (map (const AlignDefault) aligns)
                widths headers rows
  return $ (tbl $$ blankline $$ caption'') $$ blankline
blockToHaddock opts (BulletList items) = do
  contents <- mapM (bulletListItemToHaddock opts) items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToHaddock opts (OrderedList (start,_,delim) items) = do
  let attribs = (start, Decimal, delim)
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if T.length m < 3
                               then m <> T.replicate (3 - T.length m) " "
                               else m) markers
  contents <- zipWithM (orderedListItemToHaddock opts) markers' items
  return $ (if isTightList items then vcat else vsep) contents <> blankline
blockToHaddock opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToHaddock opts) items
  return $ vcat contents <> blankline

-- | Convert bullet list item (list of blocks) to haddock
bulletListItemToHaddock :: PandocMonad m
                        => WriterOptions -> [Block] -> StateT WriterState m (Doc Text)
bulletListItemToHaddock opts items = do
  contents <- blockListToHaddock opts items
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  return $ hang (writerTabStop opts) start contents $$
           if endsWithPlain items
              then cr
              else blankline

-- | Convert ordered list item (a list of blocks) to haddock
orderedListItemToHaddock :: PandocMonad m
                         => WriterOptions -- ^ options
                         -> Text        -- ^ list item marker
                         -> [Block]       -- ^ list item (list of blocks)
                         -> StateT WriterState m (Doc Text)
orderedListItemToHaddock opts marker items = do
  contents <- blockListToHaddock opts items
  let sps = case T.length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _ -> text " "
  let start = literal marker <> sps
  return $ hang (writerTabStop opts) start contents $$
           if endsWithPlain items
              then cr
              else blankline

-- | Convert definition list item (label, list of blocks) to haddock
definitionListItemToHaddock :: PandocMonad m
                            => WriterOptions
                            -> ([Inline],[[Block]])
                            -> StateT WriterState m (Doc Text)
definitionListItemToHaddock opts (label, defs) = do
  labelText <- inlineListToHaddock opts label
  defs' <- mapM (mapM (blockToHaddock opts)) defs
  let contents = (if isTightList defs then vcat else vsep) $
                 map (\d -> hang 4 empty $ vcat d <> cr) defs'
  return $ nowrap (brackets labelText) $$ contents $$
           if isTightList defs
              then cr
              else blankline

-- | Convert list of Pandoc block elements to haddock
blockListToHaddock :: PandocMonad m
                   => WriterOptions -- ^ Options
                   -> [Block]       -- ^ List of block elements
                   -> StateT WriterState m (Doc Text)
blockListToHaddock opts blocks =
  mconcat <$> mapM (blockToHaddock opts) blocks

-- | Convert list of Pandoc inline elements to haddock.
inlineListToHaddock :: PandocMonad m
                    => WriterOptions -> [Inline] -> StateT WriterState m (Doc Text)
inlineListToHaddock opts lst =
  mconcat <$> mapM (inlineToHaddock opts) lst

-- | Convert Pandoc inline element to haddock.
inlineToHaddock :: PandocMonad m
                => WriterOptions -> Inline -> StateT WriterState m (Doc Text)
inlineToHaddock opts (Span (ident,_,_) ils) = do
  contents <- inlineListToHaddock opts ils
  if not (T.null ident) && null ils
     then return $ "#" <> literal ident <> "#"
     else return contents
inlineToHaddock opts (Emph lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "/" <> contents <> "/"
-- Underline is not supported, treat the same as Emph
inlineToHaddock opts (Underline lst) =
  inlineToHaddock opts (Emph lst)
inlineToHaddock opts (Strong lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "__" <> contents <> "__"
inlineToHaddock opts (Strikeout lst) = do
  contents <- inlineListToHaddock opts lst
  -- not supported in haddock, but we fake it:
  return $ "~~" <> contents <> "~~"
-- not supported in haddock:
inlineToHaddock opts (Superscript lst) = inlineListToHaddock opts lst
-- not supported in haddock:
inlineToHaddock opts (Subscript lst) = inlineListToHaddock opts lst
-- not supported in haddock:
inlineToHaddock opts (SmallCaps lst) = inlineListToHaddock opts lst
inlineToHaddock opts (Quoted SingleQuote lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "‘" <> contents <> "’"
inlineToHaddock opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "“" <> contents <> "”"
inlineToHaddock _ (Code _ str) =
  return $ "@" <> literal (escapeString str) <> "@"
inlineToHaddock _ (Str str) =
  return $ literal $ escapeString str
inlineToHaddock _ (Math mt str) =
  return $ case mt of
    DisplayMath -> cr <> "\\[" <> literal str <> "\\]" <> cr
    InlineMath  -> "\\(" <> literal str <> "\\)"
inlineToHaddock _ il@(RawInline f str)
  | f == "haddock" = return $ literal str
  | otherwise = do
    report $ InlineNotRendered il
    return empty
-- no line break in haddock (see above on CodeBlock)
inlineToHaddock _ LineBreak = return cr
inlineToHaddock opts SoftBreak =
  case writerWrapText opts of
       WrapAuto     -> return space
       WrapNone     -> return space
       WrapPreserve -> return cr
inlineToHaddock _ Space = return space
inlineToHaddock opts (Cite _ lst) = inlineListToHaddock opts lst
inlineToHaddock _ (Link _ txt (src, _)) = do
  let linktext = literal $ escapeString $ stringify txt
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == src -> True
                      _       -> False
  return $ nowrap $ "<" <> literal src <>
           (if useAuto then empty else space <> linktext) <> ">"
inlineToHaddock opts (Image attr alternate (source, tit)) = do
  linkhaddock <- inlineToHaddock opts (Link attr alternate (source, tit))
  return $ "<" <> linkhaddock <> ">"
-- haddock doesn't have notes, but we can fake it:
inlineToHaddock opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = literal $ writerIdentifierPrefix opts <> tshow (length $ stNotes st)
  return $ "<#notes [" <> ref <> "]>"

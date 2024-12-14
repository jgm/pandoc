{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.ANSI
   Copyright   : Copyright (C) 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Ansi terminal output.
-}
module Text.Pandoc.Writers.ANSI ( writeANSI ) where
import Control.Monad.State.Strict ( StateT, gets, modify, evalStateT )
import Control.Monad (foldM)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.DocLayout ((<+>), ($$), ($+$))
import Text.DocTemplates (Context(..))
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (highlight, formatANSI)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math(texMathToInlines)
import Text.Pandoc.Writers.Shared
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Text.DocLayout as D

hr :: D.HasChars a => D.Doc a
hr = rule 20

rule :: D.HasChars a => Int -> D.Doc a
rule n = D.literal $ D.replicateChar n '─'

data WriterState = WriterState {
    stNotes     :: [D.Doc Text]        -- Footnotes
  , stColumns   :: Int         -- Width of the rendered text block
  , stInner     :: Bool    -- Are we at the document's top-level or in a nested construct?
  , stNextFigureNum :: Int
  , stInFigure :: Bool
  , stInTable :: Bool
  }

type TW = StateT WriterState

withFewerColumns :: PandocMonad m => Int -> TW m a -> TW m a
withFewerColumns n a = do
  cols <- gets stColumns
  inner <- gets stInner
  modify $ \s -> s{stColumns = max (cols - n) 4, stInner = True}
  result <- a
  modify $ \s -> s{stColumns = cols, stInner = inner}
  return result

-- | Convert Pandoc to ANSI
writeANSI :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeANSI opts document =
  evalStateT (pandocToANSI opts document)
            WriterState { stNotes = [],
                          stColumns = (writerColumns opts),
                          stInner = False,
                          stNextFigureNum = 1,
                          stInFigure = False,
                          stInTable = False
                        }

-- | Return ANSI-styled version of document
pandocToANSI :: PandocMonad m
                => WriterOptions -> Pandoc -> TW m Text
pandocToANSI opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
                 (blockListToANSI opts)
                 (inlineListToANSI opts) meta
  width <- gets stColumns
  let title = titleBlock width metadata
  let blocks' = makeSections (writerNumberSections opts) Nothing blocks
  body <- blockListToANSI opts blocks'
  notes <- gets $ reverse . stNotes
  let notemark x = D.literal (tshow (x :: Int) <> ".") <+> D.space
  let marks = map notemark [1..length notes]
  let hangWidth = foldr (max . D.offset) 0 marks
  let notepretty | not (null notes) = D.cblock width hr $+$ hangMarks hangWidth marks notes
                 | otherwise = D.empty
  let main = body $+$ notepretty
  let context = defField "body" main
              $ defField "titleblock" title metadata
  return $
    case writerTemplate opts of
         Nothing  -> toStrict $ D.renderANSI (Just width) main
         Just tpl -> toStrict $ D.renderANSI (Just width) $ renderTemplate tpl context

titleBlock :: Int -> Context Text -> D.Doc Text
titleBlock width meta = if null most then D.empty else D.cblock width $ most $+$ hr
  where
    title = D.bold (fromMaybe D.empty $ getField "title" meta)
    subtitle = fromMaybe D.empty $ getField "subtitle" meta
    author =  D.vcat $ fromMaybe [] $ getField "author" meta
    date = D.italic (fromMaybe D.empty $ getField "date" meta)
    most = (title $$ subtitle) $+$ author $+$ date

hangMarks :: Int -> [D.Doc Text] -> [D.Doc Text] -> D.Doc Text
hangMarks width markers contents =
  D.vsep (zipWith hangMark markers contents) where
    hangMark m d = D.rblock width m <+> D.nest (width + 1) d

stackMarks :: [D.Doc Text] -> [D.Doc Text] -> D.Doc Text
stackMarks markers contents = D.vsep (zipWith stack markers contents)
  where stack m d = m $$ D.nest 4 d

-- | Convert Pandoc block element to ANSI
blockToANSI :: PandocMonad m
               => WriterOptions -- ^ Options
               -> Block         -- ^ Block element
               -> TW m (D.Doc Text)

blockToANSI opts (Div _ bs) = blockListToANSI opts bs

blockToANSI opts (Plain inlines) = inlineListToANSI opts inlines

blockToANSI opts (Para inlines) = inlineListToANSI opts inlines

blockToANSI opts (LineBlock lns) = do
  let go [] = return D.blankline
      go xs = inlineListToANSI opts xs
  lns' <- mapM go lns
  return $ D.vcat lns'

blockToANSI _ b@(RawBlock _ _) = do
    report $ BlockNotRendered b
    return D.empty

blockToANSI _ HorizontalRule = return $ D.blankline $$ hr $$ D.blankline

blockToANSI opts (Header level (_, classes, kvs) inlines) = do
  contents <- inlineListToANSI opts inlines
  let secnum = fromMaybe mempty $ lookup "number" kvs
  let doNumber = writerNumberSections opts && not (T.null secnum) && "unnumbered" `notElem` classes
  let number | doNumber = D.hang (D.realLength secnum + 1) (header level (D.literal secnum) <> D.space)
             | otherwise = id
  return $ number (header level contents) $$ D.blankline where
    header 1 = (fmap T.toUpper) . D.bold
    header 2 = D.bold
    header _ = D.italic

-- The approach to code blocks and highlighting here is a best-effort with
-- existing tools. The Skylighting formatANSI function produces fully-rendered
-- results, and its line numbers are followed by a tab character, which can
-- produce less-than-ideal results depending on your terminal's tab stops. (See
-- tabs(1)). A more ambitious approach here could process SourceLines into a
-- Doc Text.
blockToANSI opts (CodeBlock attr str) = do
  table <- gets stInTable
  inner <- case (table, writerHighlightStyle opts) of
    (_, Nothing) -> return $ defaultStyle str
    (True, _) -> return $ defaultStyle str
    (False, Just s) -> do
      let fmt o = formatANSI o s
          result = highlight (writerSyntaxMap opts) fmt attr str
      return $ case result of
        Left _ -> defaultStyle str
        Right f -> D.literal f
  return $ nest table inner
  where defaultStyle = (D.fg D.red) . D.literal
        nest False = D.nest 4
        nest True = id

blockToANSI opts (BlockQuote blocks) = do
  contents <- withFewerColumns 2 $ blockListToANSI opts blocks
  return ( D.prefixed "│ " contents $$ D.blankline)

-- TODO: Row spans don't work
blockToANSI opts (Table _ (Caption _ caption) colSpecs (TableHead _ thead) tbody (TableFoot _ tfoot)) = do
  let captionInlines = blocksToInlines caption
  captionMarkup <-
    if null captionInlines
       then return mempty
       else D.nest 2 <$> inlineListToANSI opts (blocksToInlines caption)
  wasTable <- gets stInTable
  modify $ \s -> s{stInTable = True}
  let tw = writerColumns opts
  let ncol = length colSpecs
  let inWidths = map snd colSpecs
  let spaceForColumns = tw - ncol + 1  -- reserve a 1-char gutter between tcols
  let claimWidth ColWidthDefault = 0
      claimWidth (ColWidth n) = floor (n * fromIntegral spaceForColumns)
  let usedSpace = sum (map claimWidth inWidths)
  let remaining = spaceForColumns - usedSpace
  let defWidth = remaining `div` length (filter (== ColWidthDefault) inWidths)
  let maxWidth ColWidthDefault = defWidth
      maxWidth k = claimWidth k
  let widths = map maxWidth inWidths
  let decor = [D.hsep $ map rule widths]
  head' <- mapM (goRow widths . unRow) thead
  body' <- mapM (goRow widths . unRow) (unBodies tbody)
  foot' <- mapM (goRow widths . unRow) tfoot
  modify $ \s -> s{stInTable = wasTable}
  return $ D.vcat (head' <> decor <> body' <> decor <> foot') $+$ captionMarkup
  where
    unRow (Row _ cs) = cs
    unBody (TableBody _ _ hd bd) = hd <> bd
    unBodies = concatMap unBody
    goRow ws cs = do
      (d, _) <- foldM goCell ([], ws) cs
      return $ D.hcat $ intersperse (D.vfill " ") $ reverse d
    goCell (r, ws) (Cell _ aln _ (ColSpan cspan) inner) = do
      let (ws', render) = next ws aln cspan
      innerDoc <- blockListToANSI opts inner
      return ((render innerDoc):r, ws')
    tcell AlignLeft    = D.lblock
    tcell AlignRight   = D.rblock
    tcell AlignCenter  = D.cblock
    tcell AlignDefault = D.lblock
    next ws aln cspan =
      let (this, ws') = splitAt cspan ws
          w = sum this + cspan - 1
          cell = (tcell aln) w
       in (ws', cell)

blockToANSI opts (BulletList items) = do
  contents <- withFewerColumns 2 $ mapM (blockListToANSI opts) items
  return $ D.vsep (fmap hangMark contents) where
    hangMark d = D.hang 2 (D.literal "• ") d

blockToANSI opts (OrderedList attribs items) = do
  let markers = fmap D.literal $ take (length items) $ orderedListMarkers attribs
  let hangWidth = foldr (max . D.offset) 0 markers
  contents <- withFewerColumns hangWidth $ mapM (blockListToANSI opts) items
  return $ hangMarks hangWidth markers contents <> D.cr

blockToANSI opts (DefinitionList items) = do
  labels <- mapM (inlineListToANSI opts . fst) items
  columns <- gets stColumns
  let hangWidth = foldr (max . D.offset) 0 labels
  if hangWidth > floor (toRational columns / 10 * 3)
     then do
       contents <- withFewerColumns 4 $ mapM ((mapM (blockListToANSI opts)) . snd) items
       return $ stackMarks (D.bold <$> labels) (D.vsep <$> contents) <> D.cr
     else do
       contents <- withFewerColumns hangWidth $ mapM ((mapM (blockListToANSI opts)) . snd) items
       return $ hangMarks hangWidth (D.bold <$> labels) (D.vsep <$> contents) <> D.cr

blockToANSI opts (Figure _ (Caption _ caption)  body) = do
  let captionInlines = blocksToInlines caption
  figState <- gets stInFigure
  captionMarkup <-
    if null captionInlines
       then return mempty
       else D.nest 2 <$> inlineListToANSI opts (blocksToInlines caption)
  modify $ \s -> s{stInFigure = True}
  contents <- blockListToANSI opts body
  modify $ \s -> s{stInFigure = figState}
  return $ contents $+$ captionMarkup

-- Auxiliary functions for lists:

-- | Convert list of Pandoc block elements to ANSI
blockListToANSI :: PandocMonad m
                   => WriterOptions -- ^ Options
                   -> [Block]       -- ^ List of block elements
                   -> TW m (D.Doc Text)
blockListToANSI opts blocks =
  D.vsep <$> mapM (blockToANSI opts) blocks

-- | Convert list of Pandoc inline elements to ANSI
inlineListToANSI :: PandocMonad m
                    => WriterOptions -> [Inline] -> TW m (D.Doc Text)
inlineListToANSI opts lst =
  D.hcat <$> mapM (inlineToANSI opts) lst

-- | Convert Pandoc inline element to ANSI
inlineToANSI :: PandocMonad m => WriterOptions -> Inline -> TW m (D.Doc Text)

inlineToANSI opts (Span _ lst) =
  inlineListToANSI opts lst

inlineToANSI opts (Emph lst) = do
  contents <- inlineListToANSI opts lst
  return $ D.italic contents

inlineToANSI opts (Underline lst) = do
  contents <- inlineListToANSI opts lst
  return $ D.underlined contents

inlineToANSI opts (Strong lst) = do
  contents <- inlineListToANSI opts lst
  return $ D.bold contents

inlineToANSI opts (Strikeout lst) = do
  contents <- inlineListToANSI opts lst
  return $ D.strikeout contents

inlineToANSI opts (Superscript lst) = do
  case traverse toSuperscriptInline lst of
    Just xs -> inlineListToANSI opts xs
    Nothing -> D.parens <$> inlineListToANSI opts lst

inlineToANSI opts (Subscript lst) = do
  case traverse toSubscriptInline lst of
    Just xs -> inlineListToANSI opts xs
    Nothing -> D.parens <$> inlineListToANSI opts lst

inlineToANSI opts (SmallCaps lst) = inlineListToANSI opts lst

inlineToANSI opts (Quoted SingleQuote lst) = do
  contents <- inlineListToANSI opts lst
  return $ "‘" <> contents <> "’"

inlineToANSI opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToANSI opts lst
  return $ "“" <> contents <> "”"

inlineToANSI opts (Cite _  lst) = inlineListToANSI opts lst

-- Making a judgment call here that for ANSI-formatted output
-- intended for reading, we want to reflow inline Code on spaces
inlineToANSI _ (Code _ str) =
  return $ D.bg D.white $ D.fg D.red $ " " <> D.hcat flow <> " "
    where flow = intersperse D.space (D.literal <$> T.words str)

inlineToANSI _ (Str str) = return $ D.literal str

inlineToANSI opts (Math t str) = texMathToInlines t str >>= inlineListToANSI opts

inlineToANSI _ il@RawInline{} = do
  report $ InlineNotRendered il
  return ""

inlineToANSI _ LineBreak = return D.cr

inlineToANSI _ SoftBreak = return D.space

inlineToANSI _ Space = return D.space

inlineToANSI opts (Link (_, _, _) txt (src, _)) = do
  label <- inlineListToANSI opts txt
  return $ D.underlined $ D.fg D.cyan $ D.link src label

inlineToANSI opts (Image _ alt _) = do
  infig <- gets stInFigure
  if not infig then do
    alt' <- inlineListToANSI opts alt
    return $ D.brackets $ "image: " <> alt'
  else return $ D.brackets "image"

-- by construction, we should never be lacking in superscript characters
-- for the footnote number, but we'll fall back to square brackets anyway
inlineToANSI opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = tshow $ length curNotes + 1
  contents' <- blockListToANSI opts contents
  modify $ \s -> s { stNotes = contents' : curNotes }
  let super = T.pack <$> (traverse toSuperscript (T.unpack newnum))
  return $ D.literal $ fromMaybe ("[" <> newnum <> "]") super

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
hr = D.literal $ D.replicateChar 20 '─'

data WriterState = WriterState {
    stNotes     :: [D.Doc Text]        -- Footnotes
  , stColumns   :: Int         -- Width of the rendered text block
  , stInner     :: Bool    -- Are we at the document's top-level or in a nested construct?
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
                          stInner = False
                        }

-- | Return ANSI-styled verison of document
pandocToANSI :: PandocMonad m
                => WriterOptions -> Pandoc -> TW m Text
pandocToANSI opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
                 (blockListToANSI opts)
                 (inlineListToANSI opts) meta
  width <- gets stColumns
  let title = titleBlock width metadata
  body <- blockListToANSI opts blocks
  notes <- gets $ reverse . stNotes
  let notemark x = D.literal (tshow (x :: Int) <> ".") <+> D.space
  let marks = take (length notes) $ map notemark [1..]
  let hangWidth = foldr (max . D.offset) 0 marks
  let notepretty | not (null notes) = D.cblock width hr $+$ hangMarks hangWidth marks notes
                 | otherwise = D.empty
  let main = D.nest 4 $ body $+$ notepretty
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

blockToANSI opts (LineBlock lns) = blockToANSI opts $ linesToPara lns

blockToANSI _ b@(RawBlock _ _) = do
    report $ BlockNotRendered b
    return D.empty

blockToANSI _ HorizontalRule = return $ D.blankline $$ hr $$ D.blankline

blockToANSI opts (Header level _ inlines) = do
  contents <- inlineListToANSI opts inlines
  inner <- gets stInner
  return $ header inner level contents $$ D.blankline where
    header False 1 = (D.flush .  D.bold)
    header True 1 = (D.underlined . D.bold)
    header False 2 = ((<> D.literal "  ") . D.bold)
    header True 2 = D.bold
    header _ 3 = D.italic
    header _ _ = id

-- The approach to code blocks and highlighting here is a best-effort with
-- existing tools, and can easily produce results that aren't quite right. Using
-- line numbers together with certain highlight styles interacts poorly with
-- the "nest" combinator being applied to the whole document. The Skylighting
-- formatANSI function produces fully-rendered results; a more ambitious
-- approach here could process SourceLines into a Doc Text.
blockToANSI opts (CodeBlock attr str) =
  case writerHighlightStyle opts of
    Nothing -> return $ D.literal str
    Just s -> do
      let fmt o = formatANSI o s
          result = highlight (writerSyntaxMap opts) fmt attr str
      return $ case result of
        Left _ -> D.literal str
        Right f -> D.literal f

blockToANSI opts (BlockQuote blocks) = do
  contents <- withFewerColumns 2 $ blockListToANSI opts blocks
  return ( D.prefixed "│ " contents $$ D.blankline)

blockToANSI _ Table{} = do
  return $ D.literal "[TABLE]"

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
  captionMarkup <- if null captionInlines
                      then return D.empty
                      else inlineListToANSI opts (blocksToInlines caption)
  contents <- blockListToANSI opts  body
  return $ captionMarkup <> contents <> D.blankline

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
    Nothing -> inlineListToANSI opts lst >>= return . D.parens

inlineToANSI opts (Subscript lst) = do
  case traverse toSuperscriptInline lst of
    Just xs -> inlineListToANSI opts xs
    Nothing -> inlineListToANSI opts lst >>= return . D.parens

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
  return $ D.bg D.white $ D.fg D.magenta $ D.hcat flow
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
  alt' <- inlineListToANSI opts alt
  return $ "image: " <> alt'

-- by construction, we should never be lacking in superscript characters
-- for the footnote number, but we'll fall back to square brackets anyway
inlineToANSI opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = tshow $ length curNotes + 1
  contents' <- blockListToANSI opts contents
  modify $ \s -> s { stNotes = contents' : curNotes }
  let super = T.pack <$> (traverse toSuperscript (T.unpack newnum))
  return $ D.literal $ fromMaybe ("[" <> newnum <> "]") super

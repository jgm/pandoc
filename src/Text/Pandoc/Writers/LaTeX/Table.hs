{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Table
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Output LaTeX formatted tables.
-}
module Text.Pandoc.Writers.LaTeX.Table
  ( tableToLaTeX
  ) where
import Control.Monad.State.Strict
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.DocLayout
  ( Doc, braces, cr, empty, hcat, hsep, isEmpty, literal, nest
  , text, vcat, ($$) )
import Text.Pandoc.Shared (splitBy)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Writers.Shared (toLegacyTable)
import Text.Pandoc.Writers.LaTeX.Caption (getCaption)
import Text.Pandoc.Writers.LaTeX.Notes (notesToLaTeX)
import Text.Pandoc.Writers.LaTeX.Types
  ( LW, WriterState (stBeamer, stExternalNotes, stInMinipage, stNotes, stTable) )
import Text.Printf (printf)

tableToLaTeX :: PandocMonad m
             => ([Inline] -> LW m (Doc Text))
             -> ([Block]  -> LW m (Doc Text))
             -> Caption -> [ColSpec] -> TableHead -> [TableBody] -> TableFoot
             -> LW m (Doc Text)
tableToLaTeX inlnsToLaTeX blksToLaTeX blkCapt specs thead tbody tfoot = do
  let (caption, aligns, widths, heads, rows) =
        toLegacyTable blkCapt specs thead tbody tfoot
  -- simple tables have to have simple cells:
  let isSimple = \case
        [Plain _] -> True
        [Para  _] -> True
        []        -> True
        _         -> False
  let widths' = if all (== 0) widths && not (all (all isSimple) rows)
                   then replicate (length aligns)
                          (1 / fromIntegral (length aligns))
                   else widths
  (captionText, captForLof, captNotes) <- getCaption inlnsToLaTeX False caption
  let toHeaders hs = do contents <- tableRowToLaTeX blksToLaTeX True aligns hs
                        return ("\\toprule" $$ contents $$ "\\midrule")
  let removeNote (Note _) = Span ("", [], []) []
      removeNote x        = x
  firsthead <- if isEmpty captionText || all null heads
                  then return empty
                  else ($$ text "\\endfirsthead") <$> toHeaders heads
  head' <- if all null heads
              then return "\\toprule"
              -- avoid duplicate notes in head and firsthead:
              else toHeaders (if isEmpty firsthead
                                 then heads
                                 else walk removeNote heads)
  let capt = if isEmpty captionText
                then empty
                else "\\caption" <> captForLof <> braces captionText
                         <> "\\tabularnewline"
  rows' <- mapM (tableRowToLaTeX blksToLaTeX False aligns) rows
  let colDescriptors =
         (if all (== 0) widths'
             then hcat . map literal
             else (\xs -> cr <> nest 2 (vcat $ map literal xs))) $
         zipWith (toColDescriptor (length widths')) aligns widths'
  modify $ \s -> s{ stTable = True }
  notes <- notesToLaTeX <$> gets stNotes
  return $ "\\begin{longtable}[]" <>
              braces ("@{}" <> colDescriptors <> "@{}")
              -- the @{} removes extra space at beginning and end
         $$ capt
         $$ firsthead
         $$ head'
         $$ "\\endhead"
         $$ vcat rows'
         $$ "\\bottomrule"
         $$ "\\end{longtable}"
         $$ captNotes
         $$ notes

toColDescriptor :: Int -> Alignment -> Double -> Text
toColDescriptor _numcols align 0 =
  case align of
         AlignLeft    -> "l"
         AlignRight   -> "r"
         AlignCenter  -> "c"
         AlignDefault -> "l"
toColDescriptor numcols align width =
  T.pack $ printf
     ">{%s\\arraybackslash}p{(\\columnwidth - %d\\tabcolsep) * \\real{%0.2f}}"
     align'
     ((numcols - 1) * 2)
     width
 where
   align' :: String
   align' = case align of
              AlignLeft    -> "\\raggedright"
              AlignRight   -> "\\raggedleft"
              AlignCenter  -> "\\centering"
              AlignDefault -> "\\raggedright"

tableRowToLaTeX :: PandocMonad m
                => ([Block] -> LW m (Doc Text))
                -> Bool
                -> [Alignment]
                -> [[Block]]
                -> LW m (Doc Text)
tableRowToLaTeX blockListToLaTeX header aligns cols = do
  cells <- mapM (tableCellToLaTeX blockListToLaTeX header) $ zip aligns cols
  return $ hsep (intersperse "&" cells) <> " \\\\ \\addlinespace"

-- For simple latex tables (without minipages or parboxes),
-- we need to go to some lengths to get line breaks working:
-- as LineBreak bs = \vtop{\hbox{\strut as}\hbox{\strut bs}}.
fixLineBreaks :: Block -> Block
fixLineBreaks (Para ils)  = Para $ fixLineBreaks' ils
fixLineBreaks (Plain ils) = Plain $ fixLineBreaks' ils
fixLineBreaks x           = x

fixLineBreaks' :: [Inline] -> [Inline]
fixLineBreaks' ils = case splitBy (== LineBreak) ils of
                       []     -> []
                       [xs]   -> xs
                       chunks -> RawInline "tex" "\\vtop{" :
                                 concatMap tohbox chunks <>
                                 [RawInline "tex" "}"]
  where tohbox ys = RawInline "tex" "\\hbox{\\strut " : ys <>
                    [RawInline "tex" "}"]

-- We also change display math to inline math, since display
-- math breaks in simple tables.
displayMathToInline :: Inline -> Inline
displayMathToInline (Math DisplayMath x) = Math InlineMath x
displayMathToInline x                    = x

tableCellToLaTeX :: PandocMonad m
                 => ([Block] -> LW m (Doc Text))
                 -> Bool -> (Alignment, [Block])
                 -> LW m (Doc Text)
tableCellToLaTeX blockListToLaTeX header (align, blocks) = do
  beamer <- gets stBeamer
  externalNotes <- gets stExternalNotes
  inMinipage <- gets stInMinipage
  -- See #5367 -- footnotehyper/footnote don't work in beamer,
  -- so we need to produce the notes outside the table...
  modify $ \st -> st{ stExternalNotes = beamer }
  let isPlainOrPara = \case
        Para{}  -> True
        Plain{} -> True
        _       -> False
  result <-
    if all isPlainOrPara blocks
       then
         blockListToLaTeX $ walk fixLineBreaks $ walk displayMathToInline blocks
       else do
         modify $ \st -> st{ stInMinipage = True }
         cellContents <- blockListToLaTeX blocks
         modify $ \st -> st{ stInMinipage = inMinipage }
         let valign = text $ if header then "[b]" else "[t]"
         let halign = case align of
                        AlignLeft    -> "\\raggedright"
                        AlignRight   -> "\\raggedleft"
                        AlignCenter  -> "\\centering"
                        AlignDefault -> "\\raggedright"
         return $ "\\begin{minipage}" <> valign <>
                  braces "\\linewidth" <> halign <> cr <>
                  cellContents <> cr <>
                  "\\end{minipage}"
  modify $ \st -> st{ stExternalNotes = externalNotes }
  return result

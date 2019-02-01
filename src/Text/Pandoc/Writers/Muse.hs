{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2017-2019 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Writers.Muse
   Copyright   : Copyright (C) 2017-2019 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : stable
   Portability : portable

Conversion of 'Pandoc' documents to Muse.

This module is mostly intended for <https://amusewiki.org/ Amusewiki> markup support,
as described by <https://amusewiki.org/library/manual Text::Amuse markup manual>.
Original <https://www.gnu.org/software/emacs-muse/ Emacs Muse> markup support
is a secondary goal.

Where Text::Amuse markup
<https://metacpan.org/pod/Text::Amuse#DIFFERENCES-WITH-THE-ORIGINAL-EMACS-MUSE-MARKUP differs>
from <https://www.gnu.org/software/emacs-muse/manual/ Emacs Muse markup>,
Text::Amuse markup is supported.
For example, native tables are always used instead of Org Mode tables.
However, @\<literal style="html">@ tag is used for HTML raw blocks
even though it is supported only in Emacs Muse.
-}
module Text.Pandoc.Writers.Muse (writeMuse) where
import Prelude
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Default
import Data.List (intersperse, isInfixOf, transpose)
import Data.Monoid (Any (..))
import qualified Data.Set as Set
import Data.Text (Text)
import System.FilePath (takeExtension)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Walk

type Notes = [[Block]]

type Muse m = ReaderT WriterEnv (StateT WriterState m)

data WriterEnv =
  WriterEnv { envOptions               :: WriterOptions
            , envTopLevel              :: Bool
            , envInsideBlock           :: Bool
            , envInlineStart           :: Bool -- ^ True if there is only whitespace since last newline
            , envInsideLinkDescription :: Bool -- ^ Escape ] if True
            , envAfterSpace            :: Bool -- ^ There is whitespace (not just newline) before
            , envOneLine               :: Bool -- ^ True if newlines are not allowed
            , envInsideAsterisks       :: Bool -- ^ True if outer element is emphasis with asterisks
            , envNearAsterisks         :: Bool -- ^ Rendering inline near asterisks
            }

data WriterState =
  WriterState { stNotes   :: Notes
              , stNoteNum :: Int
              , stIds     :: Set.Set String
              , stUseTags :: Bool -- ^ Use tags for emphasis, for example because previous character is a letter
              }

instance Default WriterState
  where def = WriterState { stNotes = []
                          , stNoteNum = 1
                          , stIds = Set.empty
                          , stUseTags = False
                          }

evalMuse :: PandocMonad m => Muse m a -> WriterEnv -> WriterState -> m a
evalMuse document env = evalStateT $ runReaderT document env

-- | Convert Pandoc to Muse.
writeMuse :: PandocMonad m
          => WriterOptions
          -> Pandoc
          -> m Text
writeMuse opts document =
  evalMuse (pandocToMuse document) env def
  where env = WriterEnv { envOptions = opts
                        , envTopLevel = True
                        , envInsideBlock = False
                        , envInlineStart = True
                        , envInsideLinkDescription = False
                        , envAfterSpace = False
                        , envOneLine = False
                        , envInsideAsterisks = False
                        , envNearAsterisks = False
                        }

-- | Return Muse representation of document.
pandocToMuse :: PandocMonad m
             => Pandoc
             -> Muse m Text
pandocToMuse (Pandoc meta blocks) = do
  opts <- asks envOptions
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' :: Doc -> Text
      render' = render Nothing
  metadata <- metaToJSON opts
               (fmap render' . blockListToMuse)
               (fmap render' . inlineListToMuse)
               meta
  body <- blockListToMuse blocks
  notes <- currentNotesToMuse
  let main = render colwidth $ body $+$ notes
  let context = defField "body" main metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Helper function for flatBlockListToMuse
-- | Render all blocks and insert blank lines between the first two
catWithBlankLines :: PandocMonad m
                  => [Block]       -- ^ List of block elements
                  -> Int           -- ^ Number of blank lines
                  -> Muse m Doc
catWithBlankLines (b : bs) n = do
  b' <- blockToMuseWithNotes b
  bs' <- flatBlockListToMuse bs
  return $ b' <> blanklines n <> bs'
catWithBlankLines _ _ = error "Expected at least one block"

-- | Convert list of Pandoc block elements to Muse
-- | without setting envTopLevel.
flatBlockListToMuse :: PandocMonad m
                => [Block]       -- ^ List of block elements
                -> Muse m Doc
flatBlockListToMuse bs@(BulletList _ : BulletList _ : _) = catWithBlankLines bs 2
flatBlockListToMuse bs@(OrderedList (_, style1, _) _ : OrderedList (_, style2, _) _ : _) =
  catWithBlankLines bs (if style1' == style2' then 2 else 0)
    where
      style1' = normalizeStyle style1
      style2' = normalizeStyle style2
      normalizeStyle DefaultStyle = Decimal
      normalizeStyle s            = s
flatBlockListToMuse bs@(DefinitionList _ : DefinitionList _ : _) = catWithBlankLines bs 2
flatBlockListToMuse bs@(_ : _) = catWithBlankLines bs 0
flatBlockListToMuse [] = return mempty

simpleTable :: PandocMonad m
            => [Inline]
            -> [TableCell]
            -> [[TableCell]]
            -> Muse m Doc
simpleTable caption headers rows = do
  topLevel <- asks envTopLevel
  caption' <- inlineListToMuse caption
  headers' <- mapM blockListToMuse headers
  rows' <- mapM (mapM blockListToMuse) rows
  let widthsInChars = maximum . map offset <$> transpose (headers' : rows')
  let hpipeBlocks sep blocks = hcat $ intersperse sep' blocks
        where sep' = lblock (length sep) $ text sep
  let makeRow sep = hpipeBlocks sep . zipWith lblock widthsInChars
  let head' = makeRow " || " headers'
  rows'' <- mapM (\row -> makeRow rowSeparator <$> mapM blockListToMuse row) rows
  let body = vcat rows''
  return $ (if topLevel then nest 1 else id) ((if noHeaders then empty else head')
                                             $$ body
                                             $$ (if null caption then empty else "|+ " <> caption' <> " +|"))
         $$ blankline
  where noHeaders = all null headers
        rowSeparator = if noHeaders then " | " else " |  "

-- | Convert list of Pandoc block elements to Muse.
blockListToMuse :: PandocMonad m
                => [Block]       -- ^ List of block elements
                -> Muse m Doc
blockListToMuse =
  local (\env -> env { envTopLevel = not (envInsideBlock env)
                     , envInsideBlock = True
                     }) . flatBlockListToMuse

-- | Convert Pandoc block element to Muse.
blockToMuse :: PandocMonad m
            => Block         -- ^ Block element
            -> Muse m Doc
blockToMuse (Plain inlines) = inlineListToMuse' inlines
blockToMuse (Para inlines) = do
  contents <- inlineListToMuse' inlines
  return $ contents <> blankline
blockToMuse (LineBlock lns) = do
  lns' <- local (\env -> env { envOneLine = True }) $ mapM inlineListToMuse lns
  return $ nowrap $ vcat (map (text "> " <>) lns') <> blankline
blockToMuse (CodeBlock (_,_,_) str) =
  return $ "<example>" $$ text str $$ "</example>" $$ blankline
blockToMuse (RawBlock (Format format) str) =
  return $ blankline $$ "<literal style=\"" <> text format <> "\">" $$
           text str $$ "</literal>" $$ blankline
blockToMuse (BlockQuote blocks) = do
  contents <- flatBlockListToMuse blocks
  return $ blankline
        <> "<quote>"
        $$ nest 0 contents -- nest 0 to remove trailing blank lines
        $$ "</quote>"
        <> blankline
blockToMuse (OrderedList (start, style, _) items) = do
  let markers = take (length items) $ orderedListMarkers
                                      (start, style, Period)
  contents <- zipWithM orderedListItemToMuse markers items
  topLevel <- asks envTopLevel
  return $ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where orderedListItemToMuse :: PandocMonad m
                              => String   -- ^ marker for list item
                              -> [Block]  -- ^ list item (list of blocks)
                              -> Muse m Doc
        orderedListItemToMuse marker item = hang (length marker + 1) (text marker <> space)
          <$> blockListToMuse item
blockToMuse (BulletList items) = do
  contents <- mapM bulletListItemToMuse items
  topLevel <- asks envTopLevel
  return $ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where bulletListItemToMuse :: PandocMonad m
                             => [Block]
                             -> Muse m Doc
        bulletListItemToMuse item = do
          modify $ \st -> st { stUseTags = False }
          hang 2 "- " <$> blockListToMuse item
blockToMuse (DefinitionList items) = do
  contents <- mapM definitionListItemToMuse items
  topLevel <- asks envTopLevel
  return $ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where definitionListItemToMuse :: PandocMonad m
                                 => ([Inline], [[Block]])
                                 -> Muse m Doc
        definitionListItemToMuse (label, defs) = do
          modify $ \st -> st { stUseTags = False }
          label' <- local (\env -> env { envOneLine = True, envAfterSpace = True }) $ inlineListToMuse' label
          let ind = offset' label' -- using Text.Pandoc.Pretty.offset results in round trip failures
          hang ind (nowrap label') . vcat <$> mapM descriptionToMuse defs
          where offset' d = maximum (0: map length (lines $ render Nothing d))
        descriptionToMuse :: PandocMonad m
                          => [Block]
                          -> Muse m Doc
        descriptionToMuse desc = hang 4 " :: " <$> blockListToMuse desc
blockToMuse (Header level (ident,_,_) inlines) = do
  opts <- asks envOptions
  topLevel <- asks envTopLevel
  contents <- local (\env -> env { envOneLine = True }) $ inlineListToMuse' inlines
  ids <- gets stIds
  let autoId = uniqueIdent (writerExtensions opts) inlines ids
  modify $ \st -> st{ stIds = Set.insert autoId ids }

  let attr' = if null ident || (isEnabled Ext_auto_identifiers opts && ident == autoId)
                 then empty
                 else "#" <> text ident <> cr
  let header' = if topLevel then text (replicate level '*') <> space else mempty
  return $ blankline <> attr' $$ nowrap (header' <> contents) <> blankline
-- https://www.gnu.org/software/emacs-muse/manual/muse.html#Horizontal-Rules-and-Anchors
blockToMuse HorizontalRule = return $ blankline $$ "----" $$ blankline
blockToMuse (Table caption aligns widths headers rows) =
  if isSimple && numcols > 1
    then simpleTable caption headers rows
    else do
      opts <- asks envOptions
      gridTable opts blocksToDoc True (map (const AlignDefault) aligns) widths headers rows
  where
    blocksToDoc opts blocks =
      local (\env -> env { envOptions = opts }) $ blockListToMuse blocks
    numcols = maximum (length aligns : length widths : map length (headers:rows))
    hasSimpleCells = all isSimpleCell (concat (headers:rows))
    isLineBreak LineBreak = Any True
    isLineBreak _         = Any False
    hasLineBreak = getAny . query isLineBreak
    isSimple = hasSimpleCells && all (== 0) widths
    isSimpleCell [Plain ils] = not (hasLineBreak ils)
    isSimpleCell [Para ils ] = not (hasLineBreak ils)
    isSimpleCell []          = True
    isSimpleCell _           = False
blockToMuse (Div _ bs) = flatBlockListToMuse bs
blockToMuse Null = return empty

-- | Return Muse representation of notes collected so far.
currentNotesToMuse :: PandocMonad m
                   => Muse m Doc
currentNotesToMuse = do
  notes <- reverse <$> gets stNotes
  modify $ \st -> st { stNotes = mempty }
  notesToMuse notes

-- | Return Muse representation of notes.
notesToMuse :: PandocMonad m
            => Notes
            -> Muse m Doc
notesToMuse notes = do
  n <- gets stNoteNum
  modify $ \st -> st { stNoteNum = stNoteNum st + length notes }
  vsep <$> zipWithM noteToMuse [n ..] notes

-- | Return Muse representation of a note.
noteToMuse :: PandocMonad m
           => Int
           -> [Block]
           -> Muse m Doc
noteToMuse num note = do
  res <- hang (length marker) (text marker) <$>
    local (\env -> env { envInsideBlock = True
                       , envInlineStart = True
                       , envAfterSpace = True
                       }) (blockListToMuse note)
  return $ res <> blankline
  where
    marker = "[" ++ show num ++ "] "

-- | Return Muse representation of block and accumulated notes.
blockToMuseWithNotes :: PandocMonad m
                     => Block
                     -> Muse m Doc
blockToMuseWithNotes blk = do
  topLevel <- asks envTopLevel
  opts <- asks envOptions
  let hdrToMuse hdr@Header{} = do
        b <- blockToMuse hdr
        if topLevel && writerReferenceLocation opts == EndOfSection
          then do
            notes <- currentNotesToMuse
            return $ notes $+$ b
          else
            return b
      hdrToMuse b = blockToMuse b
  b <- hdrToMuse blk
  if topLevel && writerReferenceLocation opts == EndOfBlock
    then do
           notes <- currentNotesToMuse
           return $ b $+$ notes <> blankline
    else return b

-- | Escape special characters for Muse.
escapeString :: String -> String
escapeString s =
  "<verbatim>" ++
  substitute "</verbatim>" "<</verbatim><verbatim>/verbatim>" s ++
  "</verbatim>"

-- | Replace newlines with spaces
replaceNewlines :: String -> String
replaceNewlines ('\n':xs) = ' ':replaceNewlines xs
replaceNewlines (x:xs)    = x:replaceNewlines xs
replaceNewlines []        = []

startsWithMarker :: (Char -> Bool) -> String -> Bool
startsWithMarker f (' ':xs) = startsWithMarker f xs
startsWithMarker f (x:xs) =
  f x && (startsWithMarker f xs || startsWithDot xs)
  where
    startsWithDot ['.']     = True
    startsWithDot ('.':c:_) = isSpace c
    startsWithDot _         = False
startsWithMarker _ [] = False

-- | Escape special characters for Muse if needed.
containsFootnotes :: String -> Bool
containsFootnotes = p
  where p ('[':xs) = q xs || p xs
        p (_:xs)   = p xs
        p ""       = False
        q (x:xs)
          | x `elem` ("123456789"::String) = r xs || p xs
          | otherwise = p xs
        q [] = False
        r ('0':xs) = r xs || p xs
        r xs       = s xs || q xs || p xs
        s (']':_) = True
        s (_:xs)  = p xs
        s []      = False

-- | Return True if string should be escaped with <verbatim> tags
shouldEscapeString :: PandocMonad m
                   => String
                   -> Muse m Bool
shouldEscapeString s = do
  insideLink <- asks envInsideLinkDescription
  return $ null s ||
           any (`elem` ("#*<=|" :: String)) s ||
           "::" `isInfixOf` s ||
           "~~" `isInfixOf` s ||
           "[[" `isInfixOf` s ||
           ("]" `isInfixOf` s && insideLink) ||
           containsFootnotes s

conditionalEscapeString :: PandocMonad m
                        => String
                        -> Muse m String
conditionalEscapeString s = do
  shouldEscape <- shouldEscapeString s
  return $ if shouldEscape
             then escapeString s
             else s

-- Expand Math and Cite before normalizing inline list
preprocessInlineList :: PandocMonad m
                     => [Inline]
                     -> m [Inline]
preprocessInlineList (Math t str:xs) = (++) <$> texMathToInlines t str <*> preprocessInlineList xs
-- Amusewiki does not support <cite> tag,
-- and Emacs Muse citation support is limited
-- (https://www.gnu.org/software/emacs-muse/manual/html_node/Citations.html#Citation)
-- so just fallback to expanding inlines.
preprocessInlineList (Cite _  lst:xs) = (lst ++) <$> preprocessInlineList xs
preprocessInlineList (x:xs) = (x:) <$> preprocessInlineList xs
preprocessInlineList [] = return []

replaceSmallCaps :: Inline -> Inline
replaceSmallCaps (SmallCaps lst) = Emph lst
replaceSmallCaps x               = x

removeKeyValues :: Inline -> Inline
removeKeyValues (Code (i, cls, _) xs) = Code (i, cls, []) xs
-- Do not remove attributes from Link
-- Do not remove attributes, such as "width", from Image
removeKeyValues (Span (i, cls, _) xs) = Span (i, cls, []) xs
removeKeyValues x                     = x

normalizeInlineList :: [Inline] -> [Inline]
normalizeInlineList (Str "" : xs)
  = normalizeInlineList xs
normalizeInlineList (x : Str "" : xs)
  = normalizeInlineList (x:xs)
normalizeInlineList (Str x1 : Str x2 : xs)
  = normalizeInlineList $ Str (x1 ++ x2) : xs
normalizeInlineList (Emph x1 : Emph x2 : ils)
  = normalizeInlineList $ Emph (x1 ++ x2) : ils
normalizeInlineList (Strong x1 : Strong x2 : ils)
  = normalizeInlineList $ Strong (x1 ++ x2) : ils
normalizeInlineList (Strikeout x1 : Strikeout x2 : ils)
  = normalizeInlineList $ Strikeout (x1 ++ x2) : ils
normalizeInlineList (Superscript x1 : Superscript x2 : ils)
  = normalizeInlineList $ Superscript (x1 ++ x2) : ils
normalizeInlineList (Subscript x1 : Subscript x2 : ils)
  = normalizeInlineList $ Subscript (x1 ++ x2) : ils
normalizeInlineList (SmallCaps x1 : SmallCaps x2 : ils)
  = normalizeInlineList $ SmallCaps (x1 ++ x2) : ils
normalizeInlineList (Code _ x1 : Code _ x2 : ils)
  = normalizeInlineList $ Code nullAttr (x1 ++ x2) : ils
normalizeInlineList (RawInline f1 x1 : RawInline f2 x2 : ils) | f1 == f2
  = normalizeInlineList $ RawInline f1 (x1 ++ x2) : ils
-- Do not join Span's during normalization
normalizeInlineList (x:xs) = x : normalizeInlineList xs
normalizeInlineList [] = []

fixNotes :: [Inline] -> [Inline]
fixNotes []                            = []
fixNotes (Space : n@Note{} : rest)     = Str " " : n : fixNotes rest
fixNotes (SoftBreak : n@Note{} : rest) = Str " " : n : fixNotes rest
fixNotes (x:xs)                        = x : fixNotes xs

startsWithSpace :: [Inline] -> Bool
startsWithSpace (Space:_)     = True
startsWithSpace (SoftBreak:_) = True
startsWithSpace (Str s:_)     = stringStartsWithSpace s
startsWithSpace _             = False

endsWithSpace :: [Inline] -> Bool
endsWithSpace [Space]     = True
endsWithSpace [SoftBreak] = True
endsWithSpace [Str s]     = stringStartsWithSpace $ reverse s
endsWithSpace (_:xs)      = endsWithSpace xs
endsWithSpace []          = False

urlEscapeBrackets :: String -> String
urlEscapeBrackets (']':xs) = '%':'5':'D':urlEscapeBrackets xs
urlEscapeBrackets (x:xs)   = x:urlEscapeBrackets xs
urlEscapeBrackets []       = []

isHorizontalRule :: String -> Bool
isHorizontalRule s = length s >= 4 && all (== '-') s

stringStartsWithSpace :: String -> Bool
stringStartsWithSpace (x:_) = isSpace x
stringStartsWithSpace ""    = False

fixOrEscape :: Bool -> Inline -> Bool
fixOrEscape sp (Str "-") = sp
fixOrEscape sp (Str s@('-':x:_)) = (sp && isSpace x) || isHorizontalRule s
fixOrEscape sp (Str (";")) = not sp
fixOrEscape sp (Str (';':x:_)) = not sp && isSpace x
fixOrEscape _ (Str (">")) = True
fixOrEscape _ (Str ('>':x:_)) = isSpace x
fixOrEscape sp (Str s) = (sp && (startsWithMarker isDigit s ||
                                startsWithMarker isAsciiLower s ||
                                startsWithMarker isAsciiUpper s))
                         || stringStartsWithSpace s
fixOrEscape _ Space = True
fixOrEscape _ SoftBreak = True
fixOrEscape _ _ = False

inlineListStartsWithAlnum :: PandocMonad m
                          => [Inline]
                          -> Muse m Bool
inlineListStartsWithAlnum (Str s:_) = do
  esc <- shouldEscapeString s
  return $ esc || isAlphaNum (head s)
inlineListStartsWithAlnum _ = return False

-- | Convert list of Pandoc inline elements to Muse
renderInlineList :: PandocMonad m
                 => [Inline]
                 -> Muse m Doc
renderInlineList [] = do
  start <- asks envInlineStart
  pure $ if start then "<verbatim></verbatim>" else ""
renderInlineList (x:xs) = do
  start <- asks envInlineStart
  afterSpace <- asks envAfterSpace
  topLevel <- asks envTopLevel
  insideAsterisks <- asks envInsideAsterisks
  nearAsterisks <- asks envNearAsterisks
  useTags <- gets stUseTags
  alnumNext <- inlineListStartsWithAlnum xs
  let newUseTags = useTags || alnumNext
  modify $ \st -> st { stUseTags = newUseTags }

  r <- local (\env -> env { envInlineStart = False
                          , envInsideAsterisks = False
                          , envNearAsterisks = nearAsterisks || (null xs && insideAsterisks)
                          }) $ inlineToMuse x
  opts <- asks envOptions
  let isNewline = (x == SoftBreak && writerWrapText opts == WrapPreserve) || x == LineBreak
  lst' <- local (\env -> env { envInlineStart = isNewline
                             , envAfterSpace = x == Space || (not topLevel && isNewline)
                             , envNearAsterisks = False
                             }) $ renderInlineList xs
  if start && fixOrEscape afterSpace x
    then pure (text "<verbatim></verbatim>" <> r <> lst')
    else pure (r <> lst')

-- | Normalize and convert list of Pandoc inline elements to Muse.
inlineListToMuse :: PandocMonad m
                 => [Inline]
                 -> Muse m Doc
inlineListToMuse lst = do
  lst' <- normalizeInlineList . fixNotes <$> preprocessInlineList (map (removeKeyValues . replaceSmallCaps) lst)
  insideAsterisks <- asks envInsideAsterisks
  modify $ \st -> st { stUseTags = False } -- Previous character is likely a '>' or some other markup
  local (\env -> env { envNearAsterisks = insideAsterisks }) $ renderInlineList lst'

inlineListToMuse' :: PandocMonad m => [Inline] -> Muse m Doc
inlineListToMuse' lst = do
  topLevel <- asks envTopLevel
  afterSpace <- asks envAfterSpace
  local (\env -> env { envInlineStart = True
                     , envAfterSpace = afterSpace || not topLevel
                     }) $ inlineListToMuse lst

emphasis :: PandocMonad m => String -> String -> [Inline] -> Muse m Doc
emphasis b e lst = do
  contents <- local (\env -> env { envInsideAsterisks = inAsterisks }) $ inlineListToMuse lst
  modify $ \st -> st { stUseTags = useTags }
  return $ text b <> contents <> text e
  where inAsterisks = last b == '*' || head e == '*'
        useTags = last e /= '>'

-- | Convert Pandoc inline element to Muse.
inlineToMuse :: PandocMonad m
             => Inline
             -> Muse m Doc
inlineToMuse (Str str) = do
  escapedStr <- conditionalEscapeString $ replaceNewlines str
  let useTags = isAlphaNum $ last escapedStr -- escapedStr is never empty because empty strings are escaped
  modify $ \st -> st { stUseTags = useTags }
  return $ text escapedStr
inlineToMuse (Emph [Strong lst]) = do
  useTags <- gets stUseTags
  let lst' = normalizeInlineList lst
  if useTags
    then emphasis "<em>**" "**</em>" lst'
    else if null lst' || startsWithSpace lst' || endsWithSpace lst'
           then emphasis "*<strong>" "</strong>*" lst'
           else emphasis "***" "***" lst'
inlineToMuse (Emph lst) = do
  useTags <- gets stUseTags
  let lst' = normalizeInlineList lst
  if useTags || null lst' || startsWithSpace lst' || endsWithSpace lst'
    then emphasis "<em>" "</em>" lst'
    else emphasis "*" "*" lst'
inlineToMuse (Strong [Emph lst]) = do
  useTags <- gets stUseTags
  let lst' = normalizeInlineList lst
  if useTags
    then emphasis "<strong>*" "*</strong>" lst'
    else if null lst' || startsWithSpace lst' || endsWithSpace lst'
           then emphasis "**<em>" "</em>**" lst'
           else emphasis "***" "***" lst'
inlineToMuse (Strong lst) = do
  useTags <- gets stUseTags
  let lst' = normalizeInlineList lst
  if useTags || null lst' || startsWithSpace lst' || endsWithSpace lst'
    then emphasis "<strong>" "</strong>" lst'
    else emphasis "**" "**" lst'
inlineToMuse (Strikeout lst) = do
  contents <- inlineListToMuse lst
  modify $ \st -> st { stUseTags = False }
  return $ "<del>" <> contents <> "</del>"
inlineToMuse (Superscript lst) = do
  contents <- inlineListToMuse lst
  modify $ \st -> st { stUseTags = False }
  return $ "<sup>" <> contents <> "</sup>"
inlineToMuse (Subscript lst) = do
  contents <- inlineListToMuse lst
  modify $ \st -> st { stUseTags = False }
  return $ "<sub>" <> contents <> "</sub>"
inlineToMuse SmallCaps {} =
  fail "SmallCaps should be expanded before normalization"
inlineToMuse (Quoted SingleQuote lst) = do
  contents <- inlineListToMuse lst
  modify $ \st -> st { stUseTags = False }
  return $ "‘" <> contents <> "’"
inlineToMuse (Quoted DoubleQuote lst) = do
  contents <- inlineListToMuse lst
  modify $ \st -> st { stUseTags = False }
  return $ "“" <> contents <> "”"
inlineToMuse Cite {} =
  fail "Citations should be expanded before normalization"
inlineToMuse (Code _ str) = do
  useTags <- gets stUseTags
  modify $ \st -> st { stUseTags = False }
  return $ if useTags || null str || '=' `elem` str || isSpace (head str) || isSpace (last str)
             then "<code>" <> text (substitute "</code>" "<</code><code>/code>" str) <> "</code>"
             else "=" <> text str <> "="
inlineToMuse Math{} =
  fail "Math should be expanded before normalization"
inlineToMuse (RawInline (Format f) str) = do
  modify $ \st -> st { stUseTags = False }
  return $ "<literal style=\"" <> text f <> "\">" <> text str <> "</literal>"
inlineToMuse LineBreak = do
  oneline <- asks envOneLine
  modify $ \st -> st { stUseTags = False }
  return $ if oneline then "<br>" else "<br>" <> cr
inlineToMuse Space = do
  modify $ \st -> st { stUseTags = False }
  return space
inlineToMuse SoftBreak = do
  oneline <- asks envOneLine
  wrapText <- asks $ writerWrapText . envOptions
  modify $ \st -> st { stUseTags = False }
  return $ if not oneline && wrapText == WrapPreserve then cr else space
inlineToMuse (Link _ txt (src, _)) =
  case txt of
        [Str x] | escapeURI x == src -> do
             modify $ \st -> st { stUseTags = False }
             return $ "[[" <> text (escapeLink x) <> "]]"
        _ -> do contents <- local (\env -> env { envInsideLinkDescription = True }) $ inlineListToMuse txt
                modify $ \st -> st { stUseTags = False }
                return $ "[[" <> text (escapeLink src) <> "][" <> contents <> "]]"
  where escapeLink lnk = if isImageUrl lnk then "URL:" ++ urlEscapeBrackets lnk else urlEscapeBrackets lnk
        -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
        imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
        isImageUrl = (`elem` imageExtensions) . takeExtension
inlineToMuse (Image attr alt (source,'f':'i':'g':':':title)) =
  inlineToMuse (Image attr alt (source,title))
inlineToMuse (Image attr@(_, classes, _) inlines (source, title)) = do
  opts <- asks envOptions
  alt <- local (\env -> env { envInsideLinkDescription = True }) $ inlineListToMuse inlines
  title' <- if null title
            then if null inlines
                 then return ""
                 else return $ "[" <> alt <> "]"
            else do s <- local (\env -> env { envInsideLinkDescription = True }) $ conditionalEscapeString title
                    return $ "[" <> text s <> "]"
  let width = case dimension Width attr of
                Just (Percent x) | isEnabled Ext_amuse opts -> " " ++ show (round x :: Integer)
                _ -> ""
  let leftalign = if "align-left" `elem` classes
                  then " l"
                  else ""
  let rightalign = if "align-right" `elem` classes
                   then " r"
                   else ""
  modify $ \st -> st { stUseTags = False }
  return $ "[[" <> text (urlEscapeBrackets source ++ width ++ leftalign ++ rightalign) <> "]" <> title' <> "]"
inlineToMuse (Note contents) = do
  -- add to notes in state
  notes <- gets stNotes
  modify $ \st -> st { stNotes = contents:notes
                     , stUseTags = False
                     }
  n <- gets stNoteNum
  let ref = show $ n + length notes
  return $ "[" <> text ref <> "]"
inlineToMuse (Span (anchor,names,_) inlines) = do
  contents <- inlineListToMuse inlines
  let anchorDoc = if null anchor
                     then mempty
                     else text ('#':anchor) <> space
  modify $ \st -> st { stUseTags = False }
  return $ anchorDoc <> (if null inlines && not (null anchor)
                         then mempty
                         else (if null names
                               then "<class>"
                               else "<class name=\"" <> text (head names) <> "\">") <> contents <> "</class>")

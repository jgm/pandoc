{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.Ms
   Copyright   : Copyright (C) 2007-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to roff ms format.

TODO:

[ ] use base URL to construct absolute URLs from relative ones for external
    links
[ ] is there a better way to do strikeout?
[ ] tight/loose list distinction
-}

module Text.Pandoc.Writers.Ms ( writeMs ) where
import Control.Monad.State.Strict
import Data.Char (isLower, isUpper, ord)
import Data.List (intercalate, intersperse)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (escapeURIString, isAllowedInURI)
import Skylighting
import System.FilePath (takeExtension)
import Text.Pandoc.Asciify (toAsciiChar)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Writers.Roff
import Text.Printf (printf)
import Text.TeXMath (writeEqn)

-- | Convert Pandoc to Ms.
writeMs :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMs opts document =
  evalStateT (pandocToMs opts document) defaultWriterState

-- | Return roff ms representation of document.
pandocToMs :: PandocMonad m => WriterOptions -> Pandoc -> MS m Text
pandocToMs opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  metadata <- metaToContext opts
              (blockListToMs opts)
              (fmap chomp . inlineListToMs' opts)
              meta
  main <- blockListToMs opts blocks
  hasInlineMath <- gets stHasInlineMath
  let titleMeta = (escapeStr opts . stringify) $ docTitle meta
  let authorsMeta = map (escapeStr opts . stringify) $ docAuthors meta
  hasHighlighting <- gets stHighlighting
  let highlightingMacros = if hasHighlighting
                              then maybe mempty styleToMs $ writerHighlightStyle opts
                              else mempty

  let context = defField "body" main
              $ defField "has-inline-math" hasInlineMath
              $ defField "hyphenate" True
              $ defField "pandoc-version" pandocVersion
              $ defField "toc" (writerTableOfContents opts)
              $ defField "title-meta" titleMeta
              $ defField "author-meta" (T.intercalate "; " authorsMeta)
              $ defField "highlighting-macros" highlightingMacros metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

escapeStr :: WriterOptions -> Text -> Text
escapeStr opts =
  escapeString (if writerPreferAscii opts then AsciiOnly else AllowUTF8)

escapeUri :: Text -> Text
escapeUri = T.pack . escapeURIString (\c -> c /= '@' && isAllowedInURI c) . T.unpack

toSmallCaps :: WriterOptions -> Text -> Text
toSmallCaps opts s = case T.uncons s of
  Nothing -> ""
  Just (c, cs)
    | isLower c -> let (lowers,rest) = T.span isLower s
                   in  "\\s-2" <> escapeStr opts (T.toUpper lowers) <>
                       "\\s0" <> toSmallCaps opts rest
    | isUpper c -> let (uppers,rest) = T.span isUpper s
                   in  escapeStr opts uppers <> toSmallCaps opts rest
    | otherwise -> escapeStr opts (T.singleton c) <> toSmallCaps opts cs

-- We split inline lists into sentences, and print one sentence per
-- line.  roff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

blockToMs :: PandocMonad m
          => WriterOptions -- ^ Options
          -> Block         -- ^ Block element
          -> MS m (Doc Text)
blockToMs _ Null = return empty
blockToMs opts (Div (ident,cls,kvs) bs) = do
  let anchor = if T.null ident
                  then empty
                  else nowrap $
                         literal ".pdfhref M "
                         <> doubleQuotes (literal (toAscii ident))
  case cls of
    _ | "csl-entry" `elem` cls ->
       (".CSLENTRY" $$) . vcat <$> mapM (cslEntryToMs True opts) bs
      | "csl-bib-body" `elem` cls -> do
       res <- blockListToMs opts bs
       return $ anchor $$
                -- so that XP paragraphs are indented:
                ".nr PI 3n" $$
                -- space between entries
                ".de CSLENTRY" $$
                (case lookup "entry-spacing" kvs >>= safeRead of
                   Just n | n > (0 :: Int) -> ".sp"
                   _ -> mempty) $$
                ".." $$
                ".de CSLP" $$
                (if "hanging-indent" `elem` cls
                    then ".XP"
                    else ".LP") $$
                ".." $$
                res
    _ -> do
       setFirstPara
       res <- blockListToMs opts bs
       setFirstPara
       return $ anchor $$ res
blockToMs opts (Plain inlines) =
  liftM vcat $ mapM (inlineListToMs' opts) $ splitSentences inlines
blockToMs opts (Para [Image attr alt (src,_tit)])
  | let ext = takeExtension (T.unpack src) in (ext == ".ps" || ext == ".eps") = do
  let (mbW,mbH) = (inPoints opts <$> dimension Width attr,
                   inPoints opts <$> dimension Height attr)
  let sizeAttrs = case (mbW, mbH) of
                       (Just wp, Nothing) -> space <> doubleQuotes
                              (literal (tshow (floor wp :: Int) <> "p"))
                       (Just wp, Just hp) -> space <> doubleQuotes
                              (literal (tshow (floor wp :: Int) <> "p")) <>
                              space <>
                              doubleQuotes (literal (tshow (floor hp :: Int)))
                       _ -> empty
  capt <- inlineListToMs' opts alt
  return $ nowrap (literal ".PSPIC -C " <>
             doubleQuotes (literal (escapeStr opts src)) <>
             sizeAttrs) $$
           literal ".ce 1000" $$
           capt $$
           literal ".ce 0"
blockToMs opts (Para inlines) = do
  firstPara <- gets stFirstPara
  resetFirstPara
  contents <- liftM vcat $ mapM (inlineListToMs' opts) $
    splitSentences inlines
  return $ literal (if firstPara then ".LP" else ".PP") $$ contents
blockToMs _ b@(RawBlock f str)
  | f == Format "ms" = return $ literal str
  | otherwise        = do
      report $ BlockNotRendered b
      return empty
blockToMs _ HorizontalRule = do
  resetFirstPara
  return $ literal ".HLINE"
blockToMs opts (Header level (ident,classes,_) inlines) = do
  setFirstPara
  modify $ \st -> st{ stInHeader = True }
  contents <- inlineListToMs' opts $ map breakToSpace inlines
  modify $ \st -> st{ stInHeader = False }
  let (heading, secnum) = if writerNumberSections opts &&
                              "unnumbered" `notElem` classes
                             then (".NH", "\\*[SN]")
                             else (".SH", "")
  let anchor = if T.null ident
                  then empty
                  else nowrap $
                         literal ".pdfhref M "
                         <> doubleQuotes (literal (toAscii ident))
  let bookmark = literal ".pdfhref O " <> literal (tshow level <> " ") <>
                      doubleQuotes (literal $ secnum <>
                                      (if T.null secnum
                                          then ""
                                          else "  ") <>
                                      escapeStr opts (stringify inlines))
  let backlink = nowrap (literal ".pdfhref L -D " <>
       doubleQuotes (literal (toAscii ident)) <> space <> literal "\\") <> cr <>
       literal " -- "
  let tocEntry = if writerTableOfContents opts &&
                     level <= writerTOCDepth opts
                    then literal ".XS"
                         $$ backlink <> doubleQuotes (
                            nowrap (literal (T.replicate level "\t") <>
                             (if T.null secnum
                                 then empty
                                 else literal secnum <> literal "\\~\\~")
                              <> contents))
                         $$ literal ".XE"
                    else empty
  modify $ \st -> st{ stFirstPara = True }
  return $ (literal heading <> space <> literal (tshow level)) $$
           contents $$
           bookmark $$
           anchor $$
           tocEntry
blockToMs opts (CodeBlock attr str) = do
  hlCode <- highlightCode opts attr str
  setFirstPara
  return $
    literal ".IP" $$
    literal ".nf" $$
    literal "\\f[C]" $$
    ((case T.uncons str of
      Just ('.',_) -> literal "\\&"
      _            -> mempty) <> hlCode) $$
    literal "\\f[]" $$
    literal ".fi"
blockToMs opts (LineBlock ls) = do
  setFirstPara  -- use .LP, see #5588
  blockToMs opts $ Para $ intercalate [LineBreak] ls
blockToMs opts (BlockQuote blocks) = do
  setFirstPara
  contents <- blockListToMs opts blocks
  setFirstPara
  return $ literal ".QS" $$ contents $$ literal ".QE"
blockToMs opts (Table _ blkCapt specs thead tbody tfoot) =
  let (caption, alignments, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
      aligncode AlignLeft    = "l"
      aligncode AlignRight   = "r"
      aligncode AlignCenter  = "c"
      aligncode AlignDefault = "l"
  in do
  caption' <- inlineListToMs' opts caption
  let isSimple = all (== 0) widths
  let totalWidth = 70
  -- 78n default width - 8n indent = 70n
  let coldescriptions = literal $ T.unwords
                        (zipWith (\align width -> aligncode align <>
                                    if width == 0
                                       then ""
                                       else T.pack $
                                              printf "w(%0.1fn)"
                                              (totalWidth * width))
                        alignments widths) <> "."
  colheadings <- mapM (blockListToMs opts) headers
  let makeRow cols = literal "T{" $$
                     vcat (intersperse (literal "T}\tT{") cols) $$
                     literal "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (\(cell, w) ->
                                   (if isSimple
                                       then id
                                       else (literal (".nr LL " <>
                                              T.pack (printf "%0.1fn"
                                                (w * totalWidth))) $$)) <$>
                                   blockListToMs opts cell) (zip row widths)
                         return $ makeRow cols) rows
  setFirstPara
  return $ literal ".PP" $$ caption' $$
           literal ".na" $$ -- we don't want justification in table cells
           (if isSimple
               then ""
               else ".nr LLold \\n[LL]") $$
           literal ".TS" $$ literal "delim(@@) tab(\t);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ literal ".TE" $$
           (if isSimple
               then ""
               else ".nr LL \\n[LLold]") $$
           literal ".ad"

blockToMs opts (BulletList items) = do
  contents <- mapM (bulletListItemToMs opts) items
  setFirstPara
  return (vcat contents)
blockToMs opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 2 + maybe 0 maximum (nonEmpty (map T.length markers))
  contents <- mapM (\(num, item) -> orderedListItemToMs opts num indent item) $
              zip markers items
  setFirstPara
  return (vcat contents)
blockToMs opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToMs opts) items
  setFirstPara
  return (vcat contents)

-- | Convert bullet list item (list of blocks) to ms.
bulletListItemToMs :: PandocMonad m => WriterOptions -> [Block] -> MS m (Doc Text)
bulletListItemToMs _ [] = return empty
bulletListItemToMs opts (Para first:rest) =
  bulletListItemToMs opts (Plain first:rest)
bulletListItemToMs opts (Plain first:rest) = do
  first' <- blockToMs opts (Plain first)
  rest' <- blockListToMs opts rest
  let first'' = literal ".IP \\[bu] 3" $$ first'
  let rest''  = if null rest
                   then empty
                   else literal ".RS 3" $$ rest' $$ literal ".RE"
  return (first'' $$ rest'')
bulletListItemToMs opts (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  return $ literal "\\[bu] .RS 3" $$ first' $$ rest' $$ literal ".RE"

-- | Convert ordered list item (a list of blocks) to ms.
orderedListItemToMs :: PandocMonad m
                    => WriterOptions -- ^ options
                    -> Text   -- ^ order marker for list item
                    -> Int      -- ^ number of spaces to indent
                    -> [Block]  -- ^ list item (list of blocks)
                    -> MS m (Doc Text)
orderedListItemToMs _ _ _ [] = return empty
orderedListItemToMs opts num indent (Para first:rest) =
  orderedListItemToMs opts num indent (Plain first:rest)
orderedListItemToMs opts num indent (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  let num' = T.pack $ printf ("%" <> show (indent - 1) <> "s") num
  let first'' = literal (".IP \"" <> num' <> "\" " <> tshow indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else literal ".RS " <> literal (tshow indent) $$
                         rest' $$ literal ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to ms.
definitionListItemToMs :: PandocMonad m
                       => WriterOptions
                       -> ([Inline],[[Block]])
                       -> MS m (Doc Text)
definitionListItemToMs opts (label, defs) = do
  labelText <- withFontFeature 'B' $
                 inlineListToMs' opts $ map breakToSpace label
  contents <- if null defs
                 then return empty
                 else liftM vcat $ forM defs $ \blocks -> do
                        let (first, rest) = case blocks of
                              (Para x:y) -> (Plain x,y)
                              (x:y)      -> (x,y)
                              []         -> (Plain [], [])
                                               -- should not happen
                        rest' <- liftM vcat $
                                  mapM (\item -> blockToMs opts item) rest
                        first' <- blockToMs opts first
                        return $ first' $$ literal ".RS 3" $$ rest' $$ literal ".RE"
  return $ nowrap (literal ".IP " <> doubleQuotes labelText <> " 3") $$
           contents

-- | Convert list of Pandoc block elements to ms.
blockListToMs :: PandocMonad m
              => WriterOptions -- ^ Options
              -> [Block]       -- ^ List of block elements
              -> MS m (Doc Text)
blockListToMs opts blocks =
  vcat <$> mapM (blockToMs opts) blocks

-- | Convert list of Pandoc inline elements to ms.
inlineListToMs :: PandocMonad m => WriterOptions -> [Inline] -> MS m (Doc Text)
-- if list starts with ., insert a zero-width character \& so it
-- won't be interpreted as markup if it falls at the beginning of a line.
inlineListToMs opts lst = hcat <$> mapM (inlineToMs opts) lst

-- This version to be used when there is no further inline content;
-- forces a note at the end.
inlineListToMs' :: PandocMonad m => WriterOptions -> [Inline] -> MS m (Doc Text)
inlineListToMs' opts lst = do
  x <- hcat <$> mapM (inlineToMs opts) lst
  y <- handleNotes opts empty
  return $ x <> y

-- | Convert Pandoc inline element to ms.
inlineToMs :: PandocMonad m => WriterOptions -> Inline -> MS m (Doc Text)
inlineToMs opts (Span _ ils) = inlineListToMs opts ils
inlineToMs opts (Emph lst) =
  withFontFeature 'I' (inlineListToMs opts lst)
inlineToMs opts (Underline lst) =
  inlineToMs opts (Emph lst)
inlineToMs opts (Strong lst) =
  withFontFeature 'B' (inlineListToMs opts lst)
inlineToMs opts (Strikeout lst) = do
  contents <- inlineListToMs opts lst
  -- we use grey color instead of strikeout, which seems quite
  -- hard to do in roff for arbitrary bits of text
  return $ literal "\\m[strikecolor]" <> contents <> literal "\\m[]"
inlineToMs opts (Superscript lst) = do
  contents <- inlineListToMs opts lst
  return $ literal "\\*{" <> contents <> literal "\\*}"
inlineToMs opts (Subscript lst) = do
  contents <- inlineListToMs opts lst
  return $ literal "\\*<" <> contents <> literal "\\*>"
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
  return $ literal "\\[lq]" <> contents <> literal "\\[rq]"
inlineToMs opts (Cite _ lst) =
  inlineListToMs opts lst
inlineToMs opts (Code attr str) = do
  hlCode <- highlightCode opts attr str
  withFontFeature 'C' (return hlCode)
inlineToMs opts (Str str) = do
  let shim = case T.uncons str of
                  Just ('.',_) -> afterBreak "\\&"
                  _            -> empty
  smallcaps <- gets stSmallCaps
  if smallcaps
     then return $ shim <> literal (toSmallCaps opts str)
     else return $ shim <> literal (escapeStr opts str)
inlineToMs opts (Math InlineMath str) = do
  modify $ \st -> st{ stHasInlineMath = True }
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> inlineToMs opts il
       Right r -> return $ literal "@" <> literal r <> literal "@"
inlineToMs opts (Math DisplayMath str) = do
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> do
         contents <- inlineToMs opts il
         return $ cr <> literal ".RS 3" $$ contents $$ literal ".RE"
       Right r -> return $
            cr <> literal ".EQ" $$ literal r $$ literal ".EN" <> cr
inlineToMs _ il@(RawInline f str)
  | f == Format "ms" = return $ literal str
  | otherwise        = do
    report $ InlineNotRendered il
    return empty
inlineToMs _ LineBreak = return $ cr <> literal ".br" <> cr
inlineToMs opts SoftBreak =
  handleNotes opts $
    case writerWrapText opts of
         WrapAuto     -> space
         WrapNone     -> space
         WrapPreserve -> cr
inlineToMs opts Space = handleNotes opts space
inlineToMs opts (Link _ txt (T.uncons -> Just ('#',ident), _)) = do
  -- internal link
  contents <- inlineListToMs' opts $ map breakToSpace txt
  return $ literal "\\c" <> cr <> nowrap (literal ".pdfhref L -D " <>
       doubleQuotes (literal (toAscii ident)) <> literal " -A " <>
       doubleQuotes (literal "\\c") <> space <> literal "\\") <> cr <>
       literal " -- " <> doubleQuotes (nowrap contents) <> cr <> literal "\\&"
inlineToMs opts (Link _ txt (src, _)) = do
  -- external link
  contents <- inlineListToMs' opts $ map breakToSpace txt
  return $ literal "\\c" <> cr <> nowrap (literal ".pdfhref W -D " <>
       doubleQuotes (literal (escapeUri src)) <> literal " -A " <>
       doubleQuotes (literal "\\c") <> space <> literal "\\") <> cr <>
       literal " -- " <> doubleQuotes (nowrap contents) <> cr <> literal "\\&"
inlineToMs opts (Image _ alternate (_, _)) =
  return $ char '[' <> literal "IMAGE: " <>
           literal (escapeStr opts (stringify alternate))
             <> char ']'
inlineToMs _ (Note contents) = do
  modify $ \st -> st{ stNotes = contents : stNotes st }
  return $ literal "\\**"

cslEntryToMs :: PandocMonad m
             => Bool
             -> WriterOptions
             -> Block
             -> MS m (Doc Text)
cslEntryToMs atStart opts (Para xs) =
  case xs of
    (Span ("",["csl-left-margin"],[]) lils :
      rest@(Span ("",["csl-right-inline"],[]) _ : _))
      -> do lils' <- inlineListToMs' opts lils
            ((cr <> literal ".IP " <>
              doubleQuotes (nowrap lils') <>
              literal " 5") $$)
                <$> cslEntryToMs False opts (Para rest)
    (Span ("",["csl-block"],[]) ils : rest)
      -> ((cr <> literal ".LP") $$)
                <$> cslEntryToMs False opts (Para (ils ++ rest))
    (Span ("",["csl-left-margin"],[]) ils : rest)
      -> ((cr <> literal ".LP") $$)
              <$> cslEntryToMs False opts (Para (ils ++ rest))
    (Span ("",["csl-indented"],[]) ils : rest)
      -> ((cr <> literal ".LP") $$)
              <$> cslEntryToMs False opts (Para (ils ++ rest))
    _ | atStart
         -> (".CSLP" $$) <$> cslEntryToMs False opts (Para xs)
      | otherwise
         -> case xs of
           [] -> return mempty
           (x:rest) -> (<>) <$> inlineToMs opts x
                            <*> cslEntryToMs False opts (Para rest)
cslEntryToMs _ opts x = blockToMs opts x


handleNotes :: PandocMonad m => WriterOptions -> Doc Text -> MS m (Doc Text)
handleNotes opts fallback = do
  notes <- gets stNotes
  if null notes
     then return fallback
     else do
       modify $ \st -> st{ stNotes = [] }
       vcat <$> mapM (handleNote opts) notes

handleNote :: PandocMonad m => WriterOptions -> Note -> MS m (Doc Text)
handleNote opts bs = do
  -- don't start with Paragraph or we'll get a spurious blank
  -- line after the note ref:
  let bs' = case bs of
                 (Para ils : rest) -> Plain ils : rest
                 _                 -> bs
  contents <- blockListToMs opts bs'
  return $ cr <> literal ".FS" $$ contents $$ literal ".FE" <> cr

setFirstPara :: PandocMonad m => MS m ()
setFirstPara = modify $ \st -> st{ stFirstPara = True }

resetFirstPara :: PandocMonad m => MS m ()
resetFirstPara = modify $ \st -> st{ stFirstPara = False }

breakToSpace :: Inline -> Inline
breakToSpace SoftBreak = Space
breakToSpace LineBreak = Space
breakToSpace x         = x

-- Highlighting

styleToMs :: Style -> Doc Text
styleToMs sty = vcat $ colordefs <> map (toMacro sty) alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        colordefs = map toColorDef allcolors
        toColorDef c = literal (".defcolor " <>
            hexColor c <> " rgb #" <> hexColor c)
        allcolors = catMaybes $ ordNub $
          [defaultColor sty, backgroundColor sty,
           lineNumberColor sty, lineNumberBackgroundColor sty] <>
           concatMap (colorsForToken. snd) (Map.toList (tokenStyles sty))
        colorsForToken ts = [tokenColor ts, tokenBackground ts]

hexColor :: Color -> Text
hexColor (RGB r g b) = T.pack $ printf "%02x%02x%02x" r g b

toMacro :: Style -> TokenType -> Doc Text
toMacro sty toktype =
  nowrap (literal ".ds " <> literal (tshow toktype) <> literal " " <>
            setbg <> setcolor <> setfont <>
            literal "\\\\$1" <>
            resetfont <> resetcolor <> resetbg)
  where setcolor = maybe empty fgcol tokCol
        resetcolor = maybe empty (const $ literal "\\\\m[]") tokCol
        setbg = empty -- maybe empty bgcol tokBg
        resetbg = empty -- maybe empty (const $ text "\\\\M[]") tokBg
        fgcol c = literal $ "\\\\m[" <> hexColor c <> "]"
        -- bgcol c = literal $ "\\\\M[" <> hexColor c <> "]"
        setfont = if tokBold || tokItalic
                     then literal $ T.pack $ "\\\\f[C" <> ['B' | tokBold] <>
                          ['I' | tokItalic] <> "]"
                     else empty
        resetfont = if tokBold || tokItalic
                       then literal "\\\\f[C]"
                       else empty
        tokSty = Map.lookup toktype (tokenStyles sty)
        tokCol = (tokSty >>= tokenColor) `mplus` defaultColor sty
        -- tokBg  = (tokSty >>= tokenBackground) `mplus` backgroundColor sty
        tokBold = maybe False tokenBold tokSty
        tokItalic = maybe False tokenItalic tokSty
        -- tokUnderline = fromMaybe False (tokSty >>= tokUnderline)
        -- lnColor = lineNumberColor sty
        -- lnBkgColor = lineNumberBackgroundColor sty

msFormatter :: WriterOptions -> FormatOptions -> [SourceLine] -> Doc Text
msFormatter opts _fmtopts =
  literal . T.intercalate "\n" . map fmtLine
 where
  fmtLine = mconcat . map fmtToken
  fmtToken (toktype, tok) =
    "\\*[" <> tshow toktype <> " \"" <> escapeStr opts tok <> "\"]"

highlightCode :: PandocMonad m => WriterOptions -> Attr -> Text -> MS m (Doc Text)
highlightCode opts attr str =
  case highlight (writerSyntaxMap opts) (msFormatter opts) attr str of
         Left msg -> do
           unless (T.null msg) $ report $ CouldNotHighlight msg
           return $ literal (escapeStr opts str)
         Right h -> do
           modify (\st -> st{ stHighlighting = True })
           return h

-- This is used for PDF anchors.
toAscii :: Text -> Text
toAscii = T.concatMap
  (\c -> case toAsciiChar c of
              Nothing -> "_u" <> tshow (ord c) <> "_"
              Just '/' -> "_u" <> tshow (ord c) <> "_" -- see #4515
              Just c' -> T.singleton c')

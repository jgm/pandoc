{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Writers.Ms
   Copyright   : Copyright (C) 2007-2019 John MacFarlane
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
import Prelude
import Control.Monad.State.Strict
import Data.Char (isLower, isUpper, toUpper, ord)
import Data.List (intercalate, intersperse)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (escapeURIString, isAllowedInURI)
import Skylighting
import System.FilePath (takeExtension)
import Text.Pandoc.Asciify (toAsciiChar)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Legacy.Definition -- TODO text: remove Legacy
import Text.Pandoc.Legacy.Highlighting
import Text.Pandoc.ImageSize
import Text.Pandoc.Legacy.Logging
import Text.Pandoc.Legacy.Options
import Text.DocLayout
import Text.Pandoc.Legacy.Shared -- TODO text: remove Legacy
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Writers.Roff
import Text.Printf (printf)
-- import Text.TeXMath (writeEqn) TODO text: restore

-- TODO text: remove
import qualified Text.TeXMath as TM
writeEqn :: TM.DisplayType -> [TM.Exp] -> String
writeEqn dt = T.unpack . TM.writeEqn dt
--

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
                              then case writerHighlightStyle opts of
                                        Nothing  -> mempty
                                        Just sty -> styleToMs sty
                              else mempty

  let context = defField "body" main
              $ defField "has-inline-math" hasInlineMath
              $ defField "hyphenate" True
              $ defField "pandoc-version" (T.pack pandocVersion)
              $ defField "toc" (writerTableOfContents opts)
              $ defField "title-meta" (T.pack titleMeta)
              $ defField "author-meta" (T.pack $ intercalate "; " authorsMeta)
              $ defField "highlighting-macros" highlightingMacros metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

escapeStr :: WriterOptions -> String -> String
escapeStr opts =
  escapeString (if writerPreferAscii opts then AsciiOnly else AllowUTF8)

escapeUri :: String -> String
escapeUri = escapeURIString (\c -> c /= '@' && isAllowedInURI c)

toSmallCaps :: WriterOptions -> String -> String
toSmallCaps _ [] = []
toSmallCaps opts (c:cs)
  | isLower c = let (lowers,rest) = span isLower (c:cs)
                in  "\\s-2" ++ escapeStr opts (map toUpper lowers) ++
                    "\\s0" ++ toSmallCaps opts rest
  | isUpper c = let (uppers,rest) = span isUpper (c:cs)
                in  escapeStr opts uppers ++ toSmallCaps opts rest
  | otherwise = escapeStr opts [c] ++ toSmallCaps opts cs

-- We split inline lists into sentences, and print one sentence per
-- line.  roff treats the line-ending period differently.
-- See http://code.google.com/p/pandoc/issues/detail?id=148.

blockToMs :: PandocMonad m
          => WriterOptions -- ^ Options
          -> Block         -- ^ Block element
          -> MS m (Doc Text)
blockToMs _ Null = return empty
blockToMs opts (Div (ident,_,_) bs) = do
  let anchor = if null ident
                  then empty
                  else nowrap $
                         text ".pdfhref M "
                         <> doubleQuotes (text (toAscii ident))
  setFirstPara
  res <- blockListToMs opts bs
  setFirstPara
  return $ anchor $$ res
blockToMs opts (Plain inlines) =
  liftM vcat $ mapM (inlineListToMs' opts) $ splitSentences inlines
blockToMs opts (Para [Image attr alt (src,_tit)])
  | let ext = takeExtension src in (ext == ".ps" || ext == ".eps") = do
  let (mbW,mbH) = (inPoints opts <$> dimension Width attr,
                   inPoints opts <$> dimension Height attr)
  let sizeAttrs = case (mbW, mbH) of
                       (Just wp, Nothing) -> space <> doubleQuotes
                              (text (show (floor wp :: Int) ++ "p"))
                       (Just wp, Just hp) -> space <> doubleQuotes
                              (text (show (floor wp :: Int) ++ "p")) <>
                              space <>
                              doubleQuotes (text (show (floor hp :: Int)))
                       _ -> empty
  capt <- inlineListToMs' opts alt
  return $ nowrap (text ".PSPIC -C " <>
             doubleQuotes (text (escapeStr opts src)) <>
             sizeAttrs) $$
           text ".ce 1000" $$
           capt $$
           text ".ce 0"
blockToMs opts (Para inlines) = do
  firstPara <- gets stFirstPara
  resetFirstPara
  contents <- liftM vcat $ mapM (inlineListToMs' opts) $
    splitSentences inlines
  return $ text (if firstPara then ".LP" else ".PP") $$ contents
blockToMs _ b@(RawBlock f str)
  | f == Format "ms" = return $ text str
  | otherwise        = do
      report $ BlockNotRendered b
      return empty
blockToMs _ HorizontalRule = do
  resetFirstPara
  return $ text ".HLINE"
blockToMs opts (Header level (ident,classes,_) inlines) = do
  setFirstPara
  modify $ \st -> st{ stInHeader = True }
  contents <- inlineListToMs' opts $ map breakToSpace inlines
  modify $ \st -> st{ stInHeader = False }
  let (heading, secnum) = if writerNumberSections opts &&
                              "unnumbered" `notElem` classes
                             then (".NH", "\\*[SN]")
                             else (".SH", "")
  let anchor = if null ident
                  then empty
                  else nowrap $
                         text ".pdfhref M "
                         <> doubleQuotes (text (toAscii ident))
  let bookmark = text ".pdfhref O " <> text (show level ++ " ") <>
                      doubleQuotes (text $ secnum ++
                                      (if null secnum
                                          then ""
                                          else "  ") ++
                                      escapeStr opts (stringify inlines))
  let backlink = nowrap (text ".pdfhref L -D " <>
       doubleQuotes (text (toAscii ident)) <> space <> text "\\") <> cr <>
       text " -- "
  let tocEntry = if writerTableOfContents opts &&
                     level <= writerTOCDepth opts
                    then text ".XS"
                         $$ backlink <> doubleQuotes (
                            nowrap (text (replicate level '\t') <>
                             (if null secnum
                                 then empty
                                 else text secnum <> text "\\~\\~")
                              <> contents))
                         $$ text ".XE"
                    else empty
  modify $ \st -> st{ stFirstPara = True }
  return $ (text heading <> space <> text (show level)) $$
           contents $$
           bookmark $$
           anchor $$
           tocEntry
blockToMs opts (CodeBlock attr str) = do
  hlCode <- highlightCode opts attr str
  setFirstPara
  return $
    text ".IP" $$
    text ".nf" $$
    text "\\f[C]" $$
    hlCode $$
    text "\\f[]" $$
    text ".fi"
blockToMs opts (LineBlock ls) = do
  setFirstPara  -- use .LP, see #5588
  blockToMs opts $ Para $ intercalate [LineBreak] ls
blockToMs opts (BlockQuote blocks) = do
  setFirstPara
  contents <- blockListToMs opts blocks
  setFirstPara
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
  let coldescriptions = text $ unwords
                        (zipWith (\align width -> aligncode align ++ width)
                        alignments iwidths) ++ "."
  colheadings <- mapM (blockListToMs opts) headers
  let makeRow cols = text "T{" $$
                     vcat (intersperse (text "T}\tT{") cols) $$
                     text "T}"
  let colheadings' = if all null headers
                        then empty
                        else makeRow colheadings $$ char '_'
  body <- mapM (\row -> do
                         cols <- mapM (blockListToMs opts) row
                         return $ makeRow cols) rows
  setFirstPara
  return $ text ".PP" $$ caption' $$
           text ".TS" $$ text "delim(@@) tab(\t);" $$ coldescriptions $$
           colheadings' $$ vcat body $$ text ".TE"

blockToMs opts (BulletList items) = do
  contents <- mapM (bulletListItemToMs opts) items
  setFirstPara
  return (vcat contents)
blockToMs opts (OrderedList attribs items) = do
  let markers = take (length items) $ orderedListMarkers attribs
  let indent = 2 +
                     maximum (map length markers)
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
  let first'' = text ".IP \\[bu] 3" $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS 3" $$ rest' $$ text ".RE"
  return (first'' $$ rest'')
bulletListItemToMs opts (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  return $ text "\\[bu] .RS 3" $$ first' $$ rest' $$ text ".RE"

-- | Convert ordered list item (a list of blocks) to ms.
orderedListItemToMs :: PandocMonad m
                    => WriterOptions -- ^ options
                    -> String   -- ^ order marker for list item
                    -> Int      -- ^ number of spaces to indent
                    -> [Block]  -- ^ list item (list of blocks)
                    -> MS m (Doc Text)
orderedListItemToMs _ _ _ [] = return empty
orderedListItemToMs opts num indent (Para first:rest) =
  orderedListItemToMs opts num indent (Plain first:rest)
orderedListItemToMs opts num indent (first:rest) = do
  first' <- blockToMs opts first
  rest' <- blockListToMs opts rest
  let num' = printf ("%" ++ show (indent - 1) ++ "s") num
  let first'' = text (".IP \"" ++ num' ++ "\" " ++ show indent) $$ first'
  let rest''  = if null rest
                   then empty
                   else text ".RS " <> text (show indent) $$
                         rest' $$ text ".RE"
  return $ first'' $$ rest''

-- | Convert definition list item (label, list of blocks) to ms.
definitionListItemToMs :: PandocMonad m
                       => WriterOptions
                       -> ([Inline],[[Block]])
                       -> MS m (Doc Text)
definitionListItemToMs opts (label, defs) = do
  labelText <- inlineListToMs' opts $ map breakToSpace label
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
                        return $ first' $$ text ".RS" $$ rest' $$ text ".RE"
  return $ nowrap (text ".IP " <> doubleQuotes labelText) $$ contents

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
inlineToMs opts (Strong lst) =
  withFontFeature 'B' (inlineListToMs opts lst)
inlineToMs opts (Strikeout lst) = do
  contents <- inlineListToMs opts lst
  -- we use grey color instead of strikeout, which seems quite
  -- hard to do in roff for arbitrary bits of text
  return $ text "\\m[strikecolor]" <> contents <> text "\\m[]"
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
inlineToMs opts (Code attr str) = do
  hlCode <- highlightCode opts attr str
  withFontFeature 'C' (return hlCode)
inlineToMs opts (Str str) = do
  let shim = case str of
                  '.':_ -> afterBreak (T.pack "\\&")
                  _     -> empty
  smallcaps <- gets stSmallCaps
  if smallcaps
     then return $ shim <> text (toSmallCaps opts str)
     else return $ shim <> text (escapeStr opts str)
inlineToMs opts (Math InlineMath str) = do
  modify $ \st -> st{ stHasInlineMath = True }
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> inlineToMs opts il
       Right r -> return $ text "@" <> text r <> text "@"
inlineToMs opts (Math DisplayMath str) = do
  res <- convertMath writeEqn InlineMath str
  case res of
       Left il -> do
         contents <- inlineToMs opts il
         return $ cr <> text ".RS" $$ contents $$ text ".RE"
       Right r -> return $
            cr <> text ".EQ" $$ text r $$ text ".EN" <> cr
inlineToMs _ il@(RawInline f str)
  | f == Format "ms" = return $ text str
  | otherwise        = do
    report $ InlineNotRendered il
    return empty
inlineToMs _ LineBreak = return $ cr <> text ".br" <> cr
inlineToMs opts SoftBreak =
  handleNotes opts $
    case writerWrapText opts of
         WrapAuto     -> space
         WrapNone     -> space
         WrapPreserve -> cr
inlineToMs opts Space = handleNotes opts space
inlineToMs opts (Link _ txt ('#':ident, _)) = do
  -- internal link
  contents <- inlineListToMs' opts $ map breakToSpace txt
  return $ text "\\c" <> cr <> nowrap (text ".pdfhref L -D " <>
       doubleQuotes (text (toAscii ident)) <> text " -A " <>
       doubleQuotes (text "\\c") <> space <> text "\\") <> cr <>
       text " -- " <> doubleQuotes (nowrap contents) <> cr <> text "\\&"
inlineToMs opts (Link _ txt (src, _)) = do
  -- external link
  contents <- inlineListToMs' opts $ map breakToSpace txt
  return $ text "\\c" <> cr <> nowrap (text ".pdfhref W -D " <>
       doubleQuotes (text (escapeUri src)) <> text " -A " <>
       doubleQuotes (text "\\c") <> space <> text "\\") <> cr <>
       text " -- " <> doubleQuotes (nowrap contents) <> cr <> text "\\&"
inlineToMs opts (Image _ alternate (_, _)) =
  return $ char '[' <> text "IMAGE: " <>
           text (escapeStr opts (stringify alternate))
             <> char ']'
inlineToMs _ (Note contents) = do
  modify $ \st -> st{ stNotes = contents : stNotes st }
  return $ text "\\**"

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
  return $ cr <> text ".FS" $$ contents $$ text ".FE" <> cr

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
styleToMs sty = vcat $ colordefs ++ map (toMacro sty) alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        colordefs = map toColorDef allcolors
        toColorDef c = text (".defcolor " ++
            hexColor c ++ " rgb #" ++ hexColor c)
        allcolors = catMaybes $ ordNub $
          [defaultColor sty, backgroundColor sty,
           lineNumberColor sty, lineNumberBackgroundColor sty] ++
           concatMap (colorsForToken. snd) (Map.toList (tokenStyles sty))
        colorsForToken ts = [tokenColor ts, tokenBackground ts]

hexColor :: Color -> String
hexColor (RGB r g b) = printf "%02x%02x%02x" r g b

toMacro :: Style -> TokenType -> Doc Text
toMacro sty toktype =
  nowrap (text ".ds " <> text (show toktype) <> text " " <>
            setbg <> setcolor <> setfont <>
            text "\\\\$1" <>
            resetfont <> resetcolor <> resetbg)
  where setcolor = maybe empty fgcol tokCol
        resetcolor = maybe empty (const $ text "\\\\m[]") tokCol
        setbg = empty -- maybe empty bgcol tokBg
        resetbg = empty -- maybe empty (const $ text "\\\\M[]") tokBg
        fgcol c = text $ "\\\\m[" ++ hexColor c ++ "]"
        -- bgcol c = text $ "\\\\M[" ++ hexColor c ++ "]"
        setfont = if tokBold || tokItalic
                     then text $ "\\\\f[C" ++ ['B' | tokBold] ++
                          ['I' | tokItalic] ++ "]"
                     else empty
        resetfont = if tokBold || tokItalic
                       then text "\\\\f[C]"
                       else empty
        tokSty = Map.lookup toktype (tokenStyles sty)
        tokCol = (tokSty >>= tokenColor) `mplus` defaultColor sty
        -- tokBg  = (tokSty >>= tokenBackground) `mplus` backgroundColor sty
        tokBold = fromMaybe False (tokenBold <$> tokSty)
        tokItalic = fromMaybe False (tokenItalic <$> tokSty)
        -- tokUnderline = fromMaybe False (tokSty >>= tokUnderline)
        -- lnColor = lineNumberColor sty
        -- lnBkgColor = lineNumberBackgroundColor sty

msFormatter :: WriterOptions -> FormatOptions -> [SourceLine] -> Doc Text
msFormatter opts _fmtopts =
  vcat . map fmtLine
  where fmtLine = hcat . map fmtToken
        fmtToken (toktype, tok) = text "\\*" <>
           brackets (text (show toktype) <> text " \""
             <> text (escapeStr opts (T.unpack tok)) <> text "\"")

highlightCode :: PandocMonad m => WriterOptions -> Attr -> String -> MS m (Doc Text)
highlightCode opts attr str =
  case highlight (writerSyntaxMap opts) (msFormatter opts) attr str of
         Left msg -> do
           unless (null msg) $ report $ CouldNotHighlight msg
           return $ text (escapeStr opts str)
         Right h -> do
           modify (\st -> st{ stHighlighting = True })
           return h

-- This is used for PDF anchors.
toAscii :: String -> String
toAscii = concatMap
  (\c -> case toAsciiChar c of
              Nothing -> '_':'u':show (ord c) ++ "_"
              Just '/' -> '_':'u':show (ord c) ++ "_" -- see #4515
              Just c' -> [c'])

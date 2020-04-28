{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.DokuWiki
   Copyright   : Copyright (C) 2008-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Clare Macrae <clare.macrae@googlemail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to DokuWiki markup.

DokuWiki:  <https://www.dokuwiki.org/dokuwiki>
-}

{-
    [ ] Implement nested blockquotes (currently only ever does one level)
    [x] Implement alignment of text in tables
    [ ] Implement comments
    [ ] Work through the Dokuwiki spec, and check I've not missed anything out
    [ ] Remove dud/duplicate code
-}

module Text.Pandoc.Writers.DokuWiki ( writeDokuWiki ) where
import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Default (Default (..))
import Data.List (intersect, transpose)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options (WrapOption (..), WriterOptions (writerTableOfContents, writerTemplate, writerWrapText))
import Text.Pandoc.Shared (camelCaseToHyphenated, escapeURI, isURI, linesToPara,
                           removeFormatting, trimr, tshow)
import Text.Pandoc.Templates (renderTemplate)
import Text.DocLayout (render, literal)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable)

data WriterState = WriterState {
  }

data WriterEnvironment = WriterEnvironment {
    stIndent      :: Text          -- Indent after the marker at the beginning of list items
  , stUseTags     :: Bool            -- True if we should use HTML tags because we're in a complex list
  , stBackSlashLB :: Bool     -- True if we should produce formatted strings with newlines (as in a table cell)
  }

instance Default WriterState where
  def = WriterState {}

instance Default WriterEnvironment where
  def = WriterEnvironment { stIndent = ""
                          , stUseTags = False
                          , stBackSlashLB = False }

type DokuWiki m = ReaderT WriterEnvironment (StateT WriterState m)

-- | Convert Pandoc to DokuWiki.
writeDokuWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeDokuWiki opts document =
  runDokuWiki (pandocToDokuWiki opts document)

runDokuWiki :: PandocMonad m => DokuWiki m a -> m a
runDokuWiki = flip evalStateT def . flip runReaderT def

-- | Return DokuWiki representation of document.
pandocToDokuWiki :: PandocMonad m
                 => WriterOptions -> Pandoc -> DokuWiki m Text
pandocToDokuWiki opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
              (fmap (literal . trimr) . blockListToDokuWiki opts)
              (fmap (literal . trimr) . inlineListToDokuWiki opts)
              meta
  body <- blockListToDokuWiki opts blocks
  let context = defField "body" body
              $ defField "toc" (writerTableOfContents opts) metadata
  return $
    case writerTemplate opts of
       Nothing  -> body
       Just tpl -> render Nothing $ renderTemplate tpl context

-- | Escape special characters for DokuWiki.
escapeString :: Text -> Text
escapeString = T.replace "__" "%%__%%" .
               T.replace "**" "%%**%%" .
               T.replace "//" "%%//%%"

-- | Convert Pandoc block element to DokuWiki.
blockToDokuWiki :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> DokuWiki m Text

blockToDokuWiki _ Null = return ""

blockToDokuWiki opts (Div _attrs bs) = do
  contents <- blockListToDokuWiki opts bs
  return $ contents <> "\n"

blockToDokuWiki opts (Plain inlines) =
  inlineListToDokuWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
-- dokuwiki doesn't support captions - so combine together alt and caption into alt
blockToDokuWiki opts (Para [Image attr txt (src,tgt)])
  | Just tit <- T.stripPrefix "fig:" tgt
  = do
      capt <- if null txt
              then return ""
              else (" " <>) `fmap` inlineListToDokuWiki opts txt
      let opt = if null txt
                then ""
                else "|" <> if T.null tit then capt else tit <> capt
      return $ "{{" <> src <> imageDims opts attr <> opt <> "}}\n"

blockToDokuWiki opts (Para inlines) = do
  indent <- asks stIndent
  useTags <- asks stUseTags
  contents <- inlineListToDokuWiki opts inlines
  return $ if useTags
              then "<HTML><p></HTML>" <> contents <> "<HTML></p></HTML>"
              else contents <> if T.null indent then "\n" else ""

blockToDokuWiki opts (LineBlock lns) =
  blockToDokuWiki opts $ linesToPara lns

blockToDokuWiki _ b@(RawBlock f str)
  | f == Format "dokuwiki" = return str
  -- See https://www.dokuwiki.org/wiki:syntax
  -- use uppercase HTML tag for block-level content:
  | f == Format "html"     = return $ "<HTML>\n" <> str <> "\n</HTML>"
  | otherwise              = "" <$
         report (BlockNotRendered b)

blockToDokuWiki _ HorizontalRule = return "\n----\n"

blockToDokuWiki opts (Header level _ inlines) = do
  -- emphasis, links etc. not allowed in headers, apparently,
  -- so we remove formatting:
  contents <- inlineListToDokuWiki opts $ removeFormatting inlines
  let eqs = T.replicate ( 7 - level ) "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

blockToDokuWiki _ (CodeBlock (_,classes,_) str) = do
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  return $ "<code" <>
                (case at of
                      []    -> ">\n"
                      (x:_) -> " " <> x <> ">\n") <> str <> "\n</code>"

blockToDokuWiki opts (BlockQuote blocks) = do
  contents <- blockListToDokuWiki opts blocks
  if isSimpleBlockQuote blocks
     then return $ T.unlines $ map ("> " <>) $ T.lines contents
     else return $ "<HTML><blockquote>\n" <> contents <> "</blockquote></HTML>"

blockToDokuWiki opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (capt, aligns, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToDokuWiki opts capt
                      return $ "" <> c <> "\n"
  headers' <- if all null headers
                 then return []
                 else zipWithM (tableItemToDokuWiki opts) aligns headers
  rows' <- mapM (zipWithM (tableItemToDokuWiki opts) aligns) rows
  let widths = map (maximum . map T.length) $ transpose (headers':rows')
  let padTo (width, al) s =
          case width - T.length s of
               x | x > 0 ->
                 if al == AlignLeft || al == AlignDefault
                    then s <> T.replicate x " "
                    else if al == AlignRight
                            then T.replicate x " " <> s
                            else T.replicate (x `div` 2) " " <>
                                 s <> T.replicate (x - x `div` 2) " "
                 | otherwise -> s
  let renderRow sep cells = sep <>
          T.intercalate sep (zipWith padTo (zip widths aligns) cells) <> sep
  return $ captionDoc <>
           (if null headers' then "" else renderRow "^" headers' <> "\n") <>
           T.unlines (map (renderRow "|") rows')

blockToDokuWiki opts x@(BulletList items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (listItemToDokuWiki opts) items)
        return $ "<HTML><ul></HTML>\n" <> vcat contents <> "<HTML></ul></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (listItemToDokuWiki opts) items)
        return $ vcat contents <> if T.null indent then "\n" else ""

blockToDokuWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (orderedListItemToDokuWiki opts) items)
        return $ "<HTML><ol" <> listAttribsToString attribs <> "></HTML>\n" <> vcat contents <> "<HTML></ol></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (orderedListItemToDokuWiki opts) items)
        return $ vcat contents <> if T.null indent then "\n" else ""

-- TODO Need to decide how to make definition lists work on dokuwiki - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToDokuWiki opts x@(DefinitionList items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (definitionListItemToDokuWiki opts) items)
        return $ "<HTML><dl></HTML>\n" <> vcat contents <> "<HTML></dl></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (definitionListItemToDokuWiki opts) items)
        return $ vcat contents <> if T.null indent then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToString :: ListAttributes -> Text
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ tshow numstyle
  in  (if startnum /= 1
          then " start=\"" <> tshow startnum <> "\""
          else "") <>
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " <> numstyle' <> ";\""
          else "")

-- | Convert bullet list item (list of blocks) to DokuWiki.
listItemToDokuWiki :: PandocMonad m
                   => WriterOptions -> [Block] -> DokuWiki m Text
listItemToDokuWiki opts items = do
  useTags <- asks stUseTags
  if useTags
     then do
       contents <- blockListToDokuWiki opts items
       return $ "<HTML><li></HTML>" <> contents <> "<HTML></li></HTML>"
     else do
       bs <- mapM (blockToDokuWiki opts) items
       let contents = case items of
                           [_, CodeBlock _ _] -> T.concat bs
                           _                  -> vcat bs
       indent <- asks stIndent
       backSlash <- asks stBackSlashLB
       let indent' = if backSlash then T.drop 2 indent else indent
       return $ indent' <> "* " <> contents

-- | Convert ordered list item (list of blocks) to DokuWiki.
-- | TODO Emiminate dreadful duplication of text from listItemToDokuWiki
orderedListItemToDokuWiki :: PandocMonad m => WriterOptions -> [Block] -> DokuWiki m Text
orderedListItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- asks stUseTags
  if useTags
     then return $ "<HTML><li></HTML>" <> contents <> "<HTML></li></HTML>"
     else do
       indent <- asks stIndent
       backSlash <- asks stBackSlashLB
       let indent' = if backSlash then T.drop 2 indent else indent
       return $ indent' <> "- " <> contents

-- | Convert definition list item (label, list of blocks) to DokuWiki.
definitionListItemToDokuWiki :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> DokuWiki m Text
definitionListItemToDokuWiki opts (label, items) = do
  labelText <- inlineListToDokuWiki opts label
  contents <- mapM (blockListToDokuWiki opts) items
  useTags <- asks stUseTags
  if useTags
     then return $ "<HTML><dt></HTML>" <> labelText <> "<HTML></dt></HTML>\n" <>
           T.intercalate "\n" (map (\d -> "<HTML><dd></HTML>" <> d <> "<HTML></dd></HTML>") contents)
     else do
       indent <- asks stIndent
       backSlash <- asks stBackSlashLB
       let indent' = if backSlash then T.drop 2 indent else indent
       return $ indent' <> "* **" <> labelText <> "** " <> T.concat contents

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items            -> all isSimpleListItem items
       OrderedList (1, _, _) items -> all isSimpleListItem items
       DefinitionList items        -> all (all isSimpleListItem . snd) items
       _                           -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x, CodeBlock{}] | isPlainOrPara x = True
isSimpleListItem (x:ys) | isPlainOrPara x = all isSimpleList ys
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

isSimpleBlockQuote :: [Block] -> Bool
isSimpleBlockQuote bs  = all isPlainOrPara bs

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- | For each string in the input list, convert all newlines to
-- dokuwiki escaped newlines. Then concat the list using double linebreaks.
backSlashLineBreaks :: [Text] -> Text
backSlashLineBreaks ls = vcatBackSlash $ map (T.pack . escape . T.unpack) ls
  where
    vcatBackSlash = T.intercalate "\\\\ \\\\ " -- simulate paragraphs.
    escape ['\n']    = "" -- remove trailing newlines
    escape ('\n':cs) = "\\\\ " <> escape cs
    escape (c:cs)    = c : escape cs
    escape []        = []

-- Auxiliary functions for tables:

tableItemToDokuWiki :: PandocMonad m
                    => WriterOptions
                    -> Alignment
                    -> [Block]
                    -> DokuWiki m Text
tableItemToDokuWiki opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") <> x <>
                 (if align' == AlignLeft || align' == AlignCenter
                     then "  "
                     else "")
  contents <- local (\s -> s { stBackSlashLB = True }) $
              blockListToDokuWiki opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to DokuWiki.
blockListToDokuWiki :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> DokuWiki m Text
blockListToDokuWiki opts blocks = do
  backSlash <- asks stBackSlashLB
  let blocks' = consolidateRawBlocks blocks
  if backSlash
    then backSlashLineBreaks <$> mapM (blockToDokuWiki opts) blocks'
    else vcat <$> mapM (blockToDokuWiki opts) blocks'

consolidateRawBlocks :: [Block] -> [Block]
consolidateRawBlocks [] = []
consolidateRawBlocks (RawBlock f1 b1 : RawBlock f2 b2 : xs)
  | f1 == f2 = consolidateRawBlocks (RawBlock f1 (b1 <> "\n" <> b2) : xs)
consolidateRawBlocks (x:xs) = x : consolidateRawBlocks xs

-- | Convert list of Pandoc inline elements to DokuWiki.
inlineListToDokuWiki :: PandocMonad m
                     => WriterOptions -> [Inline] -> DokuWiki m Text
inlineListToDokuWiki opts lst =
  T.concat <$> mapM (inlineToDokuWiki opts) lst

-- | Convert Pandoc inline element to DokuWiki.
inlineToDokuWiki :: PandocMonad m
                 => WriterOptions -> Inline -> DokuWiki m Text

inlineToDokuWiki opts (Span _attrs ils) =
  inlineListToDokuWiki opts ils

inlineToDokuWiki opts (Emph lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "//" <> contents <> "//"

inlineToDokuWiki opts (Underline lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "__" <> contents <> "__"

inlineToDokuWiki opts (Strong lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "**" <> contents <> "**"

inlineToDokuWiki opts (Strikeout lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<del>" <> contents <> "</del>"

inlineToDokuWiki opts (Superscript lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<sup>" <> contents <> "</sup>"

inlineToDokuWiki opts (Subscript lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<sub>" <> contents <> "</sub>"

inlineToDokuWiki opts (SmallCaps lst) = inlineListToDokuWiki opts lst

inlineToDokuWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "\8216" <> contents <> "\8217"

inlineToDokuWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "\8220" <> contents <> "\8221"

inlineToDokuWiki opts (Cite _  lst) = inlineListToDokuWiki opts lst

inlineToDokuWiki _ (Code _ str) =
  -- In dokuwiki, text surrounded by '' is really just a font statement, i.e. <tt>,
  -- and so other formatting can be present inside.
  -- However, in pandoc, and markdown, inlined code doesn't contain formatting.
  -- So I have opted for using %% to disable all formatting inside inline code blocks.
  -- This gives the best results when converting from other formats to dokuwiki, even if
  -- the resultand code is a little ugly, for short strings that don't contain formatting
  -- characters.
  -- It does mean that if pandoc could ever read dokuwiki, and so round-trip the format,
  -- any formatting inside inlined code blocks would be lost, or presented incorrectly.
  return $ "''%%" <> str <> "%%''"

inlineToDokuWiki _ (Str str) = return $ escapeString str

inlineToDokuWiki _ (Math mathType str) = return $ delim <> str <> delim
                                 -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

inlineToDokuWiki _ il@(RawInline f str)
  | f == Format "dokuwiki" = return str
  | f == Format "html"     = return $ "<html>" <> str <> "</html>"
  | otherwise              = "" <$ report (InlineNotRendered il)

inlineToDokuWiki _ LineBreak = do
  backSlash <- asks stBackSlashLB
  return $ if backSlash
           then "\n"
           else "\\\\\n"

inlineToDokuWiki opts SoftBreak =
  case writerWrapText opts of
       WrapNone     -> return " "
       WrapAuto     -> return " "
       WrapPreserve -> return "\n"

inlineToDokuWiki _ Space = return " "

inlineToDokuWiki opts (Link _ txt (src, _)) = do
  label <- inlineListToDokuWiki opts txt
  case txt of
     [Str s] | "mailto:" `T.isPrefixOf` src -> return $ "<" <> s <> ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" <> src  <> "|" <> label <> "]]"
              else return $ "[[" <> src' <> "|" <> label <> "]]"
                     where src' = case T.uncons src of
                                     Just ('/',xs) -> xs  -- with leading / it's a
                                     _             -> src -- link to a help page
inlineToDokuWiki opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToDokuWiki opts alt
  let txt = case (tit, alt) of
              ("", []) -> ""
              ("", _ ) -> "|" <> alt'
              (_ , _ ) -> "|" <> tit
  return $ "{{" <> source <> imageDims opts attr <> txt <> "}}"

inlineToDokuWiki opts (Note contents) = do
  contents' <- blockListToDokuWiki opts contents
  return $ "((" <> contents' <> "))"
  -- note - may not work for notes with multiple blocks

imageDims :: WriterOptions -> Attr -> Text
imageDims opts attr = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
  where
    toPx = fmap (showInPixel opts) . checkPct
    checkPct (Just (Percent _)) = Nothing
    checkPct maybeDim           = maybeDim
    go (Just w) Nothing  = "?" <> w
    go (Just w) (Just h) = "?" <> w <> "x" <> h
    go Nothing  (Just h) = "?0x" <> h
    go Nothing  Nothing  = ""

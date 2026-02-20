{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Txt2Tags
   Copyright   : Copyright (C) 2008-2024 Eric Forgeot, based on John MacFarlane DokuWiki writer
   License     : GNU GPL, version 2 or above

   Maintainer  : Clare Macrae <clare.macrae@googlemail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Txt2Tags markup.

Txt2Tags:  <https://www.txt2tags.org/>
-}

{-
   It works for most of the syntax. But some improvements or fix can be made:
   - On lists (2 extra lines to terminate a list)
   - On the 3 lines header at the begining of a file
   - Definition lists are broken
   - Tables could be improved
   - Some formats make better results than others (html is ok, md is somehow broken)
   - code related to dokuwiki only could be removed
   - a test case should be made
-}

module Text.Pandoc.Writers.Txt2Tags ( writeTxt2Tags ) where
import Control.Monad (zipWithM)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Default (Default (..))
import Data.List (transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options (WrapOption (..), WriterOptions (writerTableOfContents,
                            writerTemplate, writerWrapText), isEnabled)
import Text.Pandoc.Shared (figureDiv, linesToPara, removeFormatting, trimr)
import Text.Pandoc.URI (escapeURI, isURI)
import Text.Pandoc.Templates (renderTemplate)
import Text.DocLayout (render, literal)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data WriterState = WriterState {
  }

data WriterEnvironment = WriterEnvironment {
    stIndent      :: Text          -- Indent after the marker at the beginning of list items
  , stBackSlashLB :: Bool     -- True if we should produce formatted strings with newlines (as in a table cell)
  , stBlockQuoteLevel :: Int   -- Block quote level
  }

instance Default WriterState where
  def = WriterState {}

instance Default WriterEnvironment where
  def = WriterEnvironment { stIndent = ""
                          , stBackSlashLB = False
                          , stBlockQuoteLevel = 0 }

type Txt2Tags m = ReaderT WriterEnvironment (StateT WriterState m)

-- | Convert Pandoc to Txt2Tags.
writeTxt2Tags :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTxt2Tags opts document =
  runTxt2Tags (pandocToTxt2Tags opts document)

runTxt2Tags :: PandocMonad m => Txt2Tags m a -> m a
runTxt2Tags = flip evalStateT def . flip runReaderT def

-- | Return Txt2Tags representation of document.
pandocToTxt2Tags :: PandocMonad m
                 => WriterOptions -> Pandoc -> Txt2Tags m Text
pandocToTxt2Tags opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
              (fmap (literal . trimr) . blockListToTxt2Tags opts)
              (fmap (literal . trimr) . inlineListToTxt2Tags opts)
              meta
  body <- blockListToTxt2Tags opts blocks
  let context = defField "body" body
              $ defField "toc" (writerTableOfContents opts) metadata
  return $
    case writerTemplate opts of
       Nothing  -> body
       Just tpl -> render Nothing $ renderTemplate tpl context

-- | Escape special characters for Txt2Tags.
escapeString :: Text -> Text
escapeString = T.replace "__" "%%__%%" .
               T.replace "**" "%%**%%" .
               T.replace "//" "%%//%%"

-- | Convert Pandoc block element to Txt2Tags.
blockToTxt2Tags :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> Txt2Tags m Text

blockToTxt2Tags opts (Div _attrs bs) = do
  contents <- blockListToTxt2Tags opts bs
  indent <- asks stIndent
  return $ contents <> if T.null indent then "\n\n" else ""

blockToTxt2Tags opts (Plain inlines) =
  inlineListToTxt2Tags opts inlines

blockToTxt2Tags opts (Para inlines) = do
  bqLevel <- asks stBlockQuoteLevel
  let bqPrefix = case bqLevel of
                    0 -> ""
                    n -> T.replicate n ">" <> " "
  indent <- asks stIndent
  contents <- inlineListToTxt2Tags opts inlines
  return $ bqPrefix <> contents <> if T.null indent then "\n" else ""

blockToTxt2Tags opts (LineBlock lns) =
  blockToTxt2Tags opts $ linesToPara lns

blockToTxt2Tags opts b@(RawBlock f str)
  | f == Format "Txt2Tags" = return str
  -- See https://www.Txt2Tags.org/wiki:syntax
  -- use uppercase HTML tag for block-level content:
  | f == Format "html"
  , isEnabled Ext_raw_html opts = return $ "<HTML>\n" <> str <> "\n</HTML>"
  | otherwise              = "" <$
         report (BlockNotRendered b)

blockToTxt2Tags _ HorizontalRule = return "\n---------------\n"

blockToTxt2Tags opts (Header level _ inlines) = do
  -- emphasis, links etc. not allowed in headers, apparently,
  -- so we remove formatting:
  contents <- inlineListToTxt2Tags opts $ removeFormatting inlines
  let eqs = T.replicate level  "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

blockToTxt2Tags _ (CodeBlock (_,classes,_) str) = do
  bqLevel <- asks stBlockQuoteLevel
  let bqPrefix = case bqLevel of
                    0 -> ""
                    n -> T.replicate n ">" <> " "
  return $ bqPrefix <>
           "<code" <>
           (case classes of
               []    -> ""
               (x:_) -> " " <> fromMaybe x (M.lookup x languageNames)) <>
           ">\n" <> str <>
           (if "\n" `T.isSuffixOf` str then "" else "\n") <> "</code>\n"

blockToTxt2Tags opts (BlockQuote blocks) =
  local (\st -> st{ stBlockQuoteLevel = stBlockQuoteLevel st + 1 })
               (blockListToTxt2Tags opts blocks)

blockToTxt2Tags opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (capt, aligns, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToTxt2Tags opts capt
                      return $ "" <> c <> "\n"
  headers' <- if all null headers
                 then return []
                 else zipWithM (tableItemToTxt2Tags opts) aligns headers
  rows' <- mapM (zipWithM (tableItemToTxt2Tags opts) aligns) rows
  let widths = map (maybe 0 maximum . nonEmpty . map T.length)
                   $ transpose (headers':rows')
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
           (if null headers' then "" else renderRow "|" headers' <> "\n") <>
           T.unlines (map (renderRow "|") rows')

blockToTxt2Tags opts (BulletList items) = do
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  contents <- local (\s -> s { stIndent = stIndent s <> "  "
                             , stBackSlashLB = backSlash})
                      (mapM (listItemToTxt2Tags opts) items)
  return $ vcat contents <> if T.null indent then "\n" else ""

blockToTxt2Tags opts (OrderedList _attribs items) = do
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  contents <- local (\s -> s { stIndent = stIndent s <> "  "
                             , stBackSlashLB = backSlash})
                (mapM (orderedListItemToTxt2Tags opts) items)
  return $ vcat contents <> if T.null indent then "\n" else ""

blockToTxt2Tags opts (Figure attr capt body) =
  blockToTxt2Tags opts $ figureDiv attr capt body

-- TODO Need to decide how to make definition lists work on Txt2Tags - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToTxt2Tags opts (DefinitionList items) = do
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  contents <- local (\s -> s { stIndent = stIndent s <> "  "
                             , stBackSlashLB = backSlash})
                (mapM (definitionListItemToTxt2Tags opts) items)
  return $ vcat contents <> if T.null indent then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert bullet list item (list of blocks) to Txt2Tags.
listItemToTxt2Tags :: PandocMonad m
                   => WriterOptions -> [Block] -> Txt2Tags m Text
listItemToTxt2Tags opts items = do
  bqLevel <- asks stBlockQuoteLevel
  let bqPrefix = case bqLevel of
                    0 -> ""
                    n -> T.replicate n ">" <> " "
  let useWrap = not (isSimpleListItem items)
  bs <- mapM (blockToTxt2Tags opts) items
  let contents = case items of
                      [_, CodeBlock _ _] -> T.concat bs
                      _                  -> vcat bs
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let indent' = if backSlash then T.drop 2 indent else indent
  return $ bqPrefix <> indent' <> "- " <>
    if useWrap
       then "<WRAP>\n" <> contents <> "\n</WRAP>"
       else contents

-- | Convert ordered list item (list of blocks) to Txt2Tags.
-- | TODO Emiminate dreadful duplication of text from listItemToTxt2Tags
orderedListItemToTxt2Tags :: PandocMonad m => WriterOptions -> [Block] -> Txt2Tags m Text
orderedListItemToTxt2Tags opts items = do
  bqLevel <- asks stBlockQuoteLevel
  let bqPrefix = case bqLevel of
                    0 -> ""
                    n -> T.replicate n ">" <> " "
  let useWrap = not (isSimpleListItem items)
  contents <- local (\st -> st{ stBlockQuoteLevel = 0 })
               (blockListToTxt2Tags opts items)
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let indent' = if backSlash then T.drop 2 indent else indent
  return $ bqPrefix <> indent' <> "+ " <>
    if useWrap
       then "<WRAP>\n" <> contents <> "\n</WRAP>"
       else contents

-- | Convert definition list item (label, list of blocks) to Txt2Tags.
definitionListItemToTxt2Tags :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> Txt2Tags m Text
definitionListItemToTxt2Tags opts (label, items) = do
  let useWrap = not (all isSimpleListItem items)
  bqLevel <- asks stBlockQuoteLevel
  let bqPrefix = case bqLevel of
                    0 -> ""
                    n -> T.replicate n ">" <> " "
  labelText <- inlineListToTxt2Tags opts label
  contents <- local (\st -> st{ stBlockQuoteLevel = 0 })
               (mapM (blockListToTxt2Tags opts) items)
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let indent' = if backSlash then T.drop 2 indent else indent
  return $ bqPrefix <> indent' <> "* **" <> labelText <> "** " <>
    if useWrap
       then "<WRAP>\n" <> vcat contents <> "\n</WRAP>"
       else T.intercalate "; " contents

-- | True if list item can be handled with the simple wiki syntax.  False if
--   WRAP tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x, CodeBlock{}] | isPlainOrPara x = True
isSimpleListItem (Div _ bs : ys) = -- see #8920
  isSimpleListItem bs && all isSimpleList ys
isSimpleListItem (x:ys) | isPlainOrPara x = all isSimpleList ys
isSimpleListItem _ = False
--- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.

isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items            -> all isSimpleListItem items
       OrderedList (1, _, _) items -> all isSimpleListItem items
       DefinitionList items        -> all (all isSimpleListItem . snd) items
       _                           -> False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- | For each string in the input list, convert all newlines to
-- Txt2Tags escaped newlines. Then concat the list using double linebreaks.
backSlashLineBreaks :: [Text] -> Text
backSlashLineBreaks ls = vcatBackSlash $ map (T.pack . escape . T.unpack) ls
  where
    vcatBackSlash = T.intercalate "\\\\ \\\\ " -- simulate paragraphs.
    escape ['\n']    = "" -- remove trailing newlines
    escape ('\n':cs) = "\\\\ " <> escape cs
    escape (c:cs)    = c : escape cs
    escape []        = []

-- Auxiliary functions for tables:

tableItemToTxt2Tags :: PandocMonad m
                    => WriterOptions
                    -> Alignment
                    -> [Block]
                    -> Txt2Tags m Text
tableItemToTxt2Tags opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") <> x <>
                 (if align' == AlignLeft || align' == AlignCenter
                     then "  "
                     else "")
  contents <- local (\s -> s { stBackSlashLB = True
                             , stBlockQuoteLevel = 0 }) $
                blockListToTxt2Tags opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Txt2Tags.
blockListToTxt2Tags :: PandocMonad m
                    => WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> Txt2Tags m Text
blockListToTxt2Tags opts blocks = do
  backSlash <- asks stBackSlashLB
  let blocks' = consolidateRawBlocks blocks
  if backSlash
    then backSlashLineBreaks <$> mapM (blockToTxt2Tags opts) blocks'
    else vcat <$> mapM (blockToTxt2Tags opts) blocks'

consolidateRawBlocks :: [Block] -> [Block]
consolidateRawBlocks [] = []
consolidateRawBlocks (RawBlock f1 b1 : RawBlock f2 b2 : xs)
  | f1 == f2 = consolidateRawBlocks (RawBlock f1 (b1 <> "\n" <> b2) : xs)
consolidateRawBlocks (x:xs) = x : consolidateRawBlocks xs

-- | Convert list of Pandoc inline elements to Txt2Tags.
inlineListToTxt2Tags :: PandocMonad m
                     => WriterOptions -> [Inline] -> Txt2Tags m Text
inlineListToTxt2Tags opts lst =
  T.concat <$> mapM (inlineToTxt2Tags opts) lst

-- | Convert Pandoc inline element to Txt2Tags.
inlineToTxt2Tags :: PandocMonad m
                 => WriterOptions -> Inline -> Txt2Tags m Text

inlineToTxt2Tags opts (Span _attrs ils) =
  inlineListToTxt2Tags opts ils

inlineToTxt2Tags opts (Emph lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "//" <> contents <> "//"

inlineToTxt2Tags opts (Underline lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "__" <> contents <> "__"

inlineToTxt2Tags opts (Strong lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "**" <> contents <> "**"

inlineToTxt2Tags opts (Strikeout lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "--" <> contents <> "--"

inlineToTxt2Tags opts (Superscript lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "<sup>" <> contents <> "</sup>"

inlineToTxt2Tags opts (Subscript lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "<sub>" <> contents <> "</sub>"

inlineToTxt2Tags opts (SmallCaps lst) = inlineListToTxt2Tags opts lst

inlineToTxt2Tags opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "\8216" <> contents <> "\8217"

inlineToTxt2Tags opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTxt2Tags opts lst
  return $ "\8220" <> contents <> "\8221"

inlineToTxt2Tags opts (Cite _  lst) = inlineListToTxt2Tags opts lst

inlineToTxt2Tags _ (Code _ str) =
  -- In Txt2Tags, text surrounded by '' is really just a font statement, i.e. <tt>,
  -- and so other formatting can be present inside.
  -- However, in pandoc, and markdown, inlined code doesn't contain formatting.
  -- So I have opted for using %% to disable all formatting inside inline code blocks.
  -- This gives the best results when converting from other formats to Txt2Tags, even if
  -- the resultand code is a little ugly, for short strings that don't contain formatting
  -- characters.
  -- It does mean that if pandoc could ever read Txt2Tags, and so round-trip the format,
  -- any formatting inside inlined code blocks would be lost, or presented incorrectly.
  return $ "''%%" <> str <> "%%''"

inlineToTxt2Tags _ (Str str) = return $ escapeString str

inlineToTxt2Tags _ (Math mathType str) = return $ delim <> str <> delim
                                 -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

inlineToTxt2Tags opts il@(RawInline f str)
  | f == Format "Txt2Tags" = return str
  | f == Format "html"
  , isEnabled Ext_raw_html opts = return $ "<html>" <> str <> "</html>"
  | otherwise              = "" <$ report (InlineNotRendered il)

inlineToTxt2Tags _ LineBreak = do
  backSlash <- asks stBackSlashLB
  return $ if backSlash
           then "\n"
           else "\\\\\n"

inlineToTxt2Tags opts SoftBreak =
  case writerWrapText opts of
       WrapNone     -> return " "
       WrapAuto     -> return " "
       WrapPreserve -> return "\n"

inlineToTxt2Tags _ Space = return " "

inlineToTxt2Tags opts (Link _ txt (src, _)) = do
  label <- inlineListToTxt2Tags opts txt
  case txt of
     [Str s] | "mailto:" `T.isPrefixOf` src -> return $ "<" <> s <> ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[" <> label <> " " <> src <> "]"
              else return $ "[" <> label <> " " <> src' <> "]"
                     where src' = case T.uncons src of
                                     Just ('/',xs) -> xs  -- with leading / it's a
                                     _             -> src -- link to a help page
inlineToTxt2Tags opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToTxt2Tags opts alt
  let txt = case (tit, alt) of
              ("", []) -> ""
              ("", _ ) -> "|" <> alt'
              (_ , _ ) -> "|" <> tit
  return $ "[" <> source <> imageDims opts attr <> txt <> "]"

inlineToTxt2Tags opts (Note contents) = do
  contents' <- local (\st -> st{ stBlockQuoteLevel = 0 })
                 (blockListToTxt2Tags opts contents)
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

languageNames :: M.Map Text Text
languageNames = M.fromList
  [("cs", "csharp")
  ,("coffee", "cofeescript")
  ,("commonlisp", "lisp")
  ,("gcc", "c")
  ,("html", "html5")
  ,("makefile", "make")
  ,("objectivec", "objc")
  ,("r", "rsplus")
  ,("sqlmysql", "mysql")
  ,("sqlpostgresql", "postgresql")
  ,("sci", "scilab")
  ,("xorg", "xorgconf")
  ]

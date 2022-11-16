{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Txt2Tags
   Copyright   : Copyright (C) 2020 Eric Forgeot, based on John MacFarlane dokuwiki writer
   License     : GNU GPL, version 2 or above

   Maintainer  : Eric Forgeot - https://github.com/farvardin/
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Txt2Tags markup.

Txt2Tags:  <https://www.Txt2Tags.org/>
-}

{-
   It works for most of the syntax. But some improvements or fix can be made:
   - On lists (2 extra lines to terminate a list)
   - On the 3 lines header at the begining of a file
   - Definition lists are broken
   - Tables could be improved
   - Some formats make better results than others (html is ok, md is somehow broken)
   - code related to only dokuwiki could be removed
-}

module Text.Pandoc.Writers.Txt2Tags ( writeTxt2Tags ) where
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

blockToTxt2Tags _ Null = return ""

blockToTxt2Tags opts (Div _attrs bs) = do
  contents <- blockListToTxt2Tags opts bs
  return $ contents <> "\n\n"

blockToTxt2Tags opts (Plain inlines) =
  inlineListToTxt2Tags opts inlines

-- title beginning with fig: indicates that the image is a figure
-- Txt2Tags doesn't support captions - so combine together alt and caption into alt
blockToTxt2Tags opts (Para [Image attr txt (src,tgt)])
  | Just tit <- T.stripPrefix "fig:" tgt
  = do
      capt <- if null txt
              then return ""
              else (" " <>) `fmap` inlineListToTxt2Tags opts txt
      let opt = if null txt
                then ""
                else "|" <> if T.null tit then capt else tit <> capt
      return $ "{{" <> src <> imageDims opts attr <> opt <> "}}\n"

blockToTxt2Tags opts (Para inlines) = do
  indent <- asks stIndent
  useTags <- asks stUseTags
  contents <- inlineListToTxt2Tags opts inlines
  return $ if useTags
              then "<HTML><p></HTML>" <> contents <> "<HTML></p></HTML>"
              else contents <> if T.null indent then "\n" else ""

blockToTxt2Tags opts (LineBlock lns) =
  blockToTxt2Tags opts $ linesToPara lns

blockToTxt2Tags _ b@(RawBlock f str)
  | f == Format "Txt2Tags" = return str
  -- See https://www.Txt2Tags.org/wiki:syntax
  -- use uppercase HTML tag for block-level content:
  | f == Format "html"     = return $ "<HTML>\n" <> str <> "\n</HTML>"
  | otherwise              = "" <$
         report (BlockNotRendered b)

blockToTxt2Tags _ HorizontalRule = return "\n---------------------\n"

blockToTxt2Tags opts (Header level _ inlines) = do
  -- emphasis, links etc. not allowed in headers, apparently,
  -- so we remove formatting:
  contents <- inlineListToTxt2Tags opts $ removeFormatting inlines
  let eqs = T.replicate level "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

blockToTxt2Tags _ (CodeBlock (_,classes,_) str) = do
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl",  "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  return $ "```" <>
                (case at of
                      []    -> "\n"
                      (x:_) -> " " <> x <> "\n") <> str <> "\n```"

blockToTxt2Tags opts (BlockQuote blocks) = do
  contents <- blockListToTxt2Tags opts blocks
  if isSimpleBlockQuote blocks
     then return $ T.unlines $ map ("\t" <>) $ T.lines contents
     else return $ "```\n" <> contents <> "```"

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
           (if null headers' then "" else renderRow "|" headers' <> "\n") <>
           T.unlines (map (renderRow "|") rows')

blockToTxt2Tags opts x@(BulletList items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (listItemToTxt2Tags opts) items)
        return $ "```\n" <> vcat contents <> "```\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (listItemToTxt2Tags opts) items)
        return $ vcat contents <> if T.null indent then "\n" else ""

blockToTxt2Tags opts x@(OrderedList attribs items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (orderedListItemToTxt2Tags opts) items)
        return $ "+ " <> listAttribsToString attribs <> ">\n" <> vcat contents <> "\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (orderedListItemToTxt2Tags opts) items)
        return $ vcat contents <> if T.null indent then "\n" else ""

-- TODO Need to decide how to make definition lists work on Txt2Tags - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToTxt2Tags opts x@(DefinitionList items) = do
  oldUseTags <- asks stUseTags
  indent <- asks stIndent
  backSlash <- asks stBackSlashLB
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (definitionListItemToTxt2Tags opts) items)
        return $ ": \n" <> vcat contents <> "\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s <> "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (definitionListItemToTxt2Tags opts) items)
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

-- | Convert bullet list item (list of blocks) to Txt2Tags.
listItemToTxt2Tags :: PandocMonad m
                   => WriterOptions -> [Block] -> Txt2Tags m Text
listItemToTxt2Tags opts items = do
  useTags <- asks stUseTags
  if useTags
     then do
       contents <- blockListToTxt2Tags opts items
       return $ "<HTML><li></HTML>" <> contents <> "<HTML></li></HTML>"
     else do
       bs <- mapM (blockToTxt2Tags opts) items
       let contents = case items of
                           [_, CodeBlock _ _] -> T.concat bs
                           _                  -> vcat bs
       indent <- asks stIndent
       backSlash <- asks stBackSlashLB
       let indent' = if backSlash then T.drop 2 indent else indent
       return $ indent' <> "- " <> contents

-- | Convert ordered list item (list of blocks) to Txt2Tags.
-- | TODO Emiminate dreadful duplication of text from listItemToTxt2Tags
orderedListItemToTxt2Tags :: PandocMonad m => WriterOptions -> [Block] -> Txt2Tags m Text
orderedListItemToTxt2Tags opts items = do
  contents <- blockListToTxt2Tags opts items
  useTags <- asks stUseTags
  if useTags
     then return $ "<HTML><li></HTML>" <> contents <> "<HTML></li></HTML>"
     else do
       indent <- asks stIndent
       backSlash <- asks stBackSlashLB
       let indent' = if backSlash then T.drop 2 indent else indent
       return $ indent' <> "+ " <> contents

-- | Convert definition list item (label, list of blocks) to Txt2Tags.
definitionListItemToTxt2Tags :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> Txt2Tags m Text
definitionListItemToTxt2Tags opts (label, items) = do
  labelText <- inlineListToTxt2Tags opts label
  contents <- mapM (blockListToTxt2Tags opts) items
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
  contents <- local (\s -> s { stBackSlashLB = True }) $
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

inlineToTxt2Tags _ il@(RawInline f str)
  | f == Format "Txt2Tags" = return str
  | f == Format "html"     = return $ "<html>" <> str <> "</html>"
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
  contents' <- blockListToTxt2Tags opts contents
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

{-
Copyright (C) 2008-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.DokuWiki
   Copyright   : Copyright (C) 2008-2017 John MacFarlane
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
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Default (Default (..))
import Data.List (intercalate, intersect, isPrefixOf, transpose)
import Data.Text (Text, pack)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Options (WrapOption (..), WriterOptions (writerTableOfContents, writerTemplate, writerWrapText))
import Text.Pandoc.Shared (camelCaseToHyphenated, escapeURI, isURI, linesToPara,
                           removeFormatting, substitute, trimr)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Shared (defField, metaToJSON)

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  }

data WriterEnvironment = WriterEnvironment {
    stIndent      :: String          -- Indent after the marker at the beginning of list items
  , stUseTags     :: Bool            -- True if we should use HTML tags because we're in a complex list
  , stBackSlashLB :: Bool     -- True if we should produce formatted strings with newlines (as in a table cell)
  }

instance Default WriterState where
  def = WriterState { stNotes = False }

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
  metadata <- metaToJSON opts
              (fmap trimr . blockListToDokuWiki opts)
              (inlineListToDokuWiki opts)
              meta
  body <- blockListToDokuWiki opts blocks
  notesExist <- gets stNotes
  let notes = if notesExist
                 then "" -- TODO Was "\n<references />" Check whether I can really remove this:
                         -- if it is definitely to do with footnotes, can remove this whole bit
                 else ""
  let main = pack $ body ++ notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts)
                $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Escape special characters for DokuWiki.
escapeString :: String -> String
escapeString = substitute "__" "%%__%%" .
               substitute "**" "%%**%%" .
               substitute "//" "%%//%%"

-- | Convert Pandoc block element to DokuWiki.
blockToDokuWiki :: PandocMonad m
                => WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> DokuWiki m String

blockToDokuWiki _ Null = return ""

blockToDokuWiki opts (Div _attrs bs) = do
  contents <- blockListToDokuWiki opts bs
  return $ contents ++ "\n"

blockToDokuWiki opts (Plain inlines) =
  inlineListToDokuWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
-- dokuwiki doesn't support captions - so combine together alt and caption into alt
blockToDokuWiki opts (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else (" " ++) `fmap` inlineListToDokuWiki opts txt
  let opt = if null txt
               then ""
               else "|" ++ if null tit then capt else tit ++ capt
      -- Relative links fail isURI and receive a colon
      prefix = if isURI src then "" else ":"
  return $ "{{" ++ prefix ++ src ++ imageDims opts attr ++ opt ++ "}}\n"

blockToDokuWiki opts (Para inlines) = do
  indent <- stIndent <$> ask
  useTags <- stUseTags <$> ask
  contents <- inlineListToDokuWiki opts inlines
  return $ if useTags
              then "<HTML><p></HTML>" ++ contents ++ "<HTML></p></HTML>"
              else contents ++ if null indent then "\n" else ""

blockToDokuWiki opts (LineBlock lns) =
  blockToDokuWiki opts $ linesToPara lns

blockToDokuWiki _ b@(RawBlock f str)
  | f == Format "dokuwiki" = return str
  -- See https://www.dokuwiki.org/wiki:syntax
  -- use uppercase HTML tag for block-level content:
  | f == Format "html"     = return $ "<HTML>\n" ++ str ++ "\n</HTML>"
  | otherwise              = "" <$ (report $ BlockNotRendered b)

blockToDokuWiki _ HorizontalRule = return "\n----\n"

blockToDokuWiki opts (Header level _ inlines) = do
  -- emphasis, links etc. not allowed in headers, apparently,
  -- so we remove formatting:
  contents <- inlineListToDokuWiki opts $ removeFormatting inlines
  let eqs = replicate ( 7 - level ) '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

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
  return $ "<code" ++
                (case at of
                      []    -> ">\n"
                      (x:_) -> " " ++ x ++ ">\n") ++ str ++ "\n</code>"

blockToDokuWiki opts (BlockQuote blocks) = do
  contents <- blockListToDokuWiki opts blocks
  if isSimpleBlockQuote blocks
     then return $ unlines $ map ("> " ++) $ lines contents
     else return $ "<HTML><blockquote>\n" ++ contents ++ "</blockquote></HTML>"

blockToDokuWiki opts (Table capt aligns _ headers rows) = do
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToDokuWiki opts capt
                      return $ "" ++ c ++ "\n"
  headers' <- if all null headers
                 then return []
                 else zipWithM (tableItemToDokuWiki opts) aligns headers
  rows' <- mapM (zipWithM (tableItemToDokuWiki opts) aligns) rows
  let widths = map (maximum . map length) $ transpose (headers':rows')
  let padTo (width, al) s =
          case (width - length s) of
               x | x > 0 ->
                 if al == AlignLeft || al == AlignDefault
                    then s ++ replicate x ' '
                    else if al == AlignRight
                            then replicate x ' ' ++ s
                            else replicate (x `div` 2) ' ' ++
                                 s ++ replicate (x - x `div` 2) ' '
                 | otherwise -> s
  let renderRow sep cells = sep ++
          intercalate sep (zipWith padTo (zip widths aligns) cells) ++ sep
  return $ captionDoc ++
           (if null headers' then "" else renderRow "^" headers' ++ "\n") ++
           unlines (map (renderRow "|") rows')

blockToDokuWiki opts x@(BulletList items) = do
  oldUseTags <- stUseTags <$> ask
  indent <- stIndent <$> ask
  backSlash <- stBackSlashLB <$> ask
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (listItemToDokuWiki opts) items)
        return $ "<HTML><ul></HTML>\n" ++ vcat contents ++ "<HTML></ul></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s ++ "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (listItemToDokuWiki opts) items)
        return $ vcat contents ++ if null indent then "\n" else ""

blockToDokuWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- stUseTags <$> ask
  indent <- stIndent <$> ask
  backSlash <- stBackSlashLB <$> ask
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (orderedListItemToDokuWiki opts) items)
        return $ "<HTML><ol" ++ listAttribsToString attribs ++ "></HTML>\n" ++ vcat contents ++ "<HTML></ol></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s ++ "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (orderedListItemToDokuWiki opts) items)
        return $ vcat contents ++ if null indent then "\n" else ""

-- TODO Need to decide how to make definition lists work on dokuwiki - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToDokuWiki opts x@(DefinitionList items) = do
  oldUseTags <- stUseTags <$> ask
  indent <- stIndent <$> ask
  backSlash <- stBackSlashLB <$> ask
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- local (\s -> s { stUseTags = True })
                      (mapM (definitionListItemToDokuWiki opts) items)
        return $ "<HTML><dl></HTML>\n" ++ vcat contents ++ "<HTML></dl></HTML>\n"
     else do
        contents <- local (\s -> s { stIndent = stIndent s ++ "  "
                                   , stBackSlashLB = backSlash})
                      (mapM (definitionListItemToDokuWiki opts) items)
        return $ vcat contents ++ if null indent then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToString :: ListAttributes -> String
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ show numstyle
  in  (if startnum /= 1
          then " start=\"" ++ show startnum ++ "\""
          else "") ++
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " ++ numstyle' ++ ";\""
          else "")

-- | Convert bullet list item (list of blocks) to DokuWiki.
listItemToDokuWiki :: PandocMonad m
                   => WriterOptions -> [Block] -> DokuWiki m String
listItemToDokuWiki opts items = do
  useTags <- stUseTags <$> ask
  if useTags
     then do
       contents <- blockListToDokuWiki opts items
       return $ "<HTML><li></HTML>" ++ contents ++ "<HTML></li></HTML>"
     else do
       bs <- mapM (blockToDokuWiki opts) items
       let contents = case items of
                           [_, CodeBlock _ _] -> concat bs
                           _ -> vcat bs
       indent <- stIndent <$> ask
       backSlash <- stBackSlashLB <$> ask
       let indent' = if backSlash then (drop 2 indent) else indent
       return $ indent' ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to DokuWiki.
-- | TODO Emiminate dreadful duplication of text from listItemToDokuWiki
orderedListItemToDokuWiki :: PandocMonad m => WriterOptions -> [Block] -> DokuWiki m String
orderedListItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- stUseTags <$> ask
  if useTags
     then return $ "<HTML><li></HTML>" ++ contents ++ "<HTML></li></HTML>"
     else do
       indent <- stIndent <$> ask
       backSlash <- stBackSlashLB <$> ask
       let indent' = if backSlash then (drop 2 indent) else indent
       return $ indent' ++ "- " ++ contents

-- | Convert definition list item (label, list of blocks) to DokuWiki.
definitionListItemToDokuWiki :: PandocMonad m
                             => WriterOptions
                             -> ([Inline],[[Block]])
                             -> DokuWiki m String
definitionListItemToDokuWiki opts (label, items) = do
  labelText <- inlineListToDokuWiki opts label
  contents <- mapM (blockListToDokuWiki opts) items
  useTags <- stUseTags <$> ask
  if useTags
     then return $ "<HTML><dt></HTML>" ++ labelText ++ "<HTML></dt></HTML>\n" ++
           (intercalate "\n" $ map (\d -> "<HTML><dd></HTML>" ++ d ++ "<HTML></dd></HTML>") contents)
     else do
       indent <- stIndent <$> ask
       backSlash <- stBackSlashLB <$> ask
       let indent' = if backSlash then (drop 2 indent) else indent
       return $ indent' ++ "* **" ++ labelText ++ "** " ++ concat contents

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
       DefinitionList items             -> all isSimpleListItem $ concatMap snd items
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _          -> True
       Para  _          -> True
       BulletList _     -> isSimpleList x
       OrderedList _ _  -> isSimpleList x
       DefinitionList _ -> isSimpleList x
       _                -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _     -> isSimpleList y
       OrderedList _ _  -> isSimpleList y
       DefinitionList _ -> isSimpleList y
       CodeBlock _ _    -> True
       _                -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

isSimpleBlockQuote :: [Block] -> Bool
isSimpleBlockQuote bs  = all isPlainOrPara bs

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

backSlashLineBreaks :: String -> String
backSlashLineBreaks cs = reverse $ g $ reverse $ concatMap f cs
  where f '\n' = "\\\\ "
        f c    = [c]
        g (' ' : '\\':'\\': xs) = xs
        g s                     = s

-- Auxiliary functions for tables:

tableItemToDokuWiki :: PandocMonad m
                    => WriterOptions
                    -> Alignment
                    -> [Block]
                    -> DokuWiki m String
tableItemToDokuWiki opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") ++ x ++
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
                    -> DokuWiki m String
blockListToDokuWiki opts blocks = do
  backSlash <- stBackSlashLB <$> ask
  let blocks' = consolidateRawBlocks blocks
  if backSlash
    then (backSlashLineBreaks . vcat) <$> mapM (blockToDokuWiki opts) blocks'
    else vcat <$> mapM (blockToDokuWiki opts) blocks'

consolidateRawBlocks :: [Block] -> [Block]
consolidateRawBlocks [] = []
consolidateRawBlocks (RawBlock f1 b1 : RawBlock f2 b2 : xs)
  | f1 == f2 = consolidateRawBlocks (RawBlock f1 (b1 ++ "\n" ++ b2) : xs)
consolidateRawBlocks (x:xs) = x : consolidateRawBlocks xs

-- | Convert list of Pandoc inline elements to DokuWiki.
inlineListToDokuWiki :: PandocMonad m
                     => WriterOptions -> [Inline] -> DokuWiki m String
inlineListToDokuWiki opts lst =
  concat <$> (mapM (inlineToDokuWiki opts) lst)

-- | Convert Pandoc inline element to DokuWiki.
inlineToDokuWiki :: PandocMonad m
                 => WriterOptions -> Inline -> DokuWiki m String

inlineToDokuWiki opts (Span _attrs ils) =
  inlineListToDokuWiki opts ils

inlineToDokuWiki opts (Emph lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "//" ++ contents ++ "//"

inlineToDokuWiki opts (Strong lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "**" ++ contents ++ "**"

inlineToDokuWiki opts (Strikeout lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<del>" ++ contents ++ "</del>"

inlineToDokuWiki opts (Superscript lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<sup>" ++ contents ++ "</sup>"

inlineToDokuWiki opts (Subscript lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "<sub>" ++ contents ++ "</sub>"

inlineToDokuWiki opts (SmallCaps lst) = inlineListToDokuWiki opts lst

inlineToDokuWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToDokuWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToDokuWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

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
  return $ "''%%" ++ str ++ "%%''"

inlineToDokuWiki _ (Str str) = return $ escapeString str

inlineToDokuWiki _ (Math mathType str) = return $ delim ++ str ++ delim
                                 -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

inlineToDokuWiki _ il@(RawInline f str)
  | f == Format "dokuwiki" = return str
  | f == Format "html"     = return $ "<html>" ++ str ++ "</html>"
  | otherwise              = "" <$ report (InlineNotRendered il)

inlineToDokuWiki _ LineBreak = return "\\\\\n"

inlineToDokuWiki opts SoftBreak =
  case writerWrapText opts of
       WrapNone     -> return " "
       WrapAuto     -> return " "
       WrapPreserve -> return "\n"

inlineToDokuWiki _ Space = return " "

inlineToDokuWiki opts (Link _ txt (src, _)) = do
  label <- inlineListToDokuWiki opts txt
  case txt of
     [Str s] | "mailto:" `isPrefixOf` src -> return $ "<" ++ s ++ ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" ++ src  ++ "|" ++ label ++ "]]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToDokuWiki opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToDokuWiki opts alt
  let txt = case (tit, alt) of
              ("", []) -> ""
              ("", _ ) -> "|" ++ alt'
              (_ , _ ) -> "|" ++ tit
      -- Relative links fail isURI and receive a colon
      prefix = if isURI source then "" else ":"
  return $ "{{" ++ prefix ++ source ++ imageDims opts attr ++ txt ++ "}}"

inlineToDokuWiki opts (Note contents) = do
  contents' <- blockListToDokuWiki opts contents
  modify (\s -> s { stNotes = True })
  return $ "((" ++ contents' ++ "))"
  -- note - may not work for notes with multiple blocks

imageDims :: WriterOptions -> Attr -> String
imageDims opts attr = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
  where
    toPx = fmap (showInPixel opts) . checkPct
    checkPct (Just (Percent _)) = Nothing
    checkPct maybeDim           = maybeDim
    go (Just w) Nothing  = "?" ++ w
    go (Just w) (Just h) = "?" ++ w ++ "x" ++ h
    go Nothing  (Just h) = "?0x" ++ h
    go Nothing  Nothing  = ""

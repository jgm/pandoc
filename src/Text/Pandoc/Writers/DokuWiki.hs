{-
Copyright (C) 2008-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2008-2014 John MacFarlane
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
import Text.Pandoc.Definition
import Text.Pandoc.Options ( WriterOptions(
                                writerTableOfContents
                              , writerStandalone
                              , writerTemplate) )
import Text.Pandoc.Shared ( escapeURI, removeFormatting, camelCaseToHyphenated
                          , trimr, normalize, substitute  )
import Text.Pandoc.Writers.Shared ( defField, metaToJSON )
import Text.Pandoc.Templates ( renderTemplate' )
import Data.List ( intersect, intercalate, isPrefixOf, transpose )
import Data.Default (Default(..))
import Network.URI ( isURI )
import Control.Monad ( zipWithM )
import Control.Monad.State ( modify, State, get, evalState )
import Control.Monad.Reader ( ReaderT, runReaderT, ask, local )
import Control.Applicative ( (<$>) )

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  }

data WriterEnvironment = WriterEnvironment {
    stIndent    :: String          -- Indent after the marker at the beginning of list items
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  , stBackSlashLB :: Bool     -- True if we should produce formatted strings with newlines (as in a table cell)
  }

instance Default WriterState where
  def = WriterState { stNotes = False }

instance Default WriterEnvironment where
  def = WriterEnvironment { stIndent = ""
                          , stUseTags = False
                          , stBackSlashLB = False }

type DokuWiki = ReaderT WriterEnvironment (State WriterState)

-- | Convert Pandoc to DokuWiki.
writeDokuWiki :: WriterOptions -> Pandoc -> String
writeDokuWiki opts document =
  runDokuWiki (pandocToDokuWiki opts $ normalize document)

runDokuWiki :: DokuWiki a -> a
runDokuWiki = flip evalState def . flip runReaderT def

-- | Return DokuWiki representation of document.
pandocToDokuWiki :: WriterOptions -> Pandoc -> DokuWiki String
pandocToDokuWiki opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap trimr . blockListToDokuWiki opts)
              (inlineListToDokuWiki opts)
              meta
  body <- blockListToDokuWiki opts blocks
  notesExist <- stNotes <$> get
  let notes = if notesExist
                 then "" -- TODO Was "\n<references />" Check whether I can really remove this:
                         -- if it is definitely to do with footnotes, can remove this whole bit
                 else ""
  let main = body ++ notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts)
                $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Escape special characters for DokuWiki.
escapeString :: String -> String
escapeString = substitute "__" "%%__%%" .
               substitute "**" "%%**%%" .
               substitute "//" "%%//%%"

-- | Convert Pandoc block element to DokuWiki.
blockToDokuWiki :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> DokuWiki String

blockToDokuWiki _ Null = return ""

blockToDokuWiki opts (Div _attrs bs) = do
  contents <- blockListToDokuWiki opts bs
  return $ contents ++ "\n"

blockToDokuWiki opts (Plain inlines) =
  inlineListToDokuWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
-- dokuwiki doesn't support captions - so combine together alt and caption into alt
blockToDokuWiki opts (Para [Image txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else (" " ++) `fmap` inlineListToDokuWiki opts txt
  let opt = if null txt
               then ""
               else "|" ++ if null tit then capt else tit ++ capt
  return $ "{{:" ++ src ++ opt ++ "}}\n"

blockToDokuWiki opts (Para inlines) = do
  indent <- stIndent <$> ask
  useTags <- stUseTags <$> ask
  contents <- inlineListToDokuWiki opts inlines
  return $ if useTags
              then "<HTML><p></HTML>" ++ contents ++ "<HTML></p></HTML>"
              else contents ++ if null indent then "\n" else ""

blockToDokuWiki _ (RawBlock f str)
  | f == Format "dokuwiki" = return str
  -- See https://www.dokuwiki.org/wiki:syntax
  -- use uppercase HTML tag for block-level content:
  | f == Format "html"     = return $ "<HTML>\n" ++ str ++ "\n</HTML>"
  | otherwise              = return ""

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
  let (beg, end) = if null at
                      then ("<code" ++ if null classes then ">" else " class=\"" ++ unwords classes ++ "\">", "</code>")
                      else ("<source lang=\"" ++ head at ++ "\">", "</source>")
  return $ beg ++ str ++ end

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
listItemToDokuWiki :: WriterOptions -> [Block] -> DokuWiki String
listItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- stUseTags <$> ask
  if useTags
     then return $ "<HTML><li></HTML>" ++ contents ++ "<HTML></li></HTML>"
     else do
       indent <- stIndent <$> ask
       backSlash <- stBackSlashLB <$> ask
       let indent' = if backSlash then (drop 2 indent) else indent
       return $ indent' ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to DokuWiki.
-- | TODO Emiminate dreadful duplication of text from listItemToDokuWiki
orderedListItemToDokuWiki :: WriterOptions -> [Block] -> DokuWiki String
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
definitionListItemToDokuWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> DokuWiki String
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
       Plain _           -> True
       Para  _           -> True
       BulletList _      -> isSimpleList x
       OrderedList _ _   -> isSimpleList x
       DefinitionList _  -> isSimpleList x
       _                 -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _      -> isSimpleList y
       OrderedList _ _   -> isSimpleList y
       DefinitionList _  -> isSimpleList y
       _                 -> False
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
        g s = s

-- Auxiliary functions for tables:

tableItemToDokuWiki :: WriterOptions
                     -> Alignment
                     -> [Block]
                     -> DokuWiki String
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
blockListToDokuWiki :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> DokuWiki String
blockListToDokuWiki opts blocks = do
  backSlash <- stBackSlashLB <$> ask
  if backSlash
    then (backSlashLineBreaks . vcat) <$> mapM (blockToDokuWiki opts) blocks
    else vcat <$> mapM (blockToDokuWiki opts) blocks

-- | Convert list of Pandoc inline elements to DokuWiki.
inlineListToDokuWiki :: WriterOptions -> [Inline] -> DokuWiki String
inlineListToDokuWiki opts lst =
  concat <$> (mapM (inlineToDokuWiki opts) lst)

-- | Convert Pandoc inline element to DokuWiki.
inlineToDokuWiki :: WriterOptions -> Inline -> DokuWiki String

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

inlineToDokuWiki _ (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                                 -- note:  str should NOT be escaped

inlineToDokuWiki _ (RawInline f str)
  | f == Format "dokuwiki" = return str
  | f == Format "html"     = return $ "<html>" ++ str ++ "</html>"
  | otherwise              = return ""

inlineToDokuWiki _ (LineBreak) = return "\\\\ "

inlineToDokuWiki _ Space = return " "

inlineToDokuWiki opts (Link txt (src, _)) = do
  label <- inlineListToDokuWiki opts txt
  case txt of
     [Str s] | "mailto:" `isPrefixOf` src -> return $ "<" ++ s ++ ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" ++ src ++ "|" ++ label ++ "]]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToDokuWiki opts (Image alt (source, tit)) = do
  alt' <- inlineListToDokuWiki opts alt
  let txt = case (tit, alt) of
              ("", []) -> ""
              ("", _ ) -> "|" ++ alt'
              (_ , _ ) -> "|" ++ tit
  return $ "{{:" ++ source ++ txt ++ "}}"

inlineToDokuWiki opts (Note contents) = do
  contents' <- blockListToDokuWiki opts contents
  modify (\s -> s { stNotes = True })
  return $ "((" ++ contents' ++ "))"
  -- note - may not work for notes with multiple blocks

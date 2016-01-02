{-
Copyright (C) 2008-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.MediaWiki
   Copyright   : Copyright (C) 2008-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to MediaWiki markup.

MediaWiki:  <http://www.mediawiki.org/wiki/MediaWiki>
-}
module Text.Pandoc.Writers.MediaWiki ( writeMediaWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Pretty (render)
import Text.Pandoc.ImageSize
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intersect, intercalate )
import Network.URI ( isURI )
import Control.Monad.Reader
import Control.Monad.State

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stOptions   :: WriterOptions   -- writer options
  }

data WriterReader = WriterReader {
    options     :: WriterOptions -- Writer options
  , listLevel   :: String        -- String at beginning of list items, e.g. "**"
  , useTags     :: Bool          -- True if we should use HTML tags because we're in a complex list
  }

type MediaWikiWriter = ReaderT WriterReader (State WriterState)

-- | Convert Pandoc to MediaWiki.
writeMediaWiki :: WriterOptions -> Pandoc -> String
writeMediaWiki opts document =
  let initialState = WriterState { stNotes = False, stOptions = opts }
      env = WriterReader { options = opts, listLevel = [], useTags = False }
  in  evalState (runReaderT (pandocToMediaWiki document) env) initialState

-- | Return MediaWiki representation of document.
pandocToMediaWiki :: Pandoc -> MediaWikiWriter String
pandocToMediaWiki (Pandoc meta blocks) = do
  opts <- asks options
  metadata <- metaToJSON opts
              (fmap trimr . blockListToMediaWiki)
              inlineListToMediaWiki
              meta
  body <- blockListToMediaWiki blocks
  notesExist <- gets stNotes
  let notes = if notesExist
                 then "\n<references />"
                 else ""
  let main = body ++ notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts) metadata
  return $ if writerStandalone opts
     then renderTemplate' (writerTemplate opts) context
     else main

-- | Escape special characters for MediaWiki.
escapeString :: String -> String
escapeString =  escapeStringForXML

-- | Convert Pandoc block element to MediaWiki.
blockToMediaWiki :: Block         -- ^ Block element
                 -> MediaWikiWriter String

blockToMediaWiki Null = return ""

blockToMediaWiki (Div attrs bs) = do
  contents <- blockListToMediaWiki bs
  return $ render Nothing (tagWithAttrs "div" attrs) ++ "\n\n" ++
                     contents ++ "\n\n" ++ "</div>"

blockToMediaWiki (Plain inlines) =
  inlineListToMediaWiki inlines

-- title beginning with fig: indicates that the image is a figure
blockToMediaWiki (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else ("|caption " ++) `fmap` inlineListToMediaWiki txt
  img  <- imageToMediaWiki attr
  let opt = if null txt
               then ""
               else "|alt=" ++ if null tit then capt else tit ++ capt
  return $ "[[File:" ++ src ++ "|frame|none" ++ img ++ opt ++ "]]\n"

blockToMediaWiki (Para inlines) = do
  tags <- asks useTags
  lev <- asks listLevel
  contents <- inlineListToMediaWiki inlines
  return $ if tags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null lev then "\n" else ""

blockToMediaWiki (RawBlock f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = return ""

blockToMediaWiki HorizontalRule = return "\n-----\n"

blockToMediaWiki (Header level _ inlines) = do
  contents <- inlineListToMediaWiki inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToMediaWiki (CodeBlock (_,classes,_) str) = do
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  return $
    if null at
       then "<pre" ++ (if null classes
                          then ">"
                          else " class=\"" ++ unwords classes ++ "\">") ++
            escapeString str ++ "</pre>"
       else "<source lang=\"" ++ head at ++ "\">" ++ str ++ "</source>"
            -- note:  no escape!

blockToMediaWiki (BlockQuote blocks) = do
  contents <- blockListToMediaWiki blocks
  return $ "<blockquote>" ++ contents ++ "</blockquote>"

blockToMediaWiki (Table capt aligns widths headers rows') = do
  caption <- if null capt
                then return ""
                else do
                   c <- inlineListToMediaWiki capt
                   return $ "|+ " ++ trimr c ++ "\n"
  let headless = all null headers
  let allrows = if headless then rows' else headers:rows'
  tableBody <- intercalate "|-\n" `fmap`
                mapM (tableRowToMediaWiki headless aligns widths)
                     (zip [1..] allrows)
  return $ "{|\n" ++ caption ++ tableBody ++ "|}\n"

blockToMediaWiki x@(BulletList items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        contents <- local (\ s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ "*" }) $ mapM listItemToMediaWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

blockToMediaWiki x@(OrderedList attribs items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        contents <- local (\s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ "#" }) $ mapM listItemToMediaWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

blockToMediaWiki x@(DefinitionList items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        contents <- local (\s -> s { useTags = True }) $ mapM definitionListItemToMediaWiki items
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ ";" }) $ mapM definitionListItemToMediaWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

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

-- | Convert bullet or ordered list item (list of blocks) to MediaWiki.
listItemToMediaWiki :: [Block] -> MediaWikiWriter String
listItemToMediaWiki items = do
  contents <- blockListToMediaWiki items
  tags <- asks useTags
  if tags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- asks listLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: ([Inline],[[Block]])
                              -> MediaWikiWriter String
definitionListItemToMediaWiki (label, items) = do
  labelText <- inlineListToMediaWiki label
  contents <- mapM blockListToMediaWiki items
  tags <- asks useTags
  if tags
     then return $ "<dt>" ++ labelText ++ "</dt>\n" ++
           intercalate "\n" (map (\d -> "<dd>" ++ d ++ "</dd>") contents)
     else do
       marker <- asks listLevel
       return $ marker ++ " " ++ labelText ++ "\n" ++
           intercalate "\n" (map (\d -> init marker ++ ": " ++ d) contents)

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

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables:

tableRowToMediaWiki :: Bool
                    -> [Alignment]
                    -> [Double]
                    -> (Int, [[Block]])
                    -> MediaWikiWriter String
tableRowToMediaWiki headless alignments widths (rownum, cells) = do
  cells' <- mapM (tableCellToMediaWiki headless rownum)
          $ zip3 alignments widths cells
  return $ unlines cells'

tableCellToMediaWiki :: Bool
                     -> Int
                     -> (Alignment, Double, [Block])
                     -> MediaWikiWriter String
tableCellToMediaWiki headless rownum (alignment, width, bs) = do
  contents <- blockListToMediaWiki bs
  let marker = if rownum == 1 && not headless then "!" else "|"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let attrs = ["align=" ++ show (alignmentToString alignment) |
                 alignment /= AlignDefault && alignment /= AlignLeft] ++
              ["width=\"" ++ percent width ++ "\"" |
                 width /= 0.0 && rownum == 1]
  let attr = if null attrs
                then ""
                else unwords attrs ++ "|"
  let sep = case bs of
                 [Plain _] -> " "
                 [Para  _] -> " "
                 _         -> "\n"
  return $ marker ++ attr ++ sep ++ trimr contents

alignmentToString :: Alignment -> String
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

imageToMediaWiki :: Attr -> MediaWikiWriter String
imageToMediaWiki attr = do
  opts <- gets stOptions
  let (_, cls, _) = attr
      toPx = fmap (showInPixel opts) . checkPct
      checkPct (Just (Percent _)) = Nothing
      checkPct maybeDim = maybeDim
      go (Just w) Nothing  = '|':w ++ "px"
      go (Just w) (Just h) = '|':w ++ "x" ++ h ++ "px"
      go Nothing  (Just h) = "|x" ++ h ++ "px"
      go Nothing  Nothing  = ""
      dims = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
      classes = if null cls
                   then ""
                   else "|class=" ++ unwords cls
  return $ dims ++ classes

-- | Convert list of Pandoc block elements to MediaWiki.
blockListToMediaWiki :: [Block]       -- ^ List of block elements
                     -> MediaWikiWriter String
blockListToMediaWiki blocks =
  fmap vcat $ mapM blockToMediaWiki blocks

-- | Convert list of Pandoc inline elements to MediaWiki.
inlineListToMediaWiki :: [Inline] -> MediaWikiWriter String
inlineListToMediaWiki lst =
  fmap concat $ mapM inlineToMediaWiki lst

-- | Convert Pandoc inline element to MediaWiki.
inlineToMediaWiki :: Inline -> MediaWikiWriter String

inlineToMediaWiki (Span attrs ils) = do
  contents <- inlineListToMediaWiki ils
  return $ render Nothing (tagWithAttrs "span" attrs) ++ contents ++ "</span>"

inlineToMediaWiki (Emph lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "''" ++ contents ++ "''"

inlineToMediaWiki (Strong lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "'''" ++ contents ++ "'''"

inlineToMediaWiki (Strikeout lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<s>" ++ contents ++ "</s>"

inlineToMediaWiki (Superscript lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<sup>" ++ contents ++ "</sup>"

inlineToMediaWiki (Subscript lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "<sub>" ++ contents ++ "</sub>"

inlineToMediaWiki (SmallCaps lst) = inlineListToMediaWiki lst

inlineToMediaWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToMediaWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToMediaWiki lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToMediaWiki (Cite _  lst) = inlineListToMediaWiki lst

inlineToMediaWiki (Code _ str) =
  return $ "<code>" ++ escapeString str ++ "</code>"

inlineToMediaWiki (Str str) = return $ escapeString str

inlineToMediaWiki (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                               -- note:  str should NOT be escaped

inlineToMediaWiki (RawInline f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = return ""

inlineToMediaWiki (LineBreak) = return "<br />\n"

inlineToMediaWiki SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapAuto     -> return " "
       WrapNone     -> return " "
       WrapPreserve -> return "\n"

inlineToMediaWiki Space = return " "

inlineToMediaWiki (Link _ txt (src, _)) = do
  label <- inlineListToMediaWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return src
     _  -> return $ if isURI src
              then "[" ++ src ++ " " ++ label ++ "]"
              else "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page

inlineToMediaWiki (Image attr alt (source, tit)) = do
  img  <- imageToMediaWiki attr
  alt' <- inlineListToMediaWiki alt
  let txt = if null tit
               then if null alt
                       then ""
                       else '|' : alt'
               else '|' : tit
  return $ "[[File:" ++ source ++ img ++ txt ++ "]]"

inlineToMediaWiki (Note contents) = do
  contents' <- blockListToMediaWiki contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" ++ contents' ++ "</ref>"
  -- note - may not work for notes with multiple blocks

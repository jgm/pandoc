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
   Module      : Text.Pandoc.Writers.MediaWiki
   Copyright   : Copyright (C) 2008-2014 John MacFarlane
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
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intersect, intercalate, intersperse )
import Network.URI ( isURI )
import Control.Monad.State

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stListLevel :: [Char]          -- String at beginning of list items, e.g. "**"
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to MediaWiki.
writeMediaWiki :: WriterOptions -> Pandoc -> String
writeMediaWiki opts document =
  evalState (pandocToMediaWiki opts document)
            WriterState { stNotes = False, stListLevel = [], stUseTags = False }

-- | Return MediaWiki representation of document.
pandocToMediaWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToMediaWiki opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap trimr . blockListToMediaWiki opts)
              (inlineListToMediaWiki opts)
              meta
  body <- blockListToMediaWiki opts blocks
  notesExist <- get >>= return . stNotes
  let notes = if notesExist
                 then "\n<references />"
                 else ""
  let main = body ++ notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts)
                $ metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

-- | Escape special characters for MediaWiki.
escapeString :: String -> String
escapeString =  escapeStringForXML

-- | Convert Pandoc block element to MediaWiki.
blockToMediaWiki :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToMediaWiki _ Null = return ""

blockToMediaWiki opts (Div attrs bs) = do
  contents <- blockListToMediaWiki opts bs
  return $ render Nothing (tagWithAttrs "div" attrs) ++ "\n\n" ++
                     contents ++ "\n\n" ++ "</div>"

blockToMediaWiki opts (Plain inlines) =
  inlineListToMediaWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
blockToMediaWiki opts (Para [Image txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else ("|caption " ++) `fmap` inlineListToMediaWiki opts txt
  let opt = if null txt
               then ""
               else "|alt=" ++ if null tit then capt else tit ++ capt
  return $ "[[Image:" ++ src ++ "|frame|none" ++ opt ++ "]]\n"

blockToMediaWiki opts (Para inlines) = do
  useTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToMediaWiki opts inlines
  return $ if useTags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null listLevel then "\n" else ""

blockToMediaWiki _ (RawBlock f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = return ""

blockToMediaWiki _ HorizontalRule = return "\n-----\n"

blockToMediaWiki opts (Header level _ inlines) = do
  contents <- inlineListToMediaWiki opts inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToMediaWiki _ (CodeBlock (_,classes,_) str) = do
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
                      then ("<pre" ++ if null classes then ">" else " class=\"" ++ unwords classes ++ "\">", "</pre>")
                      else ("<source lang=\"" ++ head at ++ "\">", "</source>")
  return $ beg ++ escapeString str ++ end

blockToMediaWiki opts (BlockQuote blocks) = do
  contents <- blockListToMediaWiki opts blocks
  return $ "<blockquote>" ++ contents ++ "</blockquote>"

blockToMediaWiki opts (Table capt aligns widths headers rows') = do
  caption <- if null capt
                then return ""
                else do
                   c <- inlineListToMediaWiki opts capt
                   return $ "|+ " ++ trimr c ++ "\n"
  let headless = all null headers
  let allrows = if headless then rows' else headers:rows'
  tableBody <- (concat . intersperse "|-\n") `fmap`
                mapM (tableRowToMediaWiki opts headless aligns widths)
                     (zip [1..] allrows)
  return $ "{|\n" ++ caption ++ tableBody ++ "|}\n"

blockToMediaWiki opts x@(BulletList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToMediaWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "*" }
        contents <- mapM (listItemToMediaWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

blockToMediaWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToMediaWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "#" }
        contents <- mapM (listItemToMediaWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

blockToMediaWiki opts x@(DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (definitionListItemToMediaWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ ";" }
        contents <- mapM (definitionListItemToMediaWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

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
listItemToMediaWiki :: WriterOptions -> [Block] -> State WriterState String
listItemToMediaWiki opts items = do
  contents <- blockListToMediaWiki opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToMediaWiki opts (label, items) = do
  labelText <- inlineListToMediaWiki opts label
  contents <- mapM (blockListToMediaWiki opts) items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<dt>" ++ labelText ++ "</dt>\n" ++
           (intercalate "\n" $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ labelText ++ "\n" ++
           (intercalate "\n" $ map (\d -> init marker ++ ": " ++ d) contents)

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

tableRowToMediaWiki :: WriterOptions
                    -> Bool
                    -> [Alignment]
                    -> [Double]
                    -> (Int, [[Block]])
                    -> State WriterState String
tableRowToMediaWiki opts headless alignments widths (rownum, cells) = do
  cells' <- mapM (\cellData ->
          tableCellToMediaWiki opts headless rownum cellData)
          $ zip3 alignments widths cells
  return $ unlines cells'

tableCellToMediaWiki :: WriterOptions
                     -> Bool
                     -> Int
                     -> (Alignment, Double, [Block])
                     -> State WriterState String
tableCellToMediaWiki opts headless rownum (alignment, width, bs) = do
  contents <- blockListToMediaWiki opts bs
  let marker = if rownum == 1 && not headless then "!" else "|"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let attrs = ["align=" ++ show (alignmentToString alignment) |
                 alignment /= AlignDefault && alignment /= AlignLeft] ++
              ["width=\"" ++ percent width ++ "\"" |
                 width /= 0.0 && rownum == 1]
  let attr = if null attrs
                then ""
                else unwords attrs ++ "|"
  return $ marker ++ attr ++ trimr contents

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

-- | Convert list of Pandoc block elements to MediaWiki.
blockListToMediaWiki :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToMediaWiki opts blocks =
  mapM (blockToMediaWiki opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to MediaWiki.
inlineListToMediaWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToMediaWiki opts lst =
  mapM (inlineToMediaWiki opts) lst >>= return . concat

-- | Convert Pandoc inline element to MediaWiki.
inlineToMediaWiki :: WriterOptions -> Inline -> State WriterState String

inlineToMediaWiki opts (Span attrs ils) = do
  contents <- inlineListToMediaWiki opts ils
  return $ render Nothing (tagWithAttrs "span" attrs) ++ contents ++ "</span>"

inlineToMediaWiki opts (Emph lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "''" ++ contents ++ "''"

inlineToMediaWiki opts (Strong lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "'''" ++ contents ++ "'''"

inlineToMediaWiki opts (Strikeout lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "<s>" ++ contents ++ "</s>"

inlineToMediaWiki opts (Superscript lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "<sup>" ++ contents ++ "</sup>"

inlineToMediaWiki opts (Subscript lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "<sub>" ++ contents ++ "</sub>"

inlineToMediaWiki opts (SmallCaps lst) = inlineListToMediaWiki opts lst

inlineToMediaWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToMediaWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToMediaWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToMediaWiki opts (Cite _  lst) = inlineListToMediaWiki opts lst

inlineToMediaWiki _ (Code _ str) =
  return $ "<code>" ++ (escapeString str) ++ "</code>"

inlineToMediaWiki _ (Str str) = return $ escapeString str

inlineToMediaWiki _ (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                                 -- note:  str should NOT be escaped

inlineToMediaWiki _ (RawInline f str)
  | f == Format "mediawiki" = return str
  | f == Format "html"      = return str
  | otherwise               = return ""

inlineToMediaWiki _ (LineBreak) = return "<br />"

inlineToMediaWiki _ Space = return " "

inlineToMediaWiki opts (Link txt (src, _)) = do
  label <- inlineListToMediaWiki opts txt
  case txt of
     [Str s] | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[" ++ src ++ " " ++ label ++ "]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToMediaWiki opts (Image alt (source, tit)) = do
  alt' <- inlineListToMediaWiki opts alt
  let txt = if (null tit)
               then if null alt
                       then ""
                       else "|" ++ alt'
               else "|" ++ tit
  return $ "[[Image:" ++ source ++ txt ++ "]]"

inlineToMediaWiki opts (Note contents) = do
  contents' <- blockListToMediaWiki opts contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" ++ contents' ++ "</ref>"
  -- note - may not work for notes with multiple blocks

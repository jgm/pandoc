{-
Copyright (C) 2010-2014 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Textile
   Copyright   : Copyright (C) 2010-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Textile markup.

Textile:  <http://thresholdstate.com/articles/4312/the-textile-reference-manual>
-}
module Text.Pandoc.Writers.Textile ( writeTextile ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Pretty (render)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intercalate )
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Char ( isSpace )

data WriterState = WriterState {
    stNotes     :: [String]        -- Footnotes
  , stListLevel :: [Char]          -- String at beginning of list items, e.g. "**"
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to Textile.
writeTextile :: WriterOptions -> Pandoc -> String
writeTextile opts document =
  evalState (pandocToTextile opts document)
            WriterState { stNotes = [], stListLevel = [], stUseTags = False }

-- | Return Textile representation of document.
pandocToTextile :: WriterOptions -> Pandoc -> State WriterState String
pandocToTextile opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts (blockListToTextile opts)
                 (inlineListToTextile opts) meta
  body <- blockListToTextile opts blocks
  notes <- liftM (unlines . reverse . stNotes) get
  let main = body ++ if null notes then "" else ("\n\n" ++ notes)
  let context = defField "body" main metadata
  if writerStandalone opts
     then return $ renderTemplate' (writerTemplate opts) context
     else return main

withUseTags :: State WriterState a -> State WriterState a
withUseTags action = do
  oldUseTags <- liftM stUseTags get
  modify $ \s -> s { stUseTags = True }
  result <- action
  modify $ \s -> s { stUseTags = oldUseTags }
  return result

-- | Escape one character as needed for Textile.
escapeCharForTextile :: Char -> String
escapeCharForTextile x = case x of
                         '&'      -> "&amp;"
                         '<'      -> "&lt;"
                         '>'      -> "&gt;"
                         '"'      -> "&quot;"
                         '*'      -> "&#42;"
                         '_'      -> "&#95;"
                         '@'      -> "&#64;"
                         '|'      -> "&#124;"
                         '\x2014' -> " -- "
                         '\x2013' -> " - "
                         '\x2019' -> "'"
                         '\x2026' -> "..."
                         c        -> [c]

-- | Escape string as needed for Textile.
escapeStringForTextile :: String -> String
escapeStringForTextile = concatMap escapeCharForTextile

-- | Convert Pandoc block element to Textile.
blockToTextile :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToTextile _ Null = return ""

blockToTextile opts (Div attr bs) = do
  let startTag = render Nothing $ tagWithAttrs "div" attr
  let endTag = "</div>"
  contents <- blockListToTextile opts bs
  return $ startTag ++ "\n\n" ++ contents ++ "\n\n" ++ endTag ++ "\n"

blockToTextile opts (Plain inlines) =
  inlineListToTextile opts inlines

-- title beginning with fig: indicates that the image is a figure
blockToTextile opts (Para [Image txt (src,'f':'i':'g':':':tit)]) = do
  capt <- blockToTextile opts (Para txt)
  im <- inlineToTextile opts (Image txt (src,tit))
  return $ im ++ "\n" ++ capt

blockToTextile opts (Para inlines) = do
  useTags <- liftM stUseTags get
  listLevel <- liftM stListLevel get
  contents <- inlineListToTextile opts inlines
  return $ if useTags
              then "<p>" ++ contents ++ "</p>"
              else contents ++ if null listLevel then "\n" else ""

blockToTextile _ (RawBlock f str)
  | f == Format "html" || f == Format "textile" = return str
  | otherwise                                   = return ""

blockToTextile _ HorizontalRule = return "<hr />\n"

blockToTextile opts (Header level (ident,classes,keyvals) inlines) = do
  contents <- inlineListToTextile opts inlines
  let identAttr = if null ident then "" else ('#':ident)
  let attribs = if null identAttr && null classes
                   then ""
                   else "(" ++ unwords classes ++ identAttr ++ ")"
  let lang = maybe "" (\x -> "[" ++ x ++ "]") $ lookup "lang" keyvals
  let styles = maybe "" (\x -> "{" ++ x ++ "}") $ lookup "style" keyvals
  let prefix = 'h' : show level ++ attribs ++ styles ++ lang ++ ". "
  return $ prefix ++ contents ++ "\n"

blockToTextile _ (CodeBlock (_,classes,_) str) | any (all isSpace) (lines str) =
  return $ "<pre"  ++ classes' ++ ">\n" ++ escapeStringForXML str ++
           "\n</pre>\n"
    where classes' = if null classes
                        then ""
                        else " class=\"" ++ unwords classes ++ "\""

blockToTextile _ (CodeBlock (_,classes,_) str) =
  return $ "bc" ++ classes' ++ ". " ++ str ++ "\n\n"
    where classes' = if null classes
                        then ""
                        else "(" ++ unwords classes ++ ")"

blockToTextile opts (BlockQuote bs@[Para _]) = do
  contents <- blockListToTextile opts bs
  return $ "bq. " ++ contents ++ "\n\n"

blockToTextile opts (BlockQuote blocks) = do
  contents <- blockListToTextile opts blocks
  return $ "<blockquote>\n\n" ++ contents ++ "\n</blockquote>\n"

blockToTextile opts (Table [] aligns widths headers rows') |
         all (==0) widths = do
  hs <- mapM (liftM (("_. " ++) . stripTrailingNewlines) . blockListToTextile opts) headers
  let cellsToRow cells = "|" ++ intercalate "|" cells ++ "|"
  let header = if all null headers then "" else cellsToRow hs ++ "\n"
  let blocksToCell (align, bs) = do
        contents <- stripTrailingNewlines <$> blockListToTextile opts bs
        let alignMarker = case align of
                               AlignLeft    -> "<. "
                               AlignRight   -> ">. "
                               AlignCenter  -> "=. "
                               AlignDefault -> ""
        return $ alignMarker ++ contents
  let rowToCells = mapM blocksToCell . zip aligns
  bs <- mapM rowToCells rows'
  let body = unlines $ map cellsToRow bs
  return $ header ++ body

blockToTextile opts (Table capt aligns widths headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToTextile opts capt
                      return $ "<caption>" ++ c ++ "</caption>\n"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then ""
                   else unlines $ map
                         (\w -> "<col width=\"" ++ percent w ++ "\" />") widths
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToTextile opts alignStrings 0 headers
                 return $ "<thead>\n" ++ hs ++ "\n</thead>\n"
  body' <- zipWithM (tableRowToTextile opts alignStrings) [1..] rows'
  return $ "<table>\n" ++ captionDoc ++ coltags ++ head' ++
            "<tbody>\n" ++ unlines body' ++ "</tbody>\n</table>\n"

blockToTextile opts x@(BulletList items) = do
  oldUseTags <- liftM stUseTags get
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- withUseTags $ mapM (listItemToTextile opts) items
        return $ "<ul>\n" ++ vcat contents ++ "\n</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "*" }
        level <- get >>= return . length . stListLevel
        contents <- mapM (listItemToTextile opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ (if level > 1 then "" else "\n")

blockToTextile opts x@(OrderedList attribs items) = do
  oldUseTags <- liftM stUseTags get
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        contents <- withUseTags $ mapM (listItemToTextile opts) items
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++
                   "\n</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ "#" }
        level <- get >>= return . length . stListLevel
        contents <- mapM (listItemToTextile opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ (if level > 1 then "" else "\n")

blockToTextile opts (DefinitionList items) = do
  contents <- withUseTags $ mapM (definitionListItemToTextile opts) items
  return $ "<dl>\n" ++ vcat contents ++ "\n</dl>\n"

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

-- | Convert bullet or ordered list item (list of blocks) to Textile.
listItemToTextile :: WriterOptions -> [Block] -> State WriterState String
listItemToTextile opts items = do
  contents <- blockListToTextile opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to Textile.
definitionListItemToTextile :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToTextile opts (label, items) = do
  labelText <- inlineListToTextile opts label
  contents <- mapM (blockListToTextile opts) items
  return $ "<dt>" ++ labelText ++ "</dt>\n" ++
          (intercalate "\n" $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
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
       _                 -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _      -> isSimpleList y
       OrderedList _ _   -> isSimpleList y
       _                 -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables. (TODO: these are common to HTML, MediaWiki,
-- and Textile writers, and should be abstracted out.)

tableRowToTextile :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToTextile opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToTextile opts celltype alignment item)
            alignStrings cols'
  return $ "<tr class=\"" ++ rowclass ++ "\">\n" ++ unlines cols'' ++ "</tr>"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToTextile :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToTextile opts celltype align' item = do
  let mkcell x = "<" ++ celltype ++ " align=\"" ++ align' ++ "\">" ++
                    x ++ "</" ++ celltype ++ ">"
  contents <- blockListToTextile opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Textile.
blockListToTextile :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToTextile opts blocks =
  mapM (blockToTextile opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to Textile.
inlineListToTextile :: WriterOptions -> [Inline] -> State WriterState String
inlineListToTextile opts lst =
  mapM (inlineToTextile opts) lst >>= return . concat

-- | Convert Pandoc inline element to Textile.
inlineToTextile :: WriterOptions -> Inline -> State WriterState String

inlineToTextile opts (Span _ lst) =
  inlineListToTextile opts lst

inlineToTextile opts (Emph lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '_' `elem` contents
              then "<em>" ++ contents ++ "</em>"
              else "_" ++ contents ++ "_"

inlineToTextile opts (Strong lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '*' `elem` contents
              then "<strong>" ++ contents ++ "</strong>"
              else "*" ++ contents ++ "*"

inlineToTextile opts (Strikeout lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '-' `elem` contents
              then "<del>" ++ contents ++ "</del>"
              else "-" ++ contents ++ "-"

inlineToTextile opts (Superscript lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '^' `elem` contents
              then "<sup>" ++ contents ++ "</sup>"
              else "[^" ++ contents ++ "^]"

inlineToTextile opts (Subscript lst) = do
  contents <- inlineListToTextile opts lst
  return $ if '~' `elem` contents
              then "<sub>" ++ contents ++ "</sub>"
              else "[~" ++ contents ++ "~]"

inlineToTextile opts (SmallCaps lst) = inlineListToTextile opts lst

inlineToTextile opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "'" ++ contents ++ "'"

inlineToTextile opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "\"" ++ contents ++ "\""

inlineToTextile opts (Cite _  lst) = inlineListToTextile opts lst

inlineToTextile _ (Code _ str) =
  return $ if '@' `elem` str
           then "<tt>" ++ escapeStringForXML str ++ "</tt>"
           else "@" ++ str ++ "@"

inlineToTextile _ (Str str) = return $ escapeStringForTextile str

inlineToTextile _ (Math _ str) =
  return $ "<span class=\"math\">" ++ escapeStringForXML str ++ "</math>"

inlineToTextile opts (RawInline f str)
  | f == Format "html" || f == Format "textile" = return str
  | (f == Format "latex" || f == Format "tex") &&
     isEnabled Ext_raw_tex opts                 = return str
  | otherwise                                   = return ""

inlineToTextile _ (LineBreak) = return "\n"

inlineToTextile _ Space = return " "

inlineToTextile opts (Link txt (src, _)) = do
  label <- case txt of
                [Code _ s]
                 | s == src -> return "$"
                [Str s]
                 | s == src -> return "$"
                _           -> inlineListToTextile opts txt
  return $ "\"" ++ label ++ "\":" ++ src

inlineToTextile opts (Image alt (source, tit)) = do
  alt' <- inlineListToTextile opts alt
  let txt = if null tit
               then if null alt'
                       then ""
                       else "(" ++ alt' ++ ")"
               else "(" ++ tit ++ ")"
  return $ "!" ++ source ++ txt ++ "!"

inlineToTextile opts (Note contents) = do
  curNotes <- liftM stNotes get
  let newnum = length curNotes + 1
  contents' <- blockListToTextile opts contents
  let thisnote = "fn" ++ show newnum ++ ". " ++ contents' ++ "\n"
  modify $ \s -> s { stNotes = thisnote : curNotes }
  return $ "[" ++ show newnum ++ "]"
  -- note - may not work for notes with multiple blocks

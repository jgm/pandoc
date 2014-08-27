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
    [ ] Implement alignment of text in tables
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
import Data.List ( intersect, intercalate, isPrefixOf )
import Network.URI ( isURI )
import Control.Monad ( zipWithM )
import Control.Monad.State ( modify, State, get, gets, evalState )
import Control.Applicative ( (<$>) )

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stIndent    :: String          -- Indent after the marker at the beginning of list items
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to DokuWiki.
writeDokuWiki :: WriterOptions -> Pandoc -> String
writeDokuWiki opts document =
  evalState (pandocToDokuWiki opts $ normalize document)
            (WriterState { stNotes = False, stIndent = "", stUseTags = False })

-- | Return DokuWiki representation of document.
pandocToDokuWiki :: WriterOptions -> Pandoc -> State WriterState String
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
                -> State WriterState String

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
  indent <- gets stIndent
  useTags <- gets stUseTags
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
     then return $ "> " ++ contents
     else return $ "<HTML><blockquote>\n" ++ contents ++ "</blockquote></HTML>"

blockToDokuWiki opts (Table capt aligns _ headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToDokuWiki opts capt
                      return $ "" ++ c ++ "\n"
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableHeaderToDokuWiki opts alignStrings 0 headers
                 return $ hs ++ "\n"
  body' <- zipWithM (tableRowToDokuWiki opts alignStrings) [1..] rows'
  return $ captionDoc ++ head' ++
            unlines body'

blockToDokuWiki opts x@(BulletList items) = do
  oldUseTags <- stUseTags <$> get
  indent <- stIndent <$> get
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<HTML><ul></HTML>\n" ++ vcat contents ++ "<HTML></ul></HTML>\n"
     else do
        modify $ \s -> s { stIndent = stIndent s ++ "  " }
        contents <- mapM (listItemToDokuWiki opts) items
        modify $ \s -> s { stIndent = indent }
        return $ vcat contents ++ if null indent then "\n" else ""

blockToDokuWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- stUseTags <$> get
  indent <- stIndent <$> get
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (orderedListItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<HTML><ol" ++ listAttribsToString attribs ++ "></HTML>\n" ++ vcat contents ++ "<HTML></ol></HTML>\n"
     else do
        modify $ \s -> s { stIndent = stIndent s ++ "  " }
        contents <- mapM (orderedListItemToDokuWiki opts) items
        modify $ \s -> s { stIndent = indent }
        return $ vcat contents ++ if null indent then "\n" else ""

-- TODO Need to decide how to make definition lists work on dokuwiki - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToDokuWiki opts x@(DefinitionList items) = do
  oldUseTags <- stUseTags <$> get
  indent <- stIndent <$> get
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (definitionListItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<HTML><dl></HTML>\n" ++ vcat contents ++ "<HTML></dl></HTML>\n"
     else do
        modify $ \s -> s { stIndent = stIndent s ++ "  " }
        contents <- mapM (definitionListItemToDokuWiki opts) items
        modify $ \s -> s { stIndent = indent }
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
listItemToDokuWiki :: WriterOptions -> [Block] -> State WriterState String
listItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- stUseTags <$> get
  if useTags
     then return $ "<HTML><li></HTML>" ++ contents ++ "<HTML></li></HTML>"
     else do
       indent <- stIndent <$> get
       return $ indent ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to DokuWiki.
-- | TODO Emiminate dreadful duplication of text from listItemToDokuWiki
orderedListItemToDokuWiki :: WriterOptions -> [Block] -> State WriterState String
orderedListItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- stUseTags <$> get
  if useTags
     then return $ "<HTML><li></HTML>" ++ contents ++ "<HTML></li></HTML>"
     else do
       indent <- stIndent <$> get
       return $ indent ++ "- " ++ contents

-- | Convert definition list item (label, list of blocks) to DokuWiki.
definitionListItemToDokuWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToDokuWiki opts (label, items) = do
  labelText <- inlineListToDokuWiki opts label
  contents <- mapM (blockListToDokuWiki opts) items
  useTags <- stUseTags <$> get
  if useTags
     then return $ "<HTML><dt></HTML>" ++ labelText ++ "<HTML></dt></HTML>\n" ++
           (intercalate "\n" $ map (\d -> "<HTML><dd></HTML>" ++ d ++ "<HTML></dd></HTML>") contents)
     else do
       indent <- stIndent <$> get
       return $ indent ++ "* **" ++ labelText ++ "** " ++ concat contents

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
isSimpleBlockQuote [BlockQuote bs] = isSimpleBlockQuote bs
isSimpleBlockQuote [b] = isPlainOrPara b
isSimpleBlockQuote _   = False

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables:

-- TODO Eliminate copy-and-pasted code in tableHeaderToDokuWiki and tableRowToDokuWiki
tableHeaderToDokuWiki :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableHeaderToDokuWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "" else ""
  cols'' <- zipWithM
            (tableItemToDokuWiki opts celltype)
            alignStrings cols'
  return $ "^ " ++ "" ++ joinHeaders cols'' ++ " ^"

tableRowToDokuWiki :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToDokuWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "" else ""
  cols'' <- zipWithM
            (tableItemToDokuWiki opts celltype)
            alignStrings cols'
  return $ "| " ++ "" ++ joinColumns cols'' ++ " |"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> ""
                                 AlignRight   -> ""
                                 AlignCenter  -> ""
                                 AlignDefault -> ""

tableItemToDokuWiki :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
-- TODO Fix celltype and align' defined but not used
tableItemToDokuWiki opts _celltype _align' item = do
  let mkcell x = "" ++ x ++ ""
  contents <- blockListToDokuWiki opts item
  return $ mkcell contents

-- | Concatenates columns together.
joinColumns :: [String] -> String
joinColumns = intercalate " | "

-- | Concatenates headers together.
joinHeaders :: [String] -> String
joinHeaders = intercalate " ^ "

-- | Convert list of Pandoc block elements to DokuWiki.
blockListToDokuWiki :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToDokuWiki opts blocks =
  vcat <$> mapM (blockToDokuWiki opts) blocks

-- | Convert list of Pandoc inline elements to DokuWiki.
inlineListToDokuWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToDokuWiki opts lst =
  concat <$> (mapM (inlineToDokuWiki opts) lst)

-- | Convert Pandoc inline element to DokuWiki.
inlineToDokuWiki :: WriterOptions -> Inline -> State WriterState String

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

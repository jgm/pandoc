{-
Copyright (C) 2008-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2008-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to DokuWiki markup.

DokuWiki:  <https://www.dokuwiki.org/dokuwiki>
-}

{-
    [ ] Don't generate <blockquote>...
    [ ] Implement definition lists
    [ ] Don't generate lists using <ol> and <ul>
    [ ] Don't generate <div>
    [ ] Implement alignment of text in tables
    [ ] Implement comments
    [ ] Work through the Dokuwiki spec, and check I've not missed anything out
    [ ] Test the output in Dokuwiki - and compare against the display of another format, e.g. HTML
    [ ] Remove dud/duplicate code
-}

module Text.Pandoc.Writers.DokuWiki ( writeDokuWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Templates (renderTemplate')
import Data.List ( intersect, intercalate )
import Network.URI ( isURI )
import Control.Monad.State

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stListLevel :: [Char]          -- String at beginning of list items, e.g. "**"
  , stUseTags   :: Bool            -- True if we should use HTML tags because we're in a complex list
  }

-- | Convert Pandoc to DokuWiki.
writeDokuWiki :: WriterOptions -> Pandoc -> String
writeDokuWiki opts document =
  evalState (pandocToDokuWiki opts document)
            (WriterState { stNotes = False, stListLevel = [], stUseTags = False })

-- | Return DokuWiki representation of document.
pandocToDokuWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToDokuWiki opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap trimr . blockListToDokuWiki opts)
              (inlineListToDokuWiki opts)
              meta
  body <- blockListToDokuWiki opts blocks
  notesExist <- get >>= return . stNotes
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

-- | Escape special characters for MediaWiki.
escapeString :: String -> String
escapeString str = substitute "__" "%%__%%" ( substitute "**" "%%**%%" ( substitute "//" "%%//%%" str ) )

-- | Convert Pandoc block element to DokuWiki.
blockToDokuWiki :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToDokuWiki _ Null = return ""

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
  useTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToDokuWiki opts inlines
  return $ if useTags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null listLevel then "\n" else ""

blockToDokuWiki _ (RawBlock "mediawiki" str) = return str
blockToDokuWiki _ (RawBlock "html" str) = return str
blockToDokuWiki _ (RawBlock _ _) = return ""

blockToDokuWiki _ HorizontalRule = return "\n----\n"

blockToDokuWiki opts (Header level _ inlines) = do
  contents <- inlineListToDokuWiki opts inlines
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
  return $ "<blockquote>" ++ contents ++ "</blockquote>"

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
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ " " }
        contents <- mapM (listItemToDokuWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

blockToDokuWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (orderedListItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ " " }
        contents <- mapM (orderedListItemToDokuWiki opts) items
        modify $ \s -> s { stListLevel = init (stListLevel s) }
        return $ vcat contents ++ if null listLevel then "\n" else ""

-- TODO Need to decide how to make definition lists work on dokuwiki - I don't think there
--      is a specific representation of them.
-- TODO This creates double '; ; ' if there is a bullet or ordered list inside a definition list
blockToDokuWiki opts x@(DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (definitionListItemToDokuWiki opts) items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
     else do
        modify $ \s -> s { stListLevel = stListLevel s ++ ";" }
        contents <- mapM (definitionListItemToDokuWiki opts) items
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

-- | Convert bullet list item (list of blocks) to DokuWiki.
listItemToDokuWiki :: WriterOptions -> [Block] -> State WriterState String
listItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       -- This marker ++ marker is an awful hack to write 2 spaces per indentation level.
       -- I couldn't find a cleaner way of doing it.
       return $ marker ++ marker ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to DokuWiki.
-- | TODO Emiminate dreadful duplication of text from listItemToDokuWiki
orderedListItemToDokuWiki :: WriterOptions -> [Block] -> State WriterState String
orderedListItemToDokuWiki opts items = do
  contents <- blockListToDokuWiki opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       marker <- get >>= return . stListLevel
       -- This marker ++ marker is an awful hack to write 2 spaces per indentation level.
       -- I couldn't find a cleaner way of doing it.
       return $ marker ++ marker ++ "- " ++ contents

-- | Convert definition list item (label, list of blocks) to DokuWiki.
definitionListItemToDokuWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToDokuWiki opts (label, items) = do
  labelText <- inlineListToDokuWiki opts label
  contents <- mapM (blockListToDokuWiki opts) items
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
       BulletList items                 -> True
       OrderedList (num, sty, _) items  -> True
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

-- TODO Eliminate copy-and-pasted code in tableHeaderToDokuWiki and tableRowToDokuWiki
tableHeaderToDokuWiki :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableHeaderToDokuWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "" else ""
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToDokuWiki opts celltype alignment item)
            alignStrings cols'
  return $ "^ " ++ "" ++ joinHeaders cols'' ++ " ^"

tableRowToDokuWiki :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToDokuWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "" else ""
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToDokuWiki opts celltype alignment item)
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
tableItemToDokuWiki opts celltype align' item = do
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
  mapM (blockToDokuWiki opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to DokuWiki.
inlineListToDokuWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToDokuWiki opts lst =
  mapM (inlineToDokuWiki opts) lst >>= return . concat

-- | Convert Pandoc inline element to DokuWiki.
inlineToDokuWiki :: WriterOptions -> Inline -> State WriterState String

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

inlineToDokuWiki _ (RawInline "mediawiki" str) = return str
inlineToDokuWiki _ (RawInline "html" str) = return str
inlineToDokuWiki _ (RawInline _ _) = return ""

inlineToDokuWiki _ (LineBreak) = return "\\\\ "

inlineToDokuWiki _ Space = return " "

inlineToDokuWiki opts (Link txt (src, _)) = do
  label <- inlineListToDokuWiki opts txt
  case txt of
     [Str s] | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" ++ src ++ "|" ++ label ++ "]]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToDokuWiki opts (Image alt (source, tit)) = do
  alt' <- inlineListToDokuWiki opts alt
  let txt = if (null tit)
               then if null alt
                       then ""
                       else "|" ++ alt'
               else "|" ++ tit
  return $ "{{:" ++ source ++ txt ++ "}}"

inlineToDokuWiki opts (Note contents) = do
  contents' <- blockListToDokuWiki opts contents
  modify (\s -> s { stNotes = True })
  return $ "((" ++ contents' ++ "))"
  -- note - may not work for notes with multiple blocks

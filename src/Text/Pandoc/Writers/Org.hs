{-
Copyright (C) 2006-2010 Puneeth Chaganti <punchagan@gmail.com>

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
   Module      : Text.Pandoc.Writers.Org
   Copyright   : Copyright (C) 2006-2010 Puneeth Chaganti
   License     : GNU GPL, version 2 or above 

   Maintainer  : Puneeth Chaganti <punchagan@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to reStructuredText.

reStructuredText:  <http://docutils.sourceforge.net/rst.html>
-}
module Text.Pandoc.Writers.Org ( writeOrg) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Blocks
import Text.Pandoc.Templates (renderTemplate)
import Data.List ( intersect, intersperse, transpose )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State
import Control.Applicative ( (<$>) )

data WriterState = 
  WriterState { stNotes     :: [[Block]]
              , stLinks     :: Bool
              , stImages    :: Bool
              , stHasMath   :: Bool
              , stOptions   :: WriterOptions
              }

-- | Convert Pandoc to Org.
writeOrg :: WriterOptions -> Pandoc -> String
writeOrg opts document = 
  let st = WriterState { stNotes = [], stLinks = False,
                         stImages = False, stHasMath = False,
                         stOptions = opts }
  in evalState (pandocToOrg document) st

-- | Return Org representation of document.
pandocToOrg :: Pandoc -> State WriterState String
pandocToOrg (Pandoc (Meta tit auth dat) blocks) = do
  opts <- liftM stOptions get
  title <- titleToOrg tit
  authors <- mapM inlineListToOrg auth
  date <- inlineListToOrg dat
  body <- blockListToOrg blocks
  notes <- liftM (reverse . stNotes) get >>= notesToOrg
  -- note that the notes may contain refs, so we do them first
  hasMath <- liftM stHasMath get
  let main = render $ foldl ($+$) empty $ [body, notes]
  let context = writerVariables opts ++
                [ ("body", main)
                , ("title", render title)
                , ("date", render date) ] ++
                [ ("math", "yes") | hasMath ] ++
                [ ("author", render a) | a <- authors ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Return Org representation of notes.
notesToOrg :: [[Block]] -> State WriterState Doc
notesToOrg notes = 
  mapM (\(num, note) -> noteToOrg num note) (zip [1..] notes) >>= 
  return . vcat

-- | Return Org representation of a note.
noteToOrg :: Int -> [Block] -> State WriterState Doc
noteToOrg num note = do
  contents <- blockListToOrg note
  let marker = text "[" <> text (show num) <> text "] "
  return $ marker <> contents

-- | Take list of inline elements and return wrapped doc.
wrappedOrg :: WriterOptions -> [Inline] -> State WriterState Doc
wrappedOrg opts inlines = do
  lineBreakDoc <- inlineToOrg LineBreak  
  chunks <- mapM (wrapIfNeeded opts inlineListToOrg)
                 (splitBy LineBreak inlines)
  return $ vcat $ intersperse lineBreakDoc chunks

-- | Escape special characters for Org.
escapeString :: String -> String
escapeString = escapeStringUsing (backslashEscapes "^_")

titleToOrg :: [Inline] -> State WriterState Doc
titleToOrg [] = return empty
titleToOrg lst = do
  contents <- inlineListToOrg lst
  let titleName = text "#+TITLE: "
  return $ titleName $+$ contents 

-- | Convert Pandoc block element to Org. 
blockToOrg :: Block         -- ^ Block element
           -> State WriterState Doc 
blockToOrg Null = return empty
blockToOrg (Plain inlines) = do
  opts <- get >>= (return . stOptions)
  wrappedOrg opts inlines
blockToOrg (Para [Image txt (src,tit)]) = do
  capt <- inlineListToOrg txt
  img <- inlineToOrg (Image txt (src,tit))
  return $ text "#+CAPTION: " <> capt <> text "\n" $$ img 
blockToOrg (Para inlines) = do
  opts <- get >>= (return . stOptions)
  contents <- wrappedOrg opts inlines
  return $ contents <> text "\n"
blockToOrg (RawHtml str) = 
  return $ (text "\n#+BEGIN_HTML\n") $$ (nest 2 $ vcat $ map text (lines str)) 
         $$ (text "\n#+END_HTML\n")
blockToOrg HorizontalRule = return $ text "--------------\n"
blockToOrg (Header level inlines) = do
  contents <- inlineListToOrg inlines
  let headerStr = text $ if level > 999 then " " else replicate level '*'
  return $ headerStr <> text " " <> contents <> text "\n"
blockToOrg (CodeBlock (_,classes,_) str) = do
  opts <- stOptions <$> get
  let tabstop = writerTabStop opts
  let at = classes `intersect` ["asymptote", "C", "clojure", "css", "ditaa", 
                    "dot", "emacs-lisp", "gnuplot", "haskell", "js", "latex", 
                    "ledger", "lisp", "matlab", "mscgen", "ocaml", "octave", 
                    "oz", "perl", "plantuml", "python", "R", "ruby", "sass", 
                    "scheme", "screen", "sh", "sql", "sqlite"]
  let (beg, end) = if null at
                      then ("#+BEGIN_EXAMPLE", "#+END_EXAMPLE")
                      else ("#+BEGIN_SRC" ++ head at, "#+END_SRC")
  return $ text beg $+$ (nest tabstop $ vcat $ map text (lines str)) 
         $+$ text end
blockToOrg (BlockQuote blocks) = do
  contents <- blockListToOrg blocks 
  return $ (text "\n#+BEGIN_QUOTE\n") $$ (nest 2 contents) 
         $$ (text "\n#+END_QUOTE\n")
blockToOrg (Table caption' _ _ headers rows) =  do
  caption'' <- inlineListToOrg caption'
  let caption = if null caption'
                   then empty
                   else (text "#+CAPTION: " <> caption'')
  headers' <- mapM blockListToOrg headers
  rawRows <- mapM (mapM blockListToOrg) rows
  let numChars = maximum . map (length . render)
  -- FIXME: width is not being used. 
  let widthsInChars =
       map ((+2) . numChars) $ transpose (headers' : rawRows)
  -- FIXME: Org doesn't allow blocks with height more than 1. 
  let hpipeBlocks blocks = hcatBlocks [beg, middle, end] 
        where height = maximum (map heightOfBlock blocks)
              sep'   = TextBlock 3 height (replicate height " | ")
              beg    = TextBlock 2 height (replicate height "| ")
              end    = TextBlock 2 height (replicate height " |")
              middle = hcatBlocks $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith docToBlock widthsInChars
  let head' = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM blockListToOrg row
                            return $ makeRow cols) rows
  let border ch = char '|' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $ 
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '|'
  let body = vcat $ map blockToDoc rows'
  let head'' = if all null headers
                  then empty
                  else blockToDoc head' $+$ border '-'
  return $ head'' $+$ body $$ caption $$ text ""
blockToOrg (BulletList items) = do
  contents <- mapM bulletListItemToOrg items
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToOrg (OrderedList (start, style', delim) items) = do
  let markers = take (length items) $ orderedListMarkers 
                                      (start, style', delim)
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- mapM (\(item, num) -> orderedListItemToOrg item num) $
              zip markers' items  
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToOrg (DefinitionList items) = do
  contents <- mapM definitionListItemToOrg items
  return $ (vcat contents) <> text "\n"

-- | Convert bullet list item (list of blocks) to Org.
bulletListItemToOrg :: [Block] -> State WriterState Doc
bulletListItemToOrg items = do
  contents <- blockListToOrg items
  return $ (text "-  ") <> contents

-- | Convert ordered list item (a list of blocks) to Org.
orderedListItemToOrg :: String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> State WriterState Doc
orderedListItemToOrg marker items = do
  contents <- blockListToOrg items
  return $ (text marker <> char ' ') <> contents 

-- | Convert defintion list item (label, list of blocks) to Org.
definitionListItemToOrg :: ([Inline], [[Block]]) -> State WriterState Doc
definitionListItemToOrg (label, defs) = do
  label' <- inlineListToOrg label
  contents <- liftM vcat $ mapM blockListToOrg defs
  return $ (text "-  ") <> label' <> (text " :: ") <> contents

-- | Convert list of Pandoc block elements to Org.
blockListToOrg :: [Block]       -- ^ List of block elements
               -> State WriterState Doc 
blockListToOrg blocks = mapM blockToOrg blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to Org.
inlineListToOrg :: [Inline] -> State WriterState Doc
inlineListToOrg lst = mapM inlineToOrg lst >>= return . hcat

-- | Convert Pandoc inline element to Org.
inlineToOrg :: Inline -> State WriterState Doc
inlineToOrg (Emph lst) = do 
  contents <- inlineListToOrg lst
  return $ char '/' <> contents <> char '/'
inlineToOrg (Strong lst) = do
  contents <- inlineListToOrg lst
  return $ text "*" <> contents <> text "*"
inlineToOrg (Strikeout lst) = do 
  contents <- inlineListToOrg lst
  return $ text "+" <> contents <> char '+'
inlineToOrg (Superscript lst) = do 
  contents <- inlineListToOrg lst
  return $ text "^{" <> contents <> text "}"
inlineToOrg (Subscript lst) = do 
  contents <- inlineListToOrg lst
  return $ text "_{" <> contents <> text "}"
inlineToOrg (SmallCaps lst) = inlineListToOrg lst
inlineToOrg (Quoted SingleQuote lst) = do
  contents <- inlineListToOrg lst
  return $ char '\'' <> contents <> char '\''
inlineToOrg (Quoted DoubleQuote lst) = do
  contents <- inlineListToOrg lst
  return $ char '\"' <> contents <> char '\"'
inlineToOrg (Cite _  lst) =
  inlineListToOrg lst
inlineToOrg EmDash = return $ text "---"
inlineToOrg EnDash = return $ text "--"
inlineToOrg Apostrophe = return $ char '\''
inlineToOrg Ellipses = return $ text "..."
inlineToOrg (Code str) = return $ text $ "=" ++ str ++ "="
inlineToOrg (Str str) = return $ text $ escapeString str
inlineToOrg (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then text $ "$" ++ str ++ "$"
              else text $ "$$" ++ str ++ "$$"
inlineToOrg (TeX str) = return $ text str
inlineToOrg (HtmlInline _) = return empty
inlineToOrg (LineBreak) = do
  return $ empty -- there's no line break in Org
inlineToOrg Space = return $ char ' '
inlineToOrg (Link txt (src, _)) = do
  case txt of
        [Code x] | x == src ->  -- autolink
             do modify $ \s -> s{ stLinks = True }
                return $ text $ "[[" ++ x ++ "]]"
        _ -> do contents <- inlineListToOrg txt
                modify $ \s -> s{ stLinks = True }
                return $ text ("[[" ++ src ++ "][") <> contents <> 
                         (text "]]")
inlineToOrg (Image _ (source', _)) = do
  let source = unescapeURI source'
  modify $ \s -> s{ stImages = True }
  return $ text $ "[[" ++ source ++ "]]"
inlineToOrg (Note contents) = do 
  -- add to notes in state
  notes <- get >>= (return . stNotes)
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ (length notes) + 1
  return $ text " [" <> text ref <> text "]"

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
   Module      : Text.Pandoc.Writers.ZimWiki
   Copyright   : Copyright (C) 2008-2015 John MacFarlane, 2016 Alex Ivkin
   License     : GNU GPL, version 2 or above

   Maintainer  : Alex Ivkin <alex@ivkin.net>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to ZimWiki markup.

http://zim-wiki.org/manual/Help/Wiki_Syntax.html
-}

module Text.Pandoc.Writers.ZimWiki ( writeZimWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options ( WriterOptions(writerTableOfContents, writerTemplate, writerWrapText), WrapOption(..) )
import Text.Pandoc.Shared ( escapeURI, linesToPara, removeFormatting, trimr
                          , substitute )
import Text.Pandoc.Writers.Shared ( defField, metaToJSON )
import Text.Pandoc.ImageSize
import Text.Pandoc.Templates ( renderTemplate' )
import Data.List ( intercalate, isPrefixOf, transpose, isInfixOf )
import Data.Text ( breakOnAll, pack )
import Data.Default (Default(..))
import Network.URI ( isURI )
import Control.Monad ( zipWithM )
import Control.Monad.State ( modify, State, get, evalState )
--import Control.Monad.Reader ( ReaderT, runReaderT, ask, local )

data WriterState = WriterState {
    stItemNum   :: Int,
    stIndent    :: String          -- Indent after the marker at the beginning of list items
  }

instance Default WriterState where
  def = WriterState { stItemNum = 1, stIndent = "" }

-- | Convert Pandoc to ZimWiki.
writeZimWiki :: WriterOptions -> Pandoc -> String
writeZimWiki opts document = evalState (pandocToZimWiki opts document) (WriterState 1 "")

-- | Return ZimWiki representation of document.
pandocToZimWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToZimWiki opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap trimr . blockListToZimWiki opts)
              (inlineListToZimWiki opts)
              meta
  body <- blockListToZimWiki opts blocks
  --let header = "Content-Type: text/x-zim-wiki\nWiki-Format: zim 0.4\n"
  let main = body
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts)
                $ metadata
  case writerTemplate opts of
       Just tpl -> return $ renderTemplate' tpl context
       Nothing  -> return main

-- | Escape special characters for ZimWiki.
escapeString :: String -> String
escapeString = substitute "__" "''__''" .
               substitute "**" "''**''" .
               substitute "~~" "''~~''" .
               substitute "//" "''//''"

-- | Convert Pandoc block element to ZimWiki.
blockToZimWiki :: WriterOptions -> Block -> State WriterState String

blockToZimWiki _ Null = return ""

blockToZimWiki opts (Div _attrs bs) = do
  contents <- blockListToZimWiki opts bs
  return $ contents ++ "\n"

blockToZimWiki opts (Plain inlines) = inlineListToZimWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
-- ZimWiki doesn't support captions - so combine together alt and caption into alt
blockToZimWiki opts (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else (" " ++) `fmap` inlineListToZimWiki opts txt
  let opt = if null txt
               then ""
               else "|" ++ if null tit then capt else tit ++ capt
      -- Relative links fail isURI and receive a colon
      prefix = if isURI src then "" else ":"
  return $ "{{" ++ prefix ++ src ++ imageDims opts attr ++ opt ++ "}}\n"

blockToZimWiki opts (Para inlines) = do
  indent <- stIndent <$> get
  -- useTags <- stUseTags <$> get
  contents <- inlineListToZimWiki opts inlines
  return $ contents ++ if null indent then "\n" else ""

blockToZimWiki opts (LineBlock lns) = do
  blockToZimWiki opts $ linesToPara lns

blockToZimWiki opts (RawBlock f str)
  | f == Format "zimwiki"  = return str
  | f == Format "html"     = do cont <- indentFromHTML opts str; return cont
  | otherwise              = return ""

blockToZimWiki _ HorizontalRule = return "\n----\n"

blockToZimWiki opts (Header level _ inlines) = do
  contents <- inlineListToZimWiki opts $ removeFormatting inlines   -- emphasis, links etc. not allowed in headers
  let eqs = replicate ( 7 - level ) '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToZimWiki _ (CodeBlock (_,classes,_) str) = do
  return $ case classes of
                []              -> "'''\n" ++ cleanupCode str ++ "\n'''\n" -- no lang block is a quote block
                (x:_)   -> "{{{code: lang=\"" ++ x ++ "\" linenumbers=\"True\"\n" ++ str ++ "\n}}}\n"    -- for zim's code plugin, go verbatim on the lang spec

blockToZimWiki opts (BlockQuote blocks) = do
  contents <- blockListToZimWiki opts blocks
  return $ unlines $ map ("> " ++) $ lines contents

blockToZimWiki opts (Table capt aligns _ headers rows) = do
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToZimWiki opts capt
                      return $ "" ++ c ++ "\n"
  headers' <- if all null headers
                 then zipWithM (tableItemToZimWiki opts) aligns (rows !! 0)
                 else zipWithM (tableItemToZimWiki opts) aligns headers
  rows' <- mapM (zipWithM (tableItemToZimWiki opts) aligns) rows
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
  let borderCell (width, al) _ =
                 if al == AlignLeft
                    then ":"++ replicate (width-1) '-'
                        else if al == AlignDefault
                        then replicate width '-'
                        else if al == AlignRight
                            then replicate (width-1) '-' ++ ":"
                            else ":" ++ replicate (width-2) '-' ++ ":"
  let underheader  = "|" ++ intercalate "|" (zipWith borderCell (zip widths aligns) headers') ++ "|"
  let renderRow sep cells = sep ++ intercalate sep (zipWith padTo (zip widths aligns) cells) ++ sep
  return $ captionDoc ++
           (if null headers' then "" else renderRow "|" headers' ++ "\n") ++ underheader ++ "\n" ++
           unlines (map (renderRow "|") rows')

blockToZimWiki opts (BulletList items) = do
  indent <- stIndent <$> get
  modify $ \s -> s { stIndent = stIndent s ++ "\t" }
  contents <- (mapM (listItemToZimWiki opts) items)
  modify $ \s -> s{ stIndent = indent } -- drop 1 (stIndent s) }
  return $ vcat contents ++ if null indent then "\n" else ""

blockToZimWiki opts (OrderedList _ items) = do
  indent <- stIndent <$> get
  modify $ \s -> s { stIndent = stIndent s ++ "\t", stItemNum = 1 }
  contents <- (mapM (orderedListItemToZimWiki opts) items)
  modify $ \s -> s{ stIndent = indent } -- drop 1 (stIndent s) }
  return $ vcat contents ++ if null indent then "\n" else ""

blockToZimWiki opts (DefinitionList items) = do
  contents <- (mapM (definitionListItemToZimWiki opts) items)
  return $ vcat contents

definitionListItemToZimWiki :: WriterOptions -> ([Inline],[[Block]]) -> State WriterState String
definitionListItemToZimWiki opts (label, items) = do
  labelText <- inlineListToZimWiki opts label
  contents <- mapM (blockListToZimWiki opts) items
  indent <- stIndent <$> get
  return $ indent ++ "* **" ++ labelText ++ "** " ++ concat contents

-- Auxiliary functions for lists:
indentFromHTML :: WriterOptions -> String -> State WriterState String
indentFromHTML _ str = do
   indent <- stIndent  <$> get
   itemnum <- stItemNum  <$> get
   if isInfixOf "<li>" str then return $ indent ++ show itemnum ++ "."
        else if isInfixOf "</li>" str then return "\n"
                else if isInfixOf "<li value=" str then do
                        -- poor man's cut
                        let val = drop 10 $ reverse $ drop 1 $ reverse str
                        --let val = take ((length valls) - 2) valls
                        modify $ \s -> s { stItemNum = read val }
                        return ""
                        else if isInfixOf "<ol>" str then do
                                let olcount=countSubStrs "<ol>" str
                                modify $ \s -> s { stIndent = stIndent s ++ replicate olcount '\t', stItemNum = 1 }
                                return ""
                                else if isInfixOf "</ol>" str then do
                                        let olcount=countSubStrs "/<ol>" str
                                        modify $ \s -> s{ stIndent = drop olcount (stIndent s) }
                                        return ""
                                        else
                                                return ""

countSubStrs :: String -> String -> Int
countSubStrs sub str = length $ breakOnAll (pack sub) (pack str)

cleanupCode :: String -> String
cleanupCode = substitute "<nowiki>" "" . substitute "</nowiki>" ""

vcat :: [String] -> String
vcat = intercalate "\n"

-- | Convert bullet list item (list of blocks) to ZimWiki.
listItemToZimWiki :: WriterOptions -> [Block] -> State WriterState String
listItemToZimWiki opts items = do
  contents <- blockListToZimWiki opts items
  indent <- stIndent <$> get
  return $ indent ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to ZimWiki.
orderedListItemToZimWiki :: WriterOptions -> [Block] -> State WriterState String
orderedListItemToZimWiki opts items = do
  contents <- blockListToZimWiki opts items
  indent <- stIndent <$> get
  itemnum <- stItemNum  <$> get
  --modify $ \s -> s { stItemNum = itemnum + 1 } -- this is not strictly necessary for zim as zim does its own renumbering
  return $ indent ++ show itemnum ++ ". " ++ contents

-- Auxiliary functions for tables:
tableItemToZimWiki :: WriterOptions -> Alignment -> [Block] -> State WriterState String
tableItemToZimWiki opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") ++ x ++
                 (if align' == AlignLeft || align' == AlignCenter
                     then "  "
                     else "")
  contents <- blockListToZimWiki opts item -- local (\s -> s { stBackSlashLB = True }) $
  return $ mkcell contents

-- | Convert list of Pandoc block elements to ZimWiki.
blockListToZimWiki :: WriterOptions -> [Block] -> State WriterState String
blockListToZimWiki opts blocks = vcat <$> mapM (blockToZimWiki opts) blocks

-- | Convert list of Pandoc inline elements to ZimWiki.
inlineListToZimWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToZimWiki opts lst =  concat <$> (mapM (inlineToZimWiki opts) lst)

-- | Convert Pandoc inline element to ZimWiki.
inlineToZimWiki :: WriterOptions -> Inline -> State WriterState String

inlineToZimWiki opts (Emph lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "//" ++ contents ++ "//"

inlineToZimWiki opts (Strong lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "**" ++ contents ++ "**"

inlineToZimWiki opts (Strikeout lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "~~" ++ contents ++ "~~"

inlineToZimWiki opts (Superscript lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "^{" ++ contents ++ "}"

inlineToZimWiki opts (Subscript lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "_{" ++ contents ++ "}"

inlineToZimWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToZimWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToZimWiki opts (Span _attrs ils) = inlineListToZimWiki opts ils

inlineToZimWiki opts (SmallCaps lst) = inlineListToZimWiki opts lst

inlineToZimWiki opts (Cite _  lst) = inlineListToZimWiki opts lst

inlineToZimWiki _ (Code _ str) = return $ "''" ++ str ++ "''"

inlineToZimWiki _ (Str str) = return $ escapeString str

inlineToZimWiki _ (Math mathType str) = return $ delim ++ str ++ delim   -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

-- | f == Format "html"     = return $ "<html>" ++ str ++ "</html>"
inlineToZimWiki opts (RawInline f str)
  | f == Format "zimwiki" = return str
  | f == Format "html"     = do cont <- indentFromHTML opts str; return cont
  | otherwise              = return ""

inlineToZimWiki _ LineBreak = return "\n" -- was \\\\

inlineToZimWiki _ PageBreak = return mempty

inlineToZimWiki opts SoftBreak =
  case writerWrapText opts of
       WrapNone     -> return " "
       WrapAuto     -> return " "
       WrapPreserve -> return "\n"

inlineToZimWiki _ Space = return " "

inlineToZimWiki opts (Link _ txt (src, _)) = do
  label <- inlineListToZimWiki opts txt
  case txt of
     [Str s] | "mailto:" `isPrefixOf` src -> return $ "<" ++ s ++ ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" ++ src  ++ "|" ++ label ++ "]]"
              else return $ "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToZimWiki opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToZimWiki opts alt
  let txt = case (tit, alt) of
              ("", []) -> ""
              ("", _ ) -> "|" ++ alt'
              (_ , _ ) -> "|" ++ tit
      -- Relative links fail isURI and receive a colon
      prefix = if isURI source then "" else ":"
  return $ "{{" ++ prefix ++ source ++ imageDims opts attr ++ txt ++ "}}"

inlineToZimWiki opts (Note contents) = do
  contents' <- blockListToZimWiki opts contents
  return $ "((" ++ contents' ++ "))"
  -- note - may not work for notes with multiple blocks

imageDims :: WriterOptions -> Attr -> String
imageDims opts attr = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
  where
    toPx = fmap (showInPixel opts) . checkPct
    checkPct (Just (Percent _)) = Nothing
    checkPct maybeDim = maybeDim
    go (Just w) Nothing  = "?" ++ w
    go (Just w) (Just h) = "?" ++ w ++ "x" ++ h
    go Nothing  (Just h) = "?0x" ++ h
    go Nothing  Nothing  = ""

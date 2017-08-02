{-
Copyright (C) 2008-2017 John MacFarlane <jgm@berkeley.edu>
              2017 Alex Ivkin

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
   Copyright   : Copyright (C) 2008-2017 John MacFarlane, 2017 Alex Ivkin
   License     : GNU GPL, version 2 or above

   Maintainer  : Alex Ivkin <alex@ivkin.net>
   Stability   : beta
   Portability : portable

Conversion of 'Pandoc' documents to ZimWiki markup.

http://zim-wiki.org/manual/Help/Wiki_Syntax.html
-}

module Text.Pandoc.Writers.ZimWiki ( writeZimWiki ) where
import Control.Monad (zipWithM)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Default (Default (..))
import Data.List (intercalate, isInfixOf, isPrefixOf, transpose)
import qualified Data.Map as Map
import Data.Text (breakOnAll, pack, Text)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Options (WrapOption (..), WriterOptions (writerTableOfContents, writerTemplate, writerWrapText))
import Text.Pandoc.Shared (isURI, escapeURI, linesToPara, removeFormatting,
                           substitute, trimr)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Shared (defField, metaToJSON)

data WriterState = WriterState {
    stItemNum :: Int,
    stIndent  :: String,         -- Indent after the marker at the beginning of list items
    stInTable :: Bool,           -- Inside a table
    stInLink  :: Bool            -- Inside a link description
  }

instance Default WriterState where
  def = WriterState { stItemNum = 1, stIndent = "", stInTable = False, stInLink = False }

type ZW = StateT WriterState

-- | Convert Pandoc to ZimWiki.
writeZimWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeZimWiki opts document = evalStateT (pandocToZimWiki opts document) def

-- | Return ZimWiki representation of document.
pandocToZimWiki :: PandocMonad m => WriterOptions -> Pandoc -> ZW m Text
pandocToZimWiki opts (Pandoc meta blocks) = do
  metadata <- metaToJSON opts
              (fmap trimr . blockListToZimWiki opts)
              (inlineListToZimWiki opts)
              meta
  body <- pack <$> blockListToZimWiki opts blocks
  --let header = "Content-Type: text/x-zim-wiki\nWiki-Format: zim 0.4\n"
  let main = body
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts)
                $ metadata
  case writerTemplate opts of
       Just tpl -> renderTemplate' tpl context
       Nothing  -> return main

-- | Escape special characters for ZimWiki.
escapeString :: String -> String
escapeString = substitute "__" "''__''" .
               substitute "**" "''**''" .
               substitute "~~" "''~~''" .
               substitute "//" "''//''"

-- | Convert Pandoc block element to ZimWiki.
blockToZimWiki :: PandocMonad m => WriterOptions -> Block -> ZW m String

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
  indent <- gets stIndent
  -- useTags <- gets stUseTags
  contents <- inlineListToZimWiki opts inlines
  return $ contents ++ if null indent then "\n" else ""

blockToZimWiki opts (LineBlock lns) = do
  blockToZimWiki opts $ linesToPara lns

blockToZimWiki opts b@(RawBlock f str)
  | f == Format "zimwiki"  = return str
  | f == Format "html"     = do cont <- indentFromHTML opts str; return cont
  | otherwise              = do
      report $ BlockNotRendered b
      return ""

blockToZimWiki _ HorizontalRule = return "\n----\n"

blockToZimWiki opts (Header level _ inlines) = do
  contents <- inlineListToZimWiki opts $ removeFormatting inlines   -- emphasis, links etc. not allowed in headers
  let eqs = replicate ( 7 - level ) '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToZimWiki _ (CodeBlock (_,classes,_) str) = do
  -- Remap languages into the gtksourceview2 convention that ZimWiki source code plugin is using
  let langal = [("javascript", "js"), ("bash", "sh"), ("winbatch", "dosbatch")]
  let langmap = Map.fromList langal
  return $ case classes of
                []      -> "'''\n" ++ cleanupCode str ++ "\n'''\n"   -- turn no lang block into a quote block
                (x:_)   -> "{{{code: lang=\"" ++
                        (case Map.lookup x langmap of
                                Nothing -> x
                                Just y  -> y) ++ "\" linenumbers=\"True\"\n" ++ str ++ "\n}}}\n"  -- for zim's code plugin, go verbatim on the lang spec

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
                 else mapM (inlineListToZimWiki opts) (map removeFormatting headers)  -- emphasis, links etc. are not allowed in table headers
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
  let renderRow cells = "|" ++ intercalate "|" (zipWith padTo (zip widths aligns) cells) ++ "|"
  return $ captionDoc ++
           (if null headers' then "" else renderRow headers' ++ "\n") ++ underheader ++ "\n" ++
           unlines (map renderRow rows')

blockToZimWiki opts (BulletList items) = do
  indent <- gets stIndent
  modify $ \s -> s { stIndent = stIndent s ++ "\t" }
  contents <- (mapM (listItemToZimWiki opts) items)
  modify $ \s -> s{ stIndent = indent } -- drop 1 (stIndent s) }
  return $ vcat contents ++ if null indent then "\n" else ""

blockToZimWiki opts (OrderedList _ items) = do
  indent <- gets stIndent
  modify $ \s -> s { stIndent = stIndent s ++ "\t", stItemNum = 1 }
  contents <- (mapM (orderedListItemToZimWiki opts) items)
  modify $ \s -> s{ stIndent = indent } -- drop 1 (stIndent s) }
  return $ vcat contents ++ if null indent then "\n" else ""

blockToZimWiki opts (DefinitionList items) = do
  contents <- (mapM (definitionListItemToZimWiki opts) items)
  return $ vcat contents

definitionListItemToZimWiki :: PandocMonad m
                            => WriterOptions
                            -> ([Inline],[[Block]])
                            -> ZW m String
definitionListItemToZimWiki opts (label, items) = do
  labelText <- inlineListToZimWiki opts label
  contents <- mapM (blockListToZimWiki opts) items
  indent <- gets stIndent
  return $ indent ++ "* **" ++ labelText ++ "** " ++ concat contents

-- Auxiliary functions for lists:
indentFromHTML :: PandocMonad m => WriterOptions -> String -> ZW m String
indentFromHTML _ str = do
   indent <- gets stIndent
   itemnum <- gets stItemNum
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
listItemToZimWiki :: PandocMonad m => WriterOptions -> [Block] -> ZW m String
listItemToZimWiki opts items = do
  contents <- blockListToZimWiki opts items
  indent <- gets stIndent
  return $ indent ++ "* " ++ contents

-- | Convert ordered list item (list of blocks) to ZimWiki.
orderedListItemToZimWiki :: PandocMonad m
                         => WriterOptions -> [Block] -> ZW m String
orderedListItemToZimWiki opts items = do
  contents <- blockListToZimWiki opts items
  indent <- gets stIndent
  itemnum <- gets stItemNum
  --modify $ \s -> s { stItemNum = itemnum + 1 } -- this is not strictly necessary for zim as zim does its own renumbering
  return $ indent ++ show itemnum ++ ". " ++ contents

-- Auxiliary functions for tables:
tableItemToZimWiki :: PandocMonad m
                   => WriterOptions -> Alignment -> [Block] -> ZW m String
tableItemToZimWiki opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") ++ x ++
                 (if align' == AlignLeft || align' == AlignCenter
                     then "  "
                     else "")
  modify $ \s -> s { stInTable = True }
  contents <- blockListToZimWiki opts item
  modify $ \s -> s { stInTable = False }
  return $ mkcell contents

-- | Convert list of Pandoc block elements to ZimWiki.
blockListToZimWiki :: PandocMonad m
                   => WriterOptions -> [Block] -> ZW m String
blockListToZimWiki opts blocks = vcat <$> mapM (blockToZimWiki opts) blocks

-- | Convert list of Pandoc inline elements to ZimWiki.
inlineListToZimWiki :: PandocMonad m
                    => WriterOptions -> [Inline] -> ZW m String
inlineListToZimWiki opts lst =  concat <$> (mapM (inlineToZimWiki opts) lst)

-- | Convert Pandoc inline element to ZimWiki.
inlineToZimWiki :: PandocMonad m
                => WriterOptions -> Inline -> ZW m String

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

inlineToZimWiki _ (Str str) = do
  inTable <- gets stInTable
  inLink  <- gets stInLink
  if inTable
      then return $ substitute "|" "\\|" . escapeString $ str
      else
          if inLink
          then return $ str
          else return $ escapeString str

inlineToZimWiki _ (Math mathType str) = return $ delim ++ str ++ delim   -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

-- | f == Format "html"     = return $ "<html>" ++ str ++ "</html>"
inlineToZimWiki opts il@(RawInline f str)
  | f == Format "zimwiki" = return str
  | f == Format "html"     = do cont <- indentFromHTML opts str; return cont
  | otherwise              = do
      report $ InlineNotRendered il
      return ""

inlineToZimWiki _ LineBreak = do
  inTable <- gets stInTable
  if inTable
      then return "\\n"
      else return "\n"

inlineToZimWiki opts SoftBreak =
  case writerWrapText opts of
       WrapNone     -> return " "
       WrapAuto     -> return " "
       WrapPreserve -> return "\n"

inlineToZimWiki _ Space = return " "

inlineToZimWiki opts (Link _ txt (src, _)) = do
  inTable <- gets stInTable
  modify $ \s -> s { stInLink = True }
  label <- inlineListToZimWiki opts $ removeFormatting txt -- zim does not allow formatting in link text, it takes the text verbatim, no need to escape it
  modify $ \s -> s { stInLink = False }
  let label'= if inTable
            then "" -- no label is allowed in a table
            else "|"++label
  case txt of
     [Str s] | "mailto:" `isPrefixOf` src -> return $ "<" ++ s ++ ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" ++ src  ++ label' ++ "]]"
              else return $ "[[" ++ src' ++ label' ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
inlineToZimWiki opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToZimWiki opts alt
  inTable <- gets stInTable
  let txt = case (tit, alt, inTable) of
              ("",[], _)      -> ""
              ("", _, False ) -> "|" ++ alt'
              (_ , _, False ) -> "|" ++ tit
              (_ , _, True )  -> ""
      -- Relative links fail isURI and receive a colon
      prefix = if isURI source then "" else ":"
  return $ "{{" ++ prefix ++ source ++ imageDims opts attr ++ txt ++ "}}"

inlineToZimWiki opts (Note contents) = do
  -- no concept of notes in zim wiki, use a text block
  contents' <- blockListToZimWiki opts contents
  return $ " **{Note:** " ++ trimr contents' ++ "**}**"

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

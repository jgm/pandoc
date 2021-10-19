{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.ZimWiki
   Copyright   : © 2008-2021 John MacFarlane,
                   2017-2019 Alex Ivkin
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
import Data.List (transpose)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map as Map
import Text.DocLayout (render, literal)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options (WrapOption (..),
           WriterOptions (writerTableOfContents, writerTemplate,
                          writerWrapText))
import Text.Pandoc.Shared (escapeURI, isURI, linesToPara, removeFormatting, trimr)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared (defField, metaToContext, toLegacyTable)

data WriterState = WriterState {
    stIndent  :: Text,           -- Indent after the marker at the beginning of list items
    stInTable :: Bool,           -- Inside a table
    stInLink  :: Bool            -- Inside a link description
  }

instance Default WriterState where
  def = WriterState { stIndent = "", stInTable = False, stInLink = False }

type ZW = StateT WriterState

-- | Convert Pandoc to ZimWiki.
writeZimWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeZimWiki opts document = evalStateT (pandocToZimWiki opts document) def

-- | Return ZimWiki representation of document.
pandocToZimWiki :: PandocMonad m => WriterOptions -> Pandoc -> ZW m Text
pandocToZimWiki opts (Pandoc meta blocks) = do
  metadata <- metaToContext opts
              (fmap (literal . trimr) . blockListToZimWiki opts)
              (fmap (literal . trimr) . inlineListToZimWiki opts)
              meta
  main <- blockListToZimWiki opts blocks
  --let header = "Content-Type: text/x-zim-wiki\nWiki-Format: zim 0.4\n"
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts) metadata
  return $
    case writerTemplate opts of
       Just tpl -> render Nothing $ renderTemplate tpl context
       Nothing  -> main

-- | Escape special characters for ZimWiki.
escapeText :: Text -> Text
escapeText = T.replace "__" "''__''" .
               T.replace "**" "''**''" .
               T.replace "~~" "''~~''" .
               T.replace "//" "''//''"

-- | Convert Pandoc block element to ZimWiki.
blockToZimWiki :: PandocMonad m => WriterOptions -> Block -> ZW m Text

blockToZimWiki _ Null = return ""

blockToZimWiki opts (Div _attrs bs) = do
  contents <- blockListToZimWiki opts bs
  return $ contents <> "\n"

blockToZimWiki opts (Plain inlines) = inlineListToZimWiki opts inlines

-- title beginning with fig: indicates that the image is a figure
-- ZimWiki doesn't support captions - so combine together alt and caption into alt
blockToZimWiki opts (Para [Image attr txt (src,T.stripPrefix "fig:" -> Just tit)]) = do
  capt <- if null txt
             then return ""
             else (" " <>) `fmap` inlineListToZimWiki opts txt
  let opt = if null txt
               then ""
               else "|" <> if T.null tit then capt else tit <> capt
  return $ "{{" <> src <> imageDims opts attr <> opt <> "}}\n"

blockToZimWiki opts (Para inlines) = do
  indent <- gets stIndent
  -- useTags <- gets stUseTags
  contents <- inlineListToZimWiki opts inlines
  return $ contents <> if T.null indent then "\n" else ""

blockToZimWiki opts (LineBlock lns) =
  blockToZimWiki opts $ linesToPara lns

blockToZimWiki opts b@(RawBlock f str)
  | f == Format "zimwiki"  = return str
  | f == Format "html"     = indentFromHTML opts str
  | otherwise              = do
      report $ BlockNotRendered b
      return ""

blockToZimWiki _ HorizontalRule = return "\n----\n"

blockToZimWiki opts (Header level _ inlines) = do
  contents <- inlineListToZimWiki opts inlines
  let eqs = T.replicate ( 7 - level ) "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

blockToZimWiki _ (CodeBlock (_,classes,_) str) = do
  -- Remap languages into the gtksourceview2 convention that ZimWiki source code plugin is using
  let langal = [("javascript", "js"), ("bash", "sh"), ("winbatch", "dosbatch")]
  let langmap = Map.fromList langal
  return $ case classes of
                []      -> "'''\n" <> cleanupCode str <> "\n'''\n"   -- turn no lang block into a quote block
                (x:_)   -> "{{{code: lang=\"" <>
                        fromMaybe x (Map.lookup x langmap) <> "\" linenumbers=\"True\"\n" <> str <> "\n}}}\n"  -- for zim's code plugin, go verbatim on the lang spec

blockToZimWiki opts (BlockQuote blocks) = do
  contents <- blockListToZimWiki opts blocks
  return $ T.unlines $ map ("> " <>) $ T.lines contents

blockToZimWiki opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (capt, aligns, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToZimWiki opts capt
                      return $ "" <> c <> "\n"
  headers' <- if all null headers
                 then zipWithM (tableItemToZimWiki opts) aligns (head rows)
                 else mapM (inlineListToZimWiki opts . removeFormatting)headers  -- emphasis, links etc. are not allowed in table headers
  rows' <- mapM (zipWithM (tableItemToZimWiki opts) aligns) rows
  let widths = map (maybe 0 maximum . nonEmpty . map T.length) $
                  transpose (headers':rows')
  let padTo (width, al) s =
          case width - T.length s of
               x | x > 0 ->
                 if al == AlignLeft || al == AlignDefault
                    then s <> T.replicate x " "
                    else if al == AlignRight
                            then T.replicate x " " <> s
                            else T.replicate (x `div` 2) " " <>
                                 s <> T.replicate (x - x `div` 2) " "
                 | otherwise -> s
  let borderCell (width, al) _
        | al == AlignLeft = ":"<> T.replicate (width-1) "-"
        | al == AlignDefault = T.replicate width "-"
        | al == AlignRight = T.replicate (width-1) "-" <> ":"
        | otherwise = ":" <> T.replicate (width-2) "-" <> ":"
  let underheader  = "|" <> T.intercalate "|" (zipWith borderCell (zip widths aligns) headers') <> "|"
  let renderRow cells = "|" <> T.intercalate "|" (zipWith padTo (zip widths aligns) cells) <> "|"
  return $ captionDoc <>
           (if null headers' then "" else renderRow headers' <> "\n") <> underheader <> "\n" <>
           T.unlines (map renderRow rows')

blockToZimWiki opts (BulletList items) = do
  contents <- mapM (listItemToZimWiki opts) items
  indent <- gets stIndent
  return $ vcat contents <> if T.null indent then "\n" else ""

blockToZimWiki opts (OrderedList _ items) = do
  contents <- zipWithM (orderedListItemToZimWiki opts) [1..] items
  indent <- gets stIndent
  return $ vcat contents <> if T.null indent then "\n" else ""

blockToZimWiki opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToZimWiki opts) items
  return $ vcat contents

definitionListItemToZimWiki :: PandocMonad m
                            => WriterOptions
                            -> ([Inline],[[Block]])
                            -> ZW m Text
definitionListItemToZimWiki opts (label, items) = do
  labelText <- inlineListToZimWiki opts label
  contents <- mapM (blockListToZimWiki opts) items
  indent <- gets stIndent
  return $ indent <> "* **" <> labelText <> "** " <> T.concat contents

-- Auxiliary functions for lists:
indentFromHTML :: PandocMonad m => WriterOptions -> Text -> ZW m Text
indentFromHTML _ str = do
   indent <- gets stIndent
   if "<li>" `T.isInfixOf` str
      then return indent
      else if "</li>" `T.isInfixOf` str
        then return "\n"
        else if "<li value=" `T.isInfixOf` str
          then return ""
          else if "<ol>" `T.isInfixOf` str
            then do
              let olcount=countSubStrs "<ol>" str
              modify $ \s -> s { stIndent = stIndent s <>
                                 T.replicate olcount "\t" }
              return ""
            else if "</ol>" `T.isInfixOf` str
              then do
                let olcount=countSubStrs "/<ol>" str
                modify $ \s -> s{ stIndent = T.drop olcount (stIndent s) }
                return ""
              else return ""

countSubStrs :: Text -> Text -> Int
countSubStrs sub str = length $ T.breakOnAll sub str

cleanupCode :: Text -> Text
cleanupCode = T.replace "<nowiki>" "" . T.replace "</nowiki>" ""

vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- | Convert bullet list item (list of blocks) to ZimWiki.
listItemToZimWiki :: PandocMonad m => WriterOptions -> [Block] -> ZW m Text
listItemToZimWiki opts items = do
  indent <- gets stIndent
  modify $ \s -> s { stIndent = indent <> "\t" }
  contents <- blockListToZimWiki opts items
  modify $ \s -> s{ stIndent = indent }
  return $ indent <> "* " <> contents

-- | Convert ordered list item (list of blocks) to ZimWiki.
orderedListItemToZimWiki :: PandocMonad m
                         => WriterOptions -> Int -> [Block] -> ZW m Text
orderedListItemToZimWiki opts itemnum items = do
  indent <- gets stIndent
  modify $ \s -> s { stIndent = indent <> "\t" }
  contents <- blockListToZimWiki opts items
  modify $ \s -> s{ stIndent = indent }
  return $ indent <> T.pack (show itemnum) <> ". " <> contents

-- Auxiliary functions for tables:
tableItemToZimWiki :: PandocMonad m
                   => WriterOptions -> Alignment -> [Block] -> ZW m Text
tableItemToZimWiki opts align' item = do
  let mkcell x = (if align' == AlignRight || align' == AlignCenter
                     then "  "
                     else "") <> x <>
                 (if align' == AlignLeft || align' == AlignCenter
                     then "  "
                     else "")
  modify $ \s -> s { stInTable = True }
  contents <- blockListToZimWiki opts item
  modify $ \s -> s { stInTable = False }
  return $ mkcell contents

-- | Convert list of Pandoc block elements to ZimWiki.
blockListToZimWiki :: PandocMonad m
                   => WriterOptions -> [Block] -> ZW m Text
blockListToZimWiki opts blocks = vcat <$> mapM (blockToZimWiki opts) blocks

-- | Convert list of Pandoc inline elements to ZimWiki.
inlineListToZimWiki :: PandocMonad m
                    => WriterOptions -> [Inline] -> ZW m Text
inlineListToZimWiki opts lst = T.concat <$> mapM (inlineToZimWiki opts) lst

-- | Convert Pandoc inline element to ZimWiki.
inlineToZimWiki :: PandocMonad m
                => WriterOptions -> Inline -> ZW m Text

inlineToZimWiki opts (Emph lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "//" <> contents <> "//"

inlineToZimWiki opts (Underline lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "__" <> contents <> "__"

inlineToZimWiki opts (Strong lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "**" <> contents <> "**"

inlineToZimWiki opts (Strikeout lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "~~" <> contents <> "~~"

inlineToZimWiki opts (Superscript lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "^{" <> contents <> "}"

inlineToZimWiki opts (Subscript lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "_{" <> contents <> "}"

inlineToZimWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "\8216" <> contents <> "\8217"

inlineToZimWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToZimWiki opts lst
  return $ "\8220" <> contents <> "\8221"

inlineToZimWiki opts (Span _attrs ils) = inlineListToZimWiki opts ils

inlineToZimWiki opts (SmallCaps lst) = inlineListToZimWiki opts lst

inlineToZimWiki opts (Cite _  lst) = inlineListToZimWiki opts lst

inlineToZimWiki _ (Code _ str) = return $ "''" <> str <> "''"

inlineToZimWiki _ (Str str) = do
  inTable <- gets stInTable
  inLink  <- gets stInLink
  if inTable
      then return $ T.replace "|" "\\|" . escapeText $ str
      else
          if inLink
          then return str
          else return $ escapeText str

inlineToZimWiki _ (Math mathType str) = return $ delim <> str <> delim   -- note:  str should NOT be escaped
  where delim = case mathType of
                     DisplayMath -> "$$"
                     InlineMath  -> "$"

-- | f == Format "html"     = return $ "<html>" <> str <> "</html>"
inlineToZimWiki opts il@(RawInline f str)
  | f == Format "zimwiki" = return str
  | f == Format "html"    = indentFromHTML opts str
  | otherwise             = do
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
            else "|"<>label
  case txt of
     [Str s] | "mailto:" `T.isPrefixOf` src -> return $ "<" <> s <> ">"
             | escapeURI s == src -> return src
     _  -> if isURI src
              then return $ "[[" <> src  <> label' <> "]]"
              else return $ "[[" <> src' <> label' <> "]]"
  where
    -- with leading / it's a link to a help page
    src' = fromMaybe src $ T.stripPrefix "/" src

inlineToZimWiki opts (Image attr alt (source, tit)) = do
  alt' <- inlineListToZimWiki opts alt
  inTable <- gets stInTable
  let txt = case (tit, alt, inTable) of
              ("",[], _)      -> ""
              ("", _, False ) -> "|" <> alt'
              (_ , _, False ) -> "|" <> tit
              (_ , _, True )  -> ""
  return $ "{{" <> source <> imageDims opts attr <> txt <> "}}"

inlineToZimWiki opts (Note contents) = do
  -- no concept of notes in zim wiki, use a text block
  contents' <- blockListToZimWiki opts contents
  return $ " **{Note:** " <> trimr contents' <> "**}**"

imageDims :: WriterOptions -> Attr -> Text
imageDims opts attr = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
  where
    toPx = fmap (showInPixel opts) . checkPct
    checkPct (Just (Percent _)) = Nothing
    checkPct maybeDim           = maybeDim
    go (Just w) Nothing  = "?" <> w
    go (Just w) (Just h) = "?" <> w <> "x" <> h
    go Nothing  (Just h) = "?0x" <> h
    go Nothing  Nothing  = ""

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.DocLang
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to DocLang XML.
-}
module Text.Pandoc.Writers.DocLang
  ( writeDocLang
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML

-- | Convert Pandoc document to DocLang XML.
writeDocLang :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeDocLang opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  body <- blocksToDocLang opts blocks
  metadata <- metaToContext opts
                (blocksToDocLang opts)
                (inlinesToDocLang opts)
                meta
  let context = defField "body" body metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> body
       Just tpl -> renderTemplate tpl context

-- | Convert a list of blocks to DocLang XML.
blocksToDocLang :: PandocMonad m => WriterOptions -> [Block] -> m (Doc Text)
blocksToDocLang opts bs = vcat <$> mapM (blockToDocLang opts) bs

-- | Convert a Block to DocLang XML.
blockToDocLang :: PandocMonad m => WriterOptions -> Block -> m (Doc Text)
blockToDocLang opts = \case
  Plain ils -> inlinesToDocLang opts ils
  Para  ils ->
    inTagsIndented "text" <$> inlinesToDocLang opts ils
  LineBlock lns -> do
    linesDoc <- mapM (inlinesToDocLang opts) lns
    return $ inTagsIndented "text" $ hcat $ intersperse space linesDoc
  CodeBlock (_,classes,_) code -> do
    let langAttr = case classes of
          (c:_) -> [("value", c)]
          _     -> []
        label  = if null langAttr then mempty
                 else slfClose "label" langAttr <> cr
    return $ inTagsIndented "code" $ label <> contentElement code
  RawBlock (Format f) s
    | f `elem` ["doclang", "xml"] -> return $ literal s
    | otherwise -> return mempty
  BlockQuote bs -> blocksToDocLang opts bs
  OrderedList (start, style, _) items ->
    listToDocLang opts start style items
  BulletList items ->
    listToDocLang opts 1 DefaultStyle items
  DefinitionList items ->
    deflistToDocLang opts items
  Header level _ ils ->
    (if level == 1
       then inTagsSimple "heading"
       else inTags True "heading" [("level", tshow level)]) <$>
      inlinesToDocLang opts ils
  HorizontalRule -> return mempty
  Table _ caption colspecs thead tbodies tfoot ->
    tableToDocLang opts caption colspecs thead tbodies tfoot
  Figure _ _ body -> blockToDocLang opts (Div nullAttr body)
  Div _ bs -> blocksToDocLang opts bs

-- | Convert a definition list.
deflistToDocLang :: PandocMonad m
                 => WriterOptions -> [([Inline], [[Block]])]
                 -> m (Doc Text)
deflistToDocLang opts items = do
  contents <- vcat <$> mapM defItem items
  return $ inTagsIndented "list" contents
  where
    defItem (term, defs) = do
      termDoc <- inlinesToDocLang opts term
      defsDoc <- vcat <$> mapM (blocksToDocLang opts) defs
      return $ slfClose "ldiv" [] <> termDoc <> defsDoc

-- | Convert a list.
listToDocLang :: PandocMonad m
              => WriterOptions -> Int -> ListNumberStyle -> [[Block]]
              -> m (Doc Text)
listToDocLang opts start style items = do
  let attrs   = if style /= DefaultStyle || start /= 1
                   then [("class", "ordered")]
                   else []
      listItem itemBlocks = do
        inner <- virtualContent opts itemBlocks
        return $ slfClose "ldiv" [] <> inner
  contents <- vcat <$> mapM listItem items
  return $ inTags True "list" attrs contents

-- | Virtual text: for simple content, unwrap from <text> tags.
virtualContent :: PandocMonad m => WriterOptions -> [Block] -> m (Doc Text)
virtualContent opts bs = case isSimpleContent bs of
  Just ils -> inlinesToDocLang opts ils
  Nothing  -> blocksToDocLang opts bs

-- | Convert a table to DocLang OTSL format.
tableToDocLang :: PandocMonad m
               => WriterOptions -> Caption -> [ColSpec]
               -> TableHead -> [TableBody] -> TableFoot
               -> m (Doc Text)
tableToDocLang opts (Caption _ cbody) specs thead tbodies tfoot = do
  let (captionInlines, _, _, headers, rows) =
        toLegacyTable (Caption mempty cbody) specs thead tbodies tfoot
  captionDoc <- if null captionInlines then return mempty
                else inlinesToDocLang opts captionInlines >>=
                     \d -> return $ inTagsSimple "caption" d <> cr
  headerDoc  <- if null headers then return mempty
                else tableRow opts True headers
  rowsDoc    <- vcat <$> mapM (tableRow opts False) rows
  return $ inTagsIndented "table" $ captionDoc <> headerDoc <> rowsDoc
  where
    tableRow _ _ [] = return mempty
    tableRow wr isHeader cells = do
      cellsDoc <- mapM (tableCell wr isHeader) cells
      return $ foldr (<>) (slfClose "nl" []) cellsDoc
    tableCell wr isHeader cellBlocks = do
      let cellTag = if isHeader then "ched" else "fcel"
          isBlank = case cellBlocks of
            [] -> True; [Plain []] -> True; _ -> False
      if isBlank
        then return $ slfClose "ecel" []
        else do
          contents <- virtualContent wr cellBlocks
          return $ slfClose cellTag [] <> contents

-- | Convert a list of Inline elements to DocLang.
inlinesToDocLang :: PandocMonad m => WriterOptions -> [Inline] -> m (Doc Text)
inlinesToDocLang opts ils = hcat <$> mapM (inlineToDocLang opts) ils

-- | Convert an Inline element to DocLang XML.
inlineToDocLang :: PandocMonad m => WriterOptions -> Inline -> m (Doc Text)
inlineToDocLang opts = \case
  Str s -> return $ literal $ escapeStringForXML s
  Space -> return space
  SoftBreak -> return space
  LineBreak -> return space
  Emph ils       -> inTagsSimple "italic" <$> subInlines ils
  Strong ils     -> inTagsSimple "bold" <$> subInlines ils
  Strikeout ils  -> inTagsSimple "strikethrough" <$> subInlines ils
  Superscript ils -> inTagsSimple "superscript" <$> subInlines ils
  Subscript ils   -> inTagsSimple "subscript" <$> subInlines ils
  SmallCaps ils   -> subInlines ils
  Underline ils   -> inTagsSimple "underline" <$> subInlines ils
  Quoted _qt ils  -> subInlines ils
  Cite _ citations -> subInlines citations
  Code _attr code ->
    return $ inTagsSimple "code" $ literal $ escapeStringForXML code
  Math _ tex      -> return $ inTagsSimple "formula" $ literal tex
  Link _attr ils (url, _) ->
    subInlines ils >>= \d -> return $ d <> literal (" (" <> url <> ")")
  RawInline (Format f) s
    | f `elem` ["doclang", "xml"] -> return $ literal s
    | otherwise -> return mempty
  Span _ ils -> subInlines ils
  Note bs    -> inTagsIndented "footnote" <$> blocksToDocLang opts bs
  _          -> return mempty
  where subInlines = inlinesToDocLang opts

-- | Render a code content element, using CDATA when needed.
contentElement :: Text -> Doc Text
contentElement c
  | "]]>" `T.isInfixOf` c
  = inTagsIndented "content" $ literal $ escapeStringForXML c
  | T.any (\x -> x == '<' || x == '>' || x == '&') c
  = "<content><![CDATA[" <> literal c <> "]]></content>"
  | otherwise
  = inTagsIndented "content" $ literal $ escapeStringForXML c

-- | Self-closing tag with no space before /> (DocLang convention).
slfClose :: Text -> [(Text, Text)] -> Doc Text
slfClose tagType attribs =
  char '<' <> text (T.unpack tagType) <>
  foldMap attrDoc attribs <>
  "/>"
  where
    attrDoc (k, v) = char ' ' <> text (T.unpack k) <> "=\"" <>
                     text (T.unpack (escapeStringForXML v)) <> "\""

-- literal is from Text.DocLayout

-- intersperse from Data.List

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{- |
   Module      : Text.Pandoc.Writers.Docbook
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.TEI (writeTEI) where
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML

-- | Convert Pandoc document to string in Docbook format.
writeTEI :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTEI opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let startLvl = case writerTopLevelDivision opts of
                   TopLevelPart    -> -1
                   TopLevelChapter -> 0
                   TopLevelSection -> 1
                   TopLevelDefault -> 1
  let fromBlocks = blocksToTEI opts . makeSections False (Just startLvl)
  metadata <- metaToContext opts
                 fromBlocks
                 (fmap chomp . inlinesToTEI opts)
                 meta
  main    <- fromBlocks blocks
  let context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                          MathML -> True
                                          _      -> False) metadata
  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> main
       Just tpl -> renderTemplate tpl context

-- | Convert a list of Pandoc blocks to TEI.
blocksToTEI :: PandocMonad m => WriterOptions -> [Block] -> m (Doc Text)
blocksToTEI opts bs = vcat <$> mapM (blockToTEI opts) bs

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a TEI
-- list with labels and items.
deflistItemsToTEI :: PandocMonad m
                  => WriterOptions -> [([Inline],[[Block]])] -> m (Doc Text)
deflistItemsToTEI opts items =
 vcat <$> mapM (uncurry (deflistItemToTEI opts)) items

-- | Convert a term and a list of blocks into a TEI varlistentry.
deflistItemToTEI :: PandocMonad m
                 => WriterOptions -> [Inline] -> [[Block]] -> m (Doc Text)
deflistItemToTEI opts term defs = do
  term' <- inlinesToTEI opts term
  defs' <- blocksToTEI opts $ concatMap (map plainToPara) defs
  return $ inTagsIndented "label" term' $$
           inTagsIndented "item" defs'

-- | Convert a list of lists of blocks to a list of TEI list items.
listItemsToTEI :: PandocMonad m => WriterOptions -> [[Block]] -> m (Doc Text)
listItemsToTEI opts items = vcat <$> mapM (listItemToTEI opts) items

-- | Convert a list of blocks into a TEI list item.
listItemToTEI :: PandocMonad m => WriterOptions -> [Block] -> m (Doc Text)
listItemToTEI opts item =
  inTagsIndented "item" <$> blocksToTEI opts (map plainToPara item)

imageToTEI :: PandocMonad m => WriterOptions -> Attr -> Text -> m (Doc Text)
imageToTEI opts attr src = return $ selfClosingTag "graphic" $
  ("url", src) : idFromAttr opts attr ++ dims
  where
    dims = go Width "width" ++ go Height "height"
    go dir dstr = case dimension dir attr of
                    Just a  -> [(dstr, tshow a)]
                    Nothing -> []

-- | Convert a Pandoc block element to TEI.
blockToTEI :: PandocMonad m => WriterOptions -> Block -> m (Doc Text)
blockToTEI _ Null = return empty
blockToTEI opts (Div attr@(_,"section":_,_) (Header lvl _ ils : xs)) =
  do
  -- TEI doesn't allow sections with no content, so insert some if needed
  let xs' = if null xs
               then [Para []]
               else xs
      -- level numbering correspond to LaTeX internals
      divType = case lvl of
                 n | n == -1          -> "part"
                   | n == 0           -> "chapter"
                   | n >= 1 && n <= 5 -> "level" <> tshow n
                   | otherwise        -> "section"
  titleContents <- inlinesToTEI opts ils
  contents <- blocksToTEI opts xs'
  return $ inTags True "div" (("type", divType) : idFromAttr opts attr) $
      inTagsSimple "head" titleContents $$ contents
-- Add ids to paragraphs in divs with ids - this is needed for
-- pandoc-citeproc to get link anchors in bibliographies:
blockToTEI opts (Div attr [Para lst]) = do
  let attribs = idFromAttr opts attr
  inTags False "p" attribs <$> inlinesToTEI opts lst
blockToTEI opts (Div _ bs) = blocksToTEI opts $ map plainToPara bs
blockToTEI _ h@Header{} = do
  -- should not occur after makeSections, except inside lists/blockquotes
  report $ BlockNotRendered h
  return empty
-- For TEI simple, text must be within containing block element, so
-- we use treat as Para to ensure that Plain text ends up contained by
-- something:
blockToTEI opts (Plain lst) = blockToTEI opts $ Para lst
-- title beginning with fig: indicates that the image is a figure
--blockToTEI opts (Para [Image attr txt (src,'f':'i':'g':':':_)]) =
--  let alt  = inlinesToTEI opts txt
--      capt = if null txt
--                then empty
--                else inTagsSimple "title" alt
--  in  inTagsIndented "figure" $
--        capt $$
--        (inTagsIndented "mediaobject" $
--           (inTagsIndented "imageobject"
--             (imageToTEI opts attr src)) $$
--           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToTEI opts (Para lst) =
  inTags False "p" [] <$> inlinesToTEI opts lst
blockToTEI opts (LineBlock lns) =
  blockToTEI opts $ linesToPara lns
blockToTEI opts (BlockQuote blocks) =
  inTagsIndented "quote" <$> blocksToTEI opts blocks
blockToTEI _ (CodeBlock (_,classes,_) str) =
  return $ literal ("<ab type='codeblock " <> lang <> "'>") <> cr <>
     flush (literal (escapeStringForXML str) <> cr <> text "</ab>")
    where lang  = if null langs
                     then ""
                     else escapeStringForXML (head langs)
          isLang l    = T.toLower l `elem` map T.toLower languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . T.toLower $ s
          langs       = concatMap langsFrom classes
blockToTEI opts (BulletList lst) = do
  let attribs = [("type", "unordered")]
  inTags True "list" attribs <$> listItemsToTEI opts lst
blockToTEI _ (OrderedList _ []) = return empty
blockToTEI opts (OrderedList (start, numstyle, _) (first:rest)) = do
  let attribs = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("type", "ordered:arabic")]
                       Example      -> [("type", "ordered:arabic")]
                       UpperAlpha   -> [("type", "ordered:upperalpha")]
                       LowerAlpha   -> [("type", "ordered:loweralpha")]
                       UpperRoman   -> [("type", "ordered:upperroman")]
                       LowerRoman   -> [("type", "ordered:lowerroman")]
  items <- if start == 1
              then listItemsToTEI opts (first:rest)
              else do
                fi <- blocksToTEI opts $ map plainToPara first
                re <- listItemsToTEI opts rest
                return $ inTags True "item" [("n",tshow start)] fi $$ re
  return $ inTags True "list" attribs items
blockToTEI opts (DefinitionList lst) = do
  let attribs = [("type", "definition")]
  inTags True "list" attribs <$> deflistItemsToTEI opts lst
blockToTEI _ b@(RawBlock f str)
  | f == "tei"     = return $ literal str
  -- raw TEI block (should such a thing exist).
  | otherwise      = do
    report $ BlockNotRendered b
    return empty
blockToTEI _ HorizontalRule = return $
  selfClosingTag "milestone" [("unit","undefined")
                             ,("type","separator")
                             ,("rendition","line")]

-- | TEI Tables
-- TEI Simple's tables are composed of cells and rows; other
-- table info in the AST is here lossily discard.
blockToTEI opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (_, _, _, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  headers' <- if null headers then pure mempty else tableHeadersToTEI opts headers
  rows' <- mapM (tableRowToTEI opts) rows
  return $ inTags True "table" [] $ headers' $$ vcat rows'

tableRowToTEI :: PandocMonad m
              => WriterOptions
              -> [[Block]]
              -> m (Doc Text)
tableRowToTEI opts cols =
  (inTagsIndented "row" . vcat) <$> mapM (tableItemToTEI opts) cols

tableHeadersToTEI :: PandocMonad m
                  => WriterOptions
                  -> [[Block]]
                  -> m (Doc Text)
tableHeadersToTEI opts cols =
  (inTags True "row" [("role","label")] . vcat) <$>
    mapM (tableItemToTEI opts) cols

tableItemToTEI :: PandocMonad m
               => WriterOptions
               -> [Block]
               -> m (Doc Text)
tableItemToTEI opts item =
  (inTags False "cell" [] . vcat) <$> mapM (blockToTEI opts) item

-- | Convert a list of inline elements to TEI.
inlinesToTEI :: PandocMonad m => WriterOptions -> [Inline] -> m (Doc Text)
inlinesToTEI opts lst = hcat <$> mapM (inlineToTEI opts) lst

-- | Convert an inline element to TEI.
inlineToTEI :: PandocMonad m => WriterOptions -> Inline -> m (Doc Text)
inlineToTEI _ (Str str) = return $ literal $ escapeStringForXML str
inlineToTEI opts (Emph lst) =
  inTags False "hi" [("rendition","simple:italic")] <$> inlinesToTEI opts lst
inlineToTEI opts (Underline lst) =
  inTags False "hi" [("rendition","simple:underline")] <$> inlinesToTEI opts lst
inlineToTEI opts (Strong lst) =
  inTags False "hi" [("rendition", "simple:bold")] <$> inlinesToTEI opts lst
inlineToTEI opts (Strikeout lst) =
  inTags False "hi" [("rendition", "simple:strikethrough")] <$>
  inlinesToTEI opts lst
inlineToTEI opts (Superscript lst) =
  inTags False "hi" [("rendition", "simple:superscript")] <$>
    inlinesToTEI opts lst
inlineToTEI opts (Subscript lst) =
  inTags False "hi" [("rendition", "simple:subscript")] <$>
    inlinesToTEI opts lst
inlineToTEI opts (SmallCaps lst) =
  inTags False "hi" [("rendition", "simple:smallcaps")] <$>
    inlinesToTEI opts lst
inlineToTEI opts (Quoted _ lst) =
  inTagsSimple "quote" <$> inlinesToTEI opts lst
inlineToTEI opts (Cite _ lst) =
  inlinesToTEI opts lst
inlineToTEI opts (Span _ ils) =
  inlinesToTEI opts ils
inlineToTEI _ (Code _ str) = return $
  inTags False "seg" [("type","code")] $ literal (escapeStringForXML str)
-- Distinguish display from inline math by wrapping the former in a "figure."
inlineToTEI _ (Math t str) = return $
  case t of
    InlineMath  -> inTags False "formula" [("notation","TeX")] $
                   literal str
    DisplayMath -> inTags True "figure" [("type","math")] $
                   inTags False "formula" [("notation","TeX")] $ literal str

inlineToTEI _ il@(RawInline f x) | f == "tei"     = return $ literal x
                                 | otherwise      = empty <$
                                     report (InlineNotRendered il)
inlineToTEI _ LineBreak = return $ selfClosingTag "lb" []
inlineToTEI _ Space =
            return space
-- because we use \n for LineBreak, we can't do soft breaks:
inlineToTEI _ SoftBreak =
            return space
inlineToTEI opts (Link attr txt (src, _))
  | Just email <- T.stripPrefix "mailto:" src = do
      let emailLink = literal $
                      escapeStringForXML email
      case txt of
           [Str s] | escapeURI s == email ->
                       return emailLink
           _             -> do
              linktext <- inlinesToTEI opts txt
              return $ linktext <+> char '(' <> emailLink <> char ')'
  | otherwise =
      inTags False "ref" (("target", src) : idFromAttr opts attr)
                 <$> inlinesToTEI opts txt
inlineToTEI opts (Image attr description (src, tit)) = do
  let titleDoc = if T.null tit
                   then empty
                   else inTags False "figDesc" []
                           (literal $ escapeStringForXML tit)
  imageDesc <- if null description
                  then return empty
                  else inTags False "head" []
                         <$> inlinesToTEI opts description
  img <- imageToTEI opts attr src
  return $ inTagsIndented "figure" $ imageDesc $$ img $$ titleDoc
inlineToTEI opts (Note contents) =
  inTagsIndented "note" <$> blocksToTEI opts contents

idFromAttr :: WriterOptions -> Attr -> [(Text, Text)]
idFromAttr opts (id',_,_) =
  [("xml:id", writerIdentifierPrefix opts <> id') | not (T.null id')]

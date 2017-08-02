{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Docbook
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.TEI (writeTEI) where
import Data.Char (toLower)
import Data.Text (Text)
import Data.List (isPrefixOf, stripPrefix)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.ImageSize
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML

-- | Convert list of authors to a docbook <author> section
authorToTEI :: PandocMonad m => WriterOptions -> [Inline] -> m B.Inlines
authorToTEI opts name' = do
  name <- render Nothing <$> inlinesToTEI opts name'
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  return $ B.rawInline "tei" $ render colwidth $
      inTagsSimple "author" (text $ escapeStringForXML name)

-- | Convert Pandoc document to string in Docbook format.
writeTEI :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTEI opts (Pandoc meta blocks) = do
  let elements = hierarchicalize blocks
      colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      render' :: Doc -> Text
      render' = render colwidth
      startLvl = case writerTopLevelDivision opts of
                   TopLevelPart    -> -1
                   TopLevelChapter -> 0
                   TopLevelSection -> 1
                   TopLevelDefault -> 1
  auths'      <- mapM (authorToTEI opts) $ docAuthors meta
  let meta'    = B.setMeta "author" auths' meta
  metadata <- metaToJSON opts
                 (fmap (render' . vcat) .
                   mapM (elementToTEI opts startLvl) . hierarchicalize)
                 (fmap render' . inlinesToTEI opts)
                 meta'
  main    <- (render' . vcat) <$> mapM (elementToTEI opts startLvl) elements
  let context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML -> True
                                        _      -> False)
              $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Convert an Element to TEI.
elementToTEI :: PandocMonad m => WriterOptions -> Int -> Element -> m Doc
elementToTEI opts _   (Blk block) = blockToTEI opts block
elementToTEI opts lvl (Sec _ _num (id',_,_) title elements) = do
  -- TEI doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
      -- level numbering correspond to LaTeX internals
      divType = case lvl of
                 n | n == -1          -> "part"
                   | n == 0           -> "chapter"
                   | n >= 1 && n <= 5 -> "level" ++ show n
                   | otherwise        -> "section"
  contents <- vcat <$> mapM (elementToTEI opts (lvl + 1)) elements'
  titleContents <- inlinesToTEI opts title
  return $ inTags True "div" (("type", divType) :
    [("id", writerIdentifierPrefix opts ++ id') | not (null id')]) $
      inTagsSimple "head" titleContents $$ contents

-- | Convert a list of Pandoc blocks to TEI.
blocksToTEI :: PandocMonad m => WriterOptions -> [Block] -> m Doc
blocksToTEI opts bs = vcat <$> mapM (blockToTEI opts) bs

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a TEI
-- list with labels and items.
deflistItemsToTEI :: PandocMonad m
                  => WriterOptions -> [([Inline],[[Block]])] -> m Doc
deflistItemsToTEI opts items =
 vcat <$> mapM (\(term, defs) -> deflistItemToTEI opts term defs) items

-- | Convert a term and a list of blocks into a TEI varlistentry.
deflistItemToTEI :: PandocMonad m
                 => WriterOptions -> [Inline] -> [[Block]] -> m Doc
deflistItemToTEI opts term defs = do
  term' <- inlinesToTEI opts term
  defs' <- blocksToTEI opts $ concatMap (map plainToPara) defs
  return $ inTagsIndented "label" term' $$
           inTagsIndented "item" defs'

-- | Convert a list of lists of blocks to a list of TEI list items.
listItemsToTEI :: PandocMonad m => WriterOptions -> [[Block]] -> m Doc
listItemsToTEI opts items = vcat <$> mapM (listItemToTEI opts) items

-- | Convert a list of blocks into a TEI list item.
listItemToTEI :: PandocMonad m => WriterOptions -> [Block] -> m Doc
listItemToTEI opts item =
  inTagsIndented "item" <$> blocksToTEI opts (map plainToPara item)

imageToTEI :: PandocMonad m => WriterOptions -> Attr -> String -> m Doc
imageToTEI _ attr src = return $ selfClosingTag "graphic" $
  ("url", src) : idAndRole attr ++ dims
  where
    dims = go Width "width" ++ go Height "depth"
    go dir dstr = case (dimension dir attr) of
                    Just a  -> [(dstr, show a)]
                    Nothing -> []

-- | Convert a Pandoc block element to TEI.
blockToTEI :: PandocMonad m => WriterOptions -> Block -> m Doc
blockToTEI _ Null = return empty
-- Add ids to paragraphs in divs with ids - this is needed for
-- pandoc-citeproc to get link anchors in bibliographies:
blockToTEI opts (Div (ident,_,_) [Para lst]) = do
  let attribs = [("id", ident) | not (null ident)]
  inTags False "p" attribs <$> inlinesToTEI opts lst
blockToTEI opts (Div _ bs) = blocksToTEI opts $ map plainToPara bs
blockToTEI _ h@(Header _ _ _) = do
  -- should not occur after hierarchicalize, except inside lists/blockquotes
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
  return $ text ("<ab type='codeblock " ++ lang ++ "'>") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</ab>")
    where lang  = if null langs
                     then ""
                     else escapeStringForXML (head langs)
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
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
                return $ (inTags True "item" [("n",show start)] fi) $$ re
  return $ inTags True "list" attribs items
blockToTEI opts (DefinitionList lst) = do
  let attribs = [("type", "definition")]
  inTags True "list" attribs <$> deflistItemsToTEI opts lst
blockToTEI _ b@(RawBlock f str)
  | f == "tei"     = return $ text str
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
blockToTEI opts (Table _ _ _ headers rows) = do
  headers' <- tableHeadersToTEI opts headers
  rows' <- mapM (tableRowToTEI opts) rows
  return $ inTags True "table" [] $ headers' $$ vcat rows'

tableRowToTEI :: PandocMonad m
              => WriterOptions
              -> [[Block]]
              -> m Doc
tableRowToTEI opts cols =
  (inTagsIndented "row" . vcat) <$> mapM (tableItemToTEI opts) cols

tableHeadersToTEI :: PandocMonad m
                  => WriterOptions
                  -> [[Block]]
                  -> m Doc
tableHeadersToTEI opts cols =
  (inTags True "row" [("role","label")] . vcat) <$>
    mapM (tableItemToTEI opts) cols

tableItemToTEI :: PandocMonad m
               => WriterOptions
               -> [Block]
               -> m Doc
tableItemToTEI opts item =
  (inTags False "cell" [] . vcat) <$> mapM (blockToTEI opts) item

-- | Convert a list of inline elements to TEI.
inlinesToTEI :: PandocMonad m => WriterOptions -> [Inline] -> m Doc
inlinesToTEI opts lst = hcat <$> mapM (inlineToTEI opts) lst

-- | Convert an inline element to TEI.
inlineToTEI :: PandocMonad m => WriterOptions -> Inline -> m Doc
inlineToTEI _ (Str str) = return $ text $ escapeStringForXML str
inlineToTEI opts (Emph lst) =
  inTags False "hi" [("rendition","simple:italic")] <$> inlinesToTEI opts lst
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
  inTags False "seg" [("type","code")] $ text (escapeStringForXML str)
-- Distinguish display from inline math by wrapping the former in a "figure."
inlineToTEI _ (Math t str) = return $
  case t of
    InlineMath  -> inTags False "formula" [("notation","TeX")] $
                   text (str)
    DisplayMath -> inTags True "figure" [("type","math")] $
                   inTags False "formula" [("notation","TeX")] $ text (str)

inlineToTEI _ il@(RawInline f x) | f == "tei"     = return $ text x
                                 | otherwise      = empty <$
                                     report (InlineNotRendered il)
inlineToTEI _ LineBreak = return $ selfClosingTag "lb" []
inlineToTEI _ Space = return $ space
-- because we use \n for LineBreak, we can't do soft breaks:
inlineToTEI _ SoftBreak = return $ space
inlineToTEI opts (Link attr txt (src, _))
  | Just email <- stripPrefix "mailto:" src = do
      let emailLink = text $
                      escapeStringForXML $ email
      case txt of
           [Str s] | escapeURI s == email -> return $ emailLink
           _             -> do
              linktext <- inlinesToTEI opts txt
              return $ linktext <+> char '(' <> emailLink <> char ')'
  | otherwise =
      (if isPrefixOf "#" src
            then inTags False "ref" $ ("target", drop 1 src) : idAndRole attr
            else inTags False "ref" $ ("target", src) : idAndRole attr ) <$>
        inlinesToTEI opts txt
inlineToTEI opts (Image attr description (src, tit)) = do
  let titleDoc = if null tit
                   then empty
                   else inTags False "figDesc" []
                           (text $ escapeStringForXML tit)
  imageDesc <- if null description
                  then return empty
                  else inTags False "head" []
                         <$> inlinesToTEI opts description
  img <- imageToTEI opts attr src
  return $ inTagsIndented "figure" $ imageDesc $$ img $$ titleDoc
inlineToTEI opts (Note contents) =
  inTagsIndented "note" <$> blocksToTEI opts contents

idAndRole :: Attr -> [(String, String)]
idAndRole (id',cls,_) = ident ++ role
  where
    ident = if null id'
               then []
               else [("id", id')]
    role  = if null cls
               then []
               else [("role", unwords cls)]

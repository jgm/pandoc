{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
{- In writing this module I drew heavily on Martin Fenner's
   pandoc-jats project (also GPL v2).
   Tag reference:
   https://jats.nlm.nih.gov/publishing/tag-library/1.1d3/element/mml-math.html
-}
{- |
   Module      : Text.Pandoc.Writers.JATS
   Copyright   : Copyright (C) 2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to JATS XML.
-}
module Text.Pandoc.Writers.JATS ( writeJATS ) where
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Generics (everywhere, mkT)
import Data.List (intercalate, isPrefixOf, isSuffixOf, stripPrefix)
import Data.Monoid (Any (..))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.ImageSize
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML
import Text.TeXMath
import qualified Text.XML.Light as Xml

data JATSVersion = JATS1_1
     deriving (Eq, Show)

type DB = ReaderT JATSVersion

-- | Convert list of authors to a docbook <author> section
authorToJATS :: PandocMonad m => WriterOptions -> [Inline] -> DB m B.Inlines
authorToJATS opts name' = do
  name <- render Nothing <$> inlinesToJATS opts name'
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  return $ B.rawInline "docbook" $ render colwidth $
      if ',' `elem` name
         then -- last name first
              let (lastname, rest) = break (==',') name
                  firstname = triml rest in
              inTagsSimple "firstname" (text $ escapeStringForXML firstname) <>
              inTagsSimple "surname" (text $ escapeStringForXML lastname)
         else -- last name last
              let namewords = words name
                  lengthname = length namewords
                  (firstname, lastname) = case lengthname of
                    0  -> ("","")
                    1  -> ("", name)
                    n  -> (intercalate " " (take (n-1) namewords), last namewords)
               in inTagsSimple "firstname" (text $ escapeStringForXML firstname) $$
                  inTagsSimple "surname" (text $ escapeStringForXML lastname)

writeJATS :: PandocMonad m => WriterOptions -> Pandoc -> m String
writeJATS opts d =
  runReaderT (docToJATS opts d) JATS1_1

-- | Convert Pandoc document to string in JATS format.
docToJATS :: PandocMonad m => WriterOptions -> Pandoc -> DB m String
docToJATS opts (Pandoc meta blocks) = do
  let elements = hierarchicalize blocks
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render'  = render colwidth
  let opts'    = if (maybe False (("/book>" `isSuffixOf`) . trimr)
                            (writerTemplate opts) &&
                     TopLevelDefault == writerTopLevelDivision opts)
                    then opts{ writerTopLevelDivision = TopLevelChapter }
                    else opts
  -- The numbering here follows LaTeX's internal numbering
  let startLvl = case writerTopLevelDivision opts' of
                   TopLevelPart    -> -1
                   TopLevelChapter -> 0
                   TopLevelSection -> 1
                   TopLevelDefault -> 1
  auths' <- mapM (authorToJATS opts) $ docAuthors meta
  let meta' = B.setMeta "author" auths' meta
  metadata <- metaToJSON opts
                 (fmap (render colwidth . vcat) .
                          (mapM (elementToJATS opts' startLvl) .
                            hierarchicalize))
                 (fmap (render colwidth) . inlinesToJATS opts')
                 meta'
  main <- (render' . vcat) <$> (mapM (elementToJATS opts' startLvl) elements)
  let context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML -> True
                                        _      -> False)
              $ metadata
  return $ case writerTemplate opts of
           Nothing  -> main
           Just tpl -> renderTemplate' tpl context

-- | Convert an Element to JATS.
elementToJATS :: PandocMonad m => WriterOptions -> Int -> Element -> DB m Doc
elementToJATS opts _   (Blk block) = blockToJATS opts block
elementToJATS opts lvl (Sec _ _num (id',_,kvs) title elements) = do
      idAttr = [("id", writerIdentifierPrefix opts ++ id') | not (null id')]
      otherAttrs = ["sec-type", "specific-use"]
      attribs = idAttr ++ [(k,v) | (k,v) <- kvs, k `elem` otherAttrs]
  contents <- mapM (elementToJATS opts (lvl + 1)) elements
  title' <- inlinesToJATS opts title
  return $ inTags True "sec" attribs $
      inTagsSimple "title" title' $$ vcat contents

-- | Convert a list of Pandoc blocks to JATS.
blocksToJATS :: PandocMonad m => WriterOptions -> [Block] -> DB m Doc
blocksToJATS opts = fmap vcat . mapM (blockToJATS opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of
-- JATS varlistentrys.
deflistItemsToJATS :: PandocMonad m
                      => WriterOptions -> [([Inline],[[Block]])] -> DB m Doc
deflistItemsToJATS opts items =
  vcat <$> mapM (\(term, defs) -> deflistItemToJATS opts term defs) items

-- | Convert a term and a list of blocks into a JATS varlistentry.
deflistItemToJATS :: PandocMonad m
                     => WriterOptions -> [Inline] -> [[Block]] -> DB m Doc
deflistItemToJATS opts term defs = do
  term' <- inlinesToJATS opts term
  def' <- blocksToJATS opts $ concatMap (map plainToPara) defs
  return $ inTagsIndented "varlistentry" $
      inTagsIndented "term" term' $$
      inTagsIndented "listitem" def'

-- | Convert a list of lists of blocks to a list of JATS list items.
listItemsToJATS :: PandocMonad m => WriterOptions -> [[Block]] -> DB m Doc
listItemsToJATS opts items = vcat <$> mapM (listItemToJATS opts) items

-- | Convert a list of blocks into a JATS list item.
listItemToJATS :: PandocMonad m => WriterOptions -> [Block] -> DB m Doc
listItemToJATS opts item =
  inTagsIndented "listitem" <$> blocksToJATS opts (map plainToPara item)

imageToJATS :: WriterOptions -> Attr -> String -> Doc
imageToJATS _ attr src = selfClosingTag "imagedata" $
  ("fileref", src) : idAndRole attr ++ dims
  where
    dims = go Width "width" ++ go Height "depth"
    go dir dstr = case (dimension dir attr) of
                    Just a  -> [(dstr, show a)]
                    Nothing -> []

-- | Convert a Pandoc block element to JATS.
blockToJATS :: PandocMonad m => WriterOptions -> Block -> DB m Doc
blockToJATS _ Null = return empty
-- Add ids to paragraphs in divs with ids - this is needed for
-- pandoc-citeproc to get link anchors in bibliographies:
blockToJATS opts (Div (ident,_,_) [Para lst]) =
  let attribs = [("id", ident) | not (null ident)] in
  if hasLineBreaks lst
     then (flush . nowrap . inTags False "literallayout" attribs)
                         <$> inlinesToJATS opts lst
     else inTags True "p" attribs <$> inlinesToJATS opts lst
blockToJATS opts (Div (ident,_,_) bs) = do
  contents <- blocksToJATS opts (map plainToPara bs)
  return $
    (if null ident
        then mempty
        else selfClosingTag "anchor" [("id", ident)]) $$ contents
blockToJATS _ (Header _ _ _) =
  return empty -- should not occur after hierarchicalize
blockToJATS opts (Plain lst) = inlinesToJATS opts lst
-- title beginning with fig: indicates that the image is a figure
blockToJATS opts (Para [Image attr txt (src,'f':'i':'g':':':_)]) = do
  alt <- inlinesToJATS opts txt
  let capt = if null txt
                then empty
                else inTagsSimple "title" alt
  return $ inTagsIndented "figure" $
        capt $$
        (inTagsIndented "mediaobject" $
           (inTagsIndented "imageobject"
             (imageToJATS opts attr src)) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToJATS opts (Para lst) =
  inTagsIndented "p" <$> inlinesToJATS opts lst
blockToJATS opts (LineBlock lns) =
  blockToJATS opts $ linesToPara lns
blockToJATS opts (BlockQuote blocks) =
  inTagsIndented "disp-quote" <$> blocksToJATS opts blocks
blockToJATS _ (CodeBlock (_,classes,_) str) = return $
  text ("<programlisting" ++ lang ++ ">") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</programlisting>")
    where lang  = if null langs
                     then ""
                     else " language=\"" ++ escapeStringForXML (head langs) ++
                          "\""
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToJATS opts (BulletList lst) = do
  let attribs = [("spacing", "compact") | isTightList lst]
  inTags True "itemizedlist" attribs <$> listItemsToJATS opts lst
blockToJATS _ (OrderedList _ []) = return empty
blockToJATS opts (OrderedList (start, numstyle, _) (first:rest)) = do
  let numeration = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      spacing    = [("spacing", "compact") | isTightList (first:rest)]
      attribs    = numeration ++ spacing
  items <- if start == 1
              then listItemsToJATS opts (first:rest)
              else do
                first' <- blocksToJATS opts (map plainToPara first)
                rest' <- listItemsToJATS opts rest
                return $
                  (inTags True "listitem" [("override",show start)] first') $$
                   rest'
  return $ inTags True "orderedlist" attribs items
blockToJATS opts (DefinitionList lst) = do
  let attribs = [("spacing", "compact") | isTightList $ concatMap snd lst]
  inTags True "variablelist" attribs <$> deflistItemsToJATS opts lst
blockToJATS _ b@(RawBlock f str)
  | f == "docbook" = return $ text str -- raw XML block
  | f == "html"    = return empty
  | otherwise      = do
      report $ BlockNotRendered b
      return empty
blockToJATS _ HorizontalRule = return empty -- not semantic
blockToJATS opts (Table caption aligns widths headers rows) = do
  captionDoc <- if null caption
                   then return empty
                   else inTagsIndented "title" <$>
                         inlinesToJATS opts caption
  let tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = show (truncate (100*w) :: Integer) ++ "*"
      coltags = vcat $ zipWith (\w al -> selfClosingTag "colspec"
                       ([("colwidth", percent w) | w > 0] ++
                        [("align", alignmentToString al)])) widths aligns
  head' <- if all null headers
              then return empty
              else inTagsIndented "thead" <$> tableRowToJATS opts headers
  body' <- (inTagsIndented "tbody" . vcat) <$>
              mapM (tableRowToJATS opts) rows
  return $ inTagsIndented tableType $ captionDoc $$
        (inTags True "tgroup" [("cols", show (length headers))] $
         coltags $$ head' $$ body')

hasLineBreaks :: [Inline] -> Bool
hasLineBreaks = getAny . query isLineBreak . walk removeNote
  where
    removeNote :: Inline -> Inline
    removeNote (Note _) = Str ""
    removeNote x        = x
    isLineBreak :: Inline -> Any
    isLineBreak LineBreak = Any True
    isLineBreak _         = Any False

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToJATS :: PandocMonad m
                  => WriterOptions
                  -> [[Block]]
                  -> DB m Doc
tableRowToJATS opts cols =
  (inTagsIndented "row" . vcat) <$> mapM (tableItemToJATS opts) cols

tableItemToJATS :: PandocMonad m
                   => WriterOptions
                   -> [Block]
                   -> DB m Doc
tableItemToJATS opts item =
  (inTags True "entry" [] . vcat) <$> mapM (blockToJATS opts) item

-- | Convert a list of inline elements to JATS.
inlinesToJATS :: PandocMonad m => WriterOptions -> [Inline] -> DB m Doc
inlinesToJATS opts lst = hcat <$> mapM (inlineToJATS opts) lst

-- | Convert an inline element to JATS.
inlineToJATS :: PandocMonad m => WriterOptions -> Inline -> DB m Doc
inlineToJATS _ (Str str) = return $ text $ escapeStringForXML str
inlineToJATS opts (Emph lst) =
  inTagsSimple "emphasis" <$> inlinesToJATS opts lst
inlineToJATS opts (Strong lst) =
  inTags False "emphasis" [("role", "strong")] <$> inlinesToJATS opts lst
inlineToJATS opts (Strikeout lst) =
  inTags False "emphasis" [("role", "strikethrough")] <$>
  inlinesToJATS opts lst
inlineToJATS opts (Superscript lst) =
  inTagsSimple "superscript" <$> inlinesToJATS opts lst
inlineToJATS opts (Subscript lst) =
  inTagsSimple "subscript" <$> inlinesToJATS opts lst
inlineToJATS opts (SmallCaps lst) =
  inTags False "emphasis" [("role", "smallcaps")] <$>
  inlinesToJATS opts lst
inlineToJATS opts (Quoted _ lst) =
  inTagsSimple "quote" <$> inlinesToJATS opts lst
inlineToJATS opts (Cite _ lst) =
  inlinesToJATS opts lst
inlineToJATS opts (Span (ident,_,_) ils) =
  ((if null ident
       then mempty
       else selfClosingTag "anchor" [("id", ident)]) <>) <$>
  inlinesToJATS opts ils
inlineToJATS _ (Code _ str) =
  return $ inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToJATS opts (Math t str)
  | isMathML (writerHTMLMathMethod opts) = do
    res <- convertMath writeMathML t str
    case res of
         Right r  -> return $ inTagsSimple tagtype
                     $ text $ Xml.ppcElement conf
                     $ fixNS
                     $ removeAttr r
         Left il  -> inlineToJATS opts il
  | otherwise =
     texMathToInlines t str >>= inlinesToJATS opts
     where tagtype = case t of
                       InlineMath  -> "inlineequation"
                       DisplayMath -> "informalequation"
           conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
           removeAttr e = e{ Xml.elAttribs = [] }
           fixNS' qname = qname{ Xml.qPrefix = Just "mml" }
           fixNS = everywhere (mkT fixNS')
inlineToJATS _ il@(RawInline f x)
  | f == "html" || f == "docbook" = return $ text x
  | otherwise                     = do
      report $ InlineNotRendered il
      return empty
inlineToJATS _ LineBreak = return $ text "\n"
-- currently ignore, would require the option to add custom
-- styles to the document
inlineToJATS _ Space = return space
-- because we use \n for LineBreak, we can't do soft breaks:
inlineToJATS _ SoftBreak = return space
inlineToJATS opts (Link attr txt (src, _))
  | Just email <- stripPrefix "mailto:" src =
      let emailLink = inTagsSimple "email" $ text $
                      escapeStringForXML $ email
      in  case txt of
           [Str s] | escapeURI s == email -> return emailLink
           _             -> do contents <- inlinesToJATS opts txt
                               return $ contents <+>
                                          char '(' <> emailLink <> char ')'
  | otherwise = do
      (if isPrefixOf "#" src
            then inTags False "link" $ ("linkend", writerIdentifierPrefix opts ++ drop 1 src) : idAndRole attr
            else inTags False "link" $ ("xlink:href", src) : idAndRole attr)
        <$> inlinesToJATS opts txt
inlineToJATS opts (Image attr _ (src, tit)) = return $
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (text $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ imageToJATS opts attr src
inlineToJATS opts (Note contents) =
  inTagsIndented "footnote" <$> blocksToJATS opts contents

isMathML :: HTMLMathMethod -> Bool
isMathML MathML = True
isMathML _      = False

idAndRole :: Attr -> [(String, String)]
idAndRole (id',cls,_) = ident ++ role
  where
    ident = if null id'
               then []
               else [("id", id')]
    role  = if null cls
               then []
               else [("role", unwords cls)]

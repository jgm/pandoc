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
   Module      : Text.Pandoc.Writers.JATS
   Copyright   : Copyright (C) 2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to JATS XML.
Reference:
https://jats.nlm.nih.gov/publishing/tag-library/1.1d3/element/mml-math.html
-}
module Text.Pandoc.Writers.JATS ( writeJATS ) where
import Control.Monad.Reader
import Data.Char (toLower)
import Data.Text (Text)
import Data.Generics (everywhere, mkT)
import Data.List (intercalate, isSuffixOf, partition)
import Data.Maybe (fromMaybe)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML
import Text.Pandoc.MIME (getMimeType)
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

writeJATS :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJATS opts d =
  runReaderT (docToJATS opts d) JATS1_1

-- | Convert Pandoc document to string in JATS format.
docToJATS :: PandocMonad m => WriterOptions -> Pandoc -> DB m Text
docToJATS opts (Pandoc meta blocks) = do
  let isBackBlock (Div ("refs",_,_) _) = True
      isBackBlock _ = False
  let (backblocks, bodyblocks) = partition isBackBlock blocks
  let elements = hierarchicalize bodyblocks
  let backElements = hierarchicalize backblocks
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render'  :: Doc -> Text
      render'  = render colwidth
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
                 (fmap (render' . vcat) .
                          (mapM (elementToJATS opts' startLvl) .
                            hierarchicalize))
                 (fmap render' . inlinesToJATS opts')
                 meta'
  main <- (render' . vcat) <$>
            (mapM (elementToJATS opts' startLvl) elements)
  back <- (render' . vcat) <$>
            (mapM (elementToJATS opts' startLvl) backElements)
  let context = defField "body" main
              $ defField "back" back
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML -> True
                                        _      -> False)
              $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Convert an Element to JATS.
elementToJATS :: PandocMonad m => WriterOptions -> Int -> Element -> DB m Doc
elementToJATS opts _   (Blk block) = blockToJATS opts block
elementToJATS opts lvl (Sec _ _num (id',_,kvs) title elements) = do
  let idAttr = [("id", writerIdentifierPrefix opts ++ id') | not (null id')]
  let otherAttrs = ["sec-type", "specific-use"]
  let attribs = idAttr ++ [(k,v) | (k,v) <- kvs, k `elem` otherAttrs]
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
  return $ inTagsIndented "def-item" $
      inTagsIndented "term" term' $$
      inTagsIndented "def" def'

-- | Convert a list of lists of blocks to a list of JATS list items.
listItemsToJATS :: PandocMonad m
                => WriterOptions -> (Maybe [String]) -> [[Block]] -> DB m Doc
listItemsToJATS opts markers items =
  case markers of
       Nothing -> vcat <$> mapM (listItemToJATS opts Nothing) items
       Just ms -> vcat <$> zipWithM (listItemToJATS opts) (map Just ms) items

-- | Convert a list of blocks into a JATS list item.
listItemToJATS :: PandocMonad m
               => WriterOptions -> (Maybe String) -> [Block] -> DB m Doc
listItemToJATS opts mbmarker item = do
  contents <- blocksToJATS opts item
  return $ inTagsIndented "list-item" $
           maybe empty (\lbl -> inTagsIndented "label" (text lbl)) mbmarker
           $$ contents

-- | Convert a Pandoc block element to JATS.
blockToJATS :: PandocMonad m => WriterOptions -> Block -> DB m Doc
blockToJATS _ Null = return empty
-- Bibliography reference:
blockToJATS opts (Div ('r':'e':'f':'-':_,_,_) [Para lst]) =
  inlinesToJATS opts lst
blockToJATS opts (Div ("refs",_,_) xs) = do
  contents <- blocksToJATS opts xs
  return $ inTagsIndented "ref-list" contents
blockToJATS opts (Div (ident,_,kvs) bs) = do
  contents <- blocksToJATS opts bs
  let attr = [("id", ident) | not (null ident)] ++
             [("xml:lang",l) | ("lang",l) <- kvs] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["specific-use",
                 "content-type", "orientation", "position"]]
  return $ inTags True "boxed-text" attr contents
blockToJATS _ h@(Header _ _ _) = do
  -- should not occur after hierarchicalize, except inside lists/blockquotes
  report $ BlockNotRendered h
  return empty
-- No Plain, everything needs to be in a block-level tag
blockToJATS opts (Plain lst) = blockToJATS opts (Para lst)
-- title beginning with fig: indicates that the image is a figure
blockToJATS opts (Para [Image (ident,_,kvs) txt
  (src,'f':'i':'g':':':tit)]) = do
  alt <- inlinesToJATS opts txt
  let capt = if null txt
                then empty
                else inTagsSimple "caption" alt
  let attr = [("id", ident) | not (null ident)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["fig-type", "orientation",
                                              "position", "specific-use"]]
  let mbMT = getMimeType src
  let maintype = fromMaybe "image" $
                  lookup "mimetype" kvs `mplus`
                  (takeWhile (/='/') <$> mbMT)
  let subtype = fromMaybe "" $
                  lookup "mime-subtype" kvs `mplus`
                  ((drop 1 . dropWhile (/='/')) <$> mbMT)
  let graphicattr = [("mimetype",maintype),
                     ("mime-subtype",drop 1 subtype),
                     ("xlink:href",src),  -- do we need to URL escape this?
                     ("xlink:title",tit)]
  return $ inTags True "fig" attr $
              capt $$ selfClosingTag "graphic" graphicattr
blockToJATS opts (Para lst) =
  inTagsIndented "p" <$> inlinesToJATS opts lst
blockToJATS opts (LineBlock lns) =
  blockToJATS opts $ linesToPara lns
blockToJATS opts (BlockQuote blocks) =
  inTagsIndented "disp-quote" <$> blocksToJATS opts blocks
blockToJATS _ (CodeBlock (ident,classes,kvs) str) = return $
  inTags False tag attr (flush (text (escapeStringForXML str)))
    where attr  = [("id",ident) | not (null ident)] ++
                  [("language",lang) | not (null lang)] ++
                  [(k,v) | (k,v) <- kvs, k `elem` ["code-type",
                    "code-version", "executable",
                    "language-version", "orientation",
                    "platforms", "position", "specific-use"]]
          tag   = if null lang then "preformat" else "code"
          lang  = case langs of
                     (l:_) -> escapeStringForXML l
                     []    -> ""
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToJATS _ (BulletList []) = return empty
blockToJATS opts (BulletList lst) = do
  inTags True "list" [("list-type", "bullet")] <$>
    listItemsToJATS opts Nothing lst
blockToJATS _ (OrderedList _ []) = return empty
blockToJATS opts (OrderedList (start, numstyle, delimstyle) items) = do
  let listType = case numstyle of
                       DefaultStyle -> "order"
                       Decimal      -> "order"
                       Example      -> "order"
                       UpperAlpha   -> "alpha-upper"
                       LowerAlpha   -> "alpha-lower"
                       UpperRoman   -> "roman-upper"
                       LowerRoman   -> "roman-lower"
  let simpleList = start == 1 && (delimstyle == DefaultDelim ||
                                  delimstyle == Period)
  let markers = if simpleList
                   then Nothing
                   else Just $
                          orderedListMarkers (start, numstyle, delimstyle)
  inTags True "list" [("list-type", listType)] <$>
    listItemsToJATS opts markers items
blockToJATS opts (DefinitionList lst) = do
  inTags True "def-list" [] <$> deflistItemsToJATS opts lst
blockToJATS _ b@(RawBlock f str)
  | f == "jats"    = return $ text str -- raw XML block
  | otherwise      = do
      report $ BlockNotRendered b
      return empty
blockToJATS _ HorizontalRule = return empty -- not semantic
blockToJATS opts (Table [] aligns widths headers rows) = do
  let percent w    = show (truncate (100*w) :: Integer) ++ "*"
  let coltags = vcat $ zipWith (\w al -> selfClosingTag "col"
                       ([("width", percent w) | w > 0] ++
                        [("align", alignmentToString al)])) widths aligns
  thead <- if all null headers
              then return empty
              else inTagsIndented "thead" <$> tableRowToJATS opts True headers
  tbody <- (inTagsIndented "tbody" . vcat) <$>
                mapM (tableRowToJATS opts False) rows
  return $ inTags True "table" [] $ coltags $$ thead $$ tbody
blockToJATS opts (Table caption aligns widths headers rows) = do
  captionDoc <- inTagsIndented "caption" <$> blockToJATS opts (Para caption)
  tbl <- blockToJATS opts (Table [] aligns widths headers rows)
  return $ inTags True "table-wrap" [] $ captionDoc $$ tbl

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToJATS :: PandocMonad m
                  => WriterOptions
                  -> Bool
                  -> [[Block]]
                  -> DB m Doc
tableRowToJATS opts isHeader cols =
  (inTagsIndented "tr" . vcat) <$> mapM (tableItemToJATS opts isHeader) cols

tableItemToJATS :: PandocMonad m
                   => WriterOptions
                   -> Bool
                   -> [Block]
                   -> DB m Doc
tableItemToJATS opts isHeader item =
  (inTags True (if isHeader then "th" else "td") [] . vcat) <$>
    mapM (blockToJATS opts) item

-- | Convert a list of inline elements to JATS.
inlinesToJATS :: PandocMonad m => WriterOptions -> [Inline] -> DB m Doc
inlinesToJATS opts lst = hcat <$> mapM (inlineToJATS opts) lst

-- | Convert an inline element to JATS.
inlineToJATS :: PandocMonad m => WriterOptions -> Inline -> DB m Doc
inlineToJATS _ (Str str) = return $ text $ escapeStringForXML str
inlineToJATS opts (Emph lst) =
  inTagsSimple "italic" <$> inlinesToJATS opts lst
inlineToJATS opts (Strong lst) =
  inTags False "bold" [("role", "strong")] <$> inlinesToJATS opts lst
inlineToJATS opts (Strikeout lst) =
  inTagsSimple "strike" <$> inlinesToJATS opts lst
inlineToJATS opts (Superscript lst) =
  inTagsSimple "sup" <$> inlinesToJATS opts lst
inlineToJATS opts (Subscript lst) =
  inTagsSimple "sub" <$> inlinesToJATS opts lst
inlineToJATS opts (SmallCaps lst) =
  inTags False "sc" [("role", "smallcaps")] <$>
  inlinesToJATS opts lst
inlineToJATS opts (Quoted SingleQuote lst) = do
  contents <- inlinesToJATS opts lst
  return $ char '‘' <> contents <> char '’'
inlineToJATS opts (Quoted DoubleQuote lst) = do
  contents <- inlinesToJATS opts lst
  return $ char '“' <> contents <> char '”'
inlineToJATS _ (Code _ str) =
  return $ inTagsSimple "monospace" $ text (escapeStringForXML str)
inlineToJATS _ il@(RawInline f x)
  | f == "jats" = return $ text x
  | otherwise   = do
      report $ InlineNotRendered il
      return empty
inlineToJATS _ LineBreak = return $ selfClosingTag "break" []
inlineToJATS _ Space = return space
inlineToJATS opts SoftBreak
  | writerWrapText opts == WrapPreserve = return cr
  | otherwise = return space
inlineToJATS opts (Note contents) =
  -- TODO technically only <p> tags are allowed inside
  inTagsIndented "fn" <$> blocksToJATS opts contents
inlineToJATS opts (Cite _ lst) =
  -- TODO revisit this after examining the jats.csl pipeline
  inlinesToJATS opts lst
inlineToJATS opts (Span ("",_,[]) ils) = inlinesToJATS opts ils
inlineToJATS opts (Span (ident,_,kvs) ils) = do
  contents <- inlinesToJATS opts ils
  let attr = [("id",ident) | not (null ident)] ++
             [("xml:lang",l) | ("lang",l) <- kvs] ++
             [(k,v) | (k,v) <- kvs
                    ,  k `elem` ["content-type", "rationale",
                                 "rid", "specific-use"]]
  return $ selfClosingTag "milestone-start" attr <> contents <>
           selfClosingTag "milestone-end" []
inlineToJATS _ (Math t str) = do
  let addPref (Xml.Attr q v)
         | Xml.qName q == "xmlns" = Xml.Attr q{ Xml.qName = "xmlns:mml" } v
         | otherwise = Xml.Attr q v
  let fixNS' e = e{ Xml.elName =
                         (Xml.elName e){ Xml.qPrefix = Just "mml" } }
  let fixNS = everywhere (mkT fixNS') .
              (\e -> e{ Xml.elAttribs = map addPref (Xml.elAttribs e) })
  let conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
  res <- convertMath writeMathML t str
  let tagtype = case t of
                     DisplayMath -> "disp-formula"
                     InlineMath  -> "inline-formula"
  let rawtex = inTagsSimple "tex-math"
                                $ text "<![CDATA[" <>
                                  text str <>
                                  text "]]>"
  return $ inTagsSimple tagtype $
             case res of
                   Right r  -> inTagsSimple "alternatives" $
                                  cr <> rawtex $$
                                  (text $ Xml.ppcElement conf $ fixNS r)
                   Left _   -> rawtex
inlineToJATS _ (Link _attr [Str t] ('m':'a':'i':'l':'t':'o':':':email, _))
  | escapeURI t == email =
  return $ inTagsSimple "email" $ text (escapeStringForXML email)
inlineToJATS opts (Link (ident,_,kvs) txt ('#':src, _)) = do
  let attr = [("id", ident) | not (null ident)] ++
             [("alt", stringify txt),
              ("rid", src)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["ref-type", "specific-use"]]
  contents <- inlinesToJATS opts txt
  return $ inTags False "xref" attr contents
inlineToJATS opts (Link (ident,_,kvs) txt (src, tit)) = do
  let attr = [("id", ident) | not (null ident)] ++
             [("ext-link-type", "uri"),
              ("xlink:href", src)] ++
             [("xlink:title", tit) | not (null tit)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["assigning-authority",
                                              "specific-use", "xlink:actuate",
                                              "xlink:role", "xlink:show",
                                              "xlink:type"]]
  contents <- inlinesToJATS opts txt
  return $ inTags False "ext-link" attr contents
inlineToJATS _ (Image (ident,_,kvs) _ (src, tit)) = do
  let mbMT = getMimeType src
  let maintype = fromMaybe "image" $
                  lookup "mimetype" kvs `mplus`
                  (takeWhile (/='/') <$> mbMT)
  let subtype = fromMaybe "" $
                  lookup "mime-subtype" kvs `mplus`
                  ((drop 1 . dropWhile (/='/')) <$> mbMT)
  let attr = [("id", ident) | not (null ident)] ++
             [("mimetype", maintype),
              ("mime-subtype", subtype),
              ("xlink:href", src)] ++
             [("xlink:title", tit) | not (null tit)] ++
             [(k,v) | (k,v) <- kvs, k `elem` ["baseline-shift",
                        "content-type", "specific-use", "xlink:actuate",
                        "xlink:href", "xlink:role", "xlink:show",
                        "xlink:type"]]
  return $ selfClosingTag "inline-graphic" attr

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Writers.Docbook
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.Docbook ( writeDocbook4, writeDocbook5 ) where
import Control.Monad.Reader
import Data.Generics (everywhere, mkT)
import Data.Monoid (Any (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
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
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Math
import Text.Pandoc.XML
import Text.TeXMath
import qualified Text.XML.Light as Xml

data DocBookVersion = DocBook4 | DocBook5
     deriving (Eq, Show)

type DB = ReaderT DocBookVersion

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: PandocMonad m => WriterOptions -> [Inline] -> DB m B.Inlines
authorToDocbook opts name' = do
  name <- render Nothing <$> inlinesToDocbook opts name'
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  return $ B.rawInline "docbook" $
    render colwidth $ inTags True "personname" [] $
      if T.any (== ',') name
         then -- last name first
              let (lastname, rest) = T.break (==',') name
                  firstname = triml rest in
              inTagsSimple "firstname" (literal $ escapeStringForXML firstname) <>
              inTagsSimple "surname" (literal $ escapeStringForXML lastname)
         else -- last name last
              let namewords = T.words name
                  lengthname = length namewords
                  (firstname, lastname) = case lengthname of
                    0 -> ("","")
                    1 -> ("", name)
                    n -> (T.unwords (take (n-1) namewords), last namewords)
               in inTagsSimple "firstname" (literal $ escapeStringForXML firstname) $$
                  inTagsSimple "surname" (literal $ escapeStringForXML lastname)

writeDocbook4 :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeDocbook4 opts d =
  runReaderT (writeDocbook opts d) DocBook4

writeDocbook5 :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeDocbook5 opts d =
  runReaderT (writeDocbook opts d) DocBook5

-- | Convert Pandoc document to string in Docbook format.
writeDocbook :: PandocMonad m => WriterOptions -> Pandoc -> DB m Text
writeDocbook opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  -- The numbering here follows LaTeX's internal numbering
  let startLvl = case writerTopLevelDivision opts of
                   TopLevelPart    -> -1
                   TopLevelChapter -> 0
                   TopLevelSection -> 1
                   TopLevelDefault -> 1
  let fromBlocks = blocksToDocbook opts .
                   makeSections False (Just startLvl)
  auths' <- mapM (authorToDocbook opts) $ docAuthors meta
  let meta' = B.setMeta "author" auths' meta
  metadata <- metaToContext opts
                 fromBlocks
                 (inlinesToDocbook opts)
                 meta'
  main <- fromBlocks blocks
  let context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                          MathML -> True
                                          _      -> False) metadata
  return $ render colwidth $
    (if writerPreferAscii opts then fmap toEntities else id) $
    case writerTemplate opts of
         Nothing  -> main
         Just tpl -> renderTemplate tpl context

-- | Convert a list of Pandoc blocks to Docbook.
blocksToDocbook :: PandocMonad m => WriterOptions -> [Block] -> DB m (Doc Text)
blocksToDocbook opts = fmap vcat . mapM (blockToDocbook opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of
-- Docbook varlistentrys.
deflistItemsToDocbook :: PandocMonad m
                      => WriterOptions -> [([Inline],[[Block]])] -> DB m (Doc Text)
deflistItemsToDocbook opts items =
  vcat <$> mapM (uncurry (deflistItemToDocbook opts)) items

-- | Convert a term and a list of blocks into a Docbook varlistentry.
deflistItemToDocbook :: PandocMonad m
                     => WriterOptions -> [Inline] -> [[Block]] -> DB m (Doc Text)
deflistItemToDocbook opts term defs = do
  term' <- inlinesToDocbook opts term
  def' <- blocksToDocbook opts $ concatMap (map plainToPara) defs
  return $ inTagsIndented "varlistentry" $
      inTagsIndented "term" term' $$
      inTagsIndented "listitem" def'

-- | Convert a list of lists of blocks to a list of Docbook list items.
listItemsToDocbook :: PandocMonad m => WriterOptions -> [[Block]] -> DB m (Doc Text)
listItemsToDocbook opts items = vcat <$> mapM (listItemToDocbook opts) items

-- | Convert a list of blocks into a Docbook list item.
listItemToDocbook :: PandocMonad m => WriterOptions -> [Block] -> DB m (Doc Text)
listItemToDocbook opts item =
  inTagsIndented "listitem" <$> blocksToDocbook opts (map plainToPara item)

imageToDocbook :: WriterOptions -> Attr -> Text -> Doc Text
imageToDocbook _ attr src = selfClosingTag "imagedata" $
  ("fileref", src) : idAndRole attr <> dims
  where
    dims = go Width "width" <> go Height "depth"
    go dir dstr = case dimension dir attr of
                    Just a  -> [(dstr, tshow a)]
                    Nothing -> []

-- | Convert a Pandoc block element to Docbook.
blockToDocbook :: PandocMonad m => WriterOptions -> Block -> DB m (Doc Text)
blockToDocbook _ Null = return empty
-- Add ids to paragraphs in divs with ids - this is needed for
-- pandoc-citeproc to get link anchors in bibliographies:
blockToDocbook opts (Div (id',"section":_,_) (Header lvl _ ils : xs)) = do
  version <- ask
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let bs = if null xs
              then [Para []]
              else xs
      tag = case lvl of
                 -1                   -> "part"
                 0                    -> "chapter"
                 n | n >= 1 && n <= 5 -> if version == DocBook5
                                              then "section"
                                              else "sect" <> tshow n
                 _                    -> "simplesect"
      idName = if version == DocBook5
                 then "xml:id"
                 else "id"
      idAttr = [(idName, writerIdentifierPrefix opts <> id') | not (T.null id')]
      nsAttr = if version == DocBook5 && lvl == 0 then [("xmlns", "http://docbook.org/ns/docbook"),("xmlns:xlink", "http://www.w3.org/1999/xlink")]
                                      else []
      attribs = nsAttr <> idAttr
  title' <- inlinesToDocbook opts ils
  contents <- blocksToDocbook opts bs
  return $ inTags True tag attribs $ inTagsSimple "title" title' $$ contents
blockToDocbook opts (Div (ident,_,_) [Para lst]) =
  let attribs = [("id", ident) | not (T.null ident)] in
  if hasLineBreaks lst
     then (flush . nowrap . inTags False "literallayout" attribs)
                         <$> inlinesToDocbook opts lst
     else inTags True "para" attribs <$> inlinesToDocbook opts lst
blockToDocbook opts (Div (ident,_,_) bs) = do
  contents <- blocksToDocbook opts (map plainToPara bs)
  return $
    (if T.null ident
        then mempty
        else selfClosingTag "anchor" [("id", ident)]) $$ contents
blockToDocbook _ h@Header{} = do
  -- should be handled by Div section above, except inside lists/blockquotes
  report $ BlockNotRendered h
  return empty
blockToDocbook opts (Plain lst) = inlinesToDocbook opts lst
-- title beginning with fig: indicates that the image is a figure
blockToDocbook opts (Para [Image attr txt (src,T.stripPrefix "fig:" -> Just _)]) = do
  alt <- inlinesToDocbook opts txt
  let capt = if null txt
                then empty
                else inTagsSimple "title" alt
  return $ inTagsIndented "figure" $
        capt $$
        inTagsIndented "mediaobject" (
           inTagsIndented "imageobject"
             (imageToDocbook opts attr src) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToDocbook opts (Para lst)
  | hasLineBreaks lst = (flush . nowrap . inTagsSimple "literallayout")
                        <$> inlinesToDocbook opts lst
  | otherwise         = inTagsIndented "para" <$> inlinesToDocbook opts lst
blockToDocbook opts (LineBlock lns) =
  blockToDocbook opts $ linesToPara lns
blockToDocbook opts (BlockQuote blocks) =
  inTagsIndented "blockquote" <$> blocksToDocbook opts blocks
blockToDocbook _ (CodeBlock (_,classes,_) str) = return $
  literal ("<programlisting" <> lang <> ">") <> cr <>
     flush (literal (escapeStringForXML str) <> cr <> literal "</programlisting>")
    where lang  = if null langs
                     then ""
                     else " language=\"" <> escapeStringForXML (head langs) <>
                          "\""
          isLang l    = T.toLower l `elem` map T.toLower languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . T.toLower $ s
          langs       = concatMap langsFrom classes
blockToDocbook opts (BulletList lst) = do
  let attribs = [("spacing", "compact") | isTightList lst]
  inTags True "itemizedlist" attribs <$> listItemsToDocbook opts lst
blockToDocbook _ (OrderedList _ []) = return empty
blockToDocbook opts (OrderedList (start, numstyle, _) (first:rest)) = do
  let numeration = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      spacing    = [("spacing", "compact") | isTightList (first:rest)]
      attribs    = numeration <> spacing
  items <- if start == 1
              then listItemsToDocbook opts (first:rest)
              else do
                first' <- blocksToDocbook opts (map plainToPara first)
                rest' <- listItemsToDocbook opts rest
                return $
                  inTags True "listitem" [("override",tshow start)] first' $$
                   rest'
  return $ inTags True "orderedlist" attribs items
blockToDocbook opts (DefinitionList lst) = do
  let attribs = [("spacing", "compact") | isTightList $ concatMap snd lst]
  inTags True "variablelist" attribs <$> deflistItemsToDocbook opts lst
blockToDocbook _ b@(RawBlock f str)
  | f == "docbook" = return $ literal str -- raw XML block
  | f == "html"    = do
                     version <- ask
                     if version == DocBook5
                        then return empty -- No html in Docbook5
                        else return $ literal str -- allow html for backwards compatibility
  | otherwise      = do
      report $ BlockNotRendered b
      return empty
blockToDocbook _ HorizontalRule = return empty -- not semantic
blockToDocbook opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (caption, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
  captionDoc <- if null caption
                   then return empty
                   else inTagsIndented "title" <$>
                         inlinesToDocbook opts caption
  let tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = tshow (truncate (100*w) :: Integer) <> "*"
      coltags = vcat $ zipWith (\w al -> selfClosingTag "colspec"
                       ([("colwidth", percent w) | w > 0] <>
                        [("align", alignmentToString al)])) widths aligns
  head' <- if all null headers
              then return empty
              else inTagsIndented "thead" <$> tableRowToDocbook opts headers
  body' <- (inTagsIndented "tbody" . vcat) <$>
              mapM (tableRowToDocbook opts) rows
  return $ inTagsIndented tableType $ captionDoc $$
        inTags True "tgroup" [("cols", tshow (length aligns))] (
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

alignmentToString :: Alignment -> Text
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToDocbook :: PandocMonad m
                  => WriterOptions
                  -> [[Block]]
                  -> DB m (Doc Text)
tableRowToDocbook opts cols =
  (inTagsIndented "row" . vcat) <$> mapM (tableItemToDocbook opts) cols

tableItemToDocbook :: PandocMonad m
                   => WriterOptions
                   -> [Block]
                   -> DB m (Doc Text)
tableItemToDocbook opts item =
  (inTags True "entry" [] . vcat) <$> mapM (blockToDocbook opts) item

-- | Convert a list of inline elements to Docbook.
inlinesToDocbook :: PandocMonad m => WriterOptions -> [Inline] -> DB m (Doc Text)
inlinesToDocbook opts lst = hcat <$> mapM (inlineToDocbook opts) lst

-- | Convert an inline element to Docbook.
inlineToDocbook :: PandocMonad m => WriterOptions -> Inline -> DB m (Doc Text)
inlineToDocbook _ (Str str) = return $ literal $ escapeStringForXML str
inlineToDocbook opts (Emph lst) =
  inTagsSimple "emphasis" <$> inlinesToDocbook opts lst
inlineToDocbook opts (Underline lst) =
  inTags False "emphasis" [("role", "underline")] <$> inlinesToDocbook opts lst
inlineToDocbook opts (Strong lst) =
  inTags False "emphasis" [("role", "strong")] <$> inlinesToDocbook opts lst
inlineToDocbook opts (Strikeout lst) =
  inTags False "emphasis" [("role", "strikethrough")] <$>
  inlinesToDocbook opts lst
inlineToDocbook opts (Superscript lst) =
  inTagsSimple "superscript" <$> inlinesToDocbook opts lst
inlineToDocbook opts (Subscript lst) =
  inTagsSimple "subscript" <$> inlinesToDocbook opts lst
inlineToDocbook opts (SmallCaps lst) =
  inTags False "emphasis" [("role", "smallcaps")] <$>
  inlinesToDocbook opts lst
inlineToDocbook opts (Quoted _ lst) =
  inTagsSimple "quote" <$> inlinesToDocbook opts lst
inlineToDocbook opts (Cite _ lst) =
  inlinesToDocbook opts lst
inlineToDocbook opts (Span (ident,_,_) ils) =
  ((if T.null ident
       then mempty
       else selfClosingTag "anchor" [("id", ident)]) <>) <$>
  inlinesToDocbook opts ils
inlineToDocbook _ (Code _ str) =
  return $ inTagsSimple "literal" $ literal (escapeStringForXML str)
inlineToDocbook opts (Math t str)
  | isMathML (writerHTMLMathMethod opts) = do
    res <- convertMath writeMathML t str
    case res of
         Right r  -> return $ inTagsSimple tagtype
                     $ literal $ T.pack $ Xml.ppcElement conf
                     $ fixNS
                     $ removeAttr r
         Left il  -> inlineToDocbook opts il
  | otherwise =
     texMathToInlines t str >>= inlinesToDocbook opts
     where tagtype = case t of
                       InlineMath  -> "inlineequation"
                       DisplayMath -> "informalequation"
           conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
           removeAttr e = e{ Xml.elAttribs = [] }
           fixNS' qname = qname{ Xml.qPrefix = Just "mml" }
           fixNS = everywhere (mkT fixNS')
inlineToDocbook _ il@(RawInline f x)
  | f == "html" || f == "docbook" = return $ literal x
  | otherwise                     = do
      report $ InlineNotRendered il
      return empty
inlineToDocbook _ LineBreak = return $ literal "\n"
-- currently ignore, would require the option to add custom
-- styles to the document
inlineToDocbook _ Space = return space
-- because we use \n for LineBreak, we can't do soft breaks:
inlineToDocbook _ SoftBreak = return space
inlineToDocbook opts (Link attr txt (src, _))
  | Just email <- T.stripPrefix "mailto:" src =
      let emailLink = inTagsSimple "email" $ literal $
                      escapeStringForXML email
      in  case txt of
           [Str s] | escapeURI s == email -> return emailLink
           _             -> do contents <- inlinesToDocbook opts txt
                               return $ contents <+>
                                          char '(' <> emailLink <> char ')'
  | otherwise = do
      version <- ask
      (if "#" `T.isPrefixOf` src
            then inTags False "link" $ ("linkend", writerIdentifierPrefix opts <> T.drop 1 src) : idAndRole attr
            else if version == DocBook5
                    then inTags False "link" $ ("xlink:href", src) : idAndRole attr
                    else inTags False "ulink" $ ("url", src) : idAndRole attr )
        <$> inlinesToDocbook opts txt
inlineToDocbook opts (Image attr _ (src, tit)) = return $
  let titleDoc = if T.null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (literal $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ imageToDocbook opts attr src
inlineToDocbook opts (Note contents) =
  inTagsIndented "footnote" <$> blocksToDocbook opts contents

isMathML :: HTMLMathMethod -> Bool
isMathML MathML = True
isMathML _      = False

idAndRole :: Attr -> [(Text, Text)]
idAndRole (id',cls,_) = ident <> role
  where
    ident = [("id", id') | not (T.null id')]
    role  = [("role", T.unwords cls) | not (null cls)]

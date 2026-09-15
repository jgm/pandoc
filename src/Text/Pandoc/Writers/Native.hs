{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a string representation.

This used to be implemented using pretty-show's 'ppDoc' (which shows
the document, tokenizes and parses the result, and lays it out with
Text.PrettyPrint.HughesPJ).  For performance, we now build the layout
directly from the AST, carefully reproducing the exact output of the
old implementation (with @ribbonsPerLine = 1.2@).
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))

-- | Prettyprint Pandoc document.
writeNative :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeNative opts doc@(Pandoc _ blocks) = do
  let cols = writerColumns opts
  -- HughesPJ computes the ribbon length this way (with Float division):
  let ribbon = round (fromIntegral cols / (1.2 :: Float))
  return $ case writerTemplate opts of
    -- The old code appended a (char '\n'), which participates in layout
    -- as one extra glued character on the last line; hence glue0 = 1.
    Just _  -> render cols ribbon 1 (vDoc (pandocV doc)) <> "\n"
    Nothing -> render cols ribbon 0 (vDoc (blocksV blocks))

--
-- Layout documents (mirroring the structures pretty-show builds)
--

-- | A document together with the width of its one-line rendering.
data Doc = Doc !Int DC

-- | Either literal text, or a group that is rendered like HughesPJ's
-- 'sep': all on one line (elements joined by single spaces) if it
-- fits, otherwise vertically, with each element after the first on
-- its own line, indented by its nesting relative to the column at
-- which the group starts.
data DC = DText !Text
        | DGroup [Elt]

-- | Group element: nesting, glued prefix text, document, glued suffix.
data Elt = Elt !Int !Text Doc !Text

width :: Doc -> Int
width (Doc w _) = w

dtext :: Text -> Doc
dtext t = Doc (T.length t) (DText t)

group :: [Elt] -> Doc
group es = Doc (foldl (\acc e -> acc + 1 + eltWidth e) (-1) es) (DGroup es)
  where eltWidth (Elt _ pre d post) = T.length pre + width d + T.length post

-- | A value, i.e. a document plus an indication of whether it needs
-- parentheses when used as a constructor argument.
data V = V !Bool Doc

vDoc :: V -> Doc
vDoc (V _ d) = d

-- | Constructor applied to arguments: @hang (text c) 2 (sep args)@,
-- where non-atomic arguments are parenthesized.
con :: Text -> [V] -> V
con c [] = V True (dtext c)
con c vs = V False $ group
  [ Elt 0 "" (dtext c) ""
  , Elt 2 "" (group (map atomElt vs)) "" ]
  where
    atomElt (V True d)  = Elt 0 "" d ""
    atomElt (V False d) = Elt 0 "(" d ")"

-- | Bracketed, comma-separated block: @sep [open <+> x1, ...commas..., close]@.
block :: Text -> Text -> Text -> [Doc] -> Doc
block open comma close ds =
  group $ zipWith (\pre d -> Elt 0 pre d "") (open : repeat comma) ds
          ++ [Elt 0 "" (dtext close) ""]

listV :: (a -> V) -> [a] -> V
listV _ [] = V True (dtext "[]")
listV f xs = V True $ block "[ " ", " "]" (map (vDoc . f) xs)

tupleV :: [V] -> V
tupleV vs = V True $ block "( " ", " ")" (map vDoc vs)

-- | Record: @hang (text c) 2 (block '{' '}' fields)@ where each field
-- is @hang (text name <+> char '=') 2 value@.  Records count as atoms.
recV :: Text -> [(Text, V)] -> V
recV c fields = V True $ group
  [ Elt 0 "" (dtext c) ""
  , Elt 2 "" (block "{ " ", " "}" (map fieldDoc fields)) "" ]
  where
    fieldDoc (name, v) = group
      [ Elt 0 "" (dtext (name <> " =")) ""
      , Elt 2 "" (vDoc v) "" ]

-- | Leaf rendered via 'show' (Text, Int, Double).  Values whose
-- representation starts with @-@ get parentheses in argument position.
showV :: Show a => a -> V
showV x = V (not ("-" `T.isPrefixOf` t)) (dtext t)
  where t = T.pack (show x)

-- | Nullary constructors of enumeration types (and Bool).
enumV :: Show a => a -> V
enumV = V True . dtext . T.pack . show

mapV :: (a -> V) -> M.Map Text a -> V
mapV f m = con "fromList"
  [listV (\(k, v) -> tupleV [showV k, f v]) (M.toAscList m)]

--
-- Conversion of the Pandoc AST
--

pandocV :: Pandoc -> V
pandocV (Pandoc meta blocks) = con "Pandoc" [metaV meta, blocksV blocks]

metaV :: Meta -> V
metaV (Meta m) = recV "Meta" [("unMeta", mapV metaValueV m)]

metaValueV :: MetaValue -> V
metaValueV (MetaMap m)       = con "MetaMap" [mapV metaValueV m]
metaValueV (MetaList xs)     = con "MetaList" [listV metaValueV xs]
metaValueV (MetaBool b)      = con "MetaBool" [enumV b]
metaValueV (MetaString t)    = con "MetaString" [showV t]
metaValueV (MetaInlines ils) = con "MetaInlines" [inlinesV ils]
metaValueV (MetaBlocks bs)   = con "MetaBlocks" [blocksV bs]

blocksV :: [Block] -> V
blocksV = listV blockV

inlinesV :: [Inline] -> V
inlinesV = listV inlineV

attrV :: Attr -> V
attrV (ident, classes, kvs) =
  tupleV [ showV ident
         , listV showV classes
         , listV (\(k, v) -> tupleV [showV k, showV v]) kvs ]

formatV :: Format -> V
formatV (Format f) = con "Format" [showV f]

blockV :: Block -> V
blockV blk =
  case blk of
    Plain ils -> con "Plain" [inlinesV ils]
    Para ils -> con "Para" [inlinesV ils]
    LineBlock ilss -> con "LineBlock" [listV inlinesV ilss]
    CodeBlock attr t -> con "CodeBlock" [attrV attr, showV t]
    RawBlock f t -> con "RawBlock" [formatV f, showV t]
    BlockQuote bs -> con "BlockQuote" [blocksV bs]
    OrderedList (start, sty, delim) bss ->
      con "OrderedList" [ tupleV [showV start, enumV sty, enumV delim]
                        , listV blocksV bss ]
    BulletList bss -> con "BulletList" [listV blocksV bss]
    DefinitionList defs ->
      con "DefinitionList"
        [listV (\(ils, bss) -> tupleV [inlinesV ils, listV blocksV bss]) defs]
    Header lev attr ils -> con "Header" [showV lev, attrV attr, inlinesV ils]
    HorizontalRule -> con "HorizontalRule" []
    Table attr cap colspecs thead tbodies tfoot ->
      con "Table" [ attrV attr
                  , captionV cap
                  , listV colSpecV colspecs
                  , tableHeadV thead
                  , listV tableBodyV tbodies
                  , tableFootV tfoot ]
    Figure attr cap bs -> con "Figure" [attrV attr, captionV cap, blocksV bs]
    Div attr bs -> con "Div" [attrV attr, blocksV bs]

captionV :: Caption -> V
captionV (Caption mshort bs) =
  con "Caption" [maybeV inlinesV mshort, blocksV bs]

maybeV :: (a -> V) -> Maybe a -> V
maybeV _ Nothing  = con "Nothing" []
maybeV f (Just x) = con "Just" [f x]

colSpecV :: ColSpec -> V
colSpecV (align, cw) = tupleV [enumV align, colWidthV cw]

colWidthV :: ColWidth -> V
colWidthV (ColWidth d)    = con "ColWidth" [showV d]
colWidthV ColWidthDefault = con "ColWidthDefault" []

tableHeadV :: TableHead -> V
tableHeadV (TableHead attr rows) =
  con "TableHead" [attrV attr, listV rowV rows]

tableBodyV :: TableBody -> V
tableBodyV (TableBody attr (RowHeadColumns rhc) hd bd) =
  con "TableBody" [ attrV attr
                  , con "RowHeadColumns" [showV rhc]
                  , listV rowV hd
                  , listV rowV bd ]

tableFootV :: TableFoot -> V
tableFootV (TableFoot attr rows) =
  con "TableFoot" [attrV attr, listV rowV rows]

rowV :: Row -> V
rowV (Row attr cells) = con "Row" [attrV attr, listV cellV cells]

cellV :: Cell -> V
cellV (Cell attr align (RowSpan rs) (ColSpan cs) bs) =
  con "Cell" [ attrV attr
             , enumV align
             , con "RowSpan" [showV rs]
             , con "ColSpan" [showV cs]
             , blocksV bs ]

inlineV :: Inline -> V
inlineV inln =
  case inln of
    Str t -> con "Str" [showV t]
    Emph ils -> con "Emph" [inlinesV ils]
    Underline ils -> con "Underline" [inlinesV ils]
    Strong ils -> con "Strong" [inlinesV ils]
    Strikeout ils -> con "Strikeout" [inlinesV ils]
    Superscript ils -> con "Superscript" [inlinesV ils]
    Subscript ils -> con "Subscript" [inlinesV ils]
    SmallCaps ils -> con "SmallCaps" [inlinesV ils]
    Quoted qt ils -> con "Quoted" [enumV qt, inlinesV ils]
    Cite cits ils -> con "Cite" [listV citationV cits, inlinesV ils]
    Code attr t -> con "Code" [attrV attr, showV t]
    Space -> con "Space" []
    SoftBreak -> con "SoftBreak" []
    LineBreak -> con "LineBreak" []
    Math mt t -> con "Math" [enumV mt, showV t]
    RawInline f t -> con "RawInline" [formatV f, showV t]
    Link attr ils (url, title) ->
      con "Link" [attrV attr, inlinesV ils, tupleV [showV url, showV title]]
    Image attr ils (url, title) ->
      con "Image" [attrV attr, inlinesV ils, tupleV [showV url, showV title]]
    Note bs -> con "Note" [blocksV bs]
    Span attr ils -> con "Span" [attrV attr, inlinesV ils]

citationV :: Citation -> V
citationV cit = recV "Citation"
  [ ("citationId", showV (citationId cit))
  , ("citationPrefix", inlinesV (citationPrefix cit))
  , ("citationSuffix", inlinesV (citationSuffix cit))
  , ("citationMode", enumV (citationMode cit))
  , ("citationNoteNum", showV (citationNoteNum cit))
  , ("citationHash", showV (citationHash cit)) ]

--
-- Rendering
--

-- | Render a document, reproducing HughesPJ's layout exactly.  A
-- group is rendered on one line if its width, plus any text glued
-- after it up to the next line break ('glue'), stays within both the
-- line length (measured from the start of the line) and the ribbon
-- length (measured from the end of the indentation).  Otherwise it is
-- rendered vertically, each element after the first starting on a new
-- line, indented by the group's start column plus the element's
-- nesting; each element then makes its own layout decisions.
render :: Int -> Int -> Int -> Doc -> Text
render lineLen ribbonLen glue0 d0 =
  TL.toStrict $ B.toLazyText $ go 0 0 glue0 d0
  where
    go l c g d@(Doc w dc)
      | c + w + g <= lineLen && (c - l) + w + g <= ribbonLen = flat d
      | otherwise =
          case dc of
            DText t   -> B.fromText t
            DGroup es -> vertical l c g es

    vertical l0 c0 g = goElts True
      where
        goElts _ [] = mempty
        goElts isFirst (Elt n pre d post : rest) =
          let ind  = c0 + n
              (l, c) = if isFirst then (l0, c0) else (ind, ind)
              g' = T.length post + (if null rest then g else 0)
              lead = if isFirst
                        then mempty
                        else B.singleton '\n' <> B.fromText (T.replicate ind " ")
          in  lead <> B.fromText pre <> go l (c + T.length pre) g' d
                   <> B.fromText post <> goElts False rest

    flat (Doc _ (DText t))   = B.fromText t
    flat (Doc _ (DGroup es)) =
      mconcat $ intersperse (B.singleton ' ') $ map flatElt es

    flatElt (Elt _ pre d post) =
      B.fromText pre <> flat d <> B.fromText post

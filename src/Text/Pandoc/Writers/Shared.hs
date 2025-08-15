{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- |
   Module      : Text.Pandoc.Writers.Shared
   Copyright   : Copyright (C) 2013-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Shared utility functions for pandoc writers.
-}
module Text.Pandoc.Writers.Shared (
                       metaToContext
                     , metaToContext'
                     , addVariablesToContext
                     , getField
                     , setField
                     , resetField
                     , defField
                     , getLang
                     , tagWithAttrs
                     , htmlAddStyle
                     , htmlAlignmentToString
                     , htmlAttrs
                     , isDisplayMath
                     , fixDisplayMath
                     , unsmartify
                     , gridTable
                     , lookupMetaBool
                     , lookupMetaBlocks
                     , lookupMetaInlines
                     , lookupMetaString
                     , stripLeadingTrailingSpace
                     , toSubscript
                     , toSuperscript
                     , toSubscriptInline
                     , toSuperscriptInline
                     , toTableOfContents
                     , endsWithPlain
                     , toLegacyTable
                     , splitSentences
                     , ensureValidXmlIdentifiers
                     , setupTranslations
                     , isOrderedListMarker
                     , toTaskListItem
                     , delimited
                     )
where
import Safe (lastMay, maximumMay)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (MonadPlus, mzero)
import Data.Either (isRight)
import Data.Aeson (ToJSON (..), encode)
import Data.Char (chr, ord, isSpace, isLetter, isUpper)
import Data.List (groupBy, intersperse, foldl', transpose)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Conversions (FromText(..))
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.CSS (cssAttributes)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing (runParser, eof, defaultParserState,
                            anyOrderedListMarker)
import Text.DocLayout
import Text.Pandoc.Shared (stringify, makeSections, blocksToInlines)
import Text.Pandoc.Walk (Walkable(..))
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.XML (escapeStringForXML)
import Text.DocTemplates (Context(..), Val(..), TemplateTarget,
                          ToContext(..), FromContext(..))
import Text.Pandoc.Chunks (tocToList, toTOCTree)
import Text.Collate.Lang (Lang (..))
import Text.Pandoc.Class (PandocMonad, toLang)
import Text.Pandoc.Translations (setTranslations)
import Data.Maybe (fromMaybe)
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann

-- import Debug.Trace

-- | Create template Context from a 'Meta' and an association list
-- of variables, specified at the command line or in the writer.
-- Variables overwrite metadata fields with the same names.
-- If multiple variables are set with the same name, a list is
-- assigned.  Does nothing if 'writerTemplate' is Nothing.
metaToContext :: (Monad m, TemplateTarget a)
              => WriterOptions
              -> ([Block] -> m (Doc a))
              -> ([Inline] -> m (Doc a))
              -> Meta
              -> m (Context a)
metaToContext opts blockWriter inlineWriter meta =
  case writerTemplate opts of
    Nothing -> return mempty
    Just _  -> addVariablesToContext opts <$>
                metaToContext' blockWriter inlineWriter meta

-- | Like 'metaToContext, but does not include variables and is
-- not sensitive to 'writerTemplate'.
metaToContext' :: (Monad m, TemplateTarget a)
           => ([Block] -> m (Doc a))     -- ^ block writer
           -> ([Inline] -> m (Doc a))    -- ^ inline writer
           -> Meta
           -> m (Context a)
metaToContext' blockWriter inlineWriter (Meta metamap) =
  Context <$> mapM (metaValueToVal blockWriter inlineWriter) metamap

-- | Add variables to a template Context, using monoidal append.
-- Also add `meta-json`.  Note that metadata values are used
-- in template contexts only when like-named variables aren't set.
addVariablesToContext :: TemplateTarget a
                      => WriterOptions -> Context a -> Context a
addVariablesToContext opts c1 =
  c2 <> (fromText <$> writerVariables opts) <> c1
 where
   c2 = Context $
          M.insert "meta-json" (SimpleVal $ literal $ fromText jsonrep)
                               mempty
   jsonrep = UTF8.toText $ BL.toStrict $ encode $ toJSON c1

-- | Converts a 'MetaValue' into a doctemplate 'Val', using the given
-- converter functions.
metaValueToVal :: (Monad m, TemplateTarget a)
               => ([Block] -> m (Doc a))    -- ^ block writer
               -> ([Inline] -> m (Doc a))   -- ^ inline writer
               -> MetaValue
               -> m (Val a)
metaValueToVal blockWriter inlineWriter (MetaMap metamap) =
  MapVal . Context <$> mapM (metaValueToVal blockWriter inlineWriter) metamap
metaValueToVal blockWriter inlineWriter (MetaList xs) = ListVal <$>
  mapM (metaValueToVal blockWriter inlineWriter) xs
metaValueToVal _ _ (MetaBool b) = return $ BoolVal b
metaValueToVal _ inlineWriter (MetaString s) =
   SimpleVal <$> inlineWriter (Builder.toList (Builder.text s))
metaValueToVal blockWriter _ (MetaBlocks bs) = SimpleVal <$> blockWriter bs
metaValueToVal _ inlineWriter (MetaInlines is) = SimpleVal <$> inlineWriter is


-- | Retrieve a field value from a template context.
getField   :: FromContext a b => Text -> Context a -> Maybe b
getField field (Context m) = M.lookup field m >>= fromVal

-- | Set a field of a template context.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
-- This is a utility function to be used in preparing template contexts.
setField   :: ToContext a b => Text -> b -> Context a -> Context a
setField field val (Context m) =
  Context $ M.insertWith combine field (toVal val) m
 where
  combine newval (ListVal xs)   = ListVal (xs ++ [newval])
  combine newval x              = ListVal [x, newval]

-- | Reset a field of a template context.  If the field already has a
-- value, the new value replaces it.
-- This is a utility function to be used in preparing template contexts.
resetField :: ToContext a b => Text -> b -> Context a -> Context a
resetField field val (Context m) =
  Context (M.insert field (toVal val) m)

-- | Set a field of a template context if it currently has no value.
-- If it has a value, do nothing.
-- This is a utility function to be used in preparing template contexts.
defField   :: ToContext a b => Text -> b -> Context a -> Context a
defField field val (Context m) =
  Context (M.insertWith f field (toVal val) m)
  where
    f _newval oldval = oldval

-- | Get the contents of the `lang` metadata field or variable.
getLang :: WriterOptions -> Meta -> Maybe Text
getLang opts meta =
  case lookupContext "lang" (writerVariables opts) of
        Just s -> Just s
        _      ->
          case lookupMeta "lang" meta of
               Just (MetaBlocks [Para [Str s]])  -> Just s
               Just (MetaBlocks [Plain [Str s]]) -> Just s
               Just (MetaInlines [Str s])        -> Just s
               Just (MetaString s)               -> Just s
               _                                 -> Nothing

-- | Produce an HTML tag with the given pandoc attributes.
tagWithAttrs :: HasChars a => a -> Attr -> Doc a
tagWithAttrs tag attr = "<" <> literal tag <> (htmlAttrs attr) <> ">"

-- | Produce HTML for the given pandoc attributes, to be used in HTML tags
htmlAttrs :: HasChars a => Attr -> Doc a
htmlAttrs (ident, classes, kvs) = addSpaceIfNotEmpty (hsep [
  if T.null ident
      then empty
      else "id=" <> doubleQuotes (text $ T.unpack ident)
  ,if null classes
      then empty
      else "class=" <> doubleQuotes (text $ T.unpack (T.unwords classes))
  ,hsep (map (\(k,v) -> text (T.unpack k) <> "=" <>
                doubleQuotes (text $ T.unpack (escapeStringForXML v))) kvs)
  ])

addSpaceIfNotEmpty :: HasChars a => Doc a -> Doc a
addSpaceIfNotEmpty f = if isEmpty f then f else " " <> f

-- | Adds a key-value pair to the @style@ attribute.
htmlAddStyle :: (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
htmlAddStyle (key, value) kvs =
  let cssToStyle = T.intercalate " " . map (\(k, v) -> k <> ": " <> v <> ";")
  in case break ((== "style") . fst) kvs of
    (_, []) ->
      -- no style attribute yet, add new one
      ("style", cssToStyle [(key, value)]) : kvs
    (xs, (_,cssStyles):rest) ->
      -- modify the style attribute
      xs ++ ("style", cssToStyle modifiedCssStyles) : rest
      where
        modifiedCssStyles =
          case break ((== key) . fst) $ cssAttributes cssStyles of
            (cssAttribs, []) -> (key, value) : cssAttribs
            (pre, _:post)    -> pre ++ (key, value) : post

-- | Get the html representation of an alignment key
htmlAlignmentToString :: Alignment -> Maybe Text
htmlAlignmentToString = \case
  AlignLeft    -> Just "left"
  AlignRight   -> Just "right"
  AlignCenter  -> Just "center"
  AlignDefault -> Nothing

-- | Returns 'True' iff the argument is an inline 'Math' element of type
-- 'DisplayMath'.
isDisplayMath :: Inline -> Bool
isDisplayMath (Math DisplayMath _)          = True
isDisplayMath (Span _ [Math DisplayMath _]) = True
isDisplayMath _                             = False

-- | Remove leading and trailing 'Space' and 'SoftBreak' elements.
stripLeadingTrailingSpace :: [Inline] -> [Inline]
stripLeadingTrailingSpace = go . reverse . go . reverse
  where go (Space:xs)     = xs
        go (SoftBreak:xs) = xs
        go xs             = xs

-- | Put display math in its own block (for ODT/DOCX).
fixDisplayMath :: Block -> Block
fixDisplayMath (Plain lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $
       map Plain $
       filter (not . null) $
       map stripLeadingTrailingSpace $
       groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
fixDisplayMath (Para lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $
       map Para $
       filter (not . null) $
       map stripLeadingTrailingSpace $
       groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
fixDisplayMath x = x

-- | Converts a Unicode character into the ASCII sequence used to
-- represent the character in "smart" Markdown.
unsmartify :: WriterOptions -> Text -> Text
unsmartify opts = T.concatMap $ \c -> case c of
  '\8217' -> "'"
  '\8230' -> "..."
  '\8211'
    | isEnabled Ext_old_dashes opts -> "-"
    | otherwise                     -> "--"
  '\8212'
    | isEnabled Ext_old_dashes opts -> "--"
    | otherwise                     -> "---"
  '\8220' -> "\""
  '\8221' -> "\""
  '\8216' -> "'"
  _       -> T.singleton c

-- | Writes a grid table.
gridTable :: Monad m
           => WriterOptions
           -> (WriterOptions -> [Block] -> m (Doc Text)) -- ^ format Doc writer
           -> [ColSpec]
           -> TableHead
           -> [TableBody]
           -> TableFoot
           -> m (Doc Text)
gridTable opts blocksToDoc colspecs' thead' tbodies' tfoot' = do
  let Ann.Table _ _ colspecs thead tbodies tfoot =
        Ann.toTable mempty (Caption Nothing mempty)
                    colspecs' thead' tbodies' tfoot'
  let renderRows = fmap addDummies . mapM (gridRow opts blocksToDoc)
  let getHeadCells (Ann.HeaderRow _ _ cells) = cells
  let getHeadRows (Ann.TableHead _ rs) = map getHeadCells rs
  headCells <- renderRows (getHeadRows thead)
  let getFootRows (Ann.TableFoot _ xs) = map getHeadCells xs
  footCells <- renderRows (getFootRows tfoot)
  -- We don't distinguish between row head and regular cells here:
  let getBodyCells (Ann.BodyRow _ _ rhcells cells) = rhcells ++ cells
  let getBody (Ann.TableBody _ _ hs xs) = map getHeadCells hs <> map getBodyCells xs
  bodyCells <- mapM (renderRows . getBody) tbodies
  let rows = (setTopBorder SingleLine . setBottomBorder DoubleHeaderLine) headCells ++
             (setTopBorder (if null headCells
                               then SingleHeaderLine
                               else SingleLine) . setBottomBorder SingleLine)
                   (mconcat bodyCells) ++
             (setTopBorder DoubleLine . setBottomBorder DoubleLine) footCells
  pure $ gridRows $ redoWidths opts colspecs rows

-- Returns (current widths, full widths, min widths)
extractColWidths :: WriterOptions -> [[RenderedCell Text]] -> ([Int], [Int], [Int])
extractColWidths opts rows = (currentwidths, fullwidths, minwidths)
 where
   getWidths calcOffset =
     map (fromMaybe 0 . maximumMay) (transpose (map (concatMap (getCellWidths calcOffset)) rows))
   getCellWidths calcOffset c = replicate (cellColSpan c)
                                 (calcOffset c `div` (cellColSpan c) +
                                  calcOffset c `rem` (cellColSpan c))
   fullwidths = getWidths (max 1 . offset . cellContents)
   currentwidths = getWidths cellWidth
   minwidths =
     case writerWrapText opts of
       WrapNone -> fullwidths
       _ -> getWidths (minOffset . cellContents)

resetWidths :: [Int] -> [RenderedCell Text] -> [RenderedCell Text]
resetWidths _ [] = []
resetWidths [] cs = cs
resetWidths (w:ws) (c:cs) =
  case cellColSpan c of
    1 -> c{ cellWidth = w } : resetWidths ws cs
    n | n < 1 -> c : resetWidths ws cs
      | otherwise -> c{ cellWidth = w + sum (take (n - 1) ws) + (3 * (n-1)) }
                               : resetWidths (drop (n - 1) ws) cs

redoWidths :: WriterOptions -> [ColSpec] -> [[RenderedCell Text]] -> [[RenderedCell Text]]
redoWidths _ _ [] = []
redoWidths opts colspecs rows = map (resetWidths newwidths) rows
 where
  numcols = length colspecs
  isSimple = all ((== ColWidthDefault) . snd) colspecs
  (actualwidths, fullwidths, minwidths) = extractColWidths opts rows
  totwidth = writerColumns opts - (3 * numcols) - 1
  evenwidth = totwidth `div` numcols + totwidth `rem` numcols
  keepwidths = filter (< evenwidth) fullwidths
  evenwidth' = (totwidth - sum keepwidths) `div`
                (numcols - length keepwidths)
  ensureMinWidths = zipWith max minwidths
  newwidths = ensureMinWidths $
              case isSimple of
                True | sum fullwidths <= totwidth -> fullwidths
                     | otherwise -> map (\w -> if w < evenwidth
                                                  then w
                                                  else evenwidth') fullwidths
                False -> actualwidths

makeDummy :: RenderedCell Text -> RenderedCell Text
makeDummy c =
    RenderedCell{ cellColNum = cellColNum c,
                  cellColSpan = cellColSpan c,
                  cellColSpecs = cellColSpecs c,
                  cellAlign = AlignDefault,
                  cellRowSpan = cellRowSpan c - 1,
                  cellWidth = cellWidth c,
                  cellContents = mempty,
                  cellBottomBorder = NoLine,
                  cellTopBorder = NoLine }

addDummies :: [[RenderedCell Text]] -> [[RenderedCell Text]]
addDummies = reverse . foldl' go []
 where
   go [] cs = [cs]
   go (prevRow:rs) cs = addDummiesToRow prevRow cs : prevRow : rs
   addDummiesToRow [] cs = cs
   addDummiesToRow ds [] = map makeDummy ds
   addDummiesToRow (d:ds) (c:cs) =
     if cellColNum d < cellColNum c
        then makeDummy d : addDummiesToRow ds (c:cs)
        else c : addDummiesToRow
                   (dropWhile (\x ->
                       cellColNum x < cellColNum c + cellColSpan c) (d:ds))
                   cs


setTopBorder :: LineStyle -> [[RenderedCell Text]] -> [[RenderedCell Text]]
setTopBorder _ [] = []
setTopBorder sty (cs:rest) = (map (\c -> c{ cellTopBorder = sty }) cs) : rest

setBottomBorder :: LineStyle -> [[RenderedCell Text]] -> [[RenderedCell Text]]
setBottomBorder _ [] = []
setBottomBorder sty [cs] = [map (\c -> c{ cellBottomBorder = sty }) cs]
setBottomBorder sty (c:cs) = c : setBottomBorder sty cs

gridRows :: [[RenderedCell Text]] -> Doc Text
gridRows [] = mempty
gridRows (x:xs) =
  (case x of
     [] -> mempty
     (c:_) | isHeaderStyle (cellTopBorder c)
            -> formatHeaderLine (cellTopBorder c) (x:xs)
           | otherwise
            -> formatBorder cellTopBorder False x)
  $$
  vcat (zipWith (rowAndBottom (x:xs)) (x:xs) (xs ++ [[]]))
 where
  -- generate wrapped contents. include pipe borders, bottom and left

  renderCellContents c =
    -- we don't use cblock or lblock because the content might
    -- be interpreted as an indented code block...even though it
    -- would look better to right-align right-aligned cells...
    -- (TODO: change this on parsing side?)
    lblock (cellWidth c) (cellContents c)

  formatRow cs = vfill "| " <>
   hcat (intersperse (vfill " | ") (map renderCellContents cs)) <> vfill " |"

  rowAndBottom allRows thisRow nextRow =
    let isLastRow = null nextRow
        border1 = case thisRow of
                     [] -> mempty
                     (c:_) -> if isHeaderStyle (cellBottomBorder c)
                                 then formatHeaderLine (cellBottomBorder c) allRows
                                 else formatBorder cellBottomBorder False thisRow
        border2 = case nextRow of
                     [] -> mempty
                     (c:_) -> if isHeaderStyle (cellTopBorder c)
                                 then formatHeaderLine (cellTopBorder c) allRows
                                 else formatBorder cellTopBorder False nextRow
        combinedBorder = if isLastRow
                            then border1
                            else literal $ combineBorders
                                  (render Nothing border1) (render Nothing border2)
    in formatRow thisRow $$ combinedBorder

combineBorders :: Text -> Text -> Text
combineBorders t1 t2 =
  if T.null t1
     then t2
     else T.zipWith go t1 t2
 where
   go '+' _ = '+'
   go _ '+' = '+'
   go ':' _ = ':'
   go _ ':' = ':'
   go '|' '-' = '+'
   go '-' '|' = '+'
   go '|' '=' = '+'
   go '=' '|' = '+'
   go '=' _ = '='
   go _ '=' = '='
   go ' ' d = d
   go c _   = c

formatHeaderLine :: Show a => LineStyle -> [[RenderedCell a]] -> Doc Text
formatHeaderLine lineStyle rows =
  literal $ foldl'
    (\t row -> combineBorders t (render Nothing $ formatBorder (const lineStyle) True row))
    mempty rows

formatBorder :: Show a => (RenderedCell a -> LineStyle) -> Bool
             -> [RenderedCell a] -> Doc Text
formatBorder borderStyle alignMarkers cs =
  borderParts <> if lastBorderStyle == NoLine
                            then char '|'
                            else char '+'
 where
   (lastBorderStyle, borderParts) = foldl' addBorder (NoLine, mempty) cs
   addBorder (prevBorderStyle, accum) c =
     (borderStyle c, accum <> char junctionChar <> toBorderSection c)
      where junctionChar = case (borderStyle c, prevBorderStyle) of
                               (NoLine, NoLine) -> '|'
                               _ -> '+'
   toBorderSection c =
       text $ leftalign : replicate (cellWidth c) lineChar ++ [rightalign]
     where
       lineChar = case borderStyle c of
                     NoLine -> ' '
                     SingleLine -> '-'
                     SingleHeaderLine -> '-'
                     DoubleLine -> '='
                     DoubleHeaderLine -> '='
       (leftalign, rightalign) =
           case cellAlign c of
             _ | not alignMarkers -> (lineChar,lineChar)
             AlignLeft -> (':',lineChar)
             AlignCenter -> (':',':')
             AlignRight -> (lineChar,':')
             AlignDefault -> (lineChar,lineChar)

data LineStyle = NoLine
               | SingleLine
               | DoubleLine
               | SingleHeaderLine
               | DoubleHeaderLine
    deriving (Show, Ord, Eq)

isHeaderStyle :: LineStyle -> Bool
isHeaderStyle SingleHeaderLine = True
isHeaderStyle DoubleHeaderLine = True
isHeaderStyle _ = False

data RenderedCell a =
  RenderedCell{ cellColNum :: Int
              , cellColSpan :: Int
              , cellColSpecs :: NonEmpty ColSpec
              , cellAlign :: Alignment
              , cellRowSpan :: Int
              , cellWidth :: Int
              , cellContents :: Doc a
              , cellBottomBorder :: LineStyle
              , cellTopBorder :: LineStyle
              }
  deriving (Show)

getColWidth :: ColSpec -> Double
getColWidth (_, ColWidth n) = n
getColWidth (_, ColWidthDefault) = 0 -- TODO?

toCharWidth :: WriterOptions -> Double -> Int
toCharWidth opts width =
  max 1 (floor (width * fromIntegral (writerColumns opts)) - 3)

gridRow :: (Monad m, HasChars a)
        => WriterOptions
        -> (WriterOptions -> [Block] -> m (Doc a)) -- ^ format Doc writer
        -> [Ann.Cell]
        -> m [RenderedCell a]
gridRow opts blocksToDoc = mapM renderCell
 where
  renderer = blocksToDoc opts
  renderCell (Ann.Cell cellcolspecs (Ann.ColNumber colnum)
               (Cell _ _ (RowSpan rowspan) _ blocks)) = do
    let ((align,_):|_) = cellcolspecs
    let width = toCharWidth opts $ sum (fmap getColWidth cellcolspecs)
    rendered <- renderer blocks
    pure $ RenderedCell{ cellColNum = colnum,
                         cellColSpan = length cellcolspecs,
                         cellColSpecs = cellcolspecs,
                         cellAlign = align,
                         cellRowSpan = rowspan,
                         cellWidth = width,
                         cellContents = rendered,
                         cellBottomBorder = if rowspan < 2
                                               then SingleLine
                                               else NoLine,
                         cellTopBorder = SingleLine }


-- | Retrieve the metadata value for a given @key@
-- and convert to Bool.
lookupMetaBool :: Text -> Meta -> Bool
lookupMetaBool key meta =
  case lookupMeta key meta of
      Just (MetaBlocks _)  -> True
      Just (MetaInlines _) -> True
      Just (MetaString x)  -> not (T.null x)
      Just (MetaBool True) -> True
      _                    -> False

-- | Retrieve the metadata value for a given @key@
-- and extract blocks.
--
-- Note that an empty list is returned for maps, lists, and booleans.
lookupMetaBlocks :: Text -> Meta -> [Block]
lookupMetaBlocks key meta =
  case lookupMeta key meta of
         Just (MetaBlocks bs)   -> bs
         Just (MetaInlines ils) -> [Plain ils]
         Just (MetaString s)    -> [Plain [Str s]]
         _                      -> []

-- | Retrieve the metadata value for a given @key@
-- and extract inlines.
--
-- Note that an empty list is returned for maps and lists.
lookupMetaInlines :: Text -> Meta -> [Inline]
lookupMetaInlines key meta =
  case lookupMeta key meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Retrieve the metadata value for a given @key@
-- and convert to String.
--
-- Note that an empty list is returned for maps, lists, and booleans.
lookupMetaString :: Text -> Meta -> Text
lookupMetaString key meta =
  case lookupMeta key meta of
         Just (MetaString s)    -> s
         Just (MetaInlines ils) -> stringify ils
         Just (MetaBlocks bs)   -> stringify bs
         Just (MetaBool b)      -> T.pack (show b)
         _                      -> ""

-- | Tries to convert a character into a unicode superscript version of
-- the character.
toSuperscript :: Char -> Maybe Char
toSuperscript '1' = Just '\x00B9'
toSuperscript '2' = Just '\x00B2'
toSuperscript '3' = Just '\x00B3'
toSuperscript '+' = Just '\x207A'
toSuperscript '-' = Just '\x207B'
toSuperscript '\x2212' = Just '\x207B' -- unicode minus
toSuperscript '=' = Just '\x207C'
toSuperscript '(' = Just '\x207D'
toSuperscript ')' = Just '\x207E'
toSuperscript c
  | c >= '0' && c <= '9' =
                 Just $ chr (0x2070 + (ord c - 48))
  | isSpace c = Just c
  | otherwise = Nothing

-- | Tries to convert a character into a unicode subscript version of
-- the character.
toSubscript :: Char -> Maybe Char
toSubscript '+' = Just '\x208A'
toSubscript '-' = Just '\x208B'
toSubscript '=' = Just '\x208C'
toSubscript '(' = Just '\x208D'
toSubscript ')' = Just '\x208E'
toSubscript c
  | c >= '0' && c <= '9' =
                 Just $ chr (0x2080 + (ord c - 48))
  | isSpace c = Just c
  | otherwise = Nothing

toSubscriptInline :: Inline -> Maybe Inline
toSubscriptInline Space = Just Space
toSubscriptInline (Span attr ils) = Span attr <$> traverse toSubscriptInline ils
toSubscriptInline (Str s) = Str . T.pack <$> traverse toSubscript (T.unpack s)
toSubscriptInline LineBreak = Just LineBreak
toSubscriptInline SoftBreak = Just SoftBreak
toSubscriptInline _ = Nothing

toSuperscriptInline :: Inline -> Maybe Inline
toSuperscriptInline Space = Just Space
toSuperscriptInline (Span attr ils) = Span attr <$> traverse toSuperscriptInline ils
toSuperscriptInline (Str s) = Str . T.pack <$> traverse toSuperscript (T.unpack s)
toSuperscriptInline LineBreak = Just LineBreak
toSuperscriptInline SoftBreak = Just SoftBreak
toSuperscriptInline _ = Nothing

-- | Construct table of contents (as a bullet list) from document body.
toTableOfContents :: WriterOptions
                  -> [Block]
                  -> Block
toTableOfContents opts =
  tocToList (writerNumberSections opts) (writerTOCDepth opts)
  . toTOCTree
  . makeSections (writerNumberSections opts) Nothing

-- | Returns 'True' iff the list of blocks has a @'Plain'@ as its last
-- element.
endsWithPlain :: [Block] -> Bool
endsWithPlain xs =
  case lastMay xs of
    Just Plain{} -> True
    Just (BulletList is) -> maybe False endsWithPlain (lastMay is)
    Just (OrderedList _ is) -> maybe False endsWithPlain (lastMay is)
    _ -> False

-- | Convert the relevant components of a new-style table (with block
-- caption, row headers, row and column spans, and so on) to those of
-- an old-style table (inline caption, table head with one row, no
-- foot, and so on). Cells with a 'RowSpan' and 'ColSpan' of @(h, w)@
-- will be cut up into @h * w@ cells of dimension @(1, 1)@, with the
-- content placed in the upper-left corner.
toLegacyTable :: Caption
              -> [ColSpec]
              -> TableHead
              -> [TableBody]
              -> TableFoot
              -> ([Inline], [Alignment], [Double], [[Block]], [[[Block]]])
toLegacyTable (Caption _ cbody) specs thead tbodies tfoot
  = (cbody', aligns, widths, th', tb')
  where
    numcols = length specs
    (aligns, mwidths) = unzip specs
    fromWidth (ColWidth w) | w > 0 = w
    fromWidth _                    = 0
    widths = map fromWidth mwidths
    unRow (Row _ x) = x
    unBody (TableBody _ _ hd bd) = hd <> bd
    unBodies = concatMap unBody

    TableHead _ th = Builder.normalizeTableHead numcols thead
    tb = map (Builder.normalizeTableBody numcols) tbodies
    TableFoot _ tf = Builder.normalizeTableFoot numcols tfoot

    cbody' = blocksToInlines cbody

    (th', tb') = case th of
      r:rs -> let (pendingPieces, r') = placeCutCells [] $ unRow r
                  rs' = cutRows pendingPieces $ rs <> unBodies tb <> tf
              in (r', rs')
      []    -> ([], cutRows [] $ unBodies tb <> tf)

    -- Adapted from placeRowSection in Builders. There is probably a
    -- more abstract foldRowSection that unifies them both.
    placeCutCells pendingPieces cells
      -- If there are any pending pieces for a column, add
      -- them. Pending pieces have preference over cells due to grid
      -- layout rules.
      | (p:ps):pendingPieces' <- pendingPieces
      = let (pendingPieces'', rowPieces) = placeCutCells pendingPieces' cells
        in (ps : pendingPieces'', p : rowPieces)
      -- Otherwise cut up a cell on the row and deal with its pieces.
      | c:cells' <- cells
      = let (h, w, cBody) = getComponents c
            cRowPieces = cBody : replicate (w - 1) mempty
            cPendingPieces = replicate w $ replicate (h - 1) mempty
            pendingPieces' = drop w pendingPieces
            (pendingPieces'', rowPieces) = placeCutCells pendingPieces' cells'
        in (cPendingPieces <> pendingPieces'', cRowPieces <> rowPieces)
      | otherwise = ([], [])

    cutRows pendingPieces (r:rs)
      = let (pendingPieces', r') = placeCutCells pendingPieces $ unRow r
            rs' = cutRows pendingPieces' rs
        in r' : rs'
    cutRows _ [] = []

    getComponents (Cell _ _ (RowSpan h) (ColSpan w) body)
      = (h, w, body)

splitSentences :: Doc Text -> Doc Text
splitSentences = go . toList
 where
  go [] = mempty
  go (Text len t : AfterBreak _ : BreakingSpace : xs)
    | isSentenceEnding t = Text len t <> NewLine <> go xs
  go (Text len t : BreakingSpace : xs)
    | isSentenceEnding t = Text len t <> NewLine <> go xs
  go (x:xs) = x <> go xs

  toList (Concat (Concat a b) c) = toList (Concat a (Concat b c))
  toList (Concat a b) = a : toList b
  toList x = [x]

  isSentenceEnding t =
    case T.unsnoc t of
      Just (t',c)
        | c == '.' || c == '!' || c == '?'
        , not (isInitial t') -> True
        | c == ')' || c == ']' || c == '"' || c == '\x201D' ->
           case T.unsnoc t' of
             Just (t'',d) -> d == '.' || d == '!' || d == '?' &&
                             not (isInitial t'')
             _ -> False
      _ -> False
   where
    isInitial x = T.length x == 1 && T.all isUpper x

-- | Ensure that all identifiers start with a letter,
-- and modify internal links accordingly. (Yes, XML allows an
-- underscore, but HTML 4 doesn't, so we are more conservative.)
ensureValidXmlIdentifiers :: Pandoc -> Pandoc
ensureValidXmlIdentifiers = walk fixLinks . walkAttr fixIdentifiers
 where
  fixIdentifiers (ident, classes, kvs) =
    (case T.uncons ident of
      Nothing -> ident
      Just (c, _) | isLetter c -> ident
      _ -> "id_" <> ident,
     classes, kvs)
  needsFixing src =
    case T.uncons src of
      Just ('#',t) ->
        case T.uncons t of
          Just (c,_) | not (isLetter c) -> Just ("#id_" <> t)
          _ -> Nothing
      _ -> Nothing
  fixLinks (Link attr ils (src, tit))
    | Just src' <- needsFixing src = Link attr ils (src', tit)
  fixLinks (Image attr ils (src, tit))
    | Just src' <- needsFixing src = Image attr ils (src', tit)
  fixLinks x = x

-- | Walk Pandoc document, modifying attributes.
walkAttr :: (Attr -> Attr) -> Pandoc -> Pandoc
walkAttr f = walk goInline . walk goBlock
 where
  goInline (Span attr ils) = Span (f attr) ils
  goInline (Link attr ils target) = Link (f attr) ils target
  goInline (Image attr ils target) = Image (f attr) ils target
  goInline (Code attr txt) = Code (f attr) txt
  goInline x = x

  goBlock (Header lev attr ils) = Header lev (f attr) ils
  goBlock (CodeBlock attr txt) = CodeBlock (f attr) txt
  goBlock (Table attr cap colspecs thead tbodies tfoot) =
    Table (f attr) cap colspecs thead tbodies tfoot
  goBlock (Div attr bs) = Div (f attr) bs
  goBlock x = x

-- | Set translations based on the `lang` in metadata.
setupTranslations :: PandocMonad m => Meta -> m ()
setupTranslations meta = do
  let defLang = Lang "en" (Just "US") Nothing [] [] []
  lang <- case lookupMetaString "lang" meta of
            "" -> pure defLang
            s  -> fromMaybe defLang <$> toLang (Just s)
  setTranslations lang

-- True if the string would count as a Markdown ordered list marker.
isOrderedListMarker :: Text -> Bool
isOrderedListMarker xs = not (T.null xs) && (T.last xs `elem` ['.',')']) &&
              isRight (runParser (anyOrderedListMarker >> eof)
                       defaultParserState "" xs)

toTaskListItem :: MonadPlus m => [Block] -> m (Bool, [Block])
toTaskListItem (Plain (Str "☐":Space:ils):xs) = pure (False, Plain ils:xs)
toTaskListItem (Plain (Str "☒":Space:ils):xs) = pure (True, Plain ils:xs)
toTaskListItem (Para  (Str "☐":Space:ils):xs) = pure (False, Para ils:xs)
toTaskListItem (Para  (Str "☒":Space:ils):xs) = pure (True, Para ils:xs)
toTaskListItem _                              = mzero

-- | Add an opener and closer to a Doc. If the Doc begins or ends
-- with whitespace, export this outside the opener or closer.
-- This is used for formats, like Markdown, which don't allow spaces
-- after opening or before closing delimiters.
delimited :: Doc Text -> Doc Text -> Doc Text -> Doc Text
delimited opener closer content =
  mconcat initialWS <> opener <> mconcat middle <> closer <> mconcat finalWS
 where
  contents = toList content
  (initialWS, rest) = span isWS contents
  (reverseFinalWS, reverseMiddle) = span isWS (reverse rest)
  finalWS = reverse reverseFinalWS
  middle = reverse reverseMiddle
  isWS NewLine = True
  isWS CarriageReturn = True
  isWS BreakingSpace = True
  isWS BlankLines{} = True
  isWS _ = False
  toList (Concat (Concat a b) c) = toList (Concat a (Concat b c))
  toList (Concat a b) = a : toList b
  toList x = [x]

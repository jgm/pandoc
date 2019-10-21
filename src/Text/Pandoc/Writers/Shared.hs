{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Shared
   Copyright   : Copyright (C) 2013-2019 John MacFarlane
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
                     , tagWithAttrs
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
                     , toTableOfContents
                     , endsWithPlain
                     )
where
import Prelude
import Safe (lastMay)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (zipWithM)
import Data.Aeson (ToJSON (..), encode)
import Data.Char (chr, ord, isSpace)
import Data.List (groupBy, intersperse, transpose, foldl')
import Data.Text.Conversions (FromText(..))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.DocLayout
import Text.Pandoc.Shared (stringify, makeSections, deNote, deLink)
import Text.Pandoc.Walk (walk)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.XML (escapeStringForXML)
import Text.DocTemplates (Context(..), Val(..), TemplateTarget,
                          ToContext(..), FromContext(..))

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
           => ([Block] -> m (Doc a))
           -> ([Inline] -> m (Doc a))
           -> Meta
           -> m (Context a)
metaToContext' blockWriter inlineWriter (Meta metamap) = do
  renderedMap <- mapM (metaValueToVal blockWriter inlineWriter) metamap
  return $ Context
         $ M.foldrWithKey (\k v x -> M.insert (T.pack k) v x) mempty
         $ renderedMap

-- | Add variables to a template Context, replacing any existing values.
addVariablesToContext :: TemplateTarget a
                      => WriterOptions -> Context a -> Context a
addVariablesToContext opts (Context m1) =
  Context (m1 `M.union` m2 `M.union` m3)
 where
   m2 = case traverse go (writerVariables opts) of
                  Just (Context x) -> x
                  Nothing -> mempty
   m3 = M.insert "meta-json" (SimpleVal $ literal $ fromText jsonrep)
                             mempty
   go = Just . fromText
   jsonrep = UTF8.toText $ BL.toStrict $ encode $ toJSON m1

metaValueToVal :: (Monad m, TemplateTarget a)
               => ([Block] -> m (Doc a))
               -> ([Inline] -> m (Doc a))
               -> MetaValue
               -> m (Val a)
metaValueToVal blockWriter inlineWriter (MetaMap metamap) =
  MapVal . Context . M.mapKeys T.pack  <$>
    mapM (metaValueToVal blockWriter inlineWriter) metamap
metaValueToVal blockWriter inlineWriter (MetaList xs) = ListVal <$>
  mapM (metaValueToVal blockWriter inlineWriter) xs
metaValueToVal _ _ (MetaBool True) = return $ SimpleVal "true"
metaValueToVal _ _ (MetaBool False) = return NullVal
metaValueToVal _ inlineWriter (MetaString s) =
   SimpleVal <$> inlineWriter (Builder.toList (Builder.text s))
metaValueToVal blockWriter _ (MetaBlocks bs) = SimpleVal <$> blockWriter bs
metaValueToVal _ inlineWriter (MetaInlines is) = SimpleVal <$> inlineWriter is


-- | Retrieve a field value from a template context.
getField   :: FromContext a b => String -> Context a -> Maybe b
getField field (Context m) = M.lookup (T.pack field) m >>= fromVal

-- | Set a field of a template context.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
-- This is a utility function to be used in preparing template contexts.
setField   :: ToContext a b => String -> b -> Context a -> Context a
setField field val (Context m) =
  Context $ M.insertWith combine (T.pack field) (toVal val) m
 where
  combine newval (ListVal xs)   = ListVal (xs ++ [newval])
  combine newval x              = ListVal [x, newval]

-- | Reset a field of a template context.  If the field already has a
-- value, the new value replaces it.
-- This is a utility function to be used in preparing template contexts.
resetField :: ToContext a b => String -> b -> Context a -> Context a
resetField field val (Context m) =
  Context (M.insert (T.pack field) (toVal val) m)

-- | Set a field of a template context if it currently has no value.
-- If it has a value, do nothing.
-- This is a utility function to be used in preparing template contexts.
defField   :: ToContext a b => String -> b -> Context a -> Context a
defField field val (Context m) =
  Context (M.insertWith f (T.pack field) (toVal val) m)
  where
    f _newval oldval = oldval

-- Produce an HTML tag with the given pandoc attributes.
tagWithAttrs :: HasChars a => String -> Attr -> Doc a
tagWithAttrs tag (ident,classes,kvs) = hsep
  ["<" <> text tag
  ,if null ident
      then empty
      else "id=" <> doubleQuotes (text ident)
  ,if null classes
      then empty
      else "class=" <> doubleQuotes (text (unwords classes))
  ,hsep (map (\(k,v) -> text k <> "=" <>
                doubleQuotes (text (escapeStringForXML v))) kvs)
  ] <> ">"

isDisplayMath :: Inline -> Bool
isDisplayMath (Math DisplayMath _)          = True
isDisplayMath (Span _ [Math DisplayMath _]) = True
isDisplayMath _                             = False

stripLeadingTrailingSpace :: [Inline] -> [Inline]
stripLeadingTrailingSpace = go . reverse . go . reverse
  where go (Space:xs)     = xs
        go (SoftBreak:xs) = xs
        go xs             = xs

-- Put display math in its own block (for ODT/DOCX).
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

unsmartify :: WriterOptions -> String -> String
unsmartify opts ('\8217':xs) = '\'' : unsmartify opts xs
unsmartify opts ('\8230':xs) = "..." ++ unsmartify opts xs
unsmartify opts ('\8211':xs)
  | isEnabled Ext_old_dashes opts = '-' : unsmartify opts xs
  | otherwise                     = "--" ++ unsmartify opts xs
unsmartify opts ('\8212':xs)
  | isEnabled Ext_old_dashes opts = "--" ++ unsmartify opts xs
  | otherwise                     = "---" ++ unsmartify opts xs
unsmartify opts ('\8220':xs) = '"' : unsmartify opts xs
unsmartify opts ('\8221':xs) = '"' : unsmartify opts xs
unsmartify opts ('\8216':xs) = '\'' : unsmartify opts xs
unsmartify opts (x:xs) = x : unsmartify opts xs
unsmartify _ [] = []

gridTable :: (Monad m, HasChars a)
          => WriterOptions
          -> (WriterOptions -> [Block] -> m (Doc a))
          -> Bool -- ^ headless
          -> [Alignment]
          -> [Double]
          -> [[Block]]
          -> [[[Block]]]
          -> m (Doc a)
gridTable opts blocksToDoc headless aligns widths headers rows = do
  -- the number of columns will be used in case of even widths
  let numcols = maximum (length aligns : length widths :
                           map length (headers:rows))
  let officialWidthsInChars widths' = map (
                        (\x -> if x < 1 then 1 else x) .
                        (\x -> x - 3) . floor .
                        (fromIntegral (writerColumns opts) *)
                        ) widths'
  -- handleGivenWidths wraps the given blocks in order for them to fit
  -- in cells with given widths. the returned content can be
  -- concatenated with borders and frames
  let handleGivenWidths widths' = do
        let widthsInChars' = officialWidthsInChars widths'
        -- replace page width (in columns) in the options with a
        -- given width if smaller (adjusting by two)
        let useWidth w = opts{writerColumns = min (w - 2) (writerColumns opts)}
        -- prepare options to use with header and row cells
        let columnOptions = map useWidth widthsInChars'
        rawHeaders' <- zipWithM blocksToDoc columnOptions headers
        rawRows' <- mapM
             (\cs -> zipWithM blocksToDoc columnOptions cs)
             rows
        return (widthsInChars', rawHeaders', rawRows')
  -- handleFullWidths tries to wrap cells to the page width or even
  -- more in cases where `--wrap=none`. thus the content here is left
  -- as wide as possible
  let handleFullWidths widths' = do
        rawHeaders' <- mapM (blocksToDoc opts) headers
        rawRows' <- mapM (mapM (blocksToDoc opts)) rows
        let numChars [] = 0
            numChars xs = maximum . map offset $ xs
        let minWidthsInChars =
                map numChars $ transpose (rawHeaders' : rawRows')
        let widthsInChars' = zipWith max
                              minWidthsInChars
                              (officialWidthsInChars widths')
        return (widthsInChars', rawHeaders', rawRows')
  -- handleZeroWidths calls handleFullWidths to check whether a wide
  -- table would fit in the page. if the produced table is too wide,
  -- it calculates even widths and passes the content to
  -- handleGivenWidths
  let handleZeroWidths widths' = do
        (widthsInChars', rawHeaders', rawRows') <- handleFullWidths widths'
        if foldl' (+) 0 widthsInChars' > writerColumns opts
           then -- use even widths
                handleGivenWidths
                  (replicate numcols (1.0 / fromIntegral numcols) :: [Double])
           else return (widthsInChars', rawHeaders', rawRows')
  -- render the contents of header and row cells differently depending
  -- on command line options, widths given in this specific table, and
  -- cells' contents
  let handleWidths
        | writerWrapText opts == WrapNone    = handleFullWidths widths
        | all (== 0) widths                  = handleZeroWidths widths
        | otherwise                          = handleGivenWidths widths
  (widthsInChars, rawHeaders, rawRows) <- handleWidths
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where sep'    = vfill " | "
              beg     = vfill "| "
              end     = vfill " |"
              middle  = chomp $ hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow rawHeaders
  let rows' = map (makeRow . map chomp) rawRows
  let borderpart ch align widthInChars =
           (if align == AlignLeft || align == AlignCenter
               then char ':'
               else char ch) <>
           text (replicate widthInChars ch) <>
           (if align == AlignRight || align == AlignCenter
               then char ':'
               else char ch)
  let border ch aligns' widthsInChars' =
        char '+' <>
        hcat (intersperse (char '+') (zipWith (borderpart ch)
                aligns' widthsInChars')) <> char '+'
  let body = vcat $ intersperse (border '-' (repeat AlignDefault) widthsInChars)
                    rows'
  let head'' = if headless
                  then empty
                  else head' $$ border '=' aligns widthsInChars
  if headless
     then return $
           border '-' aligns widthsInChars $$
           body $$
           border '-' (repeat AlignDefault) widthsInChars
     else return $
           border '-' (repeat AlignDefault) widthsInChars $$
           head'' $$
           body $$
           border '-' (repeat AlignDefault) widthsInChars



-- | Retrieve the metadata value for a given @key@
-- and convert to Bool.
lookupMetaBool :: String -> Meta -> Bool
lookupMetaBool key meta =
  case lookupMeta key meta of
      Just (MetaBlocks _)     -> True
      Just (MetaInlines _)    -> True
      Just (MetaString (_:_)) -> True
      Just (MetaBool True)    -> True
      _                       -> False

-- | Retrieve the metadata value for a given @key@
-- and extract blocks.
lookupMetaBlocks :: String -> Meta -> [Block]
lookupMetaBlocks key meta =
  case lookupMeta key meta of
         Just (MetaBlocks bs)   -> bs
         Just (MetaInlines ils) -> [Plain ils]
         Just (MetaString s)    -> [Plain [Str s]]
         _                      -> []

-- | Retrieve the metadata value for a given @key@
-- and extract inlines.
lookupMetaInlines :: String -> Meta -> [Inline]
lookupMetaInlines key meta =
  case lookupMeta key meta of
         Just (MetaString s)           -> [Str s]
         Just (MetaInlines ils)        -> ils
         Just (MetaBlocks [Plain ils]) -> ils
         Just (MetaBlocks [Para ils])  -> ils
         _                             -> []

-- | Retrieve the metadata value for a given @key@
-- and convert to String.
lookupMetaString :: String -> Meta -> String
lookupMetaString key meta =
  case lookupMeta key meta of
         Just (MetaString s)    -> s
         Just (MetaInlines ils) -> stringify ils
         Just (MetaBlocks bs)   -> stringify bs
         Just (MetaBool b)      -> show b
         _                      -> ""


toSuperscript :: Char -> Maybe Char
toSuperscript '1' = Just '\x00B9'
toSuperscript '2' = Just '\x00B2'
toSuperscript '3' = Just '\x00B3'
toSuperscript '+' = Just '\x207A'
toSuperscript '-' = Just '\x207B'
toSuperscript '=' = Just '\x207C'
toSuperscript '(' = Just '\x207D'
toSuperscript ')' = Just '\x207E'
toSuperscript c
  | c >= '0' && c <= '9' =
                 Just $ chr (0x2070 + (ord c - 48))
  | isSpace c = Just c
  | otherwise = Nothing

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

-- | Construct table of contents (as a bullet list) from document body.
toTableOfContents :: WriterOptions
                  -> [Block]
                  -> Block
toTableOfContents opts bs =
  BulletList $ filter (not . null)
             $ map (sectionToListItem opts)
             $ makeSections (writerNumberSections opts) Nothing bs

-- | Converts an Element to a list item for a table of contents,
sectionToListItem :: WriterOptions -> Block -> [Block]
sectionToListItem opts (Div (ident,_,_)
                         (Header lev (_,classes,kvs) ils : subsecs))
  | not (isNothing (lookup "number" kvs) && "unlisted" `elem` classes)
  = Plain headerLink : [BulletList listContents | not (null listContents)
                                              , lev < writerTOCDepth opts]
 where
   num = fromMaybe "" $ lookup "number" kvs
   addNumber  = if null num
                   then id
                   else (Span ("",["toc-section-number"],[])
                           [Str num] :) . (Space :)
   headerText' = addNumber $ walk (deLink . deNote) ils
   headerLink = if null ident
                   then headerText'
                   else [Link nullAttr headerText' ('#':ident, "")]
   listContents = filter (not . null) $ map (sectionToListItem opts) subsecs
sectionToListItem _ _ = []

endsWithPlain :: [Block] -> Bool
endsWithPlain xs =
  case lastMay xs of
    Just (Plain{}) -> True
    _              -> False

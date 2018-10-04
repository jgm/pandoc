{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2013-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Shared
   Copyright   : Copyright (C) 2013-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Shared utility functions for pandoc writers.
-}
module Text.Pandoc.Writers.Shared (
                       metaToJSON
                     , metaToJSON'
                     , addVariablesToJSON
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
                     , groffEscape
                     )
where
import Prelude
import Control.Monad (zipWithM)
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (Object),
                   encode, fromJSON)
import qualified Data.HashMap.Strict as H
import Data.List (groupBy, intersperse, transpose)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Traversable as Traversable
import qualified Text.Pandoc.Builder as Builder
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.UTF8 (toStringLazy)
import Text.Pandoc.XML (escapeStringForXML)
import Text.Printf (printf)
import Data.Char (isAscii, ord)

-- | Create JSON value for template from a 'Meta' and an association list
-- of variables, specified at the command line or in the writer.
-- Variables overwrite metadata fields with the same names.
-- If multiple variables are set with the same name, a list is
-- assigned.  Does nothing if 'writerTemplate' is Nothing.
metaToJSON :: (Functor m, Monad m, ToJSON a)
           => WriterOptions
           -> ([Block] -> m a)
           -> ([Inline] -> m a)
           -> Meta
           -> m Value
metaToJSON opts blockWriter inlineWriter meta
  | isJust (writerTemplate opts) =
    addVariablesToJSON opts <$> metaToJSON' blockWriter inlineWriter meta
  | otherwise = return (Object H.empty)

-- | Like 'metaToJSON', but does not include variables and is
-- not sensitive to 'writerTemplate'.
metaToJSON' :: (Functor m, Monad m, ToJSON a)
           => ([Block] -> m a)
           -> ([Inline] -> m a)
           -> Meta
           -> m Value
metaToJSON' blockWriter inlineWriter (Meta metamap) = do
  renderedMap <- Traversable.mapM
                 (metaValueToJSON blockWriter inlineWriter)
                 metamap
  return $ M.foldrWithKey defField (Object H.empty) renderedMap

-- | Add variables to JSON object, replacing any existing values.
-- Also include @meta-json@, a field containing a string representation
-- of the original JSON object itself, prior to addition of variables.
addVariablesToJSON :: WriterOptions -> Value -> Value
addVariablesToJSON opts metadata =
  foldl (\acc (x,y) -> setField x y acc)
       (defField "meta-json" (toStringLazy $ encode metadata) (Object mempty))
       (writerVariables opts)
    `combineMetadata` metadata
  where combineMetadata (Object o1) (Object o2) = Object $ H.union o1 o2
        combineMetadata x _                     = x

metaValueToJSON :: (Functor m, Monad m, ToJSON a)
                => ([Block] -> m a)
                -> ([Inline] -> m a)
                -> MetaValue
                -> m Value
metaValueToJSON blockWriter inlineWriter (MetaMap metamap) = toJSON <$>
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) metamap
metaValueToJSON blockWriter inlineWriter (MetaList xs) = toJSON <$>
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) xs
metaValueToJSON _ _ (MetaBool b) = return $ toJSON b
metaValueToJSON _ inlineWriter (MetaString s) = toJSON <$>
  inlineWriter (Builder.toList (Builder.text s))
metaValueToJSON blockWriter _ (MetaBlocks bs) = toJSON <$> blockWriter bs
metaValueToJSON _ inlineWriter (MetaInlines is) = toJSON <$> inlineWriter is

-- | Retrieve a field value from a JSON object.
getField :: FromJSON a
         => String
         -> Value
         -> Maybe a
getField field (Object hashmap) = do
  result <- H.lookup (T.pack field) hashmap
  case fromJSON result of
       Success x -> return x
       _         -> fail "Could not convert from JSON"
getField _ _ = fail "Not a JSON object"

setField :: ToJSON a
         => String
         -> a
         -> Value
         -> Value
-- | Set a field of a JSON object.  If the field already has a value,
-- convert it into a list with the new value appended to the old value(s).
-- This is a utility function to be used in preparing template contexts.
setField field val (Object hashmap) =
  Object $ H.insertWith combine (T.pack field) (toJSON val) hashmap
  where combine newval oldval =
          case fromJSON oldval of
                Success xs -> toJSON $ xs ++ [newval]
                _          -> toJSON [oldval, newval]
setField _ _  x = x

resetField :: ToJSON a
           => String
           -> a
           -> Value
           -> Value
-- | Reset a field of a JSON object.  If the field already has a value,
-- the new value replaces it.
-- This is a utility function to be used in preparing template contexts.
resetField field val (Object hashmap) =
  Object $ H.insert (T.pack field) (toJSON val) hashmap
resetField _ _  x = x

defField :: ToJSON a
         => String
         -> a
         -> Value
         -> Value
-- | Set a field of a JSON object if it currently has no value.
-- If it has a value, do nothing.
-- This is a utility function to be used in preparing template contexts.
defField field val (Object hashmap) =
  Object $ H.insertWith f (T.pack field) (toJSON val) hashmap
    where f _newval oldval = oldval
defField _ _  x = x

-- Produce an HTML tag with the given pandoc attributes.
tagWithAttrs :: String -> Attr -> Doc
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

gridTable :: Monad m
          => WriterOptions
          -> (WriterOptions -> [Block] -> m Doc)
          -> Bool -- ^ headless
          -> [Alignment]
          -> [Double]
          -> [[Block]]
          -> [[[Block]]]
          -> m Doc
gridTable opts blocksToDoc headless aligns widths headers rows = do
  -- the number of columns will be used in case of even widths
  let numcols = maximum (length aligns : length widths :
                           map length (headers:rows))
  -- handleGivenWidths wraps the given blocks in order for them to fit
  -- in cells with given widths. the returned content can be
  -- concatenated with borders and frames
  let handleGivenWidths widths' = do
        let widthsInChars' = map (
                      (\x -> if x < 1 then 1 else x) .
                      (\x -> x - 3) . floor .
                      (fromIntegral (writerColumns opts) *)
                      ) widths'
            -- replace page width (in columns) in the options with a
            -- given width if smaller (adjusting by two)
            useWidth w = opts{writerColumns = min (w - 2) (writerColumns opts)}
            -- prepare options to use with header and row cells
            columnOptions = map useWidth widthsInChars'
        rawHeaders' <- zipWithM blocksToDoc columnOptions headers
        rawRows' <- mapM
             (\cs -> zipWithM blocksToDoc columnOptions cs)
             rows
        return (widthsInChars', rawHeaders', rawRows')
  -- handleFullWidths tries to wrap cells to the page width or even
  -- more in cases where `--wrap=none`. thus the content here is left
  -- as wide as possible
  let handleFullWidths = do
        rawHeaders' <- mapM (blocksToDoc opts) headers
        rawRows' <- mapM (mapM (blocksToDoc opts)) rows
        let numChars [] = 0
            numChars xs = maximum . map offset $ xs
        let widthsInChars' =
                map numChars $ transpose (rawHeaders' : rawRows')
        return (widthsInChars', rawHeaders', rawRows')
  -- handleZeroWidths calls handleFullWidths to check whether a wide
  -- table would fit in the page. if the produced table is too wide,
  -- it calculates even widths and passes the content to
  -- handleGivenWidths
  let handleZeroWidths = do
        (widthsInChars', rawHeaders', rawRows') <- handleFullWidths
        if sum widthsInChars' > writerColumns opts
           then -- use even widths
                handleGivenWidths
                  (replicate numcols (1.0 / fromIntegral numcols) :: [Double])
           else return (widthsInChars', rawHeaders', rawRows')
  -- render the contents of header and row cells differently depending
  -- on command line options, widths given in this specific table, and
  -- cells' contents
  let handleWidths
        | writerWrapText opts == WrapNone  = handleFullWidths
        | all (== 0) widths                  = handleZeroWidths
        | otherwise                          = handleGivenWidths widths
  (widthsInChars, rawHeaders, rawRows) <- handleWidths
  let hpipeBlocks blocks = hcat [beg, middle, end]
        where h       = maximum (1 : map height blocks)
              sep'    = lblock 3 $ vcat (replicate h (text " | "))
              beg     = lblock 2 $ vcat (replicate h (text "| "))
              end     = lblock 2 $ vcat (replicate h (text " |"))
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

-- | Escape non-ASCII characters using groff \u[..] sequences.
groffEscape :: T.Text -> T.Text
groffEscape = T.concatMap toUchar
  where toUchar c
         | isAscii c = T.singleton c
         | otherwise = T.pack $ printf "\\[u%04X]" (ord c)


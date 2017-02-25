{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2013-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2013-2015 John MacFarlane
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
                     , fixDisplayMath
                     , unsmartify
                     )
where
import Text.Pandoc.Definition
import Text.Pandoc.Pretty
import Text.Pandoc.Options
import Text.Pandoc.XML (escapeStringForXML)
import Control.Monad (liftM)
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), fromJSON, ToJSON (..), Value(Object), Result(..), encode)
import Text.Pandoc.UTF8 (toStringLazy)
import qualified Data.Traversable as Traversable
import Data.List ( groupBy )
import Data.Maybe ( isJust )

-- | Create JSON value for template from a 'Meta' and an association list
-- of variables, specified at the command line or in the writer.
-- Variables overwrite metadata fields with the same names.
-- If multiple variables are set with the same name, a list is
-- assigned.  Does nothing if 'writerTemplate' is Nothing.
metaToJSON :: Monad m
           => WriterOptions
           -> ([Block] -> m String)
           -> ([Inline] -> m String)
           -> Meta
           -> m Value
metaToJSON opts blockWriter inlineWriter meta
  | isJust (writerTemplate opts) =
    addVariablesToJSON opts <$> metaToJSON' blockWriter inlineWriter meta
  | otherwise = return (Object H.empty)

-- | Like 'metaToJSON', but does not include variables and is
-- not sensitive to 'writerTemplate'.
metaToJSON' :: Monad m
           => ([Block] -> m String)
           -> ([Inline] -> m String)
           -> Meta
           -> m Value
metaToJSON' blockWriter inlineWriter (Meta metamap) = do
  renderedMap <- Traversable.mapM
                 (metaValueToJSON blockWriter inlineWriter)
                 metamap
  return $ M.foldWithKey defField (Object H.empty) renderedMap

-- | Add variables to JSON object, replacing any existing values.
-- Also include @meta-json@, a field containing a string representation
-- of the original JSON object itself, prior to addition of variables.
addVariablesToJSON :: WriterOptions -> Value -> Value
addVariablesToJSON opts metadata =
  foldl (\acc (x,y) -> resetField x y acc)
       (defField "meta-json" (toStringLazy $ encode metadata) metadata)
       (writerVariables opts)

metaValueToJSON :: Monad m
                => ([Block] -> m String)
                -> ([Inline] -> m String)
                -> MetaValue
                -> m Value
metaValueToJSON blockWriter inlineWriter (MetaMap metamap) = liftM toJSON $
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) metamap
metaValueToJSON blockWriter inlineWriter (MetaList xs) = liftM toJSON $
  Traversable.mapM (metaValueToJSON blockWriter inlineWriter) xs
metaValueToJSON _ _ (MetaBool b) = return $ toJSON b
metaValueToJSON _ _ (MetaString s) = return $ toJSON s
metaValueToJSON blockWriter _ (MetaBlocks bs) = liftM toJSON $ blockWriter bs
metaValueToJSON _ inlineWriter (MetaInlines bs) = liftM toJSON $ inlineWriter bs

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
                Success xs  -> toJSON $ xs ++ [newval]
                _           -> toJSON [oldval, newval]
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
isDisplayMath (Math DisplayMath _) = True
isDisplayMath _                    = False

stripLeadingTrailingSpace :: [Inline] -> [Inline]
stripLeadingTrailingSpace = go . reverse . go . reverse
  where go (Space:xs) = xs
        go (SoftBreak:xs) = xs
        go xs         = xs

-- Put display math in its own block (for ODT/DOCX).
fixDisplayMath :: Block -> Block
fixDisplayMath (Plain lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $ map (Plain . stripLeadingTrailingSpace) $
       groupBy (\x y -> (isDisplayMath x && isDisplayMath y) ||
                         not (isDisplayMath x || isDisplayMath y)) lst
fixDisplayMath (Para lst)
  | any isDisplayMath lst && not (all isDisplayMath lst) =
    -- chop into several paragraphs so each displaymath is its own
    Div ("",["math"],[]) $ map (Para . stripLeadingTrailingSpace) $
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
unsmartify opts (x:xs) = x : unsmartify opts xs
unsmartify _ [] = []


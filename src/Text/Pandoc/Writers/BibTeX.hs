{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.BibTeX
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Writes a BibTeX or BibLaTeX bibliographies based on the
'references' metadata in a Pandoc document.
-}
module Text.Pandoc.Writers.BibTeX
  ( writeBibTeX
  , writeBibLaTeX
  )
where

import Text.Pandoc.Options
import Text.Pandoc.Definition
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Citeproc (parseLang)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Citeproc.BibTeX as BibTeX
import Text.Pandoc.Citeproc.MetaValue (metaValueToReference)
import Text.Pandoc.Writers.Shared (lookupMetaString, defField,
                                    addVariablesToContext)
import Text.DocLayout (render, vcat)
import Text.DocTemplates (Context(..))
import Text.Pandoc.Templates (renderTemplate)

-- | Write BibTeX based on the references metadata from a Pandoc document.
writeBibTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBibTeX = writeBibTeX' BibTeX.Bibtex

-- | Write BibLaTeX based on the references metadata from a Pandoc document.
writeBibLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeBibLaTeX = writeBibTeX' BibTeX.Biblatex

writeBibTeX' :: PandocMonad m => Variant -> WriterOptions -> Pandoc -> m Text
writeBibTeX' variant opts (Pandoc meta _) = do
  let mblang = case lookupMetaString "lang" meta of
                 "" -> Nothing
                 t  -> either (const Nothing) Just $ parseLang t
  let refs = case lookupMeta "references" meta of
               Just (MetaList xs) -> mapMaybe metaValueToReference xs
               _ -> []
  let main = vcat $ map (BibTeX.writeBibtexString opts variant mblang) refs
  let context  = defField "body" main
                 $ addVariablesToContext opts (mempty :: Context Text)
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  return $ render colwidth $
    case writerTemplate opts of
      Nothing  -> main
      Just tpl -> renderTemplate tpl context



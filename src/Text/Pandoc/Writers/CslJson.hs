{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.CslJson
   Copyright   : Copyright (C) 2020-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of references from 'Pandoc' metadata to CSL JSON:
<https://citeproc-js.readthedocs.io/en/latest/csl-json/markup.html>.

Note that this writer ignores everything in the body of the
document and everything in the metadata except `references`.
It assumes that the `references` field is a list with the structure
of a CSL JSON bibliography.
-}
module Text.Pandoc.Writers.CslJson ( writeCslJson )
where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Error
import Text.Pandoc.Class
import Control.Monad.Except (throwError)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString (ByteString)
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B
import Text.Pandoc.Citeproc.MetaValue (metaValueToReference, metaValueToText)
import Citeproc (parseLang, Locale, Reference(..), Lang(..))
import Control.Monad.Identity
import Citeproc.Locale (getLocale)
import Citeproc.CslJson
import Text.Pandoc.Options (WriterOptions)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Aeson.Encode.Pretty         (Config (..), Indent (Spaces),
                                         NumberFormat (Generic),
                                         defConfig, encodePretty')

writeCslJson :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeCslJson _opts (Pandoc meta _) = do
  let lang = fromMaybe (Lang "en" Nothing (Just "US") [] [] [])
               (lookupMeta "lang" meta >>= metaValueToText >>=
                  either (const Nothing) Just . parseLang)
  locale <- case getLocale lang of
               Left e  -> throwError $ PandocCiteprocError e
               Right l -> return l
  let rs = case lookupMeta "references" meta of
             Just (MetaList xs) -> xs
             _ -> []
  return $ UTF8.toText
           (toCslJson locale (mapMaybe metaValueToReference rs)) <> "\n"

fromInlines :: [Inline] -> CslJson Text
fromInlines = foldMap fromInline . B.fromList

fromInline :: Inline -> CslJson Text
fromInline (Str t) = CslText t
fromInline (Emph ils) = CslItalic (fromInlines ils)
fromInline (Strong ils) = CslBold (fromInlines ils)
fromInline (Underline ils) = CslUnderline (fromInlines ils)
fromInline (Strikeout ils) = fromInlines ils
fromInline (Superscript ils) = CslSup (fromInlines ils)
fromInline (Subscript ils) = CslSub (fromInlines ils)
fromInline (SmallCaps ils) = CslSmallCaps (fromInlines ils)
fromInline (Quoted _ ils) = CslQuoted (fromInlines ils)
fromInline (Cite _ ils) = fromInlines ils
fromInline (Code _ t) = CslText t
fromInline Space = CslText " "
fromInline SoftBreak = CslText " "
fromInline LineBreak = CslText "\n"
fromInline (Math _ t) = CslText t
fromInline (RawInline _ _) = CslEmpty
fromInline (Link _ ils _) = fromInlines ils
fromInline (Image _ ils _) = fromInlines ils
fromInline (Note _) = CslEmpty
fromInline (Span (_,[cl],_) ils)
  | "csl-" `T.isPrefixOf` cl = CslDiv cl (fromInlines ils)
  | cl == "nocase" = CslNoCase (fromInlines ils)
fromInline (Span _ ils) = fromInlines ils

toCslJson :: Locale -> [Reference Inlines] -> ByteString
toCslJson locale = toStrict .
  encodePretty' defConfig{ confIndent = Spaces 2
                         , confCompare = compare
                         , confNumFormat = Generic }
  . map (runIdentity .  traverse (return .
                                  renderCslJson False locale .
                                  foldMap fromInline))

module Text.Pandoc.Legacy.Translations (
                           TP.Term(..)
                         , TP.Translations
                         , lookupTerm
                         , readTranslations
                         )
where

import qualified Data.Text as T
import qualified Text.Pandoc.Translations as TP
import qualified Data.Map as M

lookupTerm :: TP.Term -> TP.Translations -> Maybe String
lookupTerm t (TP.Translations tm) = T.unpack <$> M.lookup t tm

readTranslations :: String -> Either String TP.Translations
readTranslations = either (Left . T.unpack) Right . TP.readTranslations . T.pack

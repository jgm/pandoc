{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{- |
   Module      : Text.Pandoc.Translations
   Copyright   : Copyright (C) 2017-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for getting localized translations of terms.
-}
module Text.Pandoc.Translations (
                           module Text.Pandoc.Translations.Types
                         , readTranslations
                         , getTranslations
                         , setTranslations
                         , translateTerm
                         )
where
import Text.Pandoc.Translations.Types
import Text.Pandoc.Class (PandocMonad(..), toTextM, report)
import Text.Pandoc.Class.CommonState (CommonState(..))
import Text.Pandoc.Data (readDataFile)
import Data.Containers.ListUtils (nubOrd)
import Text.Pandoc.Logging (LogMessage(..))
import Control.Monad.Except (catchError)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Yamlet as Y
import Yamlet.Aeson
import Text.Collate.Lang (Lang(..), renderLang)

-- | Parse YAML translations. The file path is used in error messages.
readTranslations :: FilePath -> T.Text -> Either T.Text Translations
readTranslations fp s =
  case Y.decodeAllText @(ViaAeson Translations) s of
       Left errs -> Left $ T.intercalate "\n" $
         map (T.pack . Y.prettyError fp) (NE.toList errs)
       Right (ViaAeson t:_) -> Right t
       Right []        -> Left $ T.pack fp <> ": empty YAML document"

-- | Select the language to use with 'translateTerm'.
-- Note that this does not read a translation file;
-- that is only done the first time 'translateTerm' is
-- used.
setTranslations :: PandocMonad m => Lang -> m ()
setTranslations lang =
  modifyCommonState $ \st ->
    case stTranslations st of
      -- if a translation table is already loaded for this language,
      -- keep it, so we don't have to parse the translation file again:
      Just (l, Just _) | l == lang -> st
      _ -> st{ stTranslations = Just (lang, Nothing) }

-- | Load term map.
getTranslations :: PandocMonad m => m Translations
getTranslations = do
  mbtrans <- getsCommonState stTranslations
  case mbtrans of
       Nothing -> return mempty  -- no language defined
       Just (_, Just t) -> return t
       Just (lang, Nothing) -> do  -- read from file
         let translationFiles = map (\x -> "translations/" <> T.unpack x <> ".yaml")
               (nubOrd [renderLang lang,
                      langLanguage lang <> maybe "" ("-" <>) (langScript lang),
                      langLanguage lang])
         let getTrans [] = return mempty
             getTrans (fp:fps) = do
               result <- catchError (Right <$> (readDataFile fp >>= toTextM fp))
                             (\_ -> pure (Left (T.pack fp)))
               case result >>= readTranslations fp of
                  Left e
                    | null fps -> do
                      report $ CouldNotLoadTranslations (renderLang lang) e
                      -- make sure we don't try again...
                      modifyCommonState $ \st ->
                        st{ stTranslations = Nothing }
                      return mempty
                    | otherwise -> getTrans fps
                  Right t -> do
                    modifyCommonState $ \st ->
                        st{ stTranslations = Just (lang, Just t) }
                    return t
         getTrans translationFiles

-- | Get a translation from the current term map.
-- Issue a warning if the term is not defined.
translateTerm :: PandocMonad m => Term -> m T.Text
translateTerm term = do
  translations <- getTranslations
  case lookupTerm term translations of
       Just s -> return s
       Nothing -> do
         report $ NoTranslation $ T.pack $ show term
         return ""

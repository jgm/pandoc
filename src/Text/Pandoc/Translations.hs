{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Translations
   Copyright   : Copyright (C) 2017-2022 John MacFarlane
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
import Text.Pandoc.Class (PandocMonad(..), CommonState(..), report)
import Text.Pandoc.Data (readDataFile)
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Logging (LogMessage(..))
import Control.Monad.Except (catchError)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Yaml (prettyPrintParseException)
import Text.Collate.Lang (Lang(..), renderLang)

-- | Parse YAML translations.
readTranslations :: T.Text -> Either T.Text Translations
readTranslations s =
  case Yaml.decodeAllEither' $ UTF8.fromText s of
       Left err' -> Left $ T.pack $ prettyPrintParseException err'
       Right (t:_)     -> Right t
       Right []        -> Left "empty YAML document"

-- | Select the language to use with 'translateTerm'.
-- Note that this does not read a translation file;
-- that is only done the first time 'translateTerm' is
-- used.
setTranslations :: PandocMonad m => Lang -> m ()
setTranslations lang =
  modifyCommonState $ \st -> st{ stTranslations = Just (lang, Nothing) }

-- | Load term map.
getTranslations :: PandocMonad m => m Translations
getTranslations = do
  mbtrans <- getsCommonState stTranslations
  case mbtrans of
       Nothing -> return mempty  -- no language defined
       Just (_, Just t) -> return t
       Just (lang, Nothing) -> do  -- read from file
         let translationFile = "translations/" <> renderLang lang <> ".yaml"
         let fallbackFile = "translations/" <> langLanguage lang <> ".yaml"
         let getTrans fp = do
               bs <- readDataFile fp
               case readTranslations (UTF8.toText bs) of
                    Left e   -> do
                      report $ CouldNotLoadTranslations (renderLang lang)
                        (T.pack fp <> ": " <> e)
                      -- make sure we don't try again...
                      modifyCommonState $ \st ->
                        st{ stTranslations = Nothing }
                      return mempty
                    Right t -> do
                      modifyCommonState $ \st ->
                                  st{ stTranslations = Just (lang, Just t) }
                      return t
         catchError (getTrans $ T.unpack translationFile)
           (\_ ->
             catchError (getTrans $ T.unpack fallbackFile)
               (\e -> do
                 report $ CouldNotLoadTranslations (renderLang lang)
                          $ case e of
                               PandocCouldNotFindDataFileError _ ->
                                 "data file " <> fallbackFile <> " not found"
                               _ -> ""
                 -- make sure we don't try again...
                 modifyCommonState $ \st -> st{ stTranslations = Nothing }
                 return mempty))

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

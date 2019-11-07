{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Programmatically modifications of pandoc documents via JSON filters.
-}
module Text.Pandoc.Filter.JSON (apply) where

import Prelude
import Control.Monad (unless, when)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson (eitherDecode', encode)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import System.Directory (executable, doesFileExist, findExecutable,
                         getPermissions)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeExtension)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Shared (pandocVersion)
import qualified Control.Exception as E
import qualified Text.Pandoc.UTF8 as UTF8

apply :: ReaderOptions
      -> [T.Text]
      -> FilePath
      -> Pandoc
      -> PandocIO Pandoc
apply ropts args f = liftIO . externalFilter ropts f args

externalFilter :: MonadIO m
               => ReaderOptions -> FilePath -> [T.Text] -> Pandoc -> m Pandoc
externalFilter ropts f args' d = liftIO $ do
  exists <- doesFileExist f
  isExecutable <- if exists
                     then executable <$> getPermissions f
                     else return True
  let (f', args'') = if exists
                        then case map toLower (takeExtension f) of
                                  _      | isExecutable -> ("." </> f, args')
                                  ".py"  -> ("python", fText:args')
                                  ".hs"  -> ("runhaskell", fText:args')
                                  ".pl"  -> ("perl", fText:args')
                                  ".rb"  -> ("ruby", fText:args')
                                  ".php" -> ("php", fText:args')
                                  ".js"  -> ("node", fText:args')
                                  ".r"   -> ("Rscript", fText:args')
                                  _      -> (f, args')
                        else (f, args')
  unless (exists && isExecutable) $ do
    mbExe <- findExecutable f'
    when (isNothing mbExe) $
      E.throwIO $ PandocFilterError fText (T.pack $ "Could not find executable " <> f')
  env <- getEnvironment
  let env' = Just
           ( ("PANDOC_VERSION", pandocVersion)
           : ("PANDOC_READER_OPTIONS", (T.pack . UTF8.toStringLazy) (encode ropts))
           : map (\(x, y) -> (T.pack x, T.pack y)) env )
  (exitcode, outbs) <- E.handle filterException $
                              pipeProcess env' f' args'' $ encode d
  case exitcode of
       ExitSuccess    -> either (E.throwIO . PandocFilterError fText . T.pack)
                                   return $ eitherDecode' outbs
       ExitFailure ec -> E.throwIO $ PandocFilterError fText
                           ("Filter returned error status " <> T.pack (show ec))
 where fText = T.pack f

       filterException :: E.SomeException -> IO a
       filterException e = E.throwIO $ PandocFilterError fText (T.pack $ show e)

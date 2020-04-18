{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Programmatically modifications of pandoc documents via JSON filters.
-}
module Text.Pandoc.Filter.JSON (apply) where

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
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Shared (pandocVersion, tshow)
import qualified Control.Exception as E
import qualified Text.Pandoc.UTF8 as UTF8

apply :: MonadIO m
      => ReaderOptions
      -> [String]
      -> FilePath
      -> Pandoc
      -> m Pandoc
apply ropts args f = liftIO . externalFilter ropts f args

externalFilter :: MonadIO m
               => ReaderOptions -> FilePath -> [String] -> Pandoc -> m Pandoc
externalFilter ropts f args' d = liftIO $ do
  exists <- doesFileExist f
  isExecutable <- if exists
                     then executable <$> getPermissions f
                     else return True
  let (f', args'') = if exists
                        then case map toLower (takeExtension f) of
                                  _      | isExecutable -> ("." </> f, args')
                                  ".py"  -> ("python", f:args')
                                  ".hs"  -> ("runhaskell", f:args')
                                  ".pl"  -> ("perl", f:args')
                                  ".rb"  -> ("ruby", f:args')
                                  ".php" -> ("php", f:args')
                                  ".js"  -> ("node", f:args')
                                  ".r"   -> ("Rscript", f:args')
                                  _      -> (f, args')
                        else (f, args')
  unless (exists && isExecutable) $ do
    mbExe <- findExecutable f'
    when (isNothing mbExe) $
      E.throwIO $ PandocFilterError fText (T.pack $ "Could not find executable " <> f')
  env <- getEnvironment
  let env' = Just
           ( ("PANDOC_VERSION", T.unpack pandocVersion)
           : ("PANDOC_READER_OPTIONS", UTF8.toStringLazy (encode ropts))
           : env )
  (exitcode, outbs) <- E.handle filterException $
                              pipeProcess env' f' args'' $ encode d
  case exitcode of
       ExitSuccess    -> either (E.throwIO . PandocFilterError fText . T.pack)
                                   return $ eitherDecode' outbs
       ExitFailure ec -> E.throwIO $ PandocFilterError fText
                           ("Filter returned error status " <> tshow ec)
 where fText = T.pack f

       filterException :: E.SomeException -> IO a
       filterException e = E.throwIO $ PandocFilterError fText $ tshow e

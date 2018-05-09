{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
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
import System.Directory (executable, doesFileExist, findExecutable,
                         getPermissions)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeExtension)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Filter.Path (expandFilterPath)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Shared (pandocVersion)
import qualified Control.Exception as E
import qualified Text.Pandoc.UTF8 as UTF8

apply :: ReaderOptions
      -> [String]
      -> FilePath
      -> Pandoc
      -> PandocIO Pandoc
apply ropts args f d = do
  f' <- expandFilterPath f
  liftIO $ externalFilter ropts f' args d

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
      E.throwIO $ PandocFilterError f ("Could not find executable " ++ f')
  env <- getEnvironment
  let env' = Just
           ( ("PANDOC_VERSION", pandocVersion)
           : ("PANDOC_READER_OPTIONS", UTF8.toStringLazy (encode ropts))
           : env )
  (exitcode, outbs) <- E.handle filterException $
                              pipeProcess env' f' args'' $ encode d
  case exitcode of
       ExitSuccess    -> either (E.throwIO . PandocFilterError f)
                                   return $ eitherDecode' outbs
       ExitFailure ec -> E.throwIO $ PandocFilterError f
                           ("Filter returned error status " ++ show ec)
 where filterException :: E.SomeException -> IO a
       filterException e = E.throwIO $ PandocFilterError f (show e)

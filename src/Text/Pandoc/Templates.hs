{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-
Copyright (C) 2009-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.

-}

module Text.Pandoc.Templates ( module Text.DocTemplates
                             , renderTemplate'
                             , getDefaultTemplate
                             ) where

import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON (..))
import qualified Data.Text as T
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, TemplateTarget, applyTemplate,
                          compileTemplate, renderTemplate, varListToJSON)
import Text.Pandoc.Class (PandocMonad(readDataFile))
import Text.Pandoc.Error
import qualified Text.Pandoc.UTF8 as UTF8

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => (Maybe FilePath) -- ^ User data directory to search 1st
                   -> String           -- ^ Name of writer
                   -> m String
getDefaultTemplate user writer = do
  let format = takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
  case format of
       "native"  -> return ""
       "json"    -> return ""
       "docx"    -> return ""
       "fb2"     -> return ""
       "odt"     -> getDefaultTemplate user "opendocument"
       "html"    -> getDefaultTemplate user "html5"
       "docbook" -> getDefaultTemplate user "docbook5"
       "epub"    -> getDefaultTemplate user "epub3"
       "markdown_strict"   -> getDefaultTemplate user "markdown"
       "multimarkdown"     -> getDefaultTemplate user "markdown"
       "markdown_github"   -> getDefaultTemplate user "markdown"
       "markdown_mmd"      -> getDefaultTemplate user "markdown"
       "markdown_phpextra" -> getDefaultTemplate user "markdown"
       _        -> let fname = "templates" </> "default" <.> format
                   in  UTF8.toString <$> readDataFile user fname

-- | Like 'applyTemplate', but runs in PandocMonad and
-- raises an error if compilation fails.
renderTemplate' :: (PandocMonad m, ToJSON a, TemplateTarget b)
                => String -> a -> m b
renderTemplate' template context = do
  case applyTemplate (T.pack template) context of
       Left e  -> throwError (PandocTemplateError e)
       Right r -> return r

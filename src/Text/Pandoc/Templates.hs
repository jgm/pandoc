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

module Text.Pandoc.Templates ( renderTemplate
                             , renderTemplate'
                             , TemplateTarget
                             , varListToJSON
                             , compileTemplate
                             , Template
                             , getDefaultTemplate ) where

import qualified Control.Exception as E (IOException, try)
import Data.Aeson (ToJSON (..))
import qualified Data.Text as T
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, TemplateTarget, applyTemplate,
                          compileTemplate, renderTemplate, varListToJSON)
import Text.Pandoc.Shared (readDataFileUTF8)

-- | Get default template for the specified writer.
getDefaultTemplate :: (Maybe FilePath) -- ^ User data directory to search first
                   -> String           -- ^ Name of writer
                   -> IO (Either E.IOException String)
getDefaultTemplate user writer = do
  let format = takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
  case format of
       "native"  -> return $ Right ""
       "json"    -> return $ Right ""
       "docx"    -> return $ Right ""
       "fb2"     -> return $ Right ""
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
                   in  E.try $ readDataFileUTF8 user fname

-- | Like 'applyTemplate', but raising an error if compilation fails.
renderTemplate' :: (ToJSON a, TemplateTarget b) => String -> a -> b
renderTemplate' template = either error id . applyTemplate (T.pack template)


{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2019 John MacFarlane
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

import Prelude
import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON (..))
import qualified Data.Text as T
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, applyTemplate,
                          compileTemplate, renderTemplate)
import Text.Pandoc.Class (PandocMonad, readDataFile)
import Text.Pandoc.Error
import qualified Text.Pandoc.UTF8 as UTF8

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => String           -- ^ Name of writer
                   -> m String
getDefaultTemplate writer = do
  let format = takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
  case format of
       "native"  -> return ""
       "json"    -> return ""
       "docx"    -> return ""
       "fb2"     -> return ""
       "pptx"    -> return ""
       "ipynb"   -> return ""
       "odt"     -> getDefaultTemplate "opendocument"
       "html"    -> getDefaultTemplate "html5"
       "docbook" -> getDefaultTemplate "docbook5"
       "epub"    -> getDefaultTemplate "epub3"
       "beamer"  -> getDefaultTemplate "latex"
       "markdown_strict"   -> getDefaultTemplate "markdown"
       "multimarkdown"     -> getDefaultTemplate "markdown"
       "markdown_github"   -> getDefaultTemplate "markdown"
       "markdown_mmd"      -> getDefaultTemplate "markdown"
       "markdown_phpextra" -> getDefaultTemplate "markdown"
       "gfm"               -> getDefaultTemplate "commonmark"
       _        -> let fname = "templates" </> "default" <.> format
                   in  UTF8.toString <$> readDataFile fname

-- | Like 'applyTemplate', but runs in PandocMonad and
-- raises an error if compilation fails.
renderTemplate' :: (PandocMonad m, ToJSON a)
                => String -> a -> m T.Text
renderTemplate' template context =
  case applyTemplate (T.pack template) context of
       Left e  -> throwError (PandocTemplateError e)
       Right r -> return r

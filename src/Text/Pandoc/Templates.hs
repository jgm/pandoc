{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions for working with pandoc templates.
-}

module Text.Pandoc.Templates ( Template
                             , compileTemplate
                             , renderTemplate
                             , getDefaultTemplate
                             ) where

import Prelude
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, compileTemplate, renderTemplate)
import Text.Pandoc.Class (PandocMonad, readDataFile)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text)
import qualified Data.Text as T

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => Text           -- ^ Name of writer
                   -> m Text
getDefaultTemplate writer = do
  let format = T.takeWhile (`notElem` ("+-" :: String)) writer  -- strip off extensions
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
       _        -> do
         let fname = "templates" </> "default" <.> T.unpack format
         UTF8.toText <$> readDataFile fname



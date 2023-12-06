{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions for working with pandoc templates.

'WithDefaultPartials' and 'WithPartials' are Monad wrappers. Wrapping
these around an instance of 'PandocMonad' gives different instances of
'TemplateMonad', with different search behaviors when retrieving
partials.

To compile a template and limit partial search to pandoc’s data files,
use @runWithDefaultPartials (compileTemplate ...)@.

To compile a template and allow partials to be found locally (either on
the file system or via HTTP, in the event that the main template has an
absolute URL), ue @runWithPartials (compileTemplate ...)@.

'getTemplate' seeks a template locally, or via HTTP if the template has
an absolute URL, falling back to the data files if not found.

-}

module Text.Pandoc.Templates ( Template
                             , WithDefaultPartials(..)
                             , WithPartials(..)
                             , compileTemplate
                             , renderTemplate
                             , getTemplate
                             , getDefaultTemplate
                             , compileDefaultTemplate
                             ) where

import System.FilePath ((<.>), (</>), takeFileName)
import Text.DocTemplates (Template, TemplateMonad(..), compileTemplate, renderTemplate)
import Text.Pandoc.Class.CommonState (CommonState(..))
import Text.Pandoc.Class.PandocMonad (PandocMonad, fetchItem,
                                      getCommonState, modifyCommonState,
                                      toTextM)
import Text.Pandoc.Data (readDataFile)
import Control.Monad.Except (catchError, throwError)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Error
import System.IO.Error (isDoesNotExistError)

-- | Wrap a Monad in this if you want partials to
-- be taken only from the default data files.
newtype WithDefaultPartials m a = WithDefaultPartials { runWithDefaultPartials :: m a }
 deriving (Functor, Applicative, Monad)

-- | Wrap a Monad in this if you want partials to
-- be looked for locally (or, when the main template
-- is at a URL, via HTTP), falling back to default data files.
newtype WithPartials m a = WithPartials { runWithPartials :: m a }
 deriving (Functor, Applicative, Monad)

instance PandocMonad m => TemplateMonad (WithDefaultPartials m) where
  getPartial fp = WithDefaultPartials $
    readDataFile ("templates" </> takeFileName fp) >>= toTextM fp

instance PandocMonad m => TemplateMonad (WithPartials m) where
  getPartial fp = WithPartials $ getTemplate fp

-- | Retrieve text for a template.
getTemplate :: PandocMonad m => FilePath -> m Text
getTemplate tp =
  ((do surl <- stSourceURL <$> getCommonState
       -- we don't want to look for templates remotely
       -- unless the full URL is specified:
       modifyCommonState $ \st -> st{
         stSourceURL = Nothing }
       (bs, _) <- fetchItem $ T.pack tp
       modifyCommonState $ \st -> st{
         stSourceURL = surl }
       return bs)
   `catchError`
   (\e -> case e of
             PandocResourceNotFound _ ->
                -- see #5987 on reason for takeFileName
                readDataFile ("templates" </> takeFileName tp)
             PandocIOError _ ioe | isDoesNotExistError ioe ->
                -- see #5987 on reason for takeFileName
                readDataFile ("templates" </> takeFileName tp)
             _ -> throwError e)) >>= toTextM tp

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => Text           -- ^ Name of writer
                   -> m Text
getDefaultTemplate format = do
  case format of
       "native"  -> return ""
       "csljson" -> return ""
       "json"    -> return ""
       "docx"    -> return ""
       "fb2"     -> return ""
       "pptx"    -> return ""
       "ipynb"   -> return ""
       "asciidoctor" -> getDefaultTemplate "asciidoc"
       "asciidoc_legacy" -> getDefaultTemplate "asciidoc"
       "odt"     -> getDefaultTemplate "opendocument"
       "html"    -> getDefaultTemplate "html5"
       "docbook" -> getDefaultTemplate "docbook5"
       "epub"    -> getDefaultTemplate "epub3"
       "beamer"  -> getDefaultTemplate "latex"
       "jats"    -> getDefaultTemplate "jats_archiving"
       "markdown_strict"   -> getDefaultTemplate "markdown"
       "multimarkdown"     -> getDefaultTemplate "markdown"
       "markdown_github"   -> getDefaultTemplate "markdown"
       "markdown_mmd"      -> getDefaultTemplate "markdown"
       "markdown_phpextra" -> getDefaultTemplate "markdown"
       "gfm"               -> getDefaultTemplate "commonmark"
       "commonmark_x"      -> getDefaultTemplate "commonmark"
       _        -> do
         let fname = "templates" </> "default" <.> T.unpack format
         readDataFile fname >>= toTextM fname

-- | Get and compile default template for the specified writer.
-- Raise an error on compilation failure.
compileDefaultTemplate :: PandocMonad m
                       => Text
                       -> m (Template Text)
compileDefaultTemplate writer = do
  res <- getDefaultTemplate writer >>=
          runWithDefaultPartials .
           compileTemplate ("templates/default." <> T.unpack writer)
  case res of
    Left e   -> throwError $ PandocTemplateError (T.pack e)
    Right t  -> return t

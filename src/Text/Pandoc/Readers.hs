{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Readers
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the readers.

Note:  all of the readers assume that the input text has @'\n'@
line endings.  So if you get your input text from a web form,
you should remove @'\r'@ characters using @filter (/='\r')@.

-}

module Text.Pandoc.Readers
  (
    -- * Readers: converting /to/ Pandoc format
    Reader (..)
  , readers
  , readDocx
  , readOdt
  , readMarkdown
  , readCommonMark
  , readCreole
  , readDokuWiki
  , readMediaWiki
  , readVimwiki
  , readRST
  , readOrg
  , readLaTeX
  , readHtml
  , readJATS
  , readJira
  , readTextile
  , readDocBook
  , readOPML
  , readHaddock
  , readNative
  , readJSON
  , readTWiki
  , readTikiWiki
  , readTxt2Tags
  , readEPUB
  , readMuse
  , readFB2
  , readIpynb
  , readCSV
  , readCslJson
  , readBibTeX
  , readBibLaTeX
  -- * Miscellaneous
  , getReader
  , getDefaultExtensions
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.Creole
import Text.Pandoc.Readers.DocBook
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Readers.DokuWiki
import Text.Pandoc.Readers.EPUB
import Text.Pandoc.Readers.FB2
import Text.Pandoc.Readers.Ipynb
import Text.Pandoc.Readers.Haddock
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Readers.JATS (readJATS)
import Text.Pandoc.Readers.Jira (readJira)
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Muse
import Text.Pandoc.Readers.Native
import Text.Pandoc.Readers.Odt
import Text.Pandoc.Readers.OPML
import Text.Pandoc.Readers.Org
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.Textile
import Text.Pandoc.Readers.TikiWiki
import Text.Pandoc.Readers.TWiki
import Text.Pandoc.Readers.Txt2Tags
import Text.Pandoc.Readers.Vimwiki
import Text.Pandoc.Readers.Man
import Text.Pandoc.Readers.CSV
import Text.Pandoc.Readers.CslJson
import Text.Pandoc.Readers.BibTeX
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

data Reader m = TextReader (forall a . ToSources a =>
                                ReaderOptions -> a -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- | Association list of formats and readers.
readers :: PandocMonad m => [(Text, Reader m)]
readers = [("native"       , TextReader readNative)
          ,("json"         , TextReader readJSON)
          ,("markdown"     , TextReader readMarkdown)
          ,("markdown_strict" , TextReader readMarkdown)
          ,("markdown_phpextra" , TextReader readMarkdown)
          ,("markdown_github" , TextReader readMarkdown)
          ,("markdown_mmd",  TextReader readMarkdown)
          ,("commonmark"   , TextReader readCommonMark)
          ,("commonmark_x" , TextReader readCommonMark)
          ,("creole"       , TextReader readCreole)
          ,("dokuwiki"     , TextReader readDokuWiki)
          ,("gfm"          , TextReader readCommonMark)
          ,("rst"          , TextReader readRST)
          ,("mediawiki"    , TextReader readMediaWiki)
          ,("vimwiki"      , TextReader readVimwiki)
          ,("docbook"      , TextReader readDocBook)
          ,("opml"         , TextReader readOPML)
          ,("org"          , TextReader readOrg)
          ,("textile"      , TextReader readTextile) -- TODO : textile+lhs
          ,("html"         , TextReader readHtml)
          ,("jats"         , TextReader readJATS)
          ,("jira"         , TextReader readJira)
          ,("latex"        , TextReader readLaTeX)
          ,("haddock"      , TextReader readHaddock)
          ,("twiki"        , TextReader readTWiki)
          ,("tikiwiki"     , TextReader readTikiWiki)
          ,("docx"         , ByteStringReader readDocx)
          ,("odt"          , ByteStringReader readOdt)
          ,("t2t"          , TextReader readTxt2Tags)
          ,("epub"         , ByteStringReader readEPUB)
          ,("muse"         , TextReader readMuse)
          ,("man"          , TextReader readMan)
          ,("fb2"          , TextReader readFB2)
          ,("ipynb"        , TextReader readIpynb)
          ,("csv"          , TextReader readCSV)
          ,("csljson"      , TextReader readCslJson)
          ,("bibtex"       , TextReader readBibTeX)
          ,("biblatex"     , TextReader readBibLaTeX)
           ]

-- | Retrieve reader, extensions based on formatSpec (format+extensions).
getReader :: PandocMonad m => Text -> m (Reader m, Extensions)
getReader s =
  case parseFormatSpec s of
       Left e  -> throwError $ PandocAppError $
                    "Error parsing reader format " <> tshow s <> ": " <> tshow e
       Right (readerName, extsToEnable, extsToDisable) ->
           case lookup readerName readers of
                   Nothing  -> throwError $ PandocUnknownReaderError
                                             readerName
                   Just  r  -> do
                     let allExts = getAllExtensions readerName
                     let exts = foldr disableExtension
                           (foldr enableExtension
                             (getDefaultExtensions readerName)
                                   extsToEnable) extsToDisable
                     mapM_ (\ext ->
                              unless (extensionEnabled ext allExts) $
                                throwError $
                                   PandocUnsupportedExtensionError
                                   (T.drop 4 $ T.pack $ show ext) readerName)
                          (extsToEnable ++ extsToDisable)
                     return (r, exts)

-- | Read pandoc document from JSON format.
readJSON :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readJSON _ s =
  case eitherDecode' . BL.fromStrict . UTF8.fromText
                     . sourcesToText . toSources $ s of
       Right doc -> return doc
       Left e    -> throwError $ PandocParseError ("JSON parse error: "
                                                   <> T.pack e)

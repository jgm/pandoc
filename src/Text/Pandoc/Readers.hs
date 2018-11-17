{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Readers
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
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
  , readTSV
  , readCslJson
  , readBibTeX
  , readBibLaTeX
  , readEndNoteXML
  , readRIS
  , readRTF
  -- * Miscellaneous
  , readerForFormat
  , getReader
  , getDefaultExtensions
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Extensions
import Text.Pandoc.Format.Input
import Text.Pandoc.Options (ReaderOptions)
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
import Text.Pandoc.Readers.EndNote
import Text.Pandoc.Readers.RIS
import Text.Pandoc.Readers.RTF
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

data Reader m = TextReader (forall a . ToSources a =>
                                ReaderOptions -> a -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- | Association list of formats and readers.
readerForFormat :: PandocMonad m => InputFormat -> Reader m
readerForFormat = \case
  BibLaTeX          -> TextReader readBibLaTeX
  BibTeX            -> TextReader readBibTeX
  CSV               -> TextReader readCSV
  CommonMark        -> TextReader readCommonMark
  CommonMark_X      -> TextReader readCommonMark
  Creole            -> TextReader readCreole
  CslJson           -> TextReader readCslJson
  DocBook           -> TextReader readDocBook
  Docx              -> ByteStringReader readDocx
  DokuWiki          -> TextReader readDokuWiki
  EndNoteXML        -> TextReader readEndNoteXML
  EPUB              -> ByteStringReader readEPUB
  FB2               -> TextReader readFB2
  GFM               -> TextReader readCommonMark
  HTML              -> TextReader readHtml
  Haddock           -> TextReader readHaddock
  Ipynb             -> TextReader readIpynb
  JATS              -> TextReader readJATS
  JSON              -> TextReader readJSON
  Jira              -> TextReader readJira
  LaTeX             -> TextReader readLaTeX
  Man               -> TextReader readMan
  Markdown          -> TextReader readMarkdown
  Markdown_GitHub   -> TextReader readMarkdown
  Markdown_MMD      -> TextReader readMarkdown
  Markdown_PHPExtra -> TextReader readMarkdown
  Markdown_strict   -> TextReader readMarkdown
  MediaWiki         -> TextReader readMediaWiki
  Muse              -> TextReader readMuse
  Native            -> TextReader readNative
  ODT               -> ByteStringReader readOdt
  OPML              -> TextReader readOPML
  Org               -> TextReader readOrg
  RIS               -> TextReader readRIS
  RST               -> TextReader readRST
  TSV               -> TextReader readTSV
  TWiki             -> TextReader readTWiki
  Textile           -> TextReader readTextile
  TikiWiki          -> TextReader readTikiWiki
  T2T               -> TextReader readTxt2Tags
  Vimwiki           -> TextReader readVimwiki

-- | Association list of formats and readers.
readers :: PandocMonad m => [(Text, Reader m)]
readers =
  map (\inFormat -> (name inFormat, readerForFormat inFormat))
  $ Set.toList allInputFormats

-- | Retrieve reader, extensions based on formatSpec (format+extensions).
getReader :: PandocMonad m => Text -> m (Reader m, Extensions)
getReader s = flavoredFromSpec s >>= \case
  KnownFormat f exts -> return (readerForFormat f, exts)
  CustomFormat {}    -> throwError . PandocShouldNeverHappenError $
                        "getReader called on custom reader"

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

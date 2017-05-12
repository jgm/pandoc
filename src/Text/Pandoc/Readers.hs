{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
  , readMediaWiki
  , readVimwiki
  , readRST
  , readOrg
  , readLaTeX
  , readHtml
  , readTextile
  , readDocBook
  , readOPML
  , readHaddock
  , readNative
  , readJSON
  , readTWiki
  , readTxt2Tags
  , readEPUB
  -- * Miscellaneous
  , getReader
  , getDefaultExtensions
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson
import Data.List (intercalate)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.DocBook
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Readers.EPUB
import Text.Pandoc.Readers.Haddock
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Vimwiki
import Text.Pandoc.Readers.Native
import Text.Pandoc.Readers.Odt
import Text.Pandoc.Readers.OPML
import Text.Pandoc.Readers.Org
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.Textile
import Text.Pandoc.Readers.TWiki
import Text.Pandoc.Readers.Txt2Tags
import Text.Pandoc.Shared (mapLeft)
import Text.Parsec.Error
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL

data Reader m = StringReader (ReaderOptions -> String -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- | Association list of formats and readers.
readers :: PandocMonad m => [(String, Reader m)]
readers = [ ("native"       , StringReader readNative)
           ,("json"         , StringReader $ \o s ->
                                               case readJSON o s of
                                                 Right doc -> return doc
                                                 Left _ -> throwError $ PandocParseError "JSON parse error")
           ,("markdown"     , StringReader readMarkdown)
           ,("markdown_strict" , StringReader readMarkdown)
           ,("markdown_phpextra" , StringReader readMarkdown)
           ,("markdown_github" , StringReader readMarkdown)
           ,("markdown_mmd",  StringReader readMarkdown)
           ,("commonmark"   , StringReader readCommonMark)
           ,("rst"          , StringReader readRST)
           ,("mediawiki"    , StringReader readMediaWiki)
           ,("vimwiki"      , StringReader readVimwiki)
           ,("docbook"      , StringReader readDocBook)
           ,("opml"         , StringReader readOPML)
           ,("org"          , StringReader readOrg)
           ,("textile"      , StringReader readTextile) -- TODO : textile+lhs
           ,("html"         , StringReader readHtml)
           ,("latex"        , StringReader readLaTeX)
           ,("haddock"      , StringReader readHaddock)
           ,("twiki"        , StringReader readTWiki)
           ,("docx"         , ByteStringReader readDocx)
           ,("odt"          , ByteStringReader readOdt)
           ,("t2t"          , StringReader readTxt2Tags)
           ,("epub"         , ByteStringReader readEPUB)
           ]

-- | Retrieve reader based on formatSpec (format+extensions).
getReader :: PandocMonad m => String -> Either String (Reader m)
getReader s =
  case parseFormatSpec s of
       Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
       Right (readerName, setExts) ->
           case lookup readerName readers of
                   Nothing  -> Left $ "Unknown reader: " ++ readerName
                   Just  (StringReader r)  -> Right $ StringReader $ \o ->
                                  r o{ readerExtensions = setExts $
                                            getDefaultExtensions readerName }
                   Just (ByteStringReader r) -> Right $ ByteStringReader $ \o ->
                                  r o{ readerExtensions = setExts $
                                            getDefaultExtensions readerName }

-- | Read pandoc document from JSON format.
readJSON :: ReaderOptions -> String -> Either PandocError Pandoc
readJSON _ = mapLeft PandocParseError . eitherDecode' . UTF8.fromStringLazy

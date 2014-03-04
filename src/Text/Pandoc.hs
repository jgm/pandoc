{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the main writers, readers, and data
structure definitions from the Pandoc libraries.

A typical application will chain together a reader and a writer
to convert strings from one format to another.  For example, the
following simple program will act as a filter converting markdown
fragments to reStructuredText, using reference-style links instead of
inline links:

> module Main where
> import Text.Pandoc
>
> markdownToRST :: String -> String
> markdownToRST =
>   (writeRST def {writerReferenceLinks = True}) . readMarkdown def
>
> main = getContents >>= putStrLn . markdownToRST

Note:  all of the readers assume that the input text has @'\n'@
line endings.  So if you get your input text from a web form,
you should remove @'\r'@ characters using @filter (/='\r')@.

-}

module Text.Pandoc
               (
               -- * Definitions
               module Text.Pandoc.Definition
               -- * Generics
               , module Text.Pandoc.Generic
               -- * Options
               , module Text.Pandoc.Options
               -- * Lists of readers and writers
               , readers
               , writers
               -- * Readers: converting /to/ Pandoc format
               , readMarkdown
               , readMediaWiki
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
               -- * Writers: converting /from/ Pandoc format
               , Writer (..)
               , writeNative
               , writeJSON
               , writeMarkdown
               , writePlain
               , writeRST
               , writeLaTeX
               , writeConTeXt
               , writeTexinfo
               , writeHtml
               , writeHtmlString
               , writeICML
               , writeDocbook
               , writeOPML
               , writeOpenDocument
               , writeMan
               , writeMediaWiki
               , writeTextile
               , writeRTF
               , writeODT
               , writeDocx
               , writeEPUB
               , writeFB2
               , writeOrg
               , writeAsciiDoc
               , writeCustom
               -- * Rendering templates and default templates
               , module Text.Pandoc.Templates
               -- * Version
               , pandocVersion
               -- * Miscellaneous
               , getReader
               , getWriter
               , ToJsonFilter(..)
             ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.JSON
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.Org
import Text.Pandoc.Readers.DocBook
import Text.Pandoc.Readers.OPML
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Readers.Textile
import Text.Pandoc.Readers.Native
import Text.Pandoc.Readers.Haddock
import Text.Pandoc.Writers.Native
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.RST
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.ODT
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Writers.EPUB
import Text.Pandoc.Writers.FB2
import Text.Pandoc.Writers.ICML
import Text.Pandoc.Writers.Docbook
import Text.Pandoc.Writers.OPML
import Text.Pandoc.Writers.OpenDocument
import Text.Pandoc.Writers.Man
import Text.Pandoc.Writers.RTF
import Text.Pandoc.Writers.MediaWiki
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.Org
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.Custom
import Text.Pandoc.Templates
import Text.Pandoc.Options
import Text.Pandoc.Shared (safeRead, warn)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, isSuffixOf)
import Data.Version (showVersion)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Error
import qualified Text.Pandoc.UTF8 as UTF8
import Paths_pandoc (version)

-- | Version number of pandoc library.
pandocVersion :: String
pandocVersion = showVersion version

parseFormatSpec :: String
                -> Either ParseError (String, Set Extension -> Set Extension)
parseFormatSpec = parse formatSpec ""
  where formatSpec = do
          name <- formatName
          extMods <- many extMod
          return (name, foldl (.) id extMods)
        formatName = many1 $ noneOf "-+"
        extMod = do
          polarity <- oneOf "-+"
          name <- many $ noneOf "-+"
          ext <- case safeRead ("Ext_" ++ name) of
                       Just n  -> return n
                       Nothing
                         | name == "lhs" -> return Ext_literate_haskell
                         | otherwise -> fail $ "Unknown extension: " ++ name
          return $ case polarity of
                        '-'  -> Set.delete ext
                        _    -> Set.insert ext

-- auxiliary function for readers:
markdown :: ReaderOptions -> String -> IO Pandoc
markdown o s = do
  let (doc, warnings) = readMarkdownWithWarnings o s
  mapM_ warn warnings
  return doc

-- | Association list of formats and readers.
readers :: [(String, ReaderOptions -> String -> IO Pandoc)]
readers = [ ("native"       , \_ s -> return $ readNative s)
           ,("json"         , \o s -> return $ readJSON o s)
           ,("markdown"     , markdown)
           ,("markdown_strict" , markdown)
           ,("markdown_phpextra" , markdown)
           ,("markdown_github" , markdown)
           ,("markdown_mmd",  markdown)
           ,("rst"          , \o s -> return $ readRST o s)
           ,("mediawiki"    , \o s -> return $ readMediaWiki o s)
           ,("docbook"      , \o s -> return $ readDocBook o s)
           ,("opml"         , \o s -> return $ readOPML o s)
           ,("org"          , \o s -> return $ readOrg o s)
           ,("textile"      , \o s -> return $ readTextile o s) -- TODO : textile+lhs
           ,("html"         , \o s -> return $ readHtml o s)
           ,("latex"        , \o s -> return $ readLaTeX o s)
           ,("haddock"      , \o s -> return $ readHaddock o s)
           ]

data Writer = PureStringWriter   (WriterOptions -> Pandoc -> String)
            | IOStringWriter     (WriterOptions -> Pandoc -> IO String)
            | IOByteStringWriter (WriterOptions -> Pandoc -> IO BL.ByteString)

-- | Association list of formats and writers.
writers :: [ ( String, Writer ) ]
writers = [
   ("native"       , PureStringWriter writeNative)
  ,("json"         , PureStringWriter writeJSON)
  ,("docx"         , IOByteStringWriter writeDocx)
  ,("odt"          , IOByteStringWriter writeODT)
  ,("epub"         , IOByteStringWriter $ \o ->
                      writeEPUB o{ writerEpubVersion = Just EPUB2 })
  ,("epub3"        , IOByteStringWriter $ \o ->
                       writeEPUB o{ writerEpubVersion = Just EPUB3 })
  ,("fb2"          , IOStringWriter writeFB2)
  ,("html"         , PureStringWriter writeHtmlString)
  ,("html5"        , PureStringWriter $ \o ->
     writeHtmlString o{ writerHtml5 = True })
  ,("icml"         , PureStringWriter writeICML)
  ,("s5"           , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = S5Slides
                      , writerTableOfContents = False })
  ,("slidy"        , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlidySlides })
  ,("slideous"     , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlideousSlides })
  ,("dzslides"     , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = DZSlides
                      , writerHtml5 = True })
  ,("revealjs"      , PureStringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = RevealJsSlides
                      , writerHtml5 = True })
  ,("docbook"      , PureStringWriter writeDocbook)
  ,("opml"         , PureStringWriter writeOPML)
  ,("opendocument" , PureStringWriter writeOpenDocument)
  ,("latex"        , PureStringWriter writeLaTeX)
  ,("beamer"       , PureStringWriter $ \o ->
     writeLaTeX o{ writerBeamer = True })
  ,("context"      , PureStringWriter writeConTeXt)
  ,("texinfo"      , PureStringWriter writeTexinfo)
  ,("man"          , PureStringWriter writeMan)
  ,("markdown"     , PureStringWriter writeMarkdown)
  ,("markdown_strict" , PureStringWriter writeMarkdown)
  ,("markdown_phpextra" , PureStringWriter writeMarkdown)
  ,("markdown_github" , PureStringWriter writeMarkdown)
  ,("markdown_mmd" , PureStringWriter writeMarkdown)
  ,("plain"        , PureStringWriter writePlain)
  ,("rst"          , PureStringWriter writeRST)
  ,("mediawiki"    , PureStringWriter writeMediaWiki)
  ,("textile"      , PureStringWriter writeTextile)
  ,("rtf"          , IOStringWriter writeRTFWithEmbeddedImages)
  ,("org"          , PureStringWriter writeOrg)
  ,("asciidoc"     , PureStringWriter writeAsciiDoc)
  ]

getDefaultExtensions :: String -> Set Extension
getDefaultExtensions "markdown_strict" = strictExtensions
getDefaultExtensions "markdown_phpextra" = phpMarkdownExtraExtensions
getDefaultExtensions "markdown_mmd" = multimarkdownExtensions
getDefaultExtensions "markdown_github" = githubMarkdownExtensions
getDefaultExtensions "markdown"        = pandocExtensions
getDefaultExtensions "plain"           = pandocExtensions
getDefaultExtensions "textile"         = Set.fromList [Ext_auto_identifiers, Ext_raw_tex]
getDefaultExtensions _                 = Set.fromList [Ext_auto_identifiers]

-- | Retrieve reader based on formatSpec (format+extensions).
getReader :: String -> Either String (ReaderOptions -> String -> IO Pandoc)
getReader s =
  case parseFormatSpec s of
       Left e  -> Left $ intercalate "\n" $ [m | Message m <- errorMessages e]
       Right (readerName, setExts) ->
           case lookup readerName readers of
                   Nothing  -> Left $ "Unknown reader: " ++ readerName
                   Just  r  -> Right $ \o ->
                                  r o{ readerExtensions = setExts $
                                            getDefaultExtensions readerName }

-- | Retrieve writer based on formatSpec (format+extensions).
getWriter :: String -> Either String Writer
getWriter s =
  case parseFormatSpec s of
       Left e  -> Left $ intercalate "\n" $ [m | Message m <- errorMessages e]
       Right (writerName, setExts) ->
           case lookup writerName writers of
                   Nothing
                     | ".lua" `isSuffixOf` s ->
                       Right $ IOStringWriter $ writeCustom s
                     | otherwise -> Left $ "Unknown writer: " ++ writerName
                   Just (PureStringWriter r) -> Right $ PureStringWriter $
                           \o -> r o{ writerExtensions = setExts $
                                            getDefaultExtensions writerName }
                   Just (IOStringWriter r) -> Right $ IOStringWriter $
                           \o -> r o{ writerExtensions = setExts $
                                            getDefaultExtensions writerName }
                   Just (IOByteStringWriter r) -> Right $ IOByteStringWriter $
                           \o -> r o{ writerExtensions = setExts $
                                            getDefaultExtensions writerName }

{-# DEPRECATED toJsonFilter "Use 'toJSONFilter' from 'Text.Pandoc.JSON' instead" #-}
-- | Deprecated.  Use @toJSONFilter@ from @Text.Pandoc.JSON@ instead.
class ToJSONFilter a => ToJsonFilter a
  where toJsonFilter :: a -> IO ()
        toJsonFilter = toJSONFilter

readJSON :: ReaderOptions -> String -> Pandoc
readJSON _ = either error id . eitherDecode' . UTF8.fromStringLazy

writeJSON :: WriterOptions -> Pandoc -> String
writeJSON _ = UTF8.toStringLazy . encode


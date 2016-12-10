{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, GADTs #-}
{-
Copyright (C) 2006-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
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
> markdownToRST :: String -> Either PandocError String
> markdownToRST =
>   writeRST def {writerReferenceLinks = True} . readMarkdown def
>
> main = getContents >>= either error return markdownToRST >>= putStrLn

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
               -- * Typeclass
               , module Text.Pandoc.Class
               -- * Error handling
               , module Text.Pandoc.Error
               -- * Lists of readers and writers
               , readers
               -- , writers
               , writers
               -- * Readers: converting /to/ Pandoc format
               , Reader (..)
               , readDocx
               , readOdt
               , readMarkdown
               , readCommonMark
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
               , readTWiki
               , readTxt2Tags
               , readEPUB
               -- * Writers: converting /from/ Pandoc format
               , Writer(..)
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
               , writeDokuWiki
               , writeZimWiki
               , writeTextile
               , writeRTF
               , writeODT
               , writeDocx
               , writeEPUB
               , writeFB2
               , writeOrg
               , writeAsciiDoc
               , writeHaddock
               , writeCommonMark
               , writeCustom
               , writeTEI
               -- * Rendering templates and default templates
               , module Text.Pandoc.Templates
               -- * Miscellaneous
               , getReader
               , getWriter
               , getDefaultExtensions
               , pandocVersion
             ) where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.CommonMark
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
import Text.Pandoc.Readers.TWiki
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Readers.Odt
import Text.Pandoc.Readers.Txt2Tags
import Text.Pandoc.Readers.EPUB
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
import Text.Pandoc.Writers.DokuWiki
import Text.Pandoc.Writers.ZimWiki
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.Org
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.Haddock
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.Custom
import Text.Pandoc.Writers.TEI
import Text.Pandoc.Templates
import Text.Pandoc.Options
import Text.Pandoc.Shared (safeRead, mapLeft, pandocVersion)
import Text.Pandoc.Error
import Text.Pandoc.Class (PandocMonad, runIOorExplode)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Error
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Monad.Except (throwError)

parseFormatSpec :: String
                -> Either ParseError (String, Set Extension -> Set Extension)
parseFormatSpec = parse formatSpec ""
  where formatSpec = do
          name <- formatName
          extMods <- many extMod
          return (name, \x -> foldl (flip ($)) x extMods)
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

-- TODO: when we get the PandocMonad stuff all sorted out,
-- we can simply these types considerably.  Errors/MediaBag can be
-- part of the monad's internal state.
data Reader m = StringReader (ReaderOptions -> String -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- mkStringReader :: (ReaderOptions -> String -> Either PandocError Pandoc) -> Reader IO
-- mkStringReader r = StringReader (\o s -> return $ r o s)

-- mkStringReaderWithWarnings :: (ReaderOptions -> String -> Either PandocError (Pandoc, [String])) -> Reader IO
-- mkStringReaderWithWarnings r  = StringReader $ \o s ->
--   case r o s of
--     Left err -> return $ Left err
--     Right (doc, warnings) -> do
--       mapM_ warn warnings
--       return (Right doc)

-- mkBSReader :: (ReaderOptions -> BL.ByteString -> Either PandocError (Pandoc, MediaBag)) -> Reader IO
-- mkBSReader r = ByteStringReader (\o s -> return $ r o s)

-- mkBSReaderWithWarnings :: (ReaderOptions -> BL.ByteString -> Either PandocError (Pandoc, MediaBag, [String])) -> Reader IO
-- mkBSReaderWithWarnings r = ByteStringReader $ \o s ->
--   case r o s of
--     Left err -> return $ Left err
--     Right (doc, mediaBag, warnings) -> do
--       mapM_ warn warnings
--       return $ Right (doc, mediaBag)

-- | Association list of formats and readers.
readers :: PandocMonad m => [(String, Reader m)]
readers = [ ("native"       , StringReader $ \_ s -> readNative s)
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

data Writer m = StringWriter (WriterOptions -> Pandoc -> m String)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Association list of formats and writers.
writers :: PandocMonad m => [ ( String, Writer m) ]
writers = [
   ("native"       , StringWriter writeNative)
  ,("json"         , StringWriter $ \o d -> return $ writeJSON o d)
  ,("docx"         , ByteStringWriter writeDocx)
  ,("odt"          , ByteStringWriter writeODT)
  ,("epub"         , ByteStringWriter $ \o ->
                      writeEPUB o{ writerEpubVersion = Just EPUB2 })
  ,("epub3"        , ByteStringWriter $ \o ->
                      writeEPUB o{ writerEpubVersion = Just EPUB3 })
  ,("fb2"          , StringWriter writeFB2)
  ,("html"         , StringWriter writeHtmlString)
  ,("html5"        , StringWriter $ \o ->
     writeHtmlString o{ writerHtml5 = True })
  ,("icml"         , StringWriter writeICML)
  ,("s5"           , StringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = S5Slides
                      , writerTableOfContents = False })
  ,("slidy"        , StringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlidySlides })
  ,("slideous"     , StringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = SlideousSlides })
  ,("dzslides"     , StringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = DZSlides
                      , writerHtml5 = True })
  ,("revealjs"      , StringWriter $ \o ->
     writeHtmlString o{ writerSlideVariant = RevealJsSlides
                      , writerHtml5 = True })
  ,("docbook"      , StringWriter writeDocbook)
  ,("docbook5"     , StringWriter $ \o ->
     writeDocbook o{ writerDocbook5 = True })
  ,("opml"         , StringWriter writeOPML)
  ,("opendocument" , StringWriter writeOpenDocument)
  ,("latex"        , StringWriter writeLaTeX)
  ,("beamer"       , StringWriter $ \o ->
     writeLaTeX o{ writerBeamer = True })
  ,("context"      , StringWriter writeConTeXt)
  ,("texinfo"      , StringWriter writeTexinfo)
  ,("man"          , StringWriter writeMan)
  ,("markdown"     , StringWriter writeMarkdown)
  ,("markdown_strict" , StringWriter writeMarkdown)
  ,("markdown_phpextra" , StringWriter writeMarkdown)
  ,("markdown_github" , StringWriter writeMarkdown)
  ,("markdown_mmd" , StringWriter writeMarkdown)
  ,("plain"        , StringWriter writePlain)
  ,("rst"          , StringWriter writeRST)
  ,("mediawiki"    , StringWriter writeMediaWiki)
  ,("dokuwiki"     , StringWriter writeDokuWiki)
  ,("zimwiki"      , StringWriter writeZimWiki)
  ,("textile"      , StringWriter writeTextile)
  ,("rtf"          , StringWriter writeRTF)
  ,("org"          , StringWriter writeOrg)
  ,("asciidoc"     , StringWriter writeAsciiDoc)
  ,("haddock"      , StringWriter writeHaddock)
  ,("commonmark"   , StringWriter writeCommonMark)
  ,("tei"          , StringWriter writeTEI)
  ]

getDefaultExtensions :: String -> Set Extension
getDefaultExtensions "markdown_strict" = strictExtensions
getDefaultExtensions "markdown_phpextra" = phpMarkdownExtraExtensions
getDefaultExtensions "markdown_mmd" = multimarkdownExtensions
getDefaultExtensions "markdown_github" = githubMarkdownExtensions
getDefaultExtensions "markdown"        = pandocExtensions
getDefaultExtensions "plain"           = plainExtensions
getDefaultExtensions "org"             = Set.fromList [Ext_citations,
                                                       Ext_auto_identifiers]
getDefaultExtensions "textile"         = Set.fromList [Ext_auto_identifiers]
getDefaultExtensions "html"            = Set.fromList [Ext_auto_identifiers,
                                                       Ext_native_divs,
                                                       Ext_native_spans]
getDefaultExtensions "html5"           = getDefaultExtensions "html"
getDefaultExtensions "epub"            = Set.fromList [Ext_raw_html,
                                                       Ext_native_divs,
                                                       Ext_native_spans,
                                                       Ext_epub_html_exts]
getDefaultExtensions _                 = Set.fromList [Ext_auto_identifiers]

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

getWriter :: PandocMonad m => String -> Either String (Writer m)
getWriter s
  = case parseFormatSpec s of
         Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
         Right (writerName, setExts) ->
             case lookup writerName writers of
                     Nothing -> Left $ "Unknown writer: " ++ writerName
                     Just (StringWriter r) -> Right $ StringWriter $
                             \o -> r o{ writerExtensions = setExts $
                                              getDefaultExtensions writerName }
                     Just (ByteStringWriter r) -> Right $ ByteStringWriter $
                             \o -> r o{ writerExtensions = setExts $
                                              getDefaultExtensions writerName }

readJSON :: ReaderOptions -> String -> Either PandocError Pandoc
readJSON _ = mapLeft PandocParseError . eitherDecode' . UTF8.fromStringLazy

writeJSON :: WriterOptions -> Pandoc -> String
writeJSON _ = UTF8.toStringLazy . encode

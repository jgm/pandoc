{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
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
> markdownToRST = toUTF8 .
>   (writeRST defaultWriterOptions {writerReferenceLinks = True}) .
>   (readMarkdown defaultParserState) .  fromUTF8
> 
> main = interact markdownToRST

-}

module Text.Pandoc
               ( 
               -- * Definitions
               module Text.Pandoc.Definition
               -- * Readers: converting /to/ Pandoc format
               , readMarkdown
               , readRST
               , readLaTeX
               , readHtml
               -- * Parser state used in readers
               , ParserState (..)
               , defaultParserState
               , ParserContext (..)
               , QuoteContext (..)
               , KeyTable
               , NoteTable
               , HeaderType (..)
               -- * Writers: converting /from/ Pandoc format
               , writeMarkdown
               , writeRST
               , writeLaTeX
               , writeConTeXt
               , writeHtml
               , writeHtmlString
               , writeS5
               , writeS5String
               , writeDocbook
               , writeMan
               , writeRTF
               , prettyPandoc
               -- * Writer options used in writers 
               , WriterOptions (..)
               , defaultWriterOptions
               -- * Default headers for various output formats
               , module Text.Pandoc.Writers.DefaultHeaders
               -- * Functions for converting to and from UTF-8
               , module Text.Pandoc.UTF8
             ) where

import Text.Pandoc.Definition
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.HTML
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.RST 
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.S5
import Text.Pandoc.Writers.Docbook
import Text.Pandoc.Writers.Man
import Text.Pandoc.Writers.RTF 
import Text.Pandoc.Writers.DefaultHeaders
import Text.Pandoc.UTF8
import Text.Pandoc.Shared


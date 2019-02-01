{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
#ifdef DERIVE_JSON_VIA_TH
{-# LANGUAGE TemplateHaskell     #-}
#endif
{-
Copyright (C) 2006-2019 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.App.CommandLineOptions
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App.CommandLineOptions (
            parseOptions
          , options
          , engines
          ) where
import Prelude
import Control.Monad
import Control.Monad.Trans
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder,
         defConfig, Indent(..), NumberFormat(..))
import Data.Char (toLower, toUpper)
import Data.List (intercalate, sort)
#ifdef _WINDOWS
#if MIN_VERSION_base(4,12,0)
import Data.List (isPrefixOf)
#endif
#endif
import Data.Maybe (fromMaybe)
import Skylighting (Style, Syntax (..), defaultSyntaxMap, parseTheme,
                    pygments)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.Pandoc
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..))
import Text.Pandoc.Filter (Filter (..))
import Text.Pandoc.Highlighting (highlightingStyles)
import Text.Pandoc.Writers.Math (defaultMathJaxURL, defaultKaTeXURL)
import Text.Pandoc.Shared (ordNub, safeRead)
import Text.Printf

#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
import System.Directory (getAppUserDataDirectory)
#else
import Paths_pandoc (getDataDir)
import System.Directory (getAppUserDataDirectory, getDirectoryContents)
#endif

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

parseOptions :: [OptDescr (Opt -> IO Opt)] -> Opt -> IO Opt
parseOptions options' defaults = do
  rawArgs <- map UTF8.decodeArg <$> getArgs
  prg <- getProgName

  let (actions, args, unrecognizedOpts, errors) =
           getOpt' Permute options' rawArgs

  let unknownOptionErrors =
       foldr (handleUnrecognizedOption . takeWhile (/= '=')) []
       unrecognizedOpts

  unless (null errors && null unknownOptionErrors) $
     E.throwIO $ PandocOptionError $
        concat errors ++ unlines unknownOptionErrors ++
        ("Try " ++ prg ++ " --help for more information.")

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaults) actions
  return (opts{ optInputFiles = map normalizePath args })

latexEngines :: [String]
latexEngines  = ["pdflatex", "lualatex", "xelatex"]

htmlEngines :: [String]
htmlEngines  = ["wkhtmltopdf", "weasyprint", "prince"]

engines :: [(String, String)]
engines = map ("html",) htmlEngines ++
          map ("html5",) htmlEngines ++
          map ("latex",) latexEngines ++
          map ("beamer",) latexEngines ++
          [ ("ms", "pdfroff")
          , ("context", "context")
          ]

pdfEngines :: [String]
pdfEngines = ordNub $ map snd engines

lookupHighlightStyle :: String -> IO (Maybe Style)
lookupHighlightStyle s
  | takeExtension s == ".theme" = -- attempt to load KDE theme
    do contents <- B.readFile s
       case parseTheme contents of
            Left _    -> E.throwIO $ PandocOptionError $
                           "Could not read highlighting theme " ++ s
            Right sty -> return (Just sty)
  | otherwise =
  case lookup (map toLower s) highlightingStyles of
       Just sty -> return (Just sty)
       Nothing  -> E.throwIO $ PandocOptionError $
                      "Unknown highlight-style " ++ s

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader =
                                              Just (map toLower arg) })
                  "FORMAT")
                 ""

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = Just arg })
                  "FORMAT")
                 ""

    , Option "o" ["output"]
                 (ReqArg
                  (\arg opt -> return opt { optOutputFile =
                                             Just (normalizePath arg) })
                  "FILE")
                 "" -- "Name of output file"

    , Option "" ["data-dir"]
                 (ReqArg
                  (\arg opt -> return opt { optDataDir =
                                  Just (normalizePath arg) })
                 "DIRECTORY") -- "Directory containing pandoc data files."
                ""

    , Option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 && t < 6 ->
                               return opt{ optBaseHeaderLevel = t }
                           _              -> E.throwIO $ PandocOptionError
                                               "base-header-level must be 1-5")
                  "NUMBER")
                 "" -- "Headers base level"

    , Option "" ["strip-empty-paragraphs"]
                 (NoArg
                  (\opt -> do
                      deprecatedOption "--stripEmptyParagraphs"
                        "Use +empty_paragraphs extension."
                      return opt{ optStripEmptyParagraphs = True }))
                 "" -- "Strip empty paragraphs"

    , Option "" ["indented-code-classes"]
                  (ReqArg
                   (\arg opt -> return opt { optIndentedCodeClasses = words $
                                             map (\c -> if c == ',' then ' ' else c) arg })
                   "STRING")
                  "" -- "Classes (whitespace- or comma-separated) to use for indented code-blocks"

    , Option "F" ["filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                                    JSONFilter (normalizePath arg) :
                                    optFilters opt })
                  "PROGRAM")
                 "" -- "External JSON filter"

    , Option "" ["lua-filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                                    LuaFilter (normalizePath arg) :
                                    optFilters opt })
                  "SCRIPTPATH")
                 "" -- "Lua filter"

    , Option "p" ["preserve-tabs"]
                 (NoArg
                  (\opt -> return opt { optPreserveTabs = True }))
                 "" -- "Preserve tabs instead of converting to spaces"

    , Option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 -> return opt { optTabStop = t }
                           _              -> E.throwIO $ PandocOptionError
                                  "tab-stop must be a number greater than 0")
                  "NUMBER")
                 "" -- "Tab stop (default 4)"

    , Option "" ["track-changes"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "accept" -> return AcceptChanges
                            "reject" -> return RejectChanges
                            "all"    -> return AllChanges
                            _        -> E.throwIO $ PandocOptionError
                               ("Unknown option for track-changes: " ++ arg)
                     return opt { optTrackChanges = action })
                  "accept|reject|all")
                 "" -- "Accepting or reject MS Word track-changes.""

    , Option "" ["file-scope"]
                 (NoArg
                  (\opt -> return opt { optFileScope = True }))
                 "" -- "Parse input files before combining"

    , Option "" ["extract-media"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optExtractMedia =
                                  Just (normalizePath arg) })
                  "PATH")
                 "" -- "Directory to which to extract embedded media"

    , Option "s" ["standalone"]
                 (NoArg
                  (\opt -> return opt { optStandalone = True }))
                 "" -- "Include needed header and footer on output"

    , Option "" ["template"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optTemplate = Just (normalizePath arg),
                                 optStandalone = True })
                  "FILE")
                 "" -- "Use custom template"

    , Option "M" ["metadata"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optMetadata = (key, val) : optMetadata opt })
                  "KEY[:VALUE]")
                 ""

    , Option "" ["metadata-file"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadataFile =
                                   Just (normalizePath arg) })
                  "FILE")
                 ""

    , Option "V" ["variable"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optVariables = (key, val) : optVariables opt })
                  "KEY[:VALUE]")
                 ""

    , Option "D" ["print-default-template"]
                 (ReqArg
                  (\arg _ -> do
                     templ <- runIO $ do
                                setUserDataDir Nothing
                                getDefaultTemplate arg
                     case templ of
                          Right "" -> -- e.g. for docx, odt, json:
                            E.throwIO $ PandocCouldNotFindDataFileError
                               ("templates/default." ++ arg)
                          Right t -> UTF8.hPutStr stdout t
                          Left e  -> E.throwIO e
                     exitSuccess)
                  "FORMAT")
                 "" -- "Print default template for FORMAT"

    , Option "" ["print-default-data-file"]
                 (ReqArg
                  (\arg _ -> do
                     runIOorExplode $
                       readDefaultDataFile arg >>= liftIO . BS.hPutStr stdout
                     exitSuccess)
                  "FILE")
                  "" -- "Print default data file"

    , Option "" ["print-highlight-style"]
                 (ReqArg
                  (\arg _ -> do
                     sty <- fromMaybe pygments <$> lookupHighlightStyle arg
                     B.putStr $ encodePretty'
                       defConfig{confIndent = Spaces 4
                                ,confCompare = keyOrder
                                  (map T.pack
                                   ["text-color"
                                   ,"background-color"
                                   ,"line-number-color"
                                   ,"line-number-background-color"
                                   ,"bold"
                                   ,"italic"
                                   ,"underline"
                                   ,"text-styles"])
                                ,confNumFormat = Generic
                                ,confTrailingNewline = True} sty
                     exitSuccess)
                  "STYLE|FILE")
                 "" -- "Print default template for FORMAT"

    , Option "" ["dpi"]
                 (ReqArg
                  (\arg opt ->
                    case safeRead arg of
                         Just t | t > 0 -> return opt { optDpi = t }
                         _              -> E.throwIO $ PandocOptionError
                                        "dpi must be a number greater than 0")
                  "NUMBER")
                 "" -- "Dpi (default 96)"

    , Option "" ["eol"]
                 (ReqArg
                  (\arg opt ->
                    case toLower <$> arg of
                      "crlf"   -> return opt { optEol = CRLF }
                      "lf"     -> return opt { optEol = LF }
                      "native" -> return opt { optEol = Native }
                      -- mac-syntax (cr) is not supported in ghc-base.
                      _      -> E.throwIO $ PandocOptionError
                                "--eol must be crlf, lf, or native")
                  "crlf|lf|native")
                 "" -- "EOL (default OS-dependent)"

    , Option "" ["wrap"]
                 (ReqArg
                  (\arg opt ->
                    case safeRead ("Wrap" ++ uppercaseFirstLetter arg) of
                          Just o   -> return opt { optWrapText = o }
                          Nothing  -> E.throwIO $ PandocOptionError
                                     "--wrap must be auto, none, or preserve")
                 "auto|none|preserve")
                 "" -- "Option for wrapping text in output"

    , Option "" ["columns"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 -> return opt { optColumns = t }
                           _              -> E.throwIO $ PandocOptionError
                                   "columns must be a number greater than 0")
                 "NUMBER")
                 "" -- "Length of line in characters"

    , Option "" ["strip-comments"]
                (NoArg
                 (\opt -> return opt { optStripComments = True }))
               "" -- "Strip HTML comments"

    , Option "" ["toc", "table-of-contents"]
                (NoArg
                 (\opt -> return opt { optTableOfContents = True }))
               "" -- "Include table of contents"

    , Option "" ["toc-depth"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optTOCDepth = t }
                           _      -> E.throwIO $ PandocOptionError
                                    "TOC level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Number of levels to include in TOC"

    , Option "" ["no-highlight"]
                (NoArg
                 (\opt -> return opt { optHighlightStyle = Nothing }))
                 "" -- "Don't highlight source code"

    , Option "" ["highlight-style"]
                (ReqArg
                 (\arg opt -> lookupHighlightStyle arg >>= \style ->
                     return opt{ optHighlightStyle = style })
                 "STYLE|FILE")
                 "" -- "Style for highlighted code"

    , Option "" ["syntax-definition"]
                (ReqArg
                 (\arg opt -> do
                   let tr c d = map (\x -> if x == c then d else x)
                   let arg' = case arg of -- see #4836
                                   -- HXT confuses Windows path with URI
                                   _:':':'\\':_ ->
                                       "file:///" ++ tr '\\' '/' arg
                                   _ -> normalizePath arg
                   return opt{ optSyntaxDefinitions = arg' :
                                optSyntaxDefinitions opt })
                 "FILE")
                "" -- "Syntax definition (xml) file"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeInHeader =
                                              arg : optIncludeInHeader opt,
                                            optStandalone = True })
                  "FILE")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeBeforeBody =
                                              arg : optIncludeBeforeBody opt,
                                           optStandalone = True })
                  "FILE")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeAfterBody =
                                              arg : optIncludeAfterBody opt,
                                           optStandalone = True })
                  "FILE")
                 "" -- "File to include after document body"

    , Option "" ["resource-path"]
                (ReqArg
                  (\arg opt -> return opt { optResourcePath =
                                   splitSearchPath arg })
                   "SEARCHPATH")
                  "" -- "Paths to search for images and other resources"

    , Option "" ["request-header"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optRequestHeaders =
                       (key, val) : optRequestHeaders opt })
                  "NAME:VALUE")
                 ""

    , Option "" ["self-contained"]
                 (NoArg
                  (\opt -> return opt { optSelfContained = True,
                                        optStandalone = True }))
                 "" -- "Make slide shows include all the needed js and css"

    , Option "" ["html-q-tags"]
                 (NoArg
                  (\opt ->
                     return opt { optHtmlQTags = True }))
                 "" -- "Use <q> tags for quotes in HTML"

    , Option "" ["ascii"]
                 (NoArg
                  (\opt -> return opt { optAscii = True }))
                 ""  -- "Prefer ASCII output"

    , Option "" ["reference-links"]
                 (NoArg
                  (\opt -> return opt { optReferenceLinks = True } ))
                 "" -- "Use reference links in parsing HTML"

    , Option "" ["reference-location"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "block"    -> return EndOfBlock
                            "section"  -> return EndOfSection
                            "document" -> return EndOfDocument
                            _        -> E.throwIO $ PandocOptionError
                               ("Unknown option for reference-location: " ++ arg)
                     return opt { optReferenceLocation = action })
                  "block|section|document")
                 "" -- "Accepting or reject MS Word track-changes.""

    , Option "" ["atx-headers"]
                 (NoArg
                  (\opt -> return opt { optSetextHeaders = False } ))
                 "" -- "Use atx-style headers for markdown"

    , Option "" ["top-level-division"]
                 (ReqArg
                  (\arg opt -> do
                      let tldName = "TopLevel" ++ uppercaseFirstLetter arg
                      case safeRead tldName of
                        Just tlDiv -> return opt { optTopLevelDivision = tlDiv }
                        _       -> E.throwIO $ PandocOptionError
                                     ("Top-level division must be " ++
                                      "section,  chapter, part, or default"))
                   "section|chapter|part")
                 "" -- "Use top-level division type in LaTeX, ConTeXt, DocBook"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "" -- "Number sections in LaTeX"

    , Option "" ["number-offset"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead ('[':arg ++ "]") of
                           Just ns -> return opt { optNumberOffset = ns,
                                                   optNumberSections = True }
                           _      -> E.throwIO $ PandocOptionError
                                       "could not parse number-offset")
                 "NUMBERS")
                 "" -- "Starting number for sections, subsections, etc."

    , Option "" ["listings"]
                 (NoArg
                  (\opt -> return opt { optListings = True }))
                 "" -- "Use listings package for LaTeX code blocks"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "" -- "Make list items display incrementally in Slidy/Slideous/S5"

    , Option "" ["slide-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optSlideLevel = Just t }
                           _      -> E.throwIO $ PandocOptionError
                                    "slide level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Force header level for slides"

    , Option "" ["section-divs"]
                 (NoArg
                  (\opt -> return opt { optSectionDivs = True }))
                 "" -- "Put sections in div tags in HTML"

    , Option "" ["default-image-extension"]
                 (ReqArg
                  (\arg opt -> return opt { optDefaultImageExtension = arg })
                   "extension")
                  "" -- "Default extension for extensionless images"

    , Option "" ["email-obfuscation"]
                 (ReqArg
                  (\arg opt -> do
                     method <- case arg of
                            "references" -> return ReferenceObfuscation
                            "javascript" -> return JavascriptObfuscation
                            "none"       -> return NoObfuscation
                            _            -> E.throwIO $ PandocOptionError
                               ("Unknown obfuscation method: " ++ arg)
                     return opt { optEmailObfuscation = method })
                  "none|javascript|references")
                 "" -- "Method for obfuscating email in HTML"

     , Option "" ["id-prefix"]
                  (ReqArg
                   (\arg opt -> return opt { optIdentifierPrefix = arg })
                   "STRING")
                  "" -- "Prefix to add to automatically generated HTML identifiers"

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt -> do
                    let newvars = ("title-prefix", arg) : optVariables opt
                    return opt { optVariables = newvars,
                                 optStandalone = True })
                  "STRING")
                 "" -- "String to prefix to HTML window title"

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> return opt{ optCss = arg : optCss opt })
                  -- add new link to end, so it is included in proper order
                  "URL")
                 "" -- "Link to CSS style sheet"

    , Option "" ["reference-doc"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceDoc = Just arg })
                  "FILE")
                 "" -- "Path of custom reference doc"

    , Option "" ["epub-subdirectory"]
             (ReqArg
                  (\arg opt ->
                     return opt { optEpubSubdirectory = arg })
                  "DIRNAME")
                 "" -- "Name of subdirectory for epub content in OCF container"

    , Option "" ["epub-cover-image"]
                 (ReqArg
                  (\arg opt ->
                     return opt { optVariables =
                                 ("epub-cover-image", arg) : optVariables opt })
                  "FILE")
                 "" -- "Path of epub cover image"

    , Option "" ["epub-metadata"]
                 (ReqArg
                  (\arg opt -> return opt { optEpubMetadata = Just arg })
                  "FILE")
                 "" -- "Path of epub metadata file"

    , Option "" ["epub-embed-font"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optEpubFonts = arg : optEpubFonts opt })
                  "FILE")
                 "" -- "Directory of fonts to embed"

    , Option "" ["epub-chapter-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optEpubChapterLevel = t }
                           _      -> E.throwIO $ PandocOptionError
                                    "chapter level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Header level at which to split chapters in EPUB"

    , Option "" ["pdf-engine"]
                 (ReqArg
                  (\arg opt -> do
                     let b = takeBaseName arg
                     if b `elem` pdfEngines
                        then return opt { optPdfEngine = Just arg }
                        else E.throwIO $ PandocOptionError $ "pdf-engine must be one of "
                               ++ intercalate ", " pdfEngines)
                  "PROGRAM")
                 "" -- "Name of program to use in generating PDF"

    , Option "" ["pdf-engine-opt"]
                 (ReqArg
                  (\arg opt -> do
                      let oldArgs = optPdfEngineArgs opt
                      return opt { optPdfEngineArgs = oldArgs ++ [arg]})
                  "STRING")
                 "" -- "Flags to pass to the PDF-engine, all instances of this option are accumulated and used"

    , Option "" ["bibliography"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadata =
                                 ("bibliography", arg) : optMetadata opt })
                   "FILE")
                 ""

     , Option "" ["csl"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata =
                                   ("csl", arg) : optMetadata opt })
                   "FILE")
                 ""

     , Option "" ["citation-abbreviations"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata =
                              ("citation-abbreviations", arg): optMetadata opt })
                   "FILE")
                 ""

    , Option "" ["natbib"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Natbib }))
                 "" -- "Use natbib cite commands in LaTeX output"

    , Option "" ["biblatex"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Biblatex }))
                 "" -- "Use biblatex cite commands in LaTeX output"

    , Option "" ["mathml"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = MathML }))
                 "" -- "Use mathml for HTML math"

    , Option "" ["webtex"]
                 (OptArg
                  (\arg opt -> do
                      let url' = fromMaybe "https://latex.codecogs.com/png.latex?" arg
                      return opt { optHTMLMathMethod = WebTeX url' })
                  "URL")
                 "" -- "Use web service for HTML math"

    , Option "" ["mathjax"]
                 (OptArg
                  (\arg opt -> do
                      let url' = fromMaybe (defaultMathJaxURL ++
                                  "MathJax.js?config=TeX-AMS_CHTML-full") arg
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 "" -- "Use MathJax for HTML math"

    , Option "" ["katex"]
                 (OptArg
                  (\arg opt ->
                      return opt
                        { optHTMLMathMethod = KaTeX $
                           fromMaybe defaultKaTeXURL arg })
                  "URL")
                  "" -- Use KaTeX for HTML Math

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = GladTeX }))
                 "" -- "Use gladtex for HTML math"

    , Option "" ["abbreviations"]
                (ReqArg
                 (\arg opt -> return opt { optAbbreviations = Just arg })
                "FILE")
                "" -- "Specify file for custom abbreviations"

    , Option "" ["trace"]
                 (NoArg
                  (\opt -> return opt { optTrace = True }))
                 "" -- "Turn on diagnostic tracing in readers."

    , Option "" ["dump-args"]
                 (NoArg
                  (\opt -> return opt { optDumpArgs = True }))
                 "" -- "Print output filename and arguments to stdout."

    , Option "" ["ignore-args"]
                 (NoArg
                  (\opt -> return opt { optIgnoreArgs = True }))
                 "" -- "Ignore command-line arguments."

    , Option "" ["verbose"]
                 (NoArg
                  (\opt -> return opt { optVerbosity = INFO }))
                 "" -- "Verbose diagnostic output."

    , Option "" ["quiet"]
                 (NoArg
                  (\opt -> return opt { optVerbosity = ERROR }))
                 "" -- "Suppress warnings."

    , Option "" ["fail-if-warnings"]
                 (NoArg
                  (\opt -> return opt { optFailIfWarnings = True }))
                 "" -- "Exit with error status if there were  warnings."

    , Option "" ["log"]
                 (ReqArg
                  (\arg opt -> return opt{ optLogFile = Just arg })
                "FILE")
                "" -- "Log messages in JSON format to this file."

    , Option "" ["bash-completion"]
                 (NoArg
                  (\_ -> do
                     datafiles <- getDataFileNames
                     tpl <- runIOorExplode $
                              UTF8.toString <$>
                                readDefaultDataFile "bash_completion.tpl"
                     let optnames (Option shorts longs _ _) =
                           map (\c -> ['-',c]) shorts ++
                           map ("--" ++) longs
                     let allopts = unwords (concatMap optnames options)
                     UTF8.hPutStrLn stdout $ printf tpl allopts
                         (unwords readersNames)
                         (unwords writersNames)
                         (unwords $ map fst highlightingStyles)
                         (unwords datafiles)
                     exitSuccess ))
                 "" -- "Print bash completion script"

    , Option "" ["list-input-formats"]
                 (NoArg
                  (\_ -> do
                     mapM_ (UTF8.hPutStrLn stdout) readersNames
                     exitSuccess ))
                 ""

    , Option "" ["list-output-formats"]
                 (NoArg
                  (\_ -> do
                     mapM_ (UTF8.hPutStrLn stdout) writersNames
                     exitSuccess ))
                 ""

    , Option "" ["list-extensions"]
                 (OptArg
                  (\arg _ -> do
                     let exts = getDefaultExtensions (fromMaybe "markdown" arg)
                     let showExt x = (if extensionEnabled x exts
                                         then '+'
                                         else '-') : drop 4 (show x)
                     mapM_ (UTF8.hPutStrLn stdout . showExt)
                               ([minBound..maxBound] :: [Extension])
                     exitSuccess )
                  "FORMAT")
                 ""

    , Option "" ["list-highlight-languages"]
                 (NoArg
                  (\_ -> do
                     let langs = [ T.unpack (T.toLower (sShortname s))
                                 | s <- M.elems defaultSyntaxMap
                                 , sShortname s `notElem`
                                    [T.pack "Alert", T.pack "Alert_indent"]
                                 ]
                     mapM_ (UTF8.hPutStrLn stdout) langs
                     exitSuccess ))
                 ""

    , Option "" ["list-highlight-styles"]
                 (NoArg
                  (\_ -> do
                     mapM_ (UTF8.hPutStrLn stdout . fst) highlightingStyles
                     exitSuccess ))
                 ""

    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     defaultDatadir <- E.catch
                            (getAppUserDataDirectory "pandoc")
                            (\e -> let _ = (e :: E.SomeException)
                                   in  return "")
                     UTF8.hPutStrLn stdout (prg ++ " " ++ pandocVersion ++
                       compileInfo ++ "\nDefault user data directory: " ++
                       defaultDatadir ++ copyrightMessage)
                     exitSuccess ))
                 "" -- "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     UTF8.hPutStr stdout (usageMessage prg options)
                     exitSuccess ))
                 "" -- "Show help"
    ]

getDataFileNames :: IO [FilePath]
getDataFileNames = do
#ifdef EMBED_DATA_FILES
  let allDataFiles = map fst dataFiles
#else
  allDataFiles <- filter (\x -> x /= "." && x /= "..") <$>
                      (getDataDir >>= getDirectoryContents)
#endif
  return $ "reference.docx" : "reference.odt" : "reference.pptx" : allDataFiles

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName = usageInfo (programName ++ " [OPTIONS] [FILES]")

copyrightMessage :: String
copyrightMessage = intercalate "\n" [
  "",
  "Copyright (C) 2006-2019 John MacFarlane",
  "Web:  http://pandoc.org",
  "This is free software; see the source for copying conditions.",
  "There is no warranty, not even for merchantability or fitness",
  "for a particular purpose." ]

compileInfo :: String
compileInfo =
  "\nCompiled with pandoc-types " ++ VERSION_pandoc_types ++ ", texmath " ++
  VERSION_texmath ++ ", skylighting " ++ VERSION_skylighting

handleUnrecognizedOption :: String -> [String] -> [String]
handleUnrecognizedOption "--smart" =
  (("--smart/-S has been removed.  Use +smart or -smart extension instead.\n" ++
    "For example: pandoc -f markdown+smart -t markdown-smart.") :)
handleUnrecognizedOption "--normalize" =
  ("--normalize has been removed.  Normalization is now automatic." :)
handleUnrecognizedOption "-S" = handleUnrecognizedOption "--smart"
handleUnrecognizedOption "--old-dashes" =
  ("--old-dashes has been removed.  Use +old_dashes extension instead." :)
handleUnrecognizedOption "--no-wrap" =
  ("--no-wrap has been removed.  Use --wrap=none instead." :)
handleUnrecognizedOption "--latex-engine" =
  ("--latex-engine has been removed.  Use --pdf-engine instead." :)
handleUnrecognizedOption "--latex-engine-opt" =
  ("--latex-engine-opt has been removed.  Use --pdf-engine-opt instead." :)
handleUnrecognizedOption "--chapters" =
  ("--chapters has been removed. Use --top-level-division=chapter instead." :)
handleUnrecognizedOption "--reference-docx" =
  ("--reference-docx has been removed. Use --reference-doc instead." :)
handleUnrecognizedOption "--reference-odt" =
  ("--reference-odt has been removed. Use --reference-doc instead." :)
handleUnrecognizedOption "--parse-raw" =
  ("--parse-raw/-R has been removed. Use +raw_html or +raw_tex extension.\n" :)
handleUnrecognizedOption "--epub-stylesheet" =
  ("--epub-stylesheet has been removed. Use --css instead.\n" :)
handleUnrecognizedOption "-R" = handleUnrecognizedOption "--parse-raw"
handleUnrecognizedOption x =
  (("Unknown option " ++ x ++ ".") :)

uppercaseFirstLetter :: String -> String
uppercaseFirstLetter (c:cs) = toUpper c : cs
uppercaseFirstLetter []     = []

readersNames :: [String]
readersNames = sort (map fst (readers :: [(String, Reader PandocIO)]))

writersNames :: [String]
writersNames = sort (map fst (writers :: [(String, Writer PandocIO)]))

splitField :: String -> (String, String)
splitField s =
  case break (`elem` ":=") s of
       (k,_:v) -> (k,v)
       (k,[])  -> (k,"true")

deprecatedOption :: String -> String -> IO ()
deprecatedOption o msg =
  runIO (report $ Deprecated o msg) >>=
    \r -> case r of
       Right () -> return ()
       Left e   -> E.throwIO e

-- On Windows with ghc 8.6+, we need to rewrite paths
-- beginning with \\ to \\?\UNC\. -- See #5127.
normalizePath :: FilePath -> FilePath
#ifdef _WINDOWS
#if MIN_VERSION_base(4,12,0)
normalizePath fp =
  if "\\\\" `isPrefixOf` fp && not ("\\\\?\\" `isPrefixOf` fp)
    then "\\\\?\\UNC\\" ++ drop 2 fp
    else fp
#else
normalizePath = id
#endif
#else
normalizePath = id
#endif

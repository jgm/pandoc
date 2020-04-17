{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{- |
   Module      : Text.Pandoc.App.CommandLineOptions
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
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
          , lookupHighlightStyle
          , setVariable
          ) where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except (throwError)
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder,
         defConfig, Indent(..), NumberFormat(..))
import Data.Bifunctor (second)
import Data.Char (toLower)
import Data.List (intercalate, sort)
#ifdef _WINDOWS
#if MIN_VERSION_base(4,12,0)
import Data.List (isPrefixOf)
#endif
#endif
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Safe (tailDef)
import Skylighting (Style, Syntax (..), defaultSyntaxMap, parseTheme)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.DocTemplates (Context (..), ToContext (toVal), Val (..))
import Text.Pandoc
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), IpynbOutput (..), addMeta)
import Text.Pandoc.Filter (Filter (..))
import Text.Pandoc.Highlighting (highlightingStyles)
import Text.Pandoc.Shared (ordNub, elemText, safeStrRead, defaultUserDataDirs, findM)
import Text.Printf

#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#else
import Paths_pandoc (getDataDir)
import System.Directory (getDirectoryContents)
#endif

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.YAML as Y
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
     E.throwIO $ PandocOptionError $ T.pack $
        concat errors ++ unlines unknownOptionErrors ++
        ("Try " ++ prg ++ " --help for more information.")

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaults) actions
  let mbArgs = case args of
                 [] -> Nothing
                 xs -> Just xs
  return $ opts{ optInputFiles =
                   map normalizePath <$> (optInputFiles opts <> mbArgs)
               , optStandalone = -- certain other options imply standalone
                   optStandalone opts ||
                     isJust (optTemplate opts) ||
                     optSelfContained opts ||
                     not (null (optIncludeInHeader opts)) ||
                     not (null (optIncludeBeforeBody opts)) ||
                     not (null (optIncludeAfterBody opts)) }

latexEngines :: [String]
latexEngines  = ["pdflatex", "lualatex", "xelatex", "latexmk", "tectonic"]

htmlEngines :: [String]
htmlEngines  = ["wkhtmltopdf", "weasyprint", "prince"]

engines :: [(Text, String)]
engines = map ("html",) htmlEngines ++
          map ("html5",) htmlEngines ++
          map ("latex",) latexEngines ++
          map ("beamer",) latexEngines ++
          [ ("ms", "pdfroff")
          , ("context", "context")
          ]

pdfEngines :: [String]
pdfEngines = ordNub $ map snd engines

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optFrom =
                                              Just (T.toLower $ T.pack arg) })
                  "FORMAT")
                 ""

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optTo = Just $ T.pack arg })
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

    , Option "M" ["metadata"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optMetadata = addMeta key val $
                                                 optMetadata opt })
                  "KEY[:VALUE]")
                 ""

    , Option "" ["metadata-file"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadataFiles =
                      optMetadataFiles opt ++ [normalizePath arg] })
                  "FILE")
                 ""

    , Option "d" ["defaults"]
                 (ReqArg
                  (\arg opt -> applyDefaults opt arg
                  )
                  "FILE")
                ""

    , Option "" ["file-scope"]
                 (NoArg
                  (\opt -> return opt { optFileScope = True }))
                 "" -- "Parse input files before combining"

    , Option "s" ["standalone"]
                 (NoArg
                  (\opt -> return opt { optStandalone = True }))
                 "" -- "Include needed header and footer on output"

    , Option "" ["template"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optTemplate = Just (normalizePath arg) })
                  "FILE")
                 "" -- "Use custom template"

    , Option "V" ["variable"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optVariables =
                                  setVariable (T.pack key) (T.pack val) $
                                    optVariables opt })
                  "KEY[:VALUE]")
                 ""

    , Option "" ["wrap"]
                 (ReqArg
                  (\arg opt ->
                    case arg of
                      "auto" -> return opt{ optWrap = WrapAuto }
                      "none" -> return opt{ optWrap = WrapNone }
                      "preserve" -> return opt{ optWrap = WrapPreserve }
                      _      -> E.throwIO $ PandocOptionError
                                 "--wrap must be auto, none, or preserve")
                 "auto|none|preserve")
                 "" -- "Option for wrapping text in output"

    , Option "" ["ascii"]
                 (NoArg
                  (\opt -> return opt { optAscii = True }))
                 ""  -- "Prefer ASCII output"

    , Option "" ["toc", "table-of-contents"]
                (NoArg
                 (\opt -> return opt { optTableOfContents = True }))
               "" -- "Include table of contents"

    , Option "" ["toc-depth"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optTOCDepth = t }
                           _ -> E.throwIO $ PandocOptionError
                                "TOC level must be a number 1-6")
                 "NUMBER")
                 "" -- "Number of levels to include in TOC"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "" -- "Number sections"

    , Option "" ["number-offset"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead ("[" <> arg <> "]") of
                           Just ns -> return opt { optNumberOffset = ns,
                                                   optNumberSections = True }
                           _      -> E.throwIO $ PandocOptionError
                                       "could not parse number-offset")
                 "NUMBERS")
                 "" -- "Starting number for sections, subsections, etc."

    , Option "" ["top-level-division"]
                 (ReqArg
                  (\arg opt ->
                      case arg of
                        "section" -> return opt{ optTopLevelDivision =
                                        TopLevelSection }
                        "chapter" -> return opt{ optTopLevelDivision =
                                        TopLevelChapter }
                        "part"    -> return opt{ optTopLevelDivision =
                                        TopLevelPart }
                        "default" -> return opt{ optTopLevelDivision =
                                        TopLevelDefault }
                        _ -> E.throwIO $ PandocOptionError $
                                "Top-level division must be " <>
                                "section,  chapter, part, or default" )
                   "section|chapter|part")
                 "" -- "Use top-level division type in LaTeX, ConTeXt, DocBook"

    , Option "" ["extract-media"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optExtractMedia =
                                  Just (normalizePath arg) })
                  "PATH")
                 "" -- "Directory to which to extract embedded media"

    , Option "" ["resource-path"]
                (ReqArg
                  (\arg opt -> return opt { optResourcePath =
                                   splitSearchPath arg })
                   "SEARCHPATH")
                  "" -- "Paths to search for images and other resources"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeInHeader =
                                             optIncludeInHeader opt ++ [arg] })
                  "FILE")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeBeforeBody =
                                            optIncludeBeforeBody opt ++ [arg] })
                  "FILE")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeAfterBody =
                                            optIncludeAfterBody opt ++ [arg] })
                  "FILE")
                 "" -- "File to include after document body"

    , Option "" ["no-highlight"]
                (NoArg
                 (\opt -> return opt { optHighlightStyle = Nothing }))
                 "" -- "Don't highlight source code"

    , Option "" ["highlight-style"]
                (ReqArg
                 (\arg opt ->
                     return opt{ optHighlightStyle = Just $ T.pack arg })
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

    , Option "" ["dpi"]
                 (ReqArg
                  (\arg opt ->
                    case safeStrRead arg of
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

    , Option "" ["columns"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t > 0 -> return opt { optColumns = t }
                           _              -> E.throwIO $ PandocOptionError
                                   "columns must be a number greater than 0")
                 "NUMBER")
                 "" -- "Length of line in characters"

    , Option "p" ["preserve-tabs"]
                 (NoArg
                  (\opt -> return opt { optPreserveTabs = True }))
                 "" -- "Preserve tabs instead of converting to spaces"

    , Option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t | t > 0 -> return opt { optTabStop = t }
                           _              -> E.throwIO $ PandocOptionError
                                  "tab-stop must be a number greater than 0")
                  "NUMBER")
                 "" -- "Tab stop (default 4)"

    , Option "" ["pdf-engine"]
                 (ReqArg
                  (\arg opt -> do
                     let b = takeBaseName arg
                     if b `elem` pdfEngines
                        then return opt { optPdfEngine = Just arg }
                        else E.throwIO $ PandocOptionError $ T.pack $ "pdf-engine must be one of "
                               ++ intercalate ", " pdfEngines)
                  "PROGRAM")
                 "" -- "Name of program to use in generating PDF"

    , Option "" ["pdf-engine-opt"]
                 (ReqArg
                  (\arg opt -> do
                      let oldArgs = optPdfEngineOpts opt
                      return opt { optPdfEngineOpts = oldArgs ++ [arg]})
                  "STRING")
                 "" -- "Flags to pass to the PDF-engine, all instances of this option are accumulated and used"

    , Option "" ["reference-doc"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceDoc = Just arg })
                  "FILE")
                 "" -- "Path of custom reference doc"

    , Option "" ["self-contained"]
                 (NoArg
                  (\opt -> return opt { optSelfContained = True }))
                 "" -- "Make slide shows include all the needed js and css"

    , Option "" ["request-header"]
                 (ReqArg
                  (\arg opt -> do
                     let (key, val) = splitField arg
                     return opt{ optRequestHeaders =
                       (T.pack key, T.pack val) : optRequestHeaders opt })
                  "NAME:VALUE")
                 ""

    , Option "" ["no-check-certificate"]
                (NoArg
                 (\opt -> return opt { optNoCheckCertificate = True }))
                "" -- "Disable certificate validation"

    , Option "" ["abbreviations"]
                (ReqArg
                 (\arg opt -> return opt { optAbbreviations = Just arg })
                "FILE")
                "" -- "Specify file for custom abbreviations"

    , Option "" ["indented-code-classes"]
                  (ReqArg
                   (\arg opt -> return opt { optIndentedCodeClasses = T.words $
                                             T.map (\c -> if c == ',' then ' ' else c) $
                                             T.pack arg })
                   "STRING")
                  "" -- "Classes (whitespace- or comma-separated) to use for indented code-blocks"

    , Option "" ["default-image-extension"]
                 (ReqArg
                  (\arg opt -> return opt { optDefaultImageExtension = T.pack arg })
                   "extension")
                  "" -- "Default extension for extensionless images"

    , Option "F" ["filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                      optFilters opt ++ [JSONFilter (normalizePath arg)] })
                  "PROGRAM")
                 "" -- "External JSON filter"

    , Option "L" ["lua-filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                      optFilters opt ++ [LuaFilter (normalizePath arg)] })
                  "SCRIPTPATH")
                 "" -- "Lua filter"

    , Option "" ["shift-heading-level-by"]
                 (ReqArg
                  (\arg opt ->
                      case safeStrRead arg of
                           Just t ->
                               return opt{ optShiftHeadingLevelBy = t }
                           _              -> E.throwIO $ PandocOptionError
                                               "shift-heading-level-by takes an integer argument")
                  "NUMBER")
                 "" -- "Shift heading level"

    , Option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt -> do
                      deprecatedOption "--base-header-level"
                        "Use --shift-heading-level-by instead."
                      case safeStrRead arg of
                           Just t | t > 0 && t < 6 ->
                               return opt{ optShiftHeadingLevelBy = t - 1 }
                           _              -> E.throwIO $ PandocOptionError
                                               "base-header-level must be 1-5")
                  "NUMBER")
                 "" -- "Headers base level"

    , Option "" ["strip-empty-paragraphs"]
                 (NoArg
                  (\opt -> do
                      deprecatedOption "--strip-empty-paragraphs"
                        "Use +empty_paragraphs extension."
                      return opt{ optStripEmptyParagraphs = True }))
                 "" -- "Strip empty paragraphs"

    , Option "" ["track-changes"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "accept" -> return AcceptChanges
                            "reject" -> return RejectChanges
                            "all"    -> return AllChanges
                            _        -> E.throwIO $ PandocOptionError $ T.pack
                               ("Unknown option for track-changes: " ++ arg)
                     return opt { optTrackChanges = action })
                  "accept|reject|all")
                 "" -- "Accepting or reject MS Word track-changes.""

    , Option "" ["strip-comments"]
                (NoArg
                 (\opt -> return opt { optStripComments = True }))
               "" -- "Strip HTML comments"

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
                            _        -> E.throwIO $ PandocOptionError $ T.pack
                               ("Unknown option for reference-location: " ++ arg)
                     return opt { optReferenceLocation = action })
                  "block|section|document")
                 "" -- "Accepting or reject MS Word track-changes.""

    , Option "" ["atx-headers"]
                 (NoArg
                  (\opt -> return opt { optSetextHeaders = False } ))
                 "" -- "Use atx-style headers for markdown"

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
                      case safeStrRead arg of
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

    , Option "" ["html-q-tags"]
                 (NoArg
                  (\opt ->
                     return opt { optHtmlQTags = True }))
                 "" -- "Use <q> tags for quotes in HTML"

    , Option "" ["email-obfuscation"]
                 (ReqArg
                  (\arg opt -> do
                     method <- case arg of
                            "references" -> return ReferenceObfuscation
                            "javascript" -> return JavascriptObfuscation
                            "none"       -> return NoObfuscation
                            _            -> E.throwIO $ PandocOptionError $ T.pack
                               ("Unknown obfuscation method: " ++ arg)
                     return opt { optEmailObfuscation = method })
                  "none|javascript|references")
                 "" -- "Method for obfuscating email in HTML"

     , Option "" ["id-prefix"]
                  (ReqArg
                   (\arg opt -> return opt { optIdentifierPrefix = T.pack arg })
                   "STRING")
                  "" -- "Prefix to add to automatically generated HTML identifiers"

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt ->
                    return opt {
                       optVariables =
                         setVariable "title-prefix" (T.pack arg) $
                           optVariables opt,
                       optStandalone = True })
                  "STRING")
                 "" -- "String to prefix to HTML window title"

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> return opt{ optCss = optCss opt ++ [arg] })
                  -- add new link to end, so it is included in proper order
                  "URL")
                 "" -- "Link to CSS style sheet"

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
                       setVariable "epub-cover-image" (T.pack arg) $
                         optVariables opt })
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
                      case safeStrRead arg of
                           Just t | t >= 1 && t <= 6 ->
                                    return opt { optEpubChapterLevel = t }
                           _      -> E.throwIO $ PandocOptionError
                                    "chapter level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Header level at which to split chapters in EPUB"

    , Option "" ["ipynb-output"]
                 (ReqArg
                  (\arg opt ->
                    case arg of
                      "all" -> return opt{ optIpynbOutput = IpynbOutputAll }
                      "best" -> return opt{ optIpynbOutput = IpynbOutputBest }
                      "none" -> return opt{ optIpynbOutput = IpynbOutputNone }
                      _ -> E.throwIO $ PandocOptionError
                             "ipynb-output must be all, none, or best")
                 "all|none|best")
                 "" -- "Starting number for sections, subsections, etc."

    , Option "" ["bibliography"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadata =
                                            addMeta "bibliography" arg $
                                              optMetadata opt })
                   "FILE")
                 ""

     , Option "" ["csl"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata =
                                   addMeta "csl" arg $ optMetadata opt })
                   "FILE")
                 ""

     , Option "" ["citation-abbreviations"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata =
                                  addMeta "citation-abbreviations" arg $
                                    optMetadata opt })
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
                      return opt { optHTMLMathMethod = WebTeX $ T.pack url' })
                  "URL")
                 "" -- "Use web service for HTML math"

    , Option "" ["mathjax"]
                 (OptArg
                  (\arg opt -> do
                      let url' = maybe (defaultMathJaxURL <>
                                  "tex-mml-chtml.js") T.pack arg
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 "" -- "Use MathJax for HTML math"

    , Option "" ["katex"]
                 (OptArg
                  (\arg opt ->
                      return opt
                        { optHTMLMathMethod = KaTeX $
                           maybe defaultKaTeXURL T.pack arg })
                  "URL")
                  "" -- Use KaTeX for HTML Math

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = GladTeX }))
                 "" -- "Use gladtex for HTML math"

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
                         (unwords $ map (T.unpack . fst) highlightingStyles)
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
                     let extList :: [Extension]
                         extList = [minBound..maxBound]
                     let allExts =
                           case arg of
                             Nothing  -> extensionsFromList extList
                             Just fmt -> getAllExtensions $ T.pack fmt
                     let defExts =
                           case arg of
                             Nothing   -> getDefaultExtensions
                                           "markdown"
                             Just fmt  -> getDefaultExtensions $ T.pack fmt
                     let showExt x =
                           (if extensionEnabled x defExts
                               then '+'
                               else if extensionEnabled x allExts
                                       then '-'
                                       else ' ') : drop 4 (show x)
                     mapM_ (UTF8.hPutStrLn stdout . showExt)
                       [ex | ex <- extList, extensionEnabled ex allExts]
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
                     mapM_ (UTF8.hPutStrLn stdout . T.unpack . fst) highlightingStyles
                     exitSuccess ))
                 ""

    , Option "D" ["print-default-template"]
                 (ReqArg
                  (\arg opt -> do
                     let write = case optOutputFile opt of
                                        Just f  -> UTF8.writeFile f
                                        Nothing -> UTF8.hPutStr stdout
                     templ <- runIO $ do
                                setUserDataDir Nothing
                                getDefaultTemplate (T.pack arg)
                     case templ of
                          Right t
                            | T.null t -> -- e.g. for docx, odt, json:
                                E.throwIO $ PandocCouldNotFindDataFileError $ T.pack
                                  ("templates/default." ++ arg)
                            | otherwise -> write . T.unpack $ t
                          Left e  -> E.throwIO e
                     exitSuccess)
                  "FORMAT")
                 "" -- "Print default template for FORMAT"

    , Option "" ["print-default-data-file"]
                 (ReqArg
                  (\arg opt -> do
                     let write = case optOutputFile opt of
                                        Just f  -> BS.writeFile f
                                        Nothing -> BS.hPutStr stdout
                     runIOorExplode $
                       readDefaultDataFile arg >>= liftIO . write
                     exitSuccess)
                  "FILE")
                  "" -- "Print default data file"

    , Option "" ["print-highlight-style"]
                 (ReqArg
                  (\arg opt -> do
                     let write = case optOutputFile opt of
                                        Just f  -> B.writeFile f
                                        Nothing -> B.putStr
                     sty <- runIOorExplode $ lookupHighlightStyle arg
                     write $ encodePretty'
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


    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     defaultDatadirs <- defaultUserDataDirs
                     UTF8.hPutStrLn stdout (prg ++ " " ++ T.unpack pandocVersion ++
                       compileInfo ++ "\nDefault user data directory: " ++
                       intercalate " or " defaultDatadirs ++
                       ('\n':copyrightMessage))
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
  "Copyright (C) 2006-2020 John MacFarlane",
  "Web:  https://pandoc.org",
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

readersNames :: [String]
readersNames = sort (map (T.unpack . fst) (readers :: [(Text, Reader PandocIO)]))

writersNames :: [String]
writersNames = sort
  ("pdf" : map (T.unpack . fst) (writers :: [(Text, Writer PandocIO)]))

splitField :: String -> (String, String)
splitField = second (tailDef "true") . break (`elemText` ":=")

-- | Apply defaults from --defaults file.
applyDefaults :: Opt -> FilePath -> IO Opt
applyDefaults opt file = runIOorExplode $ do
  let fp = if null (takeExtension file)
              then addExtension file "yaml"
              else file
  setVerbosity $ optVerbosity opt
  dataDirs <- liftIO defaultUserDataDirs
  let fps = fp : case optDataDir opt of
              Nothing -> map (</> ("defaults" </> fp))
                               dataDirs
              Just dd -> [dd </> "defaults" </> fp]
  fp' <- fromMaybe fp <$> findM fileExists fps
  inp <- readFileLazy fp'
  case Y.decode1 inp of
      Right (f :: Opt -> Opt) -> return $ f opt
      Left (errpos, errmsg)  -> throwError $
         PandocParseError $ T.pack $
         "Error parsing " ++ fp' ++ " line " ++
          show (Y.posLine errpos) ++ " column " ++
          show (Y.posColumn errpos) ++ ":\n" ++ errmsg

lookupHighlightStyle :: PandocMonad m => String -> m Style
lookupHighlightStyle s
  | takeExtension s == ".theme" = -- attempt to load KDE theme
    do contents <- readFileLazy s
       case parseTheme contents of
            Left _    -> throwError $ PandocOptionError $ T.pack $
                           "Could not read highlighting theme " ++ s
            Right sty -> return sty
  | otherwise =
  case lookup (T.toLower $ T.pack s) highlightingStyles of
       Just sty -> return sty
       Nothing  -> throwError $ PandocOptionError $ T.pack $
                      "Unknown highlight-style " ++ s

deprecatedOption :: String -> String -> IO ()
deprecatedOption o msg =
  runIO (report $ Deprecated (T.pack o) (T.pack msg)) >>=
    \r -> case r of
       Right () -> return ()
       Left e   -> E.throwIO e

-- | Set text value in text context.
setVariable :: Text -> Text -> Context Text -> Context Text
setVariable key val (Context ctx) = Context $ M.alter go key ctx
  where go Nothing             = Just $ toVal val
        go (Just (ListVal xs)) = Just $ ListVal $ xs ++ [toVal val]
        go (Just x)            = Just $ ListVal [x, toVal val]

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

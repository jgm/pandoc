{-# LANGUAGE CPP, TupleSections #-}
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
   Module      : Main
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Readers.LaTeX (handleIncludes)
import Text.Pandoc.Shared ( tabFilter, readDataFileUTF8, readDataFile,
                            safeRead, headerShift, normalize, err, warn,
                            openURL )
import Text.Pandoc.MediaBag ( mediaDirectory, extractMediaBag, MediaBag )
import Text.Pandoc.XML ( toEntities )
import Text.Pandoc.SelfContained ( makeSelfContained )
import Text.Pandoc.Process (pipeProcess)
import Text.Highlighting.Kate ( languages, Style, tango, pygments,
         espresso, zenburn, kate, haddock, monochrome )
import System.Environment ( getArgs, getProgName )
import System.Exit ( ExitCode (..), exitSuccess )
import System.FilePath
import System.Console.GetOpt
import Data.Char ( toLower, toUpper )
import Data.List ( delete, intercalate, isPrefixOf, isSuffixOf, sort )
import System.Directory ( getAppUserDataDirectory, findExecutable,
                          doesFileExist, Permissions(..), getPermissions )
import System.IO ( stdout, stderr )
import System.IO.Error ( isDoesNotExistError )
import qualified Control.Exception as E
import Control.Exception.Extensible ( throwIO )
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Monad (when, unless, (>=>))
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Foldable (foldrM)
import Network.URI (parseURI, isURI, URI(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Aeson (eitherDecode', encode)
import qualified Data.Map as M
import Data.Yaml (decode)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Text.Pandoc.Readers.Txt2Tags (getT2TMeta)
import Paths_pandoc (getDataDir)
import Text.Printf (printf)
import Text.Pandoc.Error

type Transform = Pandoc -> Pandoc

copyrightMessage :: String
copyrightMessage = intercalate "\n" [
  "",
  "Copyright (C) 2006-2016 John MacFarlane",
  "Web:  http://pandoc.org",
  "This is free software; see the source for copying conditions.",
  "There is no warranty, not even for merchantability or fitness",
  "for a particular purpose." ]

compileInfo :: String
compileInfo =
  "\nCompiled with texmath " ++
  VERSION_texmath ++ ", highlighting-kate " ++ VERSION_highlighting_kate ++
   ".\nSyntax highlighting is supported for the following languages:\n    " ++
       wrapWords 4 78
       [map toLower l | l <- languages, l /= "Alert" && l /= "Alert_indent"]

-- | Converts a list of strings into a single string with the items printed as
-- comma separated words in lines with a maximum line length.
wrapWords :: Int -> Int -> [String] -> String
wrapWords indent c = wrap' (c - indent) (c - indent)
  where
    wrap'   _       _      []   = ""
    wrap' cols remaining (x:xs)
      | remaining == cols =
            x ++ wrap' cols (remaining - length x) xs
      | (length x + 1) > remaining =
            ",\n" ++ replicate indent ' ' ++ x ++
            wrap' cols (cols - length x) xs
      | otherwise =
            ", "  ++ x ++
            wrap' cols (remaining - length x - 2) xs

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub","epub3"]

externalFilter :: FilePath -> [String] -> Pandoc -> IO Pandoc
externalFilter f args' d = do
      mbexe <- if '/' `elem` f
                  -- don't check PATH if filter name has a path
                  then return Nothing
                  -- we catch isDoesNotExistError because this will
                  -- be triggered if PATH not set:
                  else E.catch (findExecutable f)
                                 (\e -> if isDoesNotExistError e
                                           then return Nothing
                                           else throwIO e)
      (f', args'') <- case mbexe of
                           Just x  -> return (x, args')
                           Nothing -> do
                             exists <- doesFileExist f
                             if exists
                                then do
                                  isExecutable <- executable `fmap`
                                                    getPermissions f
                                  return $
                                    case map toLower $ takeExtension f of
                                         _ | isExecutable -> (f, args')
                                         ".py"  -> ("python", f:args')
                                         ".hs"  -> ("runhaskell", f:args')
                                         ".pl"  -> ("perl", f:args')
                                         ".rb"  -> ("ruby", f:args')
                                         ".php" -> ("php", f:args')
                                         _      -> (f, args')
                                else err 85 $ "Filter " ++ f ++ " not found"
      when (f' /= f) $ do
          mbExe <- findExecutable f'
          when (isNothing mbExe) $
            err 83 $ "Error running filter " ++ f ++ "\n" ++
                      show f' ++ " not found in path."
      (exitcode, outbs, errbs) <- E.handle filterException $
                                    pipeProcess Nothing f' args'' $ encode d
      unless (B.null errbs) $ B.hPutStr stderr errbs
      case exitcode of
           ExitSuccess    -> return $ either error id $ eitherDecode' outbs
           ExitFailure ec -> err 83 $ "Error running filter " ++ f ++ "\n" ++
                                       "Filter returned error status " ++ show ec
 where filterException :: E.SomeException -> IO a
       filterException e = err 83 $ "Error running filter " ++ f ++ "\n" ++
                                       show e

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop           :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs      :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone        :: Bool    -- ^ Include header, footer
    , optReader            :: String  -- ^ Reader format
    , optWriter            :: String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optTableOfContents   :: Bool    -- ^ Include table of contents
    , optTransforms        :: [Transform]  -- ^ Doc transforms to apply
    , optTemplate          :: Maybe FilePath  -- ^ Custom template
    , optVariables         :: [(String,String)] -- ^ Template variables to set
    , optMetadata          :: M.Map String MetaValue -- ^ Metadata fields to set
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optNumberOffset      :: [Int]   -- ^ Starting number for sections
    , optSectionDivs       :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental       :: Bool    -- ^ Use incremental lists in Slidy/Slideous/S5
    , optSelfContained     :: Bool    -- ^ Make HTML accessible offline
    , optSmart             :: Bool    -- ^ Use smart typography
    , optOldDashes         :: Bool    -- ^ Parse dashes like pandoc <=1.8.2.1
    , optHtml5             :: Bool    -- ^ Produce HTML5 in HTML
    , optHtmlQTags         :: Bool    -- ^ Use <q> tags in HTML
    , optHighlight         :: Bool    -- ^ Highlight source code
    , optHighlightStyle    :: Style   -- ^ Style to use for highlighted code
    , optChapters          :: Bool    -- ^ Use chapter for top-level sects
    , optHTMLMathMethod    :: HTMLMathMethod -- ^ Method to print HTML math
    , optReferenceODT      :: Maybe FilePath -- ^ Path of reference.odt
    , optReferenceDocx     :: Maybe FilePath -- ^ Path of reference.docx
    , optEpubStylesheet    :: Maybe String   -- ^ EPUB stylesheet
    , optEpubMetadata      :: String  -- ^ EPUB metadata
    , optEpubFonts         :: [FilePath] -- ^ EPUB fonts to embed
    , optEpubChapterLevel  :: Int     -- ^ Header level at which to split chapters
    , optTOCDepth          :: Int     -- ^ Number of levels to include in TOC
    , optDumpArgs          :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs        :: Bool    -- ^ Ignore command-line arguments
    , optVerbose           :: Bool    -- ^ Verbose diagnostic output
    , optReferenceLinks    :: Bool    -- ^ Use reference links in writing markdown, rst
    , optDpi               :: Int     -- ^ Dpi
    , optWrapText          :: WrapOption  -- ^ Options for wrapping text
    , optColumns           :: Int     -- ^ Line length in characters
    , optFilters           :: [FilePath] -- ^ Filters to apply
    , optEmailObfuscation  :: ObfuscationMethod
    , optIdentifierPrefix  :: String
    , optIndentedCodeClasses :: [String] -- ^ Default classes for indented code blocks
    , optDataDir           :: Maybe FilePath
    , optCiteMethod        :: CiteMethod -- ^ Method to output cites
    , optListings          :: Bool       -- ^ Use listings package for code blocks
    , optLaTeXEngine       :: String     -- ^ Program to use for latex -> pdf
    , optLaTeXEngineArgs   :: [String]   -- ^ Flags to pass to the latex-engine
    , optSlideLevel        :: Maybe Int  -- ^ Header level that creates slides
    , optSetextHeaders     :: Bool       -- ^ Use atx headers for markdown level 1-2
    , optAscii             :: Bool       -- ^ Use ascii characters only in html
    , optTeXLigatures      :: Bool       -- ^ Use TeX ligatures for quotes/dashes
    , optDefaultImageExtension :: String -- ^ Default image extension
    , optExtractMedia      :: Maybe FilePath -- ^ Path to extract embedded media
    , optTrace             :: Bool       -- ^ Print debug information
    , optTrackChanges      :: TrackChanges -- ^ Accept or reject MS Word track-changes.
    , optFileScope        :: Bool         -- ^ Parse input files before combining
    , optKaTeXStylesheet   :: Maybe String     -- ^ Path to stylesheet for KaTeX
    , optKaTeXJS           :: Maybe String     -- ^ Path to js file for KaTeX
    }

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop               = 4
    , optPreserveTabs          = False
    , optStandalone            = False
    , optReader                = ""    -- null for default reader
    , optWriter                = ""    -- null for default writer
    , optParseRaw              = False
    , optTableOfContents       = False
    , optTransforms            = []
    , optTemplate              = Nothing
    , optVariables             = []
    , optMetadata              = M.empty
    , optOutputFile            = "-"    -- "-" means stdout
    , optNumberSections        = False
    , optNumberOffset          = [0,0,0,0,0,0]
    , optSectionDivs           = False
    , optIncremental           = False
    , optSelfContained         = False
    , optSmart                 = False
    , optOldDashes             = False
    , optHtml5                 = False
    , optHtmlQTags             = False
    , optHighlight             = True
    , optHighlightStyle        = pygments
    , optChapters              = False
    , optHTMLMathMethod        = PlainMath
    , optReferenceODT          = Nothing
    , optReferenceDocx         = Nothing
    , optEpubStylesheet        = Nothing
    , optEpubMetadata          = ""
    , optEpubFonts             = []
    , optEpubChapterLevel      = 1
    , optTOCDepth              = 3
    , optDumpArgs              = False
    , optIgnoreArgs            = False
    , optVerbose               = False
    , optReferenceLinks        = False
    , optDpi                   = 96
    , optWrapText              = WrapAuto
    , optColumns               = 72
    , optFilters               = []
    , optEmailObfuscation      = JavascriptObfuscation
    , optIdentifierPrefix      = ""
    , optIndentedCodeClasses   = []
    , optDataDir               = Nothing
    , optCiteMethod            = Citeproc
    , optListings              = False
    , optLaTeXEngine           = "pdflatex"
    , optLaTeXEngineArgs       = []
    , optSlideLevel            = Nothing
    , optSetextHeaders         = True
    , optAscii                 = False
    , optTeXLigatures          = True
    , optDefaultImageExtension = ""
    , optExtractMedia          = Nothing
    , optTrace                 = False
    , optTrackChanges          = AcceptChanges
    , optFileScope            = False
    , optKaTeXStylesheet       = Nothing
    , optKaTeXJS               = Nothing
    }

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = arg })
                  "FORMAT")
                 ""

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = arg })
                  "FORMAT")
                 ""

    , Option "o" ["output"]
                 (ReqArg
                  (\arg opt -> return opt { optOutputFile = arg })
                  "FILENAME")
                 "" -- "Name of output file"

    , Option "" ["data-dir"]
                 (ReqArg
                  (\arg opt -> return opt { optDataDir = Just arg })
                 "DIRECTORY") -- "Directory containing pandoc data files."
                ""

    , Option "R" ["parse-raw"]
                 (NoArg
                  (\opt -> return opt { optParseRaw = True }))
                 "" -- "Parse untranslatable HTML codes and LaTeX environments as raw"

    , Option "S" ["smart"]
                 (NoArg
                  (\opt -> return opt { optSmart = True }))
                 "" -- "Use smart quotes, dashes, and ellipses"

    , Option "" ["old-dashes"]
                 (NoArg
                  (\opt -> return opt { optSmart = True
                                      , optOldDashes = True }))
                 "" -- "Use smart quotes, dashes, and ellipses"

    , Option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 -> do
                               let oldTransforms = optTransforms opt
                               let shift = t - 1
                               return opt{ optTransforms =
                                           headerShift shift : oldTransforms }
                           _              -> err 19
                                       "base-header-level must be a number > 0")
                  "NUMBER")
                 "" -- "Headers base level"

     , Option "" ["indented-code-classes"]
                  (ReqArg
                   (\arg opt -> return opt { optIndentedCodeClasses = words $
                                             map (\c -> if c == ',' then ' ' else c) arg })
                   "STRING")
                  "" -- "Classes (whitespace- or comma-separated) to use for indented code-blocks"

    , Option "F" ["filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters = arg : optFilters opt })
                  "PROGRAM")
                 "" -- "External JSON filter"

    , Option "" ["normalize"]
                 (NoArg
                  (\opt -> return opt { optTransforms =
                                   normalize : optTransforms opt } ))
                 "" -- "Normalize the Pandoc AST"

    , Option "p" ["preserve-tabs"]
                 (NoArg
                  (\opt -> return opt { optPreserveTabs = True }))
                 "" -- "Preserve tabs instead of converting to spaces"

    , Option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 -> return opt { optTabStop = t }
                           _              -> err 31
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
                            _        -> err 6
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
                    return opt { optExtractMedia = Just arg })
                  "PATH")
                 "" -- "Directory to which to extract embedded media"

    , Option "s" ["standalone"]
                 (NoArg
                  (\opt -> return opt { optStandalone = True }))
                 "" -- "Include needed header and footer on output"

    , Option "" ["template"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optTemplate = Just arg,
                                 optStandalone = True })
                  "FILENAME")
                 "" -- "Use custom template"

    , Option "M" ["metadata"]
                 (ReqArg
                  (\arg opt -> do
                     let (key,val) = case break (`elem` ":=") arg of
                                       (k,_:v) -> (k, readMetaValue v)
                                       (k,_)   -> (k, MetaBool True)
                     return opt{ optMetadata = addMetadata key val
                                             $ optMetadata opt })
                  "KEY[:VALUE]")
                 ""

    , Option "V" ["variable"]
                 (ReqArg
                  (\arg opt -> do
                     let (key,val) = case break (`elem` ":=") arg of
                                       (k,_:v) -> (k,v)
                                       (k,_)   -> (k,"true")
                     return opt{ optVariables = (key,val) : optVariables opt })
                  "KEY[:VALUE]")
                 ""

    , Option "D" ["print-default-template"]
                 (ReqArg
                  (\arg _ -> do
                     templ <- getDefaultTemplate Nothing arg
                     case templ of
                          Right t -> UTF8.hPutStr stdout t
                          Left e  -> error $ show e
                     exitSuccess)
                  "FORMAT")
                 "" -- "Print default template for FORMAT"

    , Option "" ["print-default-data-file"]
                 (ReqArg
                  (\arg _ -> do
                     readDataFile Nothing arg >>= BS.hPutStr stdout
                     exitSuccess)
                  "FILE")
                  "" -- "Print default data file"

    , Option "" ["dpi"]
                 (ReqArg
                  (\arg opt ->
                    case safeRead arg of
                         Just t | t > 0 -> return opt { optDpi = t }
                         _              -> err 31
                                        "dpi must be a number greater than 0")
                  "NUMBER")
                 "" -- "Dpi (default 96)"

    , Option "" ["no-wrap"]
                 (NoArg
                  (\opt -> do warn $ "--no-wrap is deprecated. " ++
                                     "Use --wrap=none or --wrap=preserve instead."
                              return opt { optWrapText = WrapNone }))
                 ""

    , Option "" ["wrap"]
                 (ReqArg
                  (\arg opt ->
                    case safeRead ("Wrap" ++ uppercaseFirstLetter arg) of
                          Just o   -> return opt { optWrapText = o }
                          Nothing  -> err 77 "--wrap must be auto, none, or preserve")
                 "[auto|none|preserve]")
                 "" -- "Option for wrapping text in output"

    , Option "" ["columns"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 -> return opt { optColumns = t }
                           _              -> err 33
                                   "columns must be a number greater than 0")
                 "NUMBER")
                 "" -- "Length of line in characters"

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
                           _      -> err 57
                                    "TOC level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Number of levels to include in TOC"

    , Option "" ["no-highlight"]
                (NoArg
                 (\opt -> return opt { optHighlight = False }))
                 "" -- "Don't highlight source code"

    , Option "" ["highlight-style"]
                (ReqArg
                 (\arg opt -> do
                   newStyle <- case map toLower arg of
                                     "pygments"   -> return pygments
                                     "tango"      -> return tango
                                     "espresso"   -> return espresso
                                     "zenburn"    -> return zenburn
                                     "kate"       -> return kate
                                     "monochrome" -> return monochrome
                                     "haddock"    -> return haddock
                                     _            -> err 39 $
                                         "Unknown style :" ++ arg
                   return opt{ optHighlightStyle = newStyle })
                 "STYLE")
                 "" -- "Style for highlighted code"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     -- add new ones to end, so they're included in order specified
                     let newvars = optVariables opt ++ [("header-includes",text)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     -- add new ones to end, so they're included in order specified
                     let newvars = optVariables opt ++ [("include-before",text)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     -- add new ones to end, so they're included in order specified
                     let newvars = optVariables opt ++ [("include-after",text)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include after document body"

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
                 ""  -- "Use ascii characters only in HTML output"

    , Option "" ["reference-links"]
                 (NoArg
                  (\opt -> return opt { optReferenceLinks = True } ))
                 "" -- "Use reference links in parsing HTML"

    , Option "" ["atx-headers"]
                 (NoArg
                  (\opt -> return opt { optSetextHeaders = False } ))
                 "" -- "Use atx-style headers for markdown"

    , Option "" ["chapters"]
                 (NoArg
                  (\opt -> return opt { optChapters = True }))
                 "" -- "Use chapter for top-level sections in LaTeX, DocBook"

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
                           _      -> err 57 "could not parse number-offset")
                 "NUMBERS")
                 "" -- "Starting number for sections, subsections, etc."

    , Option "" ["no-tex-ligatures"]
                 (NoArg
                  (\opt -> return opt { optTeXLigatures = False }))
                 "" -- "Don't use tex ligatures for quotes, dashes"

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
                           _      -> err 39
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
                            _            -> err 6
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
                  (\arg opt -> do
                     -- add new link to end, so it is included in proper order
                     let newvars = optVariables opt ++ [("css",arg)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "URL")
                 "" -- "Link to CSS style sheet"

    , Option "" ["reference-odt"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceODT = Just arg })
                  "FILENAME")
                 "" -- "Path of custom reference.odt"

    , Option "" ["reference-docx"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceDocx = Just arg })
                  "FILENAME")
                 "" -- "Path of custom reference.docx"

    , Option "" ["epub-stylesheet"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     return opt { optEpubStylesheet = Just text })
                  "FILENAME")
                 "" -- "Path of epub.css"

    , Option "" ["epub-cover-image"]
                 (ReqArg
                  (\arg opt ->
                     return opt { optVariables =
                                 ("epub-cover-image", arg) : optVariables opt })
                  "FILENAME")
                 "" -- "Path of epub cover image"

    , Option "" ["epub-metadata"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     return opt { optEpubMetadata = text })
                  "FILENAME")
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
                           _      -> err 59
                                    "chapter level must be a number between 1 and 6")
                 "NUMBER")
                 "" -- "Header level at which to split chapters in EPUB"

    , Option "" ["latex-engine"]
                 (ReqArg
                  (\arg opt -> do
                     let b = takeBaseName arg
                     if b `elem` ["pdflatex", "lualatex", "xelatex"]
                        then return opt { optLaTeXEngine = arg }
                        else err 45 "latex-engine must be pdflatex, lualatex, or xelatex.")
                  "PROGRAM")
                 "" -- "Name of latex program to use in generating PDF"

    , Option "" ["latex-engine-opt"]
                 (ReqArg
                  (\arg opt -> do
                      let oldArgs = optLaTeXEngineArgs opt
                      return opt { optLaTeXEngineArgs = arg : oldArgs })
                  "STRING")
                 "" -- "Flags to pass to the LaTeX engine, all instances of this option are accumulated and used"

    , Option "" ["bibliography"]
                 (ReqArg
                  (\arg opt -> return opt{ optMetadata = addMetadata
                                             "bibliography" (readMetaValue arg)
                                             $ optMetadata opt
                                         })
                   "FILE")
                 ""

     , Option "" ["csl"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata = addMetadata "csl"
                                               (readMetaValue arg)
                                               $ optMetadata opt })
                   "FILE")
                 ""

     , Option "" ["citation-abbreviations"]
                 (ReqArg
                  (\arg opt ->
                     return opt{ optMetadata = addMetadata
                                               "citation-abbreviations"
                                               (readMetaValue arg)
                                               $ optMetadata opt })
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

    , Option "m" ["latexmathml", "asciimathml"]
                 (OptArg
                  (\arg opt ->
                      return opt { optHTMLMathMethod = LaTeXMathML arg })
                  "URL")
                 "" -- "Use LaTeXMathML script in html output"

    , Option "" ["mathml"]
                 (OptArg
                  (\arg opt ->
                      return opt { optHTMLMathMethod = MathML arg })
                   "URL")
                 "" -- "Use mathml for HTML math"

    , Option "" ["mimetex"]
                 (OptArg
                  (\arg opt -> do
                      let url' = case arg of
                                      Just u   -> u ++ "?"
                                      Nothing  -> "/cgi-bin/mimetex.cgi?"
                      return opt { optHTMLMathMethod = WebTeX url' })
                  "URL")
                 "" -- "Use mimetex for HTML math"

    , Option "" ["webtex"]
                 (OptArg
                  (\arg opt -> do
                      let url' = fromMaybe "http://chart.apis.google.com/chart?cht=tx&chl=" arg
                      return opt { optHTMLMathMethod = WebTeX url' })
                  "URL")
                 "" -- "Use web service for HTML math"

    , Option "" ["jsmath"]
                 (OptArg
                  (\arg opt -> return opt { optHTMLMathMethod = JsMath arg})
                  "URL")
                 "" -- "Use jsMath for HTML math"

    , Option "" ["mathjax"]
                 (OptArg
                  (\arg opt -> do
                      let url' = fromMaybe "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" arg
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 "" -- "Use MathJax for HTML math"
    , Option "" ["katex"]
                 (OptArg
                  (\arg opt ->
                      return opt
                        { optKaTeXJS =
                           arg <|> Just "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.js"})
                  "URL")
                  "" -- Use KaTeX for HTML Math

    , Option "" ["katex-stylesheet"]
                 (ReqArg
                  (\arg opt ->
                      return opt { optKaTeXStylesheet = Just arg })
                 "URL")
                 "" -- Set the KaTeX Stylesheet location

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt -> return opt { optHTMLMathMethod = GladTeX }))
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
                  (\opt -> return opt { optVerbose = True }))
                 "" -- "Verbose diagnostic output."

    , Option "" ["bash-completion"]
                 (NoArg
                  (\_ -> do
                     ddir <- getDataDir
                     tpl <- readDataFileUTF8 Nothing "bash_completion.tpl"
                     let optnames (Option shorts longs _ _) =
                           map (\c -> ['-',c]) shorts ++
                           map ("--" ++) longs
                     let allopts = unwords (concatMap optnames options)
                     UTF8.hPutStrLn stdout $ printf tpl allopts
                         (unwords (map fst readers))
                         (unwords ("pdf": map fst writers))
                         ddir
                     exitSuccess ))
                 "" -- "Print bash completion script"

    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     defaultDatadir <- getAppUserDataDirectory "pandoc"
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


addMetadata :: String -> MetaValue -> M.Map String MetaValue
            -> M.Map String MetaValue
addMetadata k v m = case M.lookup k m of
                         Nothing -> M.insert k v m
                         Just (MetaList xs) -> M.insert k
                                              (MetaList (xs ++ [v])) m
                         Just x -> M.insert k (MetaList [v, x]) m

readMetaValue :: String -> MetaValue
readMetaValue s = case decode (UTF8.fromString s) of
                       Just (Yaml.String t) -> MetaString $ T.unpack t
                       Just (Yaml.Bool b)   -> MetaBool b
                       _                    -> MetaString s

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName = usageInfo
  (programName ++ " [OPTIONS] [FILES]" ++ "\nInput formats:  " ++
  wrapWords 16 78 readers'names ++
     '\n' : replicate 16 ' ' ++
     "[ *only Pandoc's JSON version of native AST]" ++ "\nOutput formats: " ++
  wrapWords 16 78 writers'names ++
     '\n' : replicate 16 ' ' ++
     "[**for pdf output, use latex or beamer and -o FILENAME.pdf]\nOptions:")
  where
    writers'names = sort $ "json*" : "pdf**" : delete "json" (map fst writers)
    readers'names = sort $ "json*" : delete "json" (map fst readers)

-- Determine default reader based on source file extensions
defaultReaderName :: String -> [FilePath] -> String
defaultReaderName fallback [] = fallback
defaultReaderName fallback (x:xs) =
  case takeExtension (map toLower x) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".org"      -> "org"
    ".lhs"      -> "markdown+lhs"
    ".db"       -> "docbook"
    ".opml"     -> "opml"
    ".wiki"     -> "mediawiki"
    ".dokuwiki" -> "dokuwiki"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
    ".docx"     -> "docx"
    ".t2t"      -> "t2t"
    ".epub"     -> "epub"
    ".odt"      -> "odt"
    ".pdf"      -> "pdf"  -- so we get an "unknown reader" error
    ".doc"      -> "doc"  -- so we get an "unknown reader" error
    _           -> defaultReaderName fallback xs

-- Returns True if extension of first source is .lhs
lhsExtension :: [FilePath] -> Bool
lhsExtension (x:_) = takeExtension x == ".lhs"
lhsExtension _ = False

-- Determine default writer based on output file extension
defaultWriterName :: FilePath -> String
defaultWriterName "-" = "html" -- no output file
defaultWriterName x =
  case takeExtension (map toLower x) of
    ""          -> "markdown" -- empty extension
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".context"  -> "context"
    ".ctx"      -> "context"
    ".rtf"      -> "rtf"
    ".rst"      -> "rst"
    ".s5"       -> "s5"
    ".native"   -> "native"
    ".json"     -> "json"
    ".txt"      -> "markdown"
    ".text"     -> "markdown"
    ".md"       -> "markdown"
    ".markdown" -> "markdown"
    ".textile"  -> "textile"
    ".lhs"      -> "markdown+lhs"
    ".texi"     -> "texinfo"
    ".texinfo"  -> "texinfo"
    ".db"       -> "docbook"
    ".odt"      -> "odt"
    ".docx"     -> "docx"
    ".epub"     -> "epub"
    ".org"      -> "org"
    ".asciidoc" -> "asciidoc"
    ".adoc"     -> "asciidoc"
    ".pdf"      -> "latex"
    ".fb2"      -> "fb2"
    ".opml"     -> "opml"
    ".icml"     -> "icml"
    ".tei.xml"  -> "tei"
    ".tei"      -> "tei"
    ['.',y] | y `elem` ['1'..'9'] -> "man"
    _           -> "html"

-- Transformations of a Pandoc document post-parsing:

extractMedia :: MediaBag -> FilePath -> Pandoc -> IO Pandoc
extractMedia media dir d =
  case [fp | (fp, _, _) <- mediaDirectory media] of
        []  -> return d
        fps -> do
          extractMediaBag True dir media
          return $ walk (adjustImagePath dir fps) d

adjustImagePath :: FilePath -> [FilePath] -> Inline -> Inline
adjustImagePath dir paths (Image attr lab (src, tit))
   | src `elem` paths = Image attr lab (dir ++ "/" ++ src, tit)
adjustImagePath _ _ x = x

adjustMetadata :: M.Map String MetaValue -> Pandoc -> IO Pandoc
adjustMetadata metadata d = return $ M.foldWithKey setMeta d metadata

applyTransforms :: [Transform] -> Pandoc -> IO Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

applyFilters :: [FilePath] -> [String] -> Pandoc -> IO Pandoc
applyFilters filters args d =
  foldrM ($) d $ map (flip externalFilter args) filters

uppercaseFirstLetter :: String -> String
uppercaseFirstLetter (c:cs) = toUpper c : cs
uppercaseFirstLetter [] = []

main :: IO ()
main = do

  rawArgs <- map UTF8.decodeArg <$> getArgs
  prg <- getProgName

  let (actions, args, errors) = getOpt Permute options rawArgs

  unless (null errors) $
     err 2 $ concat $ errors ++
        ["Try " ++ prg ++ " --help for more information."]

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts) actions
  convertWithOpts opts args

convertWithOpts :: Opt -> [FilePath] -> IO ()
convertWithOpts opts args = do
  let Opt    {  optTabStop               = tabStop
              , optPreserveTabs          = preserveTabs
              , optStandalone            = standalone
              , optReader                = readerName
              , optWriter                = writerName
              , optParseRaw              = parseRaw
              , optVariables             = variables
              , optMetadata              = metadata
              , optTableOfContents       = toc
              , optTransforms            = transforms
              , optTemplate              = templatePath
              , optOutputFile            = outputFile
              , optNumberSections        = numberSections
              , optNumberOffset          = numberFrom
              , optSectionDivs           = sectionDivs
              , optIncremental           = incremental
              , optSelfContained         = selfContained
              , optSmart                 = smart
              , optOldDashes             = oldDashes
              , optHtml5                 = html5
              , optHtmlQTags             = htmlQTags
              , optHighlight             = highlight
              , optHighlightStyle        = highlightStyle
              , optChapters              = chapters
              , optHTMLMathMethod        = mathMethod'
              , optReferenceODT          = referenceODT
              , optReferenceDocx         = referenceDocx
              , optEpubStylesheet        = epubStylesheet
              , optEpubMetadata          = epubMetadata
              , optEpubFonts             = epubFonts
              , optEpubChapterLevel      = epubChapterLevel
              , optTOCDepth              = epubTOCDepth
              , optDumpArgs              = dumpArgs
              , optIgnoreArgs            = ignoreArgs
              , optVerbose               = verbose
              , optReferenceLinks        = referenceLinks
              , optDpi                   = dpi
              , optWrapText              = wrap
              , optColumns               = columns
              , optFilters               = filters
              , optEmailObfuscation      = obfuscationMethod
              , optIdentifierPrefix      = idPrefix
              , optIndentedCodeClasses   = codeBlockClasses
              , optDataDir               = mbDataDir
              , optCiteMethod            = citeMethod
              , optListings              = listings
              , optLaTeXEngine           = latexEngine
              , optLaTeXEngineArgs       = latexEngineArgs
              , optSlideLevel            = slideLevel
              , optSetextHeaders         = setextHeaders
              , optAscii                 = ascii
              , optTeXLigatures          = texLigatures
              , optDefaultImageExtension = defaultImageExtension
              , optExtractMedia          = mbExtractMedia
              , optTrace                 = trace
              , optTrackChanges          = trackChanges
              , optFileScope            = fileScope
              , optKaTeXStylesheet       = katexStylesheet
              , optKaTeXJS               = katexJS
             } = opts

  when dumpArgs $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) args
       exitSuccess

  let csscdn = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.css"
  let mathMethod =
        case (katexJS, katexStylesheet) of
            (Nothing, _) -> mathMethod'
            (Just js, ss) -> KaTeX js (fromMaybe csscdn ss)


  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (M.lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      "pandoc-citeproc" `notElem` map takeBaseName filters
  let filters' = if needsCiteproc then "pandoc-citeproc" : filters
                                  else filters

  let sources = if ignoreArgs then [] else args

  datadir <- case mbDataDir of
                  Nothing   -> E.catch
                                 (Just <$> getAppUserDataDirectory "pandoc")
                                 (\e -> let _ = (e :: E.SomeException)
                                        in  return Nothing)
                  Just _    -> return mbDataDir

  -- assign reader and writer based on options and filenames
  let readerName' = case map toLower readerName of
                          []       -> defaultReaderName
                                      (if any isURI sources
                                          then "html"
                                          else "markdown") sources
                          "html4"  -> "html"
                          x        -> x

  let writerName' = case map toLower writerName of
                          []        -> defaultWriterName outputFile
                          "epub2"   -> "epub"
                          "html4"   -> "html"
                          x         -> x
  let format = takeWhile (`notElem` ['+','-'])
                       $ takeFileName writerName'  -- in case path to lua script

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

  let laTeXOutput = format `elem` ["latex", "beamer"]
  let conTeXtOutput = format == "context"
  let html5Output = format == "html5"

  let laTeXInput = "latex" `isPrefixOf` readerName' ||
                    "beamer" `isPrefixOf` readerName'

  writer <- if ".lua" `isSuffixOf` format
               -- note:  use non-lowercased version writerName
               then return $ IOStringWriter $ writeCustom writerName
               else case getWriter writerName' of
                         Left e  -> err 9 $
                           if format == "pdf"
                              then e ++
                               "\nTo create a pdf with pandoc, use " ++
                               "the latex or beamer writer and specify\n" ++
                               "an output file with .pdf extension " ++
                               "(pandoc -t latex -o filename.pdf)."
                              else e
                         Right w -> return w

  reader <- if "t2t" == readerName'
              then (mkStringReader .
                    readTxt2Tags) <$>
                      getT2TMeta sources outputFile
              else case getReader readerName' of
                Right r  -> return r
                Left e   -> err 7 e'
                  where e' = case readerName' of
                                  "pdf" -> e ++
                                     "\nPandoc can convert to PDF, but not from PDF."
                                  "doc" -> e ++
                                     "\nPandoc can convert from DOCX, but not from DOC.\nTry using Word to save your DOC file as DOCX, and convert that with pandoc."
                                  _ -> e

  let standalone' = standalone || not (isTextFormat format) || pdfOutput

  templ <- case templatePath of
                _ | not standalone' -> return ""
                Nothing -> do
                           deftemp <- getDefaultTemplate datadir format
                           case deftemp of
                                 Left e   -> throwIO e
                                 Right t  -> return t
                Just tp -> do
                           -- strip off extensions
                           let tp' = case takeExtension tp of
                                          ""   -> tp <.> format
                                          _    -> tp
                           E.catch (UTF8.readFile tp')
                             (\e -> if isDoesNotExistError e
                                       then E.catch
                                             (readDataFileUTF8 datadir
                                                ("templates" </> tp'))
                                             (\e' -> let _ = (e' :: E.SomeException)
                                                     in throwIO e')
                                       else throwIO e)

  variables' <- case mathMethod of
                      LaTeXMathML Nothing -> do
                         s <- readDataFileUTF8 datadir "LaTeXMathML.js"
                         return $ ("mathml-script", s) : variables
                      MathML Nothing -> do
                         s <- readDataFileUTF8 datadir "MathMLinHTML.js"
                         return $ ("mathml-script", s) : variables
                      _ -> return variables

  variables'' <- if format == "dzslides"
                    then do
                        dztempl <- readDataFileUTF8 datadir
                                     ("dzslides" </> "template.html")
                        let dzline = "<!-- {{{{ dzslides core"
                        let dzcore = unlines
                                   $ dropWhile (not . (dzline `isPrefixOf`))
                                   $ lines dztempl
                        return $ ("dzslides-core", dzcore) : variables'
                    else return variables'

  let sourceURL = case sources of
                    []    -> Nothing
                    (x:_) -> case parseURI x of
                                Just u
                                  | uriScheme u `elem` ["http:","https:"] ->
                                      Just $ show u{ uriQuery = "",
                                                     uriFragment = "" }
                                _ -> Nothing

  let readerOpts = def{ readerSmart = if laTeXInput
                                         then texLigatures
                                         else smart || (texLigatures &&
                                               (laTeXOutput || conTeXtOutput))
                      , readerStandalone = standalone'
                      , readerParseRaw = parseRaw
                      , readerColumns = columns
                      , readerTabStop = tabStop
                      , readerOldDashes = oldDashes
                      , readerIndentedCodeClasses = codeBlockClasses
                      , readerApplyMacros = not laTeXOutput
                      , readerDefaultImageExtension = defaultImageExtension
                      , readerTrace = trace
                      , readerTrackChanges = trackChanges
                      , readerFileScope   = fileScope
                      }

  when (not (isTextFormat format) && outputFile == "-") $
    err 5 $ "Cannot write " ++ format ++ " output to stdout.\n" ++
            "Specify an output file using the -o option."

  let readSources [] = mapM readSource ["-"]
      readSources srcs = mapM readSource srcs
      readSource "-" = UTF8.getContents
      readSource src = case parseURI src of
                            Just u | uriScheme u `elem` ["http:","https:"] ->
                                       readURI src
                            _       -> UTF8.readFile src
      readURI src = do
        res <- openURL src
        case res of
             Left e        -> throwIO e
             Right (bs,_)  -> return $ UTF8.toString bs

  let readFiles [] = error "Cannot read archive from stdin"
      readFiles [x] = B.readFile x
      readFiles (x:xs) = mapM_ (warn . ("Ignoring: " ++)) xs >> B.readFile x

  let convertTabs = tabFilter (if preserveTabs || readerName' == "t2t"
                                 then 0
                                 else tabStop)

  let handleIncludes' :: String -> IO (Either PandocError String)
      handleIncludes' = if readerName' `elem`  ["latex", "latex+lhs"]
                               then handleIncludes
                               else return . Right

  let sourceToDoc :: [FilePath] -> IO (Pandoc, MediaBag)
      sourceToDoc sources' = fmap handleError $
        case reader of
          StringReader r-> do
            srcs <- convertTabs . intercalate "\n" <$> readSources sources'
            doc <- handleIncludes' srcs
            either (return . Left) (\s -> fmap (,mempty) <$> r readerOpts s) doc
          ByteStringReader r -> readFiles sources' >>= r readerOpts

  -- We parse first if (1) fileScope is set, (2), it's a binary
  -- reader, or (3) we're reading JSON. This is easier to do of an AND
  -- of negatives as opposed to an OR of positives, so we do default
  -- parsing if it's a StringReader AND (fileScope is set AND it's not
  -- a JSON reader).
  (doc, media) <- case reader of
    (StringReader _) | not fileScope && readerName' /= "json" ->
                         sourceToDoc sources
    _ | null sources -> sourceToDoc sources
    _  -> do pairs <- mapM (\s -> sourceToDoc [s]) sources
             return (mconcat $ map fst pairs, mconcat $ map snd pairs)

  let writerOptions = def { writerStandalone       = standalone',
                            writerTemplate         = templ,
                            writerVariables        = variables'',
                            writerTabStop          = tabStop,
                            writerTableOfContents  = toc,
                            writerHTMLMathMethod   = mathMethod,
                            writerIncremental      = incremental,
                            writerCiteMethod       = citeMethod,
                            writerIgnoreNotes      = False,
                            writerNumberSections   = numberSections,
                            writerNumberOffset     = numberFrom,
                            writerSectionDivs      = sectionDivs,
                            writerReferenceLinks   = referenceLinks,
                            writerDpi              = dpi,
                            writerWrapText         = wrap,
                            writerColumns          = columns,
                            writerEmailObfuscation = obfuscationMethod,
                            writerIdentifierPrefix = idPrefix,
                            writerSourceURL        = sourceURL,
                            writerUserDataDir      = datadir,
                            writerHtml5            = html5,
                            writerHtmlQTags        = htmlQTags,
                            writerChapters         = chapters,
                            writerListings         = listings,
                            writerBeamer           = False,
                            writerSlideLevel       = slideLevel,
                            writerHighlight        = highlight,
                            writerHighlightStyle   = highlightStyle,
                            writerSetextHeaders    = setextHeaders,
                            writerTeXLigatures     = texLigatures,
                            writerEpubMetadata     = epubMetadata,
                            writerEpubStylesheet   = epubStylesheet,
                            writerEpubFonts        = epubFonts,
                            writerEpubChapterLevel = epubChapterLevel,
                            writerTOCDepth         = epubTOCDepth,
                            writerReferenceODT     = referenceODT,
                            writerReferenceDocx    = referenceDocx,
                            writerMediaBag         = media,
                            writerVerbose          = verbose,
                            writerLaTeXArgs        = latexEngineArgs
                          }


  doc' <- (maybe return (extractMedia media) mbExtractMedia >=>
           adjustMetadata metadata >=>
           applyTransforms transforms >=>
           applyFilters filters' [format]) doc

  let writeBinary :: B.ByteString -> IO ()
      writeBinary = B.writeFile (UTF8.encodePath outputFile)

  let writerFn :: FilePath -> String -> IO ()
      writerFn "-" = UTF8.putStr
      writerFn f   = UTF8.writeFile f

  case writer of
    IOStringWriter f -> f writerOptions doc' >>= writerFn outputFile
    IOByteStringWriter f -> f writerOptions doc' >>= writeBinary
    PureStringWriter f
      | pdfOutput -> do
              -- make sure writer is latex or beamer or context or html5
              unless (laTeXOutput || conTeXtOutput || html5Output) $
                err 47 $ "cannot produce pdf output with " ++ format ++
                         " writer"

              let pdfprog = case () of
                              _ | conTeXtOutput -> "context"
                              _ | html5Output   -> "wkhtmltopdf"
                              _                 -> latexEngine
              -- check for pdf creating program
              mbPdfProg <- findExecutable pdfprog
              when (isNothing mbPdfProg) $
                   err 41 $ pdfprog ++ " not found. " ++
                     pdfprog ++ " is needed for pdf output."

              res <- makePDF pdfprog f writerOptions doc'
              case res of
                   Right pdf -> writeBinary pdf
                   Left err' -> do
                     B.hPutStr stderr err'
                     B.hPut stderr $ B.pack [10]
                     err 43 "Error producing PDF"
      | otherwise -> selfcontain (f writerOptions doc' ++
                                  ['\n' | not standalone'])
                      >>= writerFn outputFile . handleEntities
          where htmlFormat = format `elem`
                  ["html","html5","s5","slidy","slideous","dzslides","revealjs"]
                selfcontain = if selfContained && htmlFormat
                                 then makeSelfContained writerOptions
                                 else return
                handleEntities = if htmlFormat && ascii
                                    then toEntities
                                    else id

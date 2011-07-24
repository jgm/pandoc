{-
Copyright (C) 2006-2011 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Text.Pandoc
import Text.Pandoc.S5 (s5HeaderIncludes)
import Text.Pandoc.Shared ( tabFilter, ObfuscationMethod (..), readDataFile,
                            headerShift, findDataFile, normalize )
#ifdef _HIGHLIGHTING
import Text.Pandoc.Highlighting ( languages )
#endif
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitWith, ExitCode (..) )
import System.FilePath
import System.Console.GetOpt
import Data.Char ( toLower )
import Data.List ( intercalate, isSuffixOf, isPrefixOf )
import System.Directory ( getAppUserDataDirectory, doesFileExist )
import System.IO ( stdout, stderr )
import System.IO.Error ( isDoesNotExistError )
import Control.Exception.Extensible ( throwIO )
import qualified Text.Pandoc.UTF8 as UTF8
import Text.CSL
import Text.Pandoc.Biblio
import Control.Monad (when, unless, liftM)
import Network.HTTP (simpleHTTP, mkRequest, getResponseBody, RequestMethod(..))
import Network.URI (parseURI, isURI, URI(..))
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString )
import Codec.Binary.UTF8.String (decodeString, encodeString)

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2006-2011 John MacFarlane\n" ++
                    "Web:  http://johnmacfarlane.net/pandoc\n" ++
                    "This is free software; see the source for copying conditions.  There is no\n" ++
                    "warranty, not even for merchantability or fitness for a particular purpose."

compileInfo :: String
compileInfo =
  "\nCompiled with citeproc support." ++
#ifdef _HIGHLIGHTING
   "\nCompiled with syntax highlighting support for:\n" ++
       wrapWords 78 languages ++
#endif
   ""

-- | Converts a list of strings into a single string with the items printed as
-- comma separated words in lines with a maximum line length.
wrapWords :: Int -> [String] -> String
wrapWords c = wrap' c c where
              wrap' _    _         []     = ""
              wrap' cols remaining (x:xs) = if remaining == cols
                                               then x ++ wrap' cols (remaining - length x) xs
                                               else if (length x + 1) > remaining
                                                       then ",\n" ++ x ++ wrap' cols (cols - length x) xs
                                                       else ", "  ++ x ++ wrap' cols (remaining - (length x + 2)) xs

isNonTextOutput :: String -> Bool
isNonTextOutput = (`elem` ["odt","epub"])

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop           :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs      :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone        :: Bool    -- ^ Include header, footer
    , optReader            :: String  -- ^ Reader format
    , optWriter            :: String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optTableOfContents   :: Bool    -- ^ Include table of contents
    , optTransforms        :: [Pandoc -> Pandoc]  -- ^ Doc transforms to apply
    , optTemplate          :: Maybe FilePath  -- ^ Custom template
    , optVariables         :: [(String,String)] -- ^ Template variables to set
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optSectionDivs       :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental       :: Bool    -- ^ Use incremental lists in Slidy/S5
    , optOffline           :: Bool    -- ^ Make slideshow accessible offline
    , optXeTeX             :: Bool    -- ^ Format latex for xetex
    , optSmart             :: Bool    -- ^ Use smart typography
    , optHtml5             :: Bool    -- ^ Produce HTML5 in HTML
    , optChapters          :: Bool    -- ^ Use chapter for top-level sects
    , optHTMLMathMethod    :: HTMLMathMethod -- ^ Method to print HTML math
    , optReferenceODT      :: Maybe FilePath -- ^ Path of reference.odt
    , optEPUBStylesheet    :: Maybe String   -- ^ EPUB stylesheet
    , optEPUBMetadata      :: String  -- ^ EPUB metadata
    , optDumpArgs          :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs        :: Bool    -- ^ Ignore command-line arguments
    , optStrict            :: Bool    -- ^ Use strict markdown syntax
    , optReferenceLinks    :: Bool    -- ^ Use reference links in writing markdown, rst
    , optWrapText          :: Bool    -- ^ Wrap text
    , optColumns           :: Int     -- ^ Line length in characters
    , optPlugins           :: [Pandoc -> IO Pandoc] -- ^ Plugins to apply
    , optEmailObfuscation  :: ObfuscationMethod
    , optIdentifierPrefix  :: String
    , optIndentedCodeClasses :: [String] -- ^ Default classes for indented code blocks
    , optDataDir           :: Maybe FilePath
    , optCiteMethod        :: CiteMethod -- ^ Method to output cites
    , optBibliography      :: [String]
    , optCslFile           :: FilePath
    , optListings          :: Bool       -- ^ Use listings package for code blocks
    , optAscii             :: Bool       -- ^ Avoid using nonascii characters
    }

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop           = 4
    , optPreserveTabs      = False
    , optStandalone        = False
    , optReader            = ""    -- null for default reader
    , optWriter            = ""    -- null for default writer
    , optParseRaw          = False
    , optTableOfContents   = False
    , optTransforms        = []
    , optTemplate          = Nothing
    , optVariables         = []
    , optOutputFile        = "-"    -- "-" means stdout
    , optNumberSections    = False
    , optSectionDivs       = False
    , optIncremental       = False
    , optOffline           = False
    , optXeTeX             = False
    , optSmart             = False
    , optHtml5             = False
    , optChapters          = False
    , optHTMLMathMethod    = PlainMath
    , optReferenceODT      = Nothing
    , optEPUBStylesheet    = Nothing
    , optEPUBMetadata      = ""
    , optDumpArgs          = False
    , optIgnoreArgs        = False
    , optStrict            = False
    , optReferenceLinks    = False
    , optWrapText          = True
    , optColumns           = 72
    , optPlugins           = []
    , optEmailObfuscation  = JavascriptObfuscation
    , optIdentifierPrefix  = ""
    , optIndentedCodeClasses = []
    , optDataDir           = Nothing
    , optCiteMethod        = Citeproc
    , optBibliography      = []
    , optCslFile           = ""
    , optListings          = False
    , optAscii             = False
    }

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = map toLower arg })
                  "FORMAT")
                 ""

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = map toLower arg })
                  "FORMAT")
                 ""

    , Option "s" ["standalone"]
                 (NoArg
                  (\opt -> return opt { optStandalone = True }))
                 "" -- "Include needed header and footer on output"

    , Option "o" ["output"]
                 (ReqArg
                  (\arg opt -> return opt { optOutputFile = arg })
                  "FILENAME")
                 "" -- "Name of output file"

    , Option "p" ["preserve-tabs"]
                 (NoArg
                  (\opt -> return opt { optPreserveTabs = True }))
                 "" -- "Preserve tabs instead of converting to spaces"

    , Option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt ->
                      case reads arg of
                           [(t,"")] | t > 0 -> return opt { optTabStop = t }
                           _          -> do
                               UTF8.hPutStrLn stderr $
                                   "tab-stop must be a number greater than 0"
                               exitWith $ ExitFailure 31)
                  "NUMBER")
                 "" -- "Tab stop (default 4)"

    , Option "" ["strict"]
                 (NoArg
                  (\opt -> return opt { optStrict = True } ))
                 "" -- "Disable markdown syntax extensions"

    , Option "" ["normalize"]
                 (NoArg
                  (\opt -> return opt { optTransforms =
                                   normalize : optTransforms opt } ))
                 "" -- "Normalize the Pandoc AST"

    , Option "" ["reference-links"]
                 (NoArg
                  (\opt -> return opt { optReferenceLinks = True } ))
                 "" -- "Use reference links in parsing HTML"

    , Option "R" ["parse-raw"]
                 (NoArg
                  (\opt -> return opt { optParseRaw = True }))
                 "" -- "Parse untranslatable HTML codes and LaTeX environments as raw"

    , Option "S" ["smart"]
                 (NoArg
                  (\opt -> return opt { optSmart = True }))
                 "" -- "Use smart quotes, dashes, and ellipses"

    , Option "5" ["html5"]
                 (NoArg
                  (\opt -> return opt { optHtml5 = True }))
                 "" -- "Produce HTML5 in HTML output"

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
                      let url' = case arg of
                                      Just u   -> u
                                      Nothing  -> "http://chart.apis.google.com/chart?cht=tx&chl="
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
                      let url' = case arg of
                                      Just u   -> u
                                      Nothing  -> "https://d3eoax9i5htok0.cloudfront.net/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 "" -- "Use MathJax for HTML math"

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt -> return opt { optHTMLMathMethod = GladTeX }))
                 "" -- "Use gladtex for HTML math"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "" -- "Make list items display incrementally in Slidy/S5"

    , Option "" ["offline"]
                 (NoArg
                  (\opt -> return opt { optOffline = True,
                                        optStandalone = True }))
                 "" -- "Make slide shows include all the needed js and css"

    , Option "" ["xetex"]
                 (NoArg
                  (\opt -> do
                     UTF8.hPutStrLn stderr $ "pandoc: --xetex is deprecated. "
                       ++ "It is no longer needed for use with XeTeX."
                     return opt { optXeTeX = True }))
                 "" -- "Format latex for processing by XeTeX"

    , Option "" ["chapters"]
                 (NoArg
                  (\opt -> return opt { optChapters = True }))
                 "" -- "Use chapter for top-level sections in LaTeX, DocBook"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "" -- "Number sections in LaTeX"

    , Option "" ["listings"]
                 (NoArg
                  (\opt -> return opt { optListings = True }))
                 "" -- "Use listings package for LaTeX code blocks"

    , Option "" ["section-divs"]
                 (NoArg
                  (\opt -> return opt { optSectionDivs = True }))
                 "" -- "Put sections in div tags in HTML"

    , Option "" ["no-wrap"]
                 (NoArg
                  (\opt -> return opt { optWrapText = False }))
                 "" -- "Do not wrap text in output"

    , Option "" ["columns"]
                 (ReqArg
                  (\arg opt ->
                      case reads arg of
                           [(t,"")] | t > 0 -> return opt { optColumns = t }
                           _          -> do
                               UTF8.hPutStrLn stderr $
                                   "columns must be a number greater than 0"
                               exitWith $ ExitFailure 33)
                 "NUMBER")
                 "" -- "Length of line in characters"

    , Option "" ["ascii"]
                 (NoArg
                  (\opt -> return opt { optAscii = True }))
                 "" -- "Avoid using non-ascii characters in output"

    , Option "" ["email-obfuscation"]
                 (ReqArg
                  (\arg opt -> do
                     method <- case arg of
                            "references" -> return ReferenceObfuscation
                            "javascript" -> return JavascriptObfuscation
                            "none"       -> return NoObfuscation
                            _            -> UTF8.hPutStrLn stderr ("Error: Unknown obfuscation method: " ++ arg) >>
                                            exitWith (ExitFailure 6)
                     return opt { optEmailObfuscation = method })
                  "none|javascript|references")
                 "" -- "Method for obfuscating email in HTML"

     , Option "" ["id-prefix"]
                  (ReqArg
                   (\arg opt -> return opt { optIdentifierPrefix = arg })
                   "STRING")
                  "" -- "Prefix to add to automatically generated HTML identifiers"

     , Option "" ["indented-code-classes"]
                  (ReqArg
                   (\arg opt -> return opt { optIndentedCodeClasses = words $
                                             map (\c -> if c == ',' then ' ' else c) arg })
                   "STRING")
                  "" -- "Classes (whitespace- or comma-separated) to use for indented code-blocks"

    , Option "" ["toc", "table-of-contents"]
                (NoArg
                 (\opt -> return opt { optTableOfContents = True }))
               "" -- "Include table of contents"

    , Option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt ->
                      case reads arg of
                           [(t,"")] | t > 0 -> do
                               let oldTransforms = optTransforms opt
                               let shift = t - 1
                               return opt{ optTransforms =
                                           headerShift shift : oldTransforms }
                           _          -> do
                               UTF8.hPutStrLn stderr $
                                   "base-header-level must be a number > 0"
                               exitWith $ ExitFailure 19)
                  "NUMBER")
                 "" -- "Headers base level"

    , Option "" ["template"]
                 (ReqArg
                  (\arg opt -> do
                     return opt{ optTemplate = Just arg,
                                 optStandalone = True })
                  "FILENAME")
                 "" -- "Use custom template"

    , Option "V" ["variable"]
                 (ReqArg
                  (\arg opt ->
                     case break (`elem` ":=") arg of
                          (k,_:v) -> do
                            let newvars = optVariables opt ++ [(k,v)]
                            return opt{ optVariables = newvars }
                          _  -> do
                            UTF8.hPutStrLn stderr $ "Could not parse `" ++ arg ++ "' as a key/value pair (k=v or k:v)"
                            exitWith $ ExitFailure 17)
                  "KEY:VALUE")
                 "" -- "Use custom template"

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> do
                     -- add new link to end, so it is included in proper order
                     let newvars = optVariables opt ++ [("css",arg)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "URL")
                 "" -- "Link to CSS style sheet"

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

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt -> do
                    let newvars = ("title-prefix", arg) : optVariables opt
                    return opt { optVariables = newvars,
                                 optStandalone = True })
                  "STRING")
                 "" -- "String to prefix to HTML window title"

    , Option "" ["reference-odt"]
                 (ReqArg
                  (\arg opt -> do
                    return opt { optReferenceODT = Just arg })
                  "FILENAME")
                 "" -- "Path of custom reference.odt"

    , Option "" ["epub-stylesheet"]
                 (ReqArg
                  (\arg opt -> do
                     text <- UTF8.readFile arg
                     return opt { optEPUBStylesheet = Just text })
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
                     return opt { optEPUBMetadata = text })
                  "FILENAME")
                 "" -- "Path of epub metadata file"

    , Option "D" ["print-default-template"]
                 (ReqArg
                  (\arg _ -> do
                     templ <- getDefaultTemplate Nothing arg
                     case templ of
                          Right t -> UTF8.hPutStr stdout t
                          Left e  -> error $ show e
                     exitWith ExitSuccess)
                  "FORMAT")
                 "" -- "Print default template for FORMAT"

    , Option "" ["bibliography"]
                 (ReqArg
                  (\arg opt -> return opt { optBibliography = (optBibliography opt) ++ [arg] })
                  "FILENAME")
                 ""

    , Option "" ["csl"]
                 (ReqArg
                  (\arg opt -> return opt { optCslFile = arg })
                  "FILENAME")
                 ""

    , Option "" ["natbib"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Natbib }))
                 "" -- "Use natbib cite commands in LaTeX output"

    , Option "" ["biblatex"]
                 (NoArg
                  (\opt -> return opt { optCiteMethod = Biblatex }))
                 "" -- "Use biblatex cite commands in LaTeX output"

    , Option "" ["data-dir"]
                 (ReqArg
                  (\arg opt -> return opt { optDataDir = Just arg })
                 "DIRECTORY") -- "Directory containing pandoc data files."
                ""

    , Option "" ["dump-args"]
                 (NoArg
                  (\opt -> return opt { optDumpArgs = True }))
                 "" -- "Print output filename and arguments to stdout."

    , Option "" ["ignore-args"]
                 (NoArg
                  (\opt -> return opt { optIgnoreArgs = True }))
                 "" -- "Ignore command-line arguments."

    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     UTF8.hPutStrLn stdout (prg ++ " " ++ pandocVersion ++ compileInfo ++
                                       copyrightMessage)
                     exitWith ExitSuccess ))
                 "" -- "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     UTF8.hPutStr stdout (usageMessage prg options)
                     exitWith ExitSuccess ))
                 "" -- "Show help"
    ]

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName = usageInfo
  (programName ++ " [OPTIONS] [FILES]" ++ "\nInput formats:  " ++
  (intercalate ", " $ map fst readers) ++ "\nOutput formats:  " ++
  (intercalate ", " $ map fst writers ++ ["odt","epub"]) ++ "\nOptions:")

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
    ".lhs"      -> "markdown+lhs"
    ".textile"  -> "textile"
    ".native"   -> "native"
    ".json"     -> "json"
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
    ".epub"     -> "epub"
    ".org"      -> "org"
    ['.',y] | y `elem` ['1'..'9'] -> "man"
    _          -> "html"

main :: IO ()
main = do

  rawArgs <- liftM (map decodeString) getArgs
  prg <- getProgName
  let compatMode = (prg == "hsmarkdown")

  let (actions, args, errors) = if compatMode
                                  then ([], rawArgs, [])
                                  else getOpt Permute options rawArgs

  unless (null errors) $
    do name <- getProgName
       mapM_ (\e -> UTF8.hPutStr stderr (name ++ ": ") >> UTF8.hPutStr stderr e) errors
       UTF8.hPutStrLn stderr $ "Try " ++ name ++ " --help for more information."
       exitWith $ ExitFailure 2

  let defaultOpts' = if compatMode
                       then defaultOpts { optReader = "markdown"
                                        , optWriter = "html"
                                        , optStrict = True }
                       else defaultOpts

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts') actions

  let Opt    {  optTabStop           = tabStop
              , optPreserveTabs      = preserveTabs
              , optStandalone        = standalone
              , optReader            = readerName
              , optWriter            = writerName
              , optParseRaw          = parseRaw
              , optVariables         = variables
              , optTableOfContents   = toc
              , optTransforms        = transforms
              , optTemplate          = templatePath
              , optOutputFile        = outputFile
              , optNumberSections    = numberSections
              , optSectionDivs       = sectionDivs
              , optIncremental       = incremental
              , optOffline           = offline
              , optSmart             = smart
              , optHtml5             = html5
              , optChapters          = chapters
              , optHTMLMathMethod    = mathMethod
              , optReferenceODT      = referenceODT
              , optEPUBStylesheet    = epubStylesheet
              , optEPUBMetadata      = epubMetadata
              , optDumpArgs          = dumpArgs
              , optIgnoreArgs        = ignoreArgs
              , optStrict            = strict
              , optReferenceLinks    = referenceLinks
              , optWrapText          = wrap
              , optColumns           = columns
              , optEmailObfuscation  = obfuscationMethod
              , optIdentifierPrefix  = idPrefix
              , optIndentedCodeClasses = codeBlockClasses
              , optDataDir           = mbDataDir
              , optBibliography      = reffiles
              , optCslFile           = cslfile
              , optCiteMethod        = citeMethod
              , optListings          = listings
              , optAscii             = ascii
             } = opts

  when dumpArgs $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (\arg -> UTF8.hPutStrLn stdout arg) args
       exitWith ExitSuccess

  let sources = if ignoreArgs then [] else args

  datadir <- case mbDataDir of
                  Nothing   -> catch
                                 (liftM Just $ getAppUserDataDirectory "pandoc")
                                 (const $ return Nothing)
                  Just _    -> return mbDataDir

  -- assign reader and writer based on options and filenames
  let readerName' = if null readerName
                      then let fallback = if any isURI sources
                                             then "html"
                                             else "markdown"
                           in  defaultReaderName fallback sources
                      else readerName 

  let writerName' = if null writerName
                      then defaultWriterName outputFile
                      else writerName

  reader <- case (lookup readerName' readers) of
     Just r  -> return r
     Nothing -> error ("Unknown reader: " ++ readerName')

  templ <- case templatePath of
                Nothing -> do
                           deftemp <- getDefaultTemplate datadir writerName'
                           case deftemp of
                                 Left e   -> throwIO e
                                 Right t  -> return t
                Just tp -> do
                           -- strip off "+lhs" if present
                           let format = takeWhile (/='+') writerName'
                           let tp' = case takeExtension tp of
                                          ""   -> tp <.> format
                                          _    -> tp
                           catch (UTF8.readFile tp')
                             (\e -> if isDoesNotExistError e
                                       then catch
                                             (readDataFile datadir $
                                               "templates" </> tp')
                                             (\_ -> throwIO e)
                                       else throwIO e)

  let standalone' = standalone || isNonTextOutput writerName'

  variables' <- case (writerName', standalone', offline) of
                      ("s5", True, True) -> do
                        inc <- s5HeaderIncludes datadir
                        return $ ("s5includes", inc) : variables
                      ("slidy", True, True) -> do
                        slidyJs <- readDataFile datadir $
                                      "slidy" </> "slidy.min.js"
                        slidyCss <- readDataFile datadir $
                                      "slidy" </> "slidy.css"
                        return $ ("slidy-js", slidyJs) :
                            ("slidy-css", slidyCss) : variables
                      _ -> return variables

  variables'' <- case mathMethod of
                      LaTeXMathML Nothing -> do
                         s <- readDataFile datadir $ "data" </> "LaTeXMathML.js"
                         return $ ("mathml-script", s) : variables'
                      MathML Nothing -> do
                         s <- readDataFile datadir $ "data"</>"MathMLinHTML.js"
                         return $ ("mathml-script", s) : variables'
                      _ -> return variables'

  refs <- mapM (\f -> catch (readBiblioFile f) $ \e -> do
         UTF8.hPutStrLn stderr $ "Error reading bibliography `" ++ f ++ "'"
         UTF8.hPutStrLn stderr $ show e
         exitWith (ExitFailure 23)) reffiles >>= \rs -> return $ concat rs

  let sourceDir = if null sources
                     then "."
                     else takeDirectory (head sources)

  let slideVariant = case writerName' of
                           "s5"    -> S5Slides
                           "slidy" -> SlidySlides
                           _       -> NoSlides

  let startParserState =
         defaultParserState { stateParseRaw        = parseRaw,
                              stateTabStop         = tabStop,
                              stateLiterateHaskell = "+lhs" `isSuffixOf` readerName' ||
                                                     lhsExtension sources,
                              stateStandalone      = standalone',
                              stateCitations       = map refId refs,
                              stateSmart           = smart || writerName' `elem`
                                                              ["latex", "context", "latex+lhs", "man"],
                              stateColumns         = columns,
                              stateStrict          = strict,
                              stateIndentedCodeClasses = codeBlockClasses,
                              stateApplyMacros     = writerName' `notElem` ["latex", "latex+lhs"] }

  let writerOptions = defaultWriterOptions
                                    { writerStandalone       = standalone',
                                      writerTemplate         = templ,
                                      writerVariables        = variables'',
                                      writerEPUBMetadata     = epubMetadata,
                                      writerTabStop          = tabStop,
                                      writerTableOfContents  = toc &&
                                                               writerName' /= "s5",
                                      writerHTMLMathMethod   = mathMethod,
                                      writerSlideVariant     = slideVariant,
                                      writerIncremental      = incremental,
                                      writerCiteMethod       = citeMethod,
                                      writerBiblioFiles      = reffiles,
                                      writerIgnoreNotes      = False,
                                      writerNumberSections   = numberSections,
                                      writerSectionDivs      = sectionDivs,
                                      writerStrictMarkdown   = strict,
                                      writerReferenceLinks   = referenceLinks,
                                      writerWrapText         = wrap,
                                      writerColumns          = columns,
                                      writerLiterateHaskell  = "+lhs" `isSuffixOf` writerName' ||
                                                               lhsExtension [outputFile],
                                      writerEmailObfuscation = if strict
                                                                  then ReferenceObfuscation
                                                                  else obfuscationMethod,
                                      writerIdentifierPrefix = idPrefix,
                                      writerSourceDirectory  = sourceDir,
                                      writerUserDataDir      = datadir,
                                      writerHtml5            = html5 &&
                                                               "html" `isPrefixOf` writerName',
                                      writerChapters         = chapters, 
                                      writerListings         = listings,
                                      writerAscii            = ascii }

  when (isNonTextOutput writerName' && outputFile == "-") $
    do UTF8.hPutStrLn stderr ("Error:  Cannot write " ++ writerName ++ " output to stdout.\n" ++
                               "Specify an output file using the -o option.")
       exitWith $ ExitFailure 5

  let readSources [] = mapM readSource ["-"]
      readSources srcs = mapM readSource srcs
      readSource "-" = UTF8.getContents
      readSource src = case parseURI src of
                            Just u | uriScheme u `elem` ["http:","https:"] ->
                                       readURI u
                            _       -> UTF8.readFile src
      readURI uri = simpleHTTP (mkRequest GET uri) >>= getResponseBody >>=
                      return . toString  -- treat all as UTF8

  let convertTabs = tabFilter (if preserveTabs then 0 else tabStop)

  doc <- fmap (reader startParserState . convertTabs . intercalate "\n") (readSources sources)

  let doc0 = foldr ($) doc transforms

  doc1 <- if writerName' == "rtf"
             then bottomUpM rtfEmbedImage doc0
             else return doc0

  doc2 <- do
          if citeMethod == Citeproc && not (null refs)
             then do
                csldir <- getAppUserDataDirectory "csl"
                cslfile' <- if null cslfile
                               then findDataFile datadir "default.csl"
                               else do
                                  ex <- doesFileExist cslfile
                                  if ex
                                     then return cslfile
                                     else findDataFile datadir $
                                            replaceDirectory
                                            (replaceExtension cslfile "csl")
                                            csldir
                processBiblio cslfile' refs doc1
             else return doc1

  case lookup writerName' writers of
        Nothing | writerName' == "epub" ->
           writeEPUB epubStylesheet writerOptions doc2
           >>= B.writeFile (encodeString outputFile)
        Nothing | writerName' == "odt"  ->
           writeODT referenceODT writerOptions doc2
           >>= B.writeFile (encodeString outputFile)
        Just r  -> writerFn outputFile result
            where writerFn "-" = UTF8.putStr
                  writerFn f   = UTF8.writeFile f
                  result       = r writerOptions doc2 ++
                                 ['\n' | not standalone']
        Nothing -> error $ "Unknown writer: " ++ writerName'

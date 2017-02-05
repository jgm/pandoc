{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables, PatternGuards #-}
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
import Text.Pandoc.App (defaultOpts, convertWithOpts, Opt(..))
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Highlighting (highlightingStyles)
import Skylighting (Syntax(..), defaultSyntaxMap)
import Data.List (sort, intercalate)
import Data.Char (toUpper)
import System.Console.GetOpt
import Control.Monad
import qualified Text.Pandoc.UTF8 as UTF8
import System.Environment
import System.FilePath (takeBaseName)
import Data.Maybe (fromMaybe)
import Text.Pandoc.Shared (err)
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.Map as M
import System.Exit
import System.IO (stdout)
import Control.Applicative ((<|>))
import System.Directory (getAppUserDataDirectory)
import Paths_pandoc (getDataDir)
import Text.Printf (printf)

main :: IO ()
main = do

  rawArgs <- map UTF8.decodeArg <$> getArgs
  prg <- getProgName

  let (actions, args, unrecognizedOpts, errors) =
           getOpt' Permute options rawArgs

  let unknownOptionErrors = foldr addDeprecationNote [] unrecognizedOpts

  unless (null errors && null unknownOptionErrors) $
     err 2 $ concat errors ++ unlines unknownOptionErrors ++
        ("Try " ++ prg ++ " --help for more information.")

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts) actions
  convertWithOpts opts args

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = Just arg })
                  "FORMAT")
                 ""

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = Just arg })
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

    , Option "" ["base-header-level"]
                 (ReqArg
                  (\arg opt ->
                      case safeRead arg of
                           Just t | t > 0 && t < 6 -> do
                               return opt{ optBaseHeaderLevel = t }
                           _              -> err 19
                                       "base-header-level must be 1-5")
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
                     let (key, val) = splitField arg
                     return opt{ optMetadata = (key, val) : optMetadata opt })
                  "KEY[:VALUE]")
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

    , Option "" ["wrap"]
                 (ReqArg
                  (\arg opt ->
                    case safeRead ("Wrap" ++ uppercaseFirstLetter arg) of
                          Just o   -> return opt { optWrapText = o }
                          Nothing  -> err 77 "--wrap must be auto, none, or preserve")
                 "auto|none|preserve")
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
                 (\opt -> return opt { optHighlightStyle = Nothing }))
                 "" -- "Don't highlight source code"

    , Option "" ["highlight-style"]
                (ReqArg
                 (\arg opt -> return opt{ optHighlightStyle = Just arg })
                 "STYLE")
                 "" -- "Style for highlighted code"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeInHeader =
                                              arg : optIncludeInHeader opt,
                                            optStandalone = True })
                  "FILENAME")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeBeforeBody =
                                              arg : optIncludeBeforeBody opt,
                                           optStandalone = True })
                  "FILENAME")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> return opt{ optIncludeAfterBody =
                                              arg : optIncludeAfterBody opt,
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

    , Option "" ["reference-location"]
                 (ReqArg
                  (\arg opt -> do
                     action <- case arg of
                            "block"    -> return EndOfBlock
                            "section"  -> return EndOfSection
                            "document" -> return EndOfDocument
                            _        -> err 6
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
                        _       -> err 76 ("Top-level division must be " ++
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
                           _      -> err 57 "could not parse number-offset")
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
                  (\arg opt -> return opt{ optCss = arg : optCss opt })
                  -- add new link to end, so it is included in proper order
                  "URL")
                 "" -- "Link to CSS style sheet"

    , Option "" ["reference-doc"]
                 (ReqArg
                  (\arg opt ->
                    return opt { optReferenceDoc = Just arg })
                  "FILENAME")
                 "" -- "Path of custom reference doc"

    , Option "" ["epub-stylesheet"]
                 (ReqArg
                  (\arg opt -> return opt { optEpubStylesheet = Just arg })
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
                  (\arg opt -> return opt { optEpubMetadata = Just arg })
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

    , Option "m" ["latexmathml", "asciimathml"]
                 (OptArg
                  (\arg opt ->
                      return opt { optHTMLMathMethod = LaTeXMathML arg })
                  "URL")
                 "" -- "Use LaTeXMathML script in html output"

    , Option "" ["mathml"]
                 (NoArg
                  (\opt ->
                      return opt { optHTMLMathMethod = MathML }))
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
                      let url' = fromMaybe "https://latex.codecogs.com/png.latex?" arg
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
                      let url' = fromMaybe "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML-full" arg
                      return opt { optHTMLMathMethod = MathJax url'})
                  "URL")
                 "" -- "Use MathJax for HTML math"
    , Option "" ["katex"]
                 (OptArg
                  (\arg opt ->
                      return opt
                        { optKaTeXJS =
                           arg <|> Just "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.js"})
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
                  (\opt -> return opt { optVerbosity = DEBUG }))
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
                         (unwords readers'names)
                         (unwords writers'names)
                         (unwords $ map fst highlightingStyles)
                         ddir
                     exitSuccess ))
                 "" -- "Print bash completion script"

    , Option "" ["list-input-formats"]
                 (NoArg
                  (\_ -> do
                     mapM_ (UTF8.hPutStrLn stdout) readers'names
                     exitSuccess ))
                 ""

    , Option "" ["list-output-formats"]
                 (NoArg
                  (\_ -> do
                     mapM_ (UTF8.hPutStrLn stdout) writers'names
                     exitSuccess ))
                 ""

    , Option "" ["list-extensions"]
                 (NoArg
                  (\_ -> do
                     let showExt x = drop 4 (show x) ++
                                       if extensionEnabled x pandocExtensions
                                          then " +"
                                          else " -"
                     mapM_ (UTF8.hPutStrLn stdout . showExt)
                               ([minBound..maxBound] :: [Extension])
                     exitSuccess ))
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
                     mapM_ (UTF8.hPutStrLn stdout) $
                           map fst highlightingStyles
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

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName = usageInfo (programName ++ " [OPTIONS] [FILES]")

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
  "\nCompiled with pandoc-types " ++ VERSION_pandoc_types ++ ", texmath " ++
  VERSION_texmath ++ ", skylighting " ++ VERSION_skylighting

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

addDeprecationNote :: String -> [String] -> [String]
addDeprecationNote "--smart" =
  (("--smart has been removed.  Use +smart or -smart extension instead.\n" ++
    "For example: pandoc -f markdown+smart -t markdown-smart.") :)
addDeprecationNote "-S" = addDeprecationNote "--smart"
addDeprecationNote "--old-dashes" =
  ("--old-dashes has been removed.  Use +old_dashes extension instead." :)
addDeprecationNote "--no-wrap" =
  ("--no-wrap has been removed.  Use --wrap=none instead." :)
addDeprecationNote "--chapters" =
  ("--chapters has been removed. Use --top-level-division=chapter instead." :)
addDeprecationNote "--reference-docx" =
  ("--reference-docx has been removed. Use --reference-doc instead." :)
addDeprecationNote "--reference-odt" =
  ("--reference-odt has been removed. Use --reference-doc instead." :)
addDeprecationNote x =
  (("Unknown option " ++ x ++ ".") :)

uppercaseFirstLetter :: String -> String
uppercaseFirstLetter (c:cs) = toUpper c : cs
uppercaseFirstLetter [] = []

readers'names :: [String]
readers'names = sort (map fst (readers :: [(String, Reader PandocIO)]))

writers'names :: [String]
writers'names = sort (map fst (writers :: [(String, Writer PandocIO)]))

splitField :: String -> (String, String)
splitField s =
  case break (`elem` ":=") s of
       (k,_:v) -> (k,v)
       (k,[])  -> (k,"true")

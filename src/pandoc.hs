{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-8 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Text.Pandoc
import Text.Pandoc.ODT
import Text.Pandoc.Writers.S5 (s5HeaderIncludes)
import Text.Pandoc.LaTeXMathML (latexMathMLScript)
import Text.Pandoc.Shared ( tabFilter, ObfuscationMethod (..) )
#ifdef _HIGHLIGHTING
import Text.Pandoc.Highlighting ( languages )
#endif
import System.Environment ( getArgs, getProgName, getEnvironment )
import System.Exit ( exitWith, ExitCode (..) )
import System.FilePath
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.Char ( toLower )
import Data.List ( intercalate, isSuffixOf )
import System.IO ( stdout, stderr )
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
import System.IO ( hPutStr, hPutStrLn )
#else
import Prelude hiding ( putStr, putStrLn, writeFile, readFile, getContents )
import System.IO.UTF8
#endif
#ifdef _CITEPROC
import Text.CSL
import Text.Pandoc.Biblio
#endif
import Control.Monad (when, unless)

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2006-8 John MacFarlane\n" ++
                    "Web:  http://johnmacfarlane.net/pandoc\n" ++
                    "This is free software; see the source for copying conditions.  There is no\n" ++
                    "warranty, not even for merchantability or fitness for a particular purpose."

compileInfo :: String
compileInfo =
#ifdef _CITEPROC
  "\nCompiled with citeproc support." ++
#endif
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

-- | Association list of formats and readers.
readers :: [(String, ParserState -> String -> Pandoc)]
readers = [("native"       , readPandoc)
          ,("markdown"     , readMarkdown)
          ,("markdown+lhs" , readMarkdown)
          ,("rst"          , readRST)
          ,("rst+lhs"      , readRST)
          ,("html"         , readHtml)
          ,("latex"        , readLaTeX)
          ,("latex+lhs"    , readLaTeX)
          ]

-- | Reader for native Pandoc format.
readPandoc :: ParserState -> String -> Pandoc
readPandoc _ = read

-- | Association list of formats and writers.
writers :: [ ( String, WriterOptions -> Pandoc -> String ) ]
writers = [("native"       , writeDoc)
          ,("html"         , writeHtmlString)
          ,("html+lhs"     , writeHtmlString)
          ,("s5"           , writeS5String)
          ,("docbook"      , writeDocbook)
          ,("opendocument" , writeOpenDocument)
          ,("odt"          , writeOpenDocument)
          ,("latex"        , writeLaTeX)
          ,("latex+lhs"    , writeLaTeX)
          ,("context"      , writeConTeXt)
          ,("texinfo"      , writeTexinfo)
          ,("man"          , writeMan)
          ,("markdown"     , writeMarkdown)
          ,("markdown+lhs" , writeMarkdown)
          ,("rst"          , writeRST)
          ,("rst+lhs"      , writeRST)
          ,("mediawiki"    , writeMediaWiki)
          ,("rtf"          , writeRTF)
          ]

isNonTextOutput :: String -> Bool
isNonTextOutput = (`elem` ["odt"])

-- | Writer for Pandoc native format.
writeDoc :: WriterOptions -> Pandoc -> String
writeDoc _ = prettyPandoc

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop           :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs      :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone        :: Bool    -- ^ Include header, footer
    , optReader            :: String  -- ^ Reader format
    , optWriter            :: String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optTableOfContents   :: Bool    -- ^ Include table of contents
    , optTemplate          :: String  -- ^ Custom template
    , optVariables         :: [(String,String)] -- ^ Template variables to set
    , optBefore            :: [String] -- ^ Texts to include before body
    , optAfter             :: [String] -- ^ Texts to include after body
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optIncremental       :: Bool    -- ^ Use incremental lists in S5
    , optXeTeX             :: Bool    -- ^ Format latex for xetex
    , optSmart             :: Bool    -- ^ Use smart typography
    , optHTMLMathMethod    :: HTMLMathMethod -- ^ Method to print HTML math
    , optReferenceODT      :: Maybe FilePath -- ^ Path of reference.odt
    , optDumpArgs          :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs        :: Bool    -- ^ Ignore command-line arguments
    , optStrict            :: Bool    -- ^ Use strict markdown syntax
    , optReferenceLinks    :: Bool    -- ^ Use reference links in writing markdown, rst
    , optWrapText          :: Bool    -- ^ Wrap text
    , optSanitizeHTML      :: Bool    -- ^ Sanitize HTML
    , optPlugins           :: [Pandoc -> IO Pandoc] -- ^ Plugins to apply
    , optEmailObfuscation  :: ObfuscationMethod
    , optIdentifierPrefix  :: String
    , optIndentedCodeClasses :: [String] -- ^ Default classes for indented code blocks
#ifdef _CITEPROC
    , optBiblioFile        :: String
    , optBiblioFormat      :: String
    , optCslFile           :: String
#endif
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
    , optTemplate          = ""
    , optVariables         = []
    , optBefore            = []
    , optAfter             = []
    , optOutputFile        = "-"    -- "-" means stdout
    , optNumberSections    = False
    , optIncremental       = False
    , optXeTeX             = False
    , optSmart             = False
    , optHTMLMathMethod    = PlainMath
    , optReferenceODT      = Nothing
    , optDumpArgs          = False
    , optIgnoreArgs        = False
    , optStrict            = False
    , optReferenceLinks    = False
    , optWrapText          = True
    , optSanitizeHTML      = False
    , optPlugins           = []
    , optEmailObfuscation  = JavascriptObfuscation
    , optIdentifierPrefix  = ""
    , optIndentedCodeClasses = []
#ifdef _CITEPROC
    , optBiblioFile        = []
    , optBiblioFormat      = []
    , optCslFile           = []
#endif
    }

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (intercalate ", " $ map fst readers) ++ ")")

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (intercalate ", " $ map fst writers) ++ ")")

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
                  (\arg opt -> return opt { optTabStop = (read arg) } )
                  "TABSTOP")
                 "" -- "Tab stop (default 4)"

    , Option "" ["strict"]
                 (NoArg
                  (\opt -> return opt { optStrict = True } ))
                 "" -- "Disable markdown syntax extensions"

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

    , Option "m" ["latexmathml", "asciimathml"]
                 (OptArg
                  (\arg opt ->
                      return opt { optHTMLMathMethod = LaTeXMathML arg })
                  "URL")
                 "" -- "Use LaTeXMathML script in html output"

    , Option "" ["mimetex"]
                 (OptArg
                  (\arg opt -> return opt { optHTMLMathMethod = MimeTeX
                                  (fromMaybe "/cgi-bin/mimetex.cgi" arg)})
                  "URL")
                 "" -- "Use mimetex for HTML math"

    , Option "" ["jsmath"]
                 (OptArg
                  (\arg opt -> return opt { optHTMLMathMethod = JsMath arg})
                  "URL")
                 "" -- "Use jsMath for HTML math"

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt -> return opt { optHTMLMathMethod = GladTeX }))
                 "" -- "Use gladtex for HTML math"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "" -- "Make list items display incrementally in S5"

    , Option "" ["xetex"]
                 (NoArg
                  (\opt -> return opt { optXeTeX = True }))
                 "" -- "Format latex for processing by XeTeX"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "" -- "Number sections in LaTeX"

    , Option "" ["no-wrap"]
                 (NoArg
                  (\opt -> return opt { optWrapText = False }))
                 "" -- "Do not wrap text in output"

    , Option "" ["sanitize-html"]
                 (NoArg
                  (\opt -> return opt { optSanitizeHTML = True }))
                 "" -- "Sanitize HTML"

    , Option "" ["email-obfuscation"]
                 (ReqArg
                  (\arg opt -> do
                     method <- case arg of
                            "references" -> return ReferenceObfuscation
                            "javascript" -> return JavascriptObfuscation
                            "none"       -> return NoObfuscation
                            _            -> hPutStrLn stderr ("Error: Unknown obfuscation method: " ++ arg) >>
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

    , Option "" ["template"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt{ optTemplate = text,
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
                            hPutStrLn stderr $ "Could not parse `" ++ arg ++ "' as a key/value pair (k=v or k:v)"
                            exitWith $ ExitFailure 17)
                  "FILENAME")
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
                     text <- readFile arg
                     -- add new ones to end, so they're included in order specified
                     let newvars = optVariables opt ++ [("header-includes",text)]
                     return opt { optVariables = newvars,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     let oldBefore = optBefore opt
                     -- add new text to end, so it is included in proper order
                     return opt { optBefore =  oldBefore ++ [text] })
                  "FILENAME")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     let oldAfter = optAfter opt
                     -- add new text to end, so it is included in proper order
                     return opt { optAfter = oldAfter ++ [text]})
                  "FILENAME")
                 "" -- "File to include after document body"

    , Option "C" ["custom-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     let newVars = ("legacy-header", text) : optVariables opt
                     return opt { optVariables = newVars
                                , optStandalone = True })
                  "FILENAME")
                 "" -- "File to use for custom header (implies -s)"

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

    , Option "D" ["print-default-template"]
                 (ReqArg
                  (\arg _ -> do
                     templ <- getDefaultTemplate arg
                     case templ of
                          Right t -> hPutStr stdout t
                          Left e  -> error $ show e
                     exitWith ExitSuccess)
                  "FORMAT")
                 "" -- "Print default template for FORMAT"
#ifdef _CITEPROC
    , Option "" ["biblio"]
                 (ReqArg
                  (\arg opt -> return opt { optBiblioFile = arg} )
                  "FILENAME")
                 ""
    , Option "" ["biblio-format"]
                 (ReqArg
                  (\arg opt -> return opt { optBiblioFormat = arg} )
                  "STRING")
                 ""
    , Option "" ["csl"]
                 (ReqArg
                  (\arg opt -> return opt { optCslFile = arg} )
                  "FILENAME")
                 ""
#endif
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
                     hPutStrLn stdout (prg ++ " " ++ pandocVersion ++ compileInfo ++
                                       copyrightMessage)
                     exitWith ExitSuccess ))
                 "" -- "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     hPutStr stdout (usageMessage prg options)
                     exitWith ExitSuccess ))
                 "" -- "Show help"
    ]

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName = usageInfo
  (programName ++ " [OPTIONS] [FILES]" ++ "\nInput formats:  " ++
  (intercalate ", " $ map fst readers) ++ "\nOutput formats:  " ++
  (intercalate ", " $ map fst writers) ++ "\nOptions:")

-- Determine default reader based on source file extensions
defaultReaderName :: [FilePath] -> String
defaultReaderName [] = "markdown"
defaultReaderName (x:xs) =
  case takeExtension (map toLower x) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".tex"      -> "latex"
    ".latex"    -> "latex"
    ".ltx"      -> "latex"
    ".rst"      -> "rst"
    ".lhs"      -> "markdown+lhs"
    ".native"   -> "native"
    _           -> defaultReaderName xs

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
    ".txt"      -> "markdown"
    ".text"     -> "markdown"
    ".md"       -> "markdown"
    ".markdown" -> "markdown"
    ".lhs"      -> "markdown+lhs"
    ".texi"     -> "texinfo"
    ".texinfo"  -> "texinfo"
    ".db"       -> "docbook"
    ".odt"      -> "odt"
    ['.',y] | y `elem` ['1'..'9'] -> "man"
    _          -> "html"

main :: IO ()
main = do

  rawArgs <- getArgs
  prg <- getProgName
  let compatMode = (prg == "hsmarkdown")

  let (actions, args, errors) = if compatMode
                                  then ([], rawArgs, [])
                                  else getOpt Permute options rawArgs

  unless (null errors) $
    do name <- getProgName
       mapM_ (\e -> hPutStr stderr (name ++ ": ") >> hPutStr stderr e) errors
       hPutStrLn stderr $ "Try " ++ name ++ " --help for more information."
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
              , optBefore            = befores
              , optAfter             = afters
              , optTableOfContents   = toc
              , optTemplate          = template
              , optOutputFile        = outputFile
              , optNumberSections    = numberSections
              , optIncremental       = incremental
              , optXeTeX             = xetex
              , optSmart             = smart
              , optHTMLMathMethod    = mathMethod
              , optReferenceODT      = referenceODT
              , optDumpArgs          = dumpArgs
              , optIgnoreArgs        = ignoreArgs
              , optStrict            = strict
              , optReferenceLinks    = referenceLinks
              , optWrapText          = wrap
              , optSanitizeHTML      = sanitize
              , optEmailObfuscation  = obfuscationMethod
              , optIdentifierPrefix  = idPrefix
              , optIndentedCodeClasses = codeBlockClasses
#ifdef _CITEPROC
              , optBiblioFile         = biblioFile
              , optBiblioFormat       = biblioFormat
              , optCslFile            = cslFile
#endif
             } = opts

  when dumpArgs $
    do hPutStrLn stdout outputFile
       mapM_ (\arg -> hPutStrLn stdout arg) args
       exitWith ExitSuccess

  -- warn about deprecated options
  case lookup "legacy-header" variables of
     Just _  -> hPutStrLn stderr $
       "Warning: The -C/--custom-header is deprecated.\n" ++
       "Please transition to using --template instead."
     Nothing -> return ()

  let sources = if ignoreArgs then [] else args

  -- assign reader and writer based on options and filenames
  let readerName' = if null readerName
                      then defaultReaderName sources
                      else readerName 

  let writerName' = if null writerName
                      then defaultWriterName outputFile
                      else writerName

  reader <- case (lookup readerName' readers) of
     Just r  -> return r
     Nothing -> error ("Unknown reader: " ++ readerName')

  writer <- case (lookup writerName' writers) of
     Just r  -> return r
     Nothing -> error ("Unknown writer: " ++ writerName')

  templ <- getDefaultTemplate writerName'
  let defaultTemplate = case templ of
                             Right t -> t
                             Left  e -> error (show e)

  environment <- getEnvironment
  let columns = case lookup "COLUMNS" environment of
                 Just cols -> read cols
                 Nothing   -> stateColumns defaultParserState

  let standalone' = standalone || isNonTextOutput writerName'

#ifdef _CITEPROC
  refs <- if null biblioFile then return [] else readBiblioFile biblioFile biblioFormat
#endif

  variables' <- if writerName' == "s5" && standalone'
                   then do
                     inc <- s5HeaderIncludes
                     return $ ("header-includes", inc) : variables
                   else return variables

  variables'' <- case mathMethod of
                      LaTeXMathML Nothing -> do
                         s <- latexMathMLScript
                         return $ ("latexmathml-script", s) : variables'
                      _ -> return variables'

  let startParserState =
         defaultParserState { stateParseRaw        = parseRaw,
                              stateTabStop         = tabStop,
                              stateSanitizeHTML    = sanitize,
                              stateLiterateHaskell = "+lhs" `isSuffixOf` readerName' ||
                                                     lhsExtension sources,
                              stateStandalone      = standalone',
#ifdef _CITEPROC
                              stateCitations       = map citeKey refs,
#endif
                              stateSmart           = smart || writerName' `elem`
                                                              ["latex", "context", "man"],
                              stateColumns         = columns,
                              stateStrict          = strict,
                              stateIndentedCodeClasses = codeBlockClasses }
  let writerOptions = WriterOptions { writerStandalone       = standalone',
                                      writerTemplate         = if null template
                                                                  then defaultTemplate
                                                                  else template,
                                      writerVariables        = variables'',
                                      writerIncludeBefore    = concat befores,
                                      writerIncludeAfter     = concat afters,
                                      writerTabStop          = tabStop,
                                      writerTableOfContents  = toc &&
                                                               writerName' /= "s5",
                                      writerHTMLMathMethod   = mathMethod,
                                      writerS5               = (writerName' == "s5"),
                                      writerXeTeX            = xetex,
                                      writerIgnoreNotes      = False,
                                      writerIncremental      = incremental,
                                      writerNumberSections   = numberSections,
                                      writerStrictMarkdown   = strict,
                                      writerReferenceLinks   = referenceLinks,
                                      writerWrapText         = wrap,
                                      writerLiterateHaskell  = "+lhs" `isSuffixOf` writerName' ||
                                                               lhsExtension [outputFile],
                                      writerEmailObfuscation = if strict
                                                                  then ReferenceObfuscation
                                                                  else obfuscationMethod,
                                      writerIdentifierPrefix = idPrefix }

  when (isNonTextOutput writerName' && outputFile == "-") $
    do hPutStrLn stderr ("Error:  Cannot write " ++ writerName ++ " output to stdout.\n" ++
                               "Specify an output file using the -o option.")
       exitWith $ ExitFailure 5

  let sourceDirRelative = if null sources
                             then ""
                             else takeDirectory (head sources)

  let readSources [] = mapM readSource ["-"]
      readSources srcs = mapM readSource srcs
      readSource "-" = getContents
      readSource src = readFile src

  let convertTabs = tabFilter (if preserveTabs then 0 else tabStop)

  doc <- fmap (reader startParserState . convertTabs . intercalate "\n") (readSources sources)

  doc' <- do
#ifdef _CITEPROC
          processBiblio cslFile refs doc
#else
          return doc
#endif

  let writerOutput = writer writerOptions doc' ++ "\n"
 
  case writerName' of
       "odt"   -> saveOpenDocumentAsODT outputFile sourceDirRelative referenceODT writerOutput
       _       -> if outputFile == "-"
                     then putStr writerOutput
                     else writeFile outputFile writerOutput

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
import Text.Pandoc.Shared ( joinWithSep, HTMLMathMethod (..) )
import Text.Pandoc.Highlighting ( languages )
import System.Environment ( getArgs, getProgName, getEnvironment )
import System.Exit ( exitWith, ExitCode (..) )
import System.FilePath ( takeExtension, takeDirectory )
import System.Console.GetOpt
import Prelude hiding ( putStrLn, writeFile, readFile, getContents )
import System.IO ( stdout, stderr )
import System.IO.UTF8
import Data.Maybe ( fromMaybe )
import Data.Char ( toLower )
import Control.Monad ( (>>=) )

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2006-7 John MacFarlane\n\
                    \Web:  http://johnmacfarlane.net/pandoc\n\
                    \This is free software; see the source for copying conditions.  There is no\n\
                    \warranty, not even for merchantability or fitness for a particular purpose."

compileOptions :: String
compileOptions =
  if null languages
     then "\nCompiled without syntax highlighting support."
     else "\nCompiled with syntax highlighting support for the following languages:\n" ++ 
          (unlines $ map unwords $ chunk 5 $ 
           map (\s -> s ++ replicate (15 - length s) ' ') languages)

-- | Splits a list into groups of at most n.
chunk :: Int -> [a] -> [[a]]
chunk size lst =
  let (next, rest) = splitAt size lst
  in  if null rest then [next] else next : chunk size rest

-- | Association list of formats and readers.
readers :: [(String, ParserState -> String -> Pandoc)]
readers = [("native"   , readPandoc)
          ,("markdown" , readMarkdown)
          ,("rst"      , readRST)
          ,("html"     , readHtml)
          ,("latex"    , readLaTeX)
          ]

-- | Reader for native Pandoc format.
readPandoc :: ParserState -> String -> Pandoc
readPandoc _ input = read input
    
-- | Association list of formats and pairs of writers and default headers.
writers :: [ ( String, ( WriterOptions -> Pandoc -> String, String ) ) ]
writers = [("native"       , (writeDoc, ""))
          ,("html"         , (writeHtmlString, ""))
          ,("s5"           , (writeS5String, defaultS5Header))
          ,("docbook"      , (writeDocbook, defaultDocbookHeader))
          ,("opendocument" , (writeOpenDocument, defaultOpenDocumentHeader))
          ,("odt"          , (writeOpenDocument, defaultOpenDocumentHeader))
          ,("latex"        , (writeLaTeX, defaultLaTeXHeader))
          ,("context"      , (writeConTeXt, defaultConTeXtHeader))
          ,("texinfo"      , (writeTexinfo, ""))
          ,("man"          , (writeMan, ""))
          ,("markdown"     , (writeMarkdown, ""))
          ,("rst"          , (writeRST, ""))
          ,("mediawiki"    , (writeMediaWiki, ""))
          ,("rtf"          , (writeRTF, defaultRTFHeader))
          ]

isNonTextOutput :: String -> Bool
isNonTextOutput "odt" = True
isNonTextOutput _     = False

-- | Writer for Pandoc native format.
writeDoc :: WriterOptions -> Pandoc -> String
writeDoc _ = prettyPandoc

-- | Data structure for command line options.
data Opt = Opt
    { optPreserveTabs      :: Bool    -- ^ Convert tabs to spaces
    , optTabStop           :: Int     -- ^ Number of spaces per tab
    , optStandalone        :: Bool    -- ^ Include header, footer
    , optReader            :: String  -- ^ Reader format
    , optWriter            :: String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optCSS               :: [String] -- ^ CSS file to link to
    , optTableOfContents   :: Bool    -- ^ Include table of contents
    , optIncludeInHeader   :: String  -- ^ File to include in header
    , optIncludeBeforeBody :: String  -- ^ File to include at top of body
    , optIncludeAfterBody  :: String  -- ^ File to include at end of body
    , optCustomHeader      :: String  -- ^ Custom header to use, or "DEFAULT"
    , optTitlePrefix       :: String  -- ^ Optional prefix for HTML title
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optIncremental       :: Bool    -- ^ Use incremental lists in S5
    , optSmart             :: Bool    -- ^ Use smart typography
    , optHTMLMathMethod    :: HTMLMathMethod -- ^ Method to print HTML math
    , optDumpArgs          :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs        :: Bool    -- ^ Ignore command-line arguments
    , optStrict            :: Bool    -- ^ Use strict markdown syntax
    , optReferenceLinks    :: Bool    -- ^ Use reference links in writing markdown, rst
    , optWrapText          :: Bool    -- ^ Wrap text
    , optSanitizeHTML      :: Bool    -- ^ Sanitize HTML
    }

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optPreserveTabs      = False
    , optTabStop           = 4
    , optStandalone        = False
    , optReader            = ""    -- null for default reader
    , optWriter            = ""    -- null for default writer
    , optParseRaw          = False
    , optCSS               = []
    , optTableOfContents   = False
    , optIncludeInHeader   = ""
    , optIncludeBeforeBody = ""
    , optIncludeAfterBody  = ""
    , optCustomHeader      = "DEFAULT"
    , optTitlePrefix       = ""
    , optOutputFile        = "-"    -- "-" means stdout
    , optNumberSections    = False
    , optIncremental       = False
    , optSmart             = False
    , optHTMLMathMethod    = PlainMath
    , optDumpArgs          = False
    , optIgnoreArgs        = False
    , optStrict            = False
    , optReferenceLinks    = False
    , optWrapText          = True
    , optSanitizeHTML      = False
    }

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (joinWithSep ", " $ map fst readers) ++ ")")

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (joinWithSep ", " $ map fst writers) ++ ")")
    
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

    , Option "m" ["asciimathml"]
                 (OptArg
                  (\arg opt -> return opt { optHTMLMathMethod = 
                                               ASCIIMathML arg })
                  "URL")
                 "" -- "Use ASCIIMathML script in html output"

    , Option "" ["mimetex"]
                 (OptArg
                  (\arg opt -> return opt { optHTMLMathMethod = MimeTeX
                                  (fromMaybe "/cgi-bin/mimetex.cgi" arg)})
                  "URL")
                 "" -- "Use mimetex for HTML math"

    , Option "" ["gladtex"]
                 (NoArg
                  (\opt -> return opt { optHTMLMathMethod = GladTeX }))
                 "" -- "Use gladtex for HTML math"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "" -- "Make list items display incrementally in S5"

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

    , Option "" ["toc", "table-of-contents"]
                (NoArg
                 (\opt -> return opt { optTableOfContents = True }))
               "" -- "Include table of contents" 

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> do 
                     let old = optCSS opt
                     return opt { optCSS = old ++ [arg], 
                                  optStandalone = True })
                  "CSS")
                 "" -- "Link to CSS style sheet"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> do
                     let old = optIncludeInHeader opt
                     text <- readFile arg
                     return opt { optIncludeInHeader = old ++ text,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> do
                     let old = optIncludeBeforeBody opt
                     text <- readFile arg
                     return opt { optIncludeBeforeBody = old ++ text })
                  "FILENAME")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> do
                     let old = optIncludeAfterBody opt
                     text <- readFile arg
                     return opt { optIncludeAfterBody = old ++ text })
                  "FILENAME")
                 "" -- "File to include after document body"

    , Option "C" ["custom-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optCustomHeader = text,
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to use for custom header (implies -s)"

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt -> return opt { optTitlePrefix = arg, 
                                            optStandalone = True })
                  "STRING")
                 "" -- "String to prefix to HTML window title"
                 
    , Option "D" ["print-default-header"]
                 (ReqArg
                  (\arg _ -> do
                     let header = case (lookup arg writers) of
                           Just (_, h) -> h
                           Nothing     -> error ("Unknown reader: " ++ arg) 
                     hPutStr stdout header
                     exitWith ExitSuccess)
                  "FORMAT")
                 "" -- "Print default header for FORMAT"

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
                     hPutStrLn stderr (prg ++ " " ++ pandocVersion ++ compileOptions ++
                                       copyrightMessage)
                     exitWith $ ExitFailure 4))
                 "" -- "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     hPutStr stderr (usageMessage prg options)
                     exitWith $ ExitFailure 2))
                 "" -- "Show help"
    ]

-- Returns usage message
usageMessage :: String -> [OptDescr (Opt -> IO Opt)] -> String
usageMessage programName opts = usageInfo
  (programName ++ " [OPTIONS] [FILES]" ++ "\nInput formats:  " ++ 
  (joinWithSep ", " $ map fst readers) ++ "\nOutput formats:  " ++ 
  (joinWithSep ", " $ map fst writers) ++ "\nOptions:")
  opts
 
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
    ".native"   -> "native"
    _           -> defaultReaderName xs

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

  if (not (null errors))
    then do
      name <- getProgName
      mapM (\e -> hPutStrLn stderr e) errors
      hPutStr stderr (usageMessage name options)
      exitWith $ ExitFailure 2
    else
      return ()

  let defaultOpts' = if compatMode 
                       then defaultOpts { optReader = "markdown"
                                        , optWriter = "html"
                                        , optStrict = True }
                       else defaultOpts

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts') actions

  let Opt    { optPreserveTabs       = preserveTabs
              , optTabStop           = tabStop
              , optStandalone        = standalone
              , optReader            = readerName
              , optWriter            = writerName
              , optParseRaw          = parseRaw
              , optCSS               = css
              , optTableOfContents   = toc
              , optIncludeInHeader   = includeHeader
              , optIncludeBeforeBody = includeBefore
              , optIncludeAfterBody  = includeAfter
              , optCustomHeader      = customHeader
              , optTitlePrefix       = titlePrefix
              , optOutputFile        = outputFile
              , optNumberSections    = numberSections
              , optIncremental       = incremental
              , optSmart             = smart
              , optHTMLMathMethod    = mathMethod
              , optDumpArgs          = dumpArgs
              , optIgnoreArgs        = ignoreArgs
              , optStrict            = strict
              , optReferenceLinks    = referenceLinks
              , optWrapText          = wrap
              , optSanitizeHTML      = sanitize
             } = opts

  if dumpArgs
    then do
        hPutStrLn stdout outputFile
        mapM (\arg -> hPutStrLn stdout arg) args
        exitWith $ ExitSuccess
    else return ()

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

  (writer, defaultHeader) <- case (lookup writerName' writers) of
     Just (w,h) -> return (w, h)
     Nothing    -> error ("Unknown writer: " ++ writerName')

  environment <- getEnvironment
  let columns = case lookup "COLUMNS" environment of
                 Just cols -> read cols
                 Nothing   -> stateColumns defaultParserState

  let tabFilter _ [] = ""
      tabFilter _ ('\n':xs) = '\n':(tabFilter tabStop xs)
                                      -- remove DOS line endings
      tabFilter _ ('\r':'\n':xs) = '\n':(tabFilter tabStop xs)
      tabFilter _ ('\r':xs) = '\n':(tabFilter tabStop xs)
      tabFilter spsToNextStop ('\t':xs) = 
        if preserveTabs
           then '\t':(tabFilter tabStop xs) 
           else replicate spsToNextStop ' ' ++ tabFilter tabStop xs 
      tabFilter 1 (x:xs) = 
        x:(tabFilter tabStop xs)
      tabFilter spsToNextStop (x:xs) = 
        x:(tabFilter (spsToNextStop - 1) xs)

  let standalone' = (standalone && not strict) || writerName' == "odt"

  let startParserState = 
         defaultParserState { stateParseRaw     = parseRaw,
                              stateTabStop      = tabStop, 
                              stateSanitizeHTML = sanitize,
                              stateStandalone   = standalone',
                              stateSmart        = smart || writerName' `elem` 
                                                           ["latex", "context"],
                              stateColumns      = columns,
                              stateStrict       = strict }
  let csslink = if null css
                   then ""
                   else concatMap 
                        (\f -> "<link rel=\"stylesheet\" href=\"" ++
                               f ++ "\" type=\"text/css\" media=\"all\" />\n")
                        css
  let header = (if (customHeader == "DEFAULT") 
                   then defaultHeader
                   else customHeader) ++ csslink ++ includeHeader
  let writerOptions = WriterOptions { writerStandalone     = standalone',
                                      writerHeader         = header, 
                                      writerTitlePrefix    = titlePrefix,
                                      writerTabStop        = tabStop, 
                                      writerTableOfContents = toc &&
                                                              (not strict) &&
                                                              writerName' /= "s5",
                                      writerHTMLMathMethod = mathMethod,
                                      writerS5             = (writerName' == "s5"),
                                      writerIgnoreNotes    = False,
                                      writerIncremental    = incremental, 
                                      writerNumberSections = numberSections,
                                      writerIncludeBefore  = includeBefore, 
                                      writerIncludeAfter   = includeAfter,
                                      writerStrictMarkdown = strict,
                                      writerReferenceLinks = referenceLinks,
                                      writerWrapText       = wrap }

  let writeOutput = if writerName' == "odt"
                       then if outputFile == "-"
                               then \_ -> do
                                 hPutStrLn stderr ("Error:  Cannot write " ++ writerName ++ 
                                     " output to stdout.\n" ++
                                     "Specify an output file using the -o option.")
                                 exitWith $ ExitFailure 5
                               else let sourceDirRelative = if null sources
                                                               then ""
                                                               else takeDirectory (head sources)
                                    in  saveOpenDocumentAsODT outputFile sourceDirRelative
                       else if outputFile == "-"
                               then putStrLn
                               else writeFile outputFile . (++ "\n")

  (readSources sources) >>=  writeOutput .
                             writer writerOptions .
                             reader startParserState .
                             tabFilter tabStop .
                             joinWithSep "\n"

  where 
    readSources [] = mapM readSource ["-"]
    readSources sources = mapM readSource sources
    readSource "-" = getContents
    readSource source = readFile source

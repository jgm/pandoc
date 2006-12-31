{-
Copyright (C) 2006 John MacFarlane <jgm at berkeley dot edu>

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
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha 
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Text.Pandoc.UTF8 ( decodeUTF8, encodeUTF8 )
import Text.Pandoc.Readers.Markdown ( readMarkdown )
import Text.Pandoc.Readers.HTML ( readHtml )
import Text.Pandoc.Writers.S5 ( s5CSS, s5Javascript, writeS5 )
import Text.Pandoc.Writers.RST ( writeRST )
import Text.Pandoc.Readers.RST ( readRST )
import Text.Pandoc.ASCIIMathML ( asciiMathMLScript )
import Text.Pandoc.Writers.HTML ( writeHtml )
import Text.Pandoc.Writers.LaTeX ( writeLaTeX )
import Text.Pandoc.Readers.LaTeX ( readLaTeX )
import Text.Pandoc.Writers.RTF ( writeRTF )
import Text.Pandoc.Writers.Markdown ( writeMarkdown )
import Text.Pandoc.Writers.DefaultHeaders ( defaultHtmlHeader, 
       defaultRTFHeader, defaultS5Header, defaultLaTeXHeader )
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Regex ( mkRegex, matchRegex )
import System ( exitWith, getArgs, getProgName )
import System.Exit
import System.Console.GetOpt
import System.IO
import Data.Maybe ( fromMaybe )
import Data.List ( isPrefixOf )
import Char ( toLower )
import Control.Monad ( (>>=) )

version :: String
version = "0.3"

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2006 John MacFarlane\nWeb:  http://sophos.berkeley.edu/macfarlane/pandoc\nThis is free software; see the source for copying conditions.  There is no\nwarranty, not even for merchantability or fitness for a particular purpose."

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
readPandoc state input = read input
    
-- | Association list of formats and pairs of writers and default headers.
writers :: [ ( String, ( WriterOptions -> Pandoc -> String, String ) ) ]
writers = [("native"   , (writeDoc, ""))
          ,("html"     , (writeHtml, defaultHtmlHeader))
          ,("s5"       , (writeS5, defaultS5Header))
          ,("latex"    , (writeLaTeX, defaultLaTeXHeader))
          ,("markdown" , (writeMarkdown, ""))
          ,("rst"      , (writeRST, ""))
          ,("rtf"      , (writeRTF, defaultRTFHeader))
          ]

-- | Writer for Pandoc native format.
writeDoc :: WriterOptions -> Pandoc -> String
writeDoc options = prettyPandoc 

-- | Data structure for command line options.
data Opt = Opt
    { optPreserveTabs      :: Bool    -- ^ Convert tabs to spaces
    , optTabStop           :: Int     -- ^ Number of spaces per tab
    , optStandalone        :: Bool    -- ^ Include header, footer
    , optReader            :: String  -- ^ Reader format
    , optWriter            :: String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optCSS               :: String  -- ^ CSS file to link to
    , optIncludeInHeader   :: String  -- ^ File to include in header
    , optIncludeBeforeBody :: String  -- ^ File to include at top of body
    , optIncludeAfterBody  :: String  -- ^ File to include at end of body
    , optCustomHeader      :: String  -- ^ Custom header to use, or "DEFAULT"
    , optTitlePrefix       :: String  -- ^ Optional prefix for HTML title
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optIncremental       :: Bool    -- ^ Use incremental lists in S5
    , optSmart             :: Bool    -- ^ Use smart typography
    , optASCIIMathML       :: Bool    -- ^ Use ASCIIMathML in HTML
    , optDebug             :: Bool    -- ^ Output debug messages 
    , optStrict            :: Bool    -- ^ Use strict markdown syntax
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
    , optCSS               = ""
    , optIncludeInHeader   = ""
    , optIncludeBeforeBody = ""
    , optIncludeAfterBody  = ""
    , optCustomHeader      = "DEFAULT"
    , optTitlePrefix       = ""
    , optOutputFile        = ""    -- null for stdout
    , optNumberSections    = False
    , optIncremental       = False
    , optSmart             = False
    , optASCIIMathML       = False
    , optDebug             = False
    , optStrict            = False
    }

-- | A list of functions, each transforming the options data structure
--   in response to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> return opt { optReader = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (joinWithSep ", " (map fst readers)) ++ ")")

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> return opt { optWriter = map toLower arg })
                  "FORMAT")
                 "" -- ("(" ++ (joinWithSep ", " (map fst writers)) ++ ")")
    
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
                 "" -- "Use strict markdown syntax with no extensions"

    , Option "R" ["parse-raw"]
                 (NoArg
                  (\opt -> return opt { optParseRaw = True }))
                 "" -- "Parse untranslatable HTML codes and LaTeX environments as raw"

    , Option "S" ["smart"]
                 (NoArg
                  (\opt -> return opt { optSmart = True }))
                 "" -- "Use smart quotes, dashes, and ellipses in HTML output"

    , Option "m" ["asciimathml"]
                 (NoArg
                  (\opt -> return opt { optASCIIMathML = True, 
                                        optStandalone = True }))
                 "" -- "Use ASCIIMathML script in html output"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "" -- "Make list items display incrementally in S5"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "" -- "Number sections in LaTeX"

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> return opt { optCSS = arg, 
                                            optStandalone = True })
                  "CSS")
                 "" -- "Link to CSS style sheet"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeInHeader = text, 
                                  optStandalone = True })
                  "FILENAME")
                 "" -- "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeBeforeBody = text })
                  "FILENAME")
                 "" -- "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeAfterBody = text })
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
                  (\arg opt -> do
                     let header = case (lookup arg writers) of
                           Just (writer, head) -> head
                           Nothing     -> error ("Unknown reader: " ++ arg) 
                     hPutStr stdout header
                     exitWith ExitSuccess)
                  "FORMAT")
                 "" -- "Print default header for FORMAT"

    , Option "d" ["debug"]
                 (NoArg
                  (\opt -> return opt { optDebug = True }))
                 "" -- "Print debug messages to stderr, output to stdout"
    
    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     hPutStrLn stderr (prg ++ " " ++ version ++ 
                                       copyrightMessage)
                     exitWith $ ExitFailure 2))
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
usageMessage programName options = usageInfo  
  (programName ++ " [OPTIONS] [FILES]" ++ "\nInput formats:  " ++ 
  joinWithSep ", " (map fst readers) ++ "\nOutput formats:  " ++ 
  joinWithSep ", " (map fst writers) ++ "\nOptions:")
  options
 
-- Determine default reader based on source file extensions
defaultReaderName :: [String] -> String
defaultReaderName [] = "markdown"
defaultReaderName (x:xs) = 
  let x' = map toLower x in
  case (matchRegex (mkRegex ".*\\.(.*)") x') of
    Nothing         -> defaultReaderName xs -- no extension
    Just ["xhtml"]  -> "html"
    Just ["html"]   -> "html"
    Just ["htm"]    -> "html"
    Just ["tex"]    -> "latex"
    Just ["latex"]  -> "latex"
    Just ["ltx"]    -> "latex"
    Just ["rst"]    -> "rst"
    Just ["native"] -> "native"
    Just _          -> "markdown"

-- Determine default writer based on output file extension
defaultWriterName :: String -> String
defaultWriterName "" = "html" -- no output file
defaultWriterName x =
  let x' = map toLower x in
  case (matchRegex (mkRegex ".*\\.(.*)") x') of
    Nothing           -> "markdown" -- no extension
    Just [""]         -> "markdown" -- empty extension 
    Just ["tex"]      -> "latex"
    Just ["latex"]    -> "latex"
    Just ["ltx"]      -> "latex"
    Just ["rtf"]      -> "rtf"
    Just ["rst"]      -> "rst"
    Just ["s5"]       -> "s5"
    Just ["native"]   -> "native"
    Just ["txt"]      -> "markdown"
    Just ["text"]     -> "markdown"
    Just ["md"]       -> "markdown"
    Just ["markdown"] -> "markdown"
    Just _            -> "html"

main = do

  args <- getArgs
  let (actions, sources, errors) = getOpt Permute options args

  if (not (null errors))
    then do
      name <- getProgName
      mapM (\e -> hPutStrLn stderr e) errors
      hPutStr stderr (usageMessage name options)
      exitWith $ ExitFailure 2
    else
      return ()

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts) actions

  let Opt    { optPreserveTabs       = preserveTabs
              , optTabStop           = tabStop
              , optStandalone        = standalone
              , optReader            = readerName
              , optWriter            = writerName
              , optParseRaw          = parseRaw
              , optCSS               = css
              , optIncludeInHeader   = includeHeader
              , optIncludeBeforeBody = includeBefore
              , optIncludeAfterBody  = includeAfter
              , optCustomHeader      = customHeader
              , optTitlePrefix       = titlePrefix
              , optOutputFile        = outputFile
              , optNumberSections    = numberSections
              , optIncremental       = incremental
              , optSmart             = smart
              , optASCIIMathML       = asciiMathML
			  , optDebug             = debug
              , optStrict            = strict
             } = opts

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

  output <- if ((null outputFile) || debug)
              then return stdout 
              else openFile outputFile WriteMode

  if debug 
	then do
        hPutStrLn stderr ("OUTPUT=" ++ outputFile)
        hPutStr stderr $ concatMap (\s -> "INPUT=" ++ s ++ "\n") sources
    else return ()

  let tabFilter = if preserveTabs then id else (tabsToSpaces tabStop)
  let addBlank str = str ++ "\n\n"
  let removeCRs str = filter (/= '\r') str  -- remove DOS-style line endings
  let filter = tabFilter . addBlank . removeCRs
  let startParserState = defaultParserState { stateParseRaw     = parseRaw,
                                              stateTabStop      = tabStop, 
                                              stateStandalone   = standalone &&
                                                                  (not strict),
                                              stateStrict       = strict }
  let csslink = if (css == "")
                   then "" 
                   else "<link rel=\"stylesheet\" href=\"" ++ css ++ 
                        "\" type=\"text/css\" media=\"all\" />\n"
  let asciiMathMLLink = if asciiMathML then asciiMathMLScript else ""
  let header = (if (customHeader == "DEFAULT") 
                   then defaultHeader
                   else customHeader) ++ 
               csslink ++ asciiMathMLLink ++ includeHeader
  let writerOptions = WriterOptions { writerStandalone     = standalone &&
                                                             (not strict), 
                                      writerHeader         = header, 
                                      writerTitlePrefix    = titlePrefix,
                                      writerSmart          = smart && 
                                                             (not strict), 
                                      writerTabStop        = tabStop, 
                                      writerS5             = (writerName=="s5"),
                                      writerIncremental    = incremental, 
                                      writerNumberSections = numberSections,
                                      writerIncludeBefore  = includeBefore, 
                                      writerIncludeAfter   = includeAfter,
                                      writerStrictMarkdown = strict }

  (readSources sources) >>= (hPutStr output . encodeUTF8 . 
                             (writer writerOptions) . 
                             (reader startParserState) .  filter .
                             decodeUTF8 . (joinWithSep "\n")) >> hClose output

  where 
    readSources [] = mapM readSource ["-"]
    readSources sources = mapM readSource sources
    readSource "-" = getContents
    readSource source = readFile source

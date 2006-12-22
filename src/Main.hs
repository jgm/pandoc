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
import Text.Regex ( mkRegex, splitRegex )
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
    { optPreserveTabs      :: Bool    -- ^ If @False@, convert tabs to spaces
    , optTabStop           :: Int     -- ^ Number of spaces per tab
    , optStandalone        :: Bool    -- ^ If @True@, include header, footer
    , optReader            :: ParserState -> String -> Pandoc -- ^ Read format
    , optWriter            :: WriterOptions -> Pandoc -> String -- ^ Write fmt
    , optParseRaw          :: Bool    -- ^ If @True@, parse unconvertable 
                                      -- HTML and TeX
    , optCSS               :: String  -- ^ CSS file to link to
    , optIncludeInHeader   :: String  -- ^ File to include in header
    , optIncludeBeforeBody :: String  -- ^ File to include at top of body
    , optIncludeAfterBody  :: String  -- ^ File to include at end of body
    , optCustomHeader      :: String  -- ^ Custom header to use, or "DEFAULT"
    , optDefaultHeader     :: String  -- ^ Default header
    , optTitlePrefix       :: String  -- ^ Optional prefix for HTML title
    , optOutputFile        :: String  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ If @True@, number sections in LaTeX
    , optIncremental       :: Bool    -- ^ If @True@, incremental lists in S5
    , optSmart             :: Bool    -- ^ If @True@, use smart typography
    , optASCIIMathML       :: Bool    -- ^ If @True@, use ASCIIMathML in HTML
    , optShowUsage         :: Bool    -- ^ If @True@, show usage message
    , optDebug             :: Bool    -- ^ If @True@, output debug messages 
    }

-- | Defaults for command-line options.
startOpt :: Opt
startOpt = Opt
    { optPreserveTabs      = False
    , optTabStop           = 4
    , optStandalone        = False
    , optReader            = readMarkdown
    , optWriter            = writeHtml
    , optParseRaw          = False
    , optCSS               = ""
    , optIncludeInHeader   = ""
    , optIncludeBeforeBody = ""
    , optIncludeAfterBody  = ""
    , optCustomHeader      = "DEFAULT"
    , optDefaultHeader     = defaultHtmlHeader
    , optTitlePrefix       = ""
    , optOutputFile        = ""    -- null for stdout
    , optNumberSections    = False
    , optIncremental       = False
    , optSmart             = False
    , optASCIIMathML       = False
    , optShowUsage         = False
    , optDebug             = False
    }

-- | A list of functions, each transforming the options data structure in response
-- to a command-line option.
allOptions :: [OptDescr (Opt -> IO Opt)]
allOptions =
    [ Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> case (lookup (map toLower arg) readers) of
                      Just reader -> return opt { optReader = reader }
                      Nothing     -> error ("Unknown reader: " ++ arg) )
                  "FORMAT")
                 ("Source format (" ++ 
                  (concatMap (\(name, fn) -> " " ++ name) readers) ++ " )")

    , Option "tw" ["to","write"]
                 (ReqArg
                  (\arg opt -> case (lookup (map toLower arg) writers) of
                      Just (writer, defaultHeader) -> 
                              return opt { optWriter = writer, 
                                           optDefaultHeader = defaultHeader }
                      Nothing     -> error ("Unknown writer: " ++ arg) )
                  "FORMAT")
                 ("Output format (" ++ (concatMap (\(name, fn) -> " " ++ name) writers) ++ " )")
    
    , Option "s" ["standalone"]
                 (NoArg
                  (\opt -> return opt { optStandalone = True }))
                 "Include needed header and footer on output"

    , Option "o" ["output"]
                 (ReqArg
                  (\arg opt -> do
                     return opt { optOutputFile = arg })
                  "FILENAME")
                 "Name of output file"

    , Option "p" ["preserve-tabs"]
                 (NoArg
                  (\opt -> return opt { optPreserveTabs = True }))
                 "Preserve tabs instead of converting to spaces"

    , Option "" ["tab-stop"]
                 (ReqArg
                  (\arg opt -> return opt { optTabStop = (read arg) } )
                  "TABSTOP")
                 "Tab stop (default 4)"

    , Option "R" ["parse-raw"]
                 (NoArg
                  (\opt -> return opt { optParseRaw = True }))
                 "Parse untranslatable HTML codes and LaTeX environments as raw"

    , Option "S" ["smart"]
                 (NoArg
                  (\opt -> return opt { optSmart = True }))
                 "Use smart quotes, dashes, and ellipses in HTML output"

    , Option "m" ["asciimathml"]
                 (NoArg
                  (\opt -> return opt { optASCIIMathML = True, 
                                        optStandalone = True }))
                 "Use ASCIIMathML script in html output"

    , Option "i" ["incremental"]
                 (NoArg
                  (\opt -> return opt { optIncremental = True }))
                 "Make list items display incrementally in S5"

    , Option "N" ["number-sections"]
                 (NoArg
                  (\opt -> return opt { optNumberSections = True }))
                 "Number sections in LaTeX"

    , Option "c" ["css"]
                 (ReqArg
                  (\arg opt -> return opt { optCSS = arg, 
                                            optStandalone = True })
                  "CSS")
                 "Link to CSS style sheet"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeInHeader = text, 
                                  optStandalone = True })
                  "FILENAME")
                 "File to include at end of header (implies -s)"

    , Option "B" ["include-before-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeBeforeBody = text })
                  "FILENAME")
                 "File to include before document body"

    , Option "A" ["include-after-body"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeAfterBody = text })
                  "FILENAME")
                 "File to include after document body"

    , Option "C" ["custom-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optCustomHeader = text, 
                                  optStandalone = True })
                  "FILENAME")
                 "File to use for custom header (implies -s)"

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt -> return opt { optTitlePrefix = arg, 
                                            optStandalone = True })
                  "STRING")
                 "String to prefix to HTML window title"
                 
    , Option "D" ["print-default-header"]
                 (ReqArg
                  (\arg opt -> do
                     let header = case (lookup arg writers) of
                           Just (writer, head) -> head
                           Nothing     -> error ("Unknown reader: " ++ arg) 
                     hPutStr stdout header
                     exitWith ExitSuccess)
                  "FORMAT")
                 "Print default header for FORMAT"

    , Option "d" ["debug"]
                 (NoArg
                  (\opt -> return opt { optDebug = True }))
                 "Print debug messages to stderr, output to stdout"
    
    , Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     hPutStrLn stderr (prg ++ " " ++ version ++ 
                                       copyrightMessage)
                     exitWith $ ExitFailure 2))
                 "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\opt -> return opt { optShowUsage = True }))
                 "Show help"
    ]

-- parse name of calling program and return default reader and writer descriptions
parseProgName name =
    case (splitRegex (mkRegex "2") (map toLower name)) of
      [from, to] -> (from, to)
      _          -> ("markdown", "html")

-- set default options based on reader and writer descriptions; start is starting options
setDefaultOpts from to start =
    case ((lookup from readers), (lookup to writers)) of
      (Just reader, Just (writer, header)) -> start {optReader      = reader, 
                                                     optWriter      = writer, 
                                                     optDefaultHeader = header}
      _                                    -> start

-- True if single-letter option is in option list
inOptList :: [Char] -> OptDescr (Opt -> IO Opt) -> Bool
inOptList list desc =
  let (Option letters _ _ _) = desc in
  any (\x -> x `elem` list) letters

-- Reformat usage message so it doesn't wrap illegibly
reformatUsageInfo = gsub "   *--" "  --" .
                    gsub "(-[A-Za-z0-9])   *--" "\\1, --" . 
                    gsub "   *([^- ])" "\n\t\\1"

main = do

  name <- getProgName
  let (from, to) = parseProgName name

  let irrelevantOptions = if not ('2' `elem` name)
         then ""
         else "frtwD" ++
              (if (to /= "html" && to /= "s5") then "SmcT" else "") ++
              (if (to /= "latex") then "N" else "") ++
              (if (to /= "s5") then "i" else "") ++
              (if (from /= "html" && from /= "latex") then "R" else "")
  
  let options = filter (not . inOptList irrelevantOptions) allOptions

  let defaultOpts = setDefaultOpts from to startOpt

  args <- getArgs
  let (actions, sources, errors) = getOpt Permute options args

  if (not (null errors))
    then do
      mapM (\e -> hPutStrLn stderr e) errors
      hPutStrLn stderr (reformatUsageInfo $ 
                        usageInfo (name ++ " [OPTIONS] [FILES]") options)
      exitWith $ ExitFailure 2
    else
      return ()

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return defaultOpts) actions

  let Opt    { optPreserveTabs       = preserveTabs
              , optTabStop           = tabStop
              , optStandalone        = standalone
              , optReader            = reader
              , optWriter            = writer
              , optParseRaw          = parseRaw
              , optCSS               = css
              , optIncludeInHeader   = includeHeader
              , optIncludeBeforeBody = includeBefore
              , optIncludeAfterBody  = includeAfter
              , optCustomHeader      = customHeader
              , optDefaultHeader     = defaultHeader 
              , optTitlePrefix       = titlePrefix
              , optOutputFile        = outputFile
              , optNumberSections    = numberSections
              , optIncremental       = incremental
              , optSmart             = smart
              , optASCIIMathML       = asciiMathML
              , optShowUsage         = showUsage
			  , optDebug             = debug
             } = opts

  if showUsage
    then do
        hPutStr stderr (reformatUsageInfo $ usageInfo (name ++ " [OPTIONS] [FILES]") options)
        exitWith $ ExitFailure 2
    else return ()

  output <- if ((null outputFile) || debug)
              then return stdout 
              else openFile outputFile WriteMode

  if debug 
	then do
        hPutStrLn stderr ("OUTPUT=" ++ outputFile)
        hPutStr stderr $ concatMap (\s -> "INPUT=" ++ s ++ "\n") sources
    else return ()

  let writingS5 = (defaultHeader == defaultS5Header)
  let tabFilter = if preserveTabs then id else (tabsToSpaces tabStop)
  let addBlank str = str ++ "\n\n"
  let removeCRs str = filter (/= '\r') str  -- remove DOS-style line endings
  let filter = tabFilter . addBlank . removeCRs
  let startParserState = defaultParserState { stateParseRaw     = parseRaw,
                                              stateTabStop      = tabStop, 
                                              stateStandalone   = standalone }
  let csslink = if (css == "")
                   then "" 
                   else "<link rel=\"stylesheet\" href=\"" ++ css ++ 
                        "\" type=\"text/css\" media=\"all\" />\n"
  let asciiMathMLLink = if asciiMathML then asciiMathMLScript else ""
  let header = (if (customHeader == "DEFAULT") 
                   then defaultHeader
                   else customHeader) ++ 
               csslink ++ asciiMathMLLink ++ includeHeader
  let writerOptions = WriterOptions { writerStandalone     = standalone, 
                                      writerHeader         = header, 
                                      writerTitlePrefix    = titlePrefix,
                                      writerSmart          = smart, 
                                      writerTabStop        = tabStop, 
                                      writerS5             = writingS5,
                                      writerIncremental    = incremental, 
                                      writerNumberSections = numberSections,
                                      writerIncludeBefore  = includeBefore, 
                                      writerIncludeAfter   = includeAfter }

  (readSources sources) >>= (hPutStr output . encodeUTF8 . 
                             (writer writerOptions) . 
                             (reader startParserState) .  filter .
                             decodeUTF8 . (joinWithSep "\n")) >> hClose output

  where 
    readSources [] = mapM readSource ["-"]
    readSources sources = mapM readSource sources
    readSource "-" = getContents
    readSource source = readFile source

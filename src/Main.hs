-- | Main Pandoc program.  Parses command-line options and calls the
-- appropriate readers and writers.
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
import Text.Pandoc.Writers.DefaultHeaders ( defaultHtmlHeader, defaultRTFHeader, defaultS5Header, defaultLaTeXHeader )
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import System ( exitWith, getArgs, getProgName )
import System.Exit
import System.Console.GetOpt
import IO ( stdout, stderr, hPutStrLn )
import Data.Maybe ( fromMaybe )
import Data.List ( isPrefixOf )
import Char ( toLower )
import Control.Monad ( (>>=) )

version :: String
version = "0.2"

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
    { optPreserveTabs       :: Bool             -- ^ If @False@, convert tabs to spaces
    , optTabStop            :: Int              -- ^ Number of spaces per tab
    , optStandalone         :: Bool             -- ^ If @True@, include header and footer
    , optReader             :: ParserState -> String -> Pandoc   -- ^ Reader to use
    , optWriter             :: WriterOptions -> Pandoc -> String -- ^ Writer to use
    , optParseRaw           :: Bool             -- ^ If @True@, parse unconvertable HTML and TeX
    , optCSS                :: String           -- ^ CSS file to link to
    , optIncludeInHeader    :: String           -- ^ File to include in header
    , optIncludeBeforeBody  :: String           -- ^ File to include at beginning of body
    , optIncludeAfterBody   :: String           -- ^ File to include at end of body
    , optCustomHeader       :: String           -- ^ Custom header to use, or "DEFAULT"
    , optDefaultHeader      :: String           -- ^ Default header
    , optTitlePrefix        :: String           -- ^ Optional prefix for HTML title
    , optNumberSections     :: Bool             -- ^ If @True@, number sections in LaTeX
    , optIncremental        :: Bool             -- ^ If @True@, show lists incrementally in S5
    , optSmartypants        :: Bool             -- ^ If @True@, use smart quotes, dashes, ...
    , optASCIIMathML        :: Bool             -- ^ If @True@, use ASCIIMathML in HTML or S5
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
    , optNumberSections    = False
    , optIncremental       = False
    , optSmartypants       = False
    , optASCIIMathML       = False
    }

-- | A list of functions, each transforming the options data structure in response
-- to a command-line option.
options :: [OptDescr (Opt -> IO Opt)]
options =
    [ Option "v" ["version"]
                 (NoArg
                  (\_ -> do
                     hPutStrLn stderr ("Version " ++ version)
                     exitWith ExitSuccess))
                 "Print version"

    , Option "h" ["help"]
                 (NoArg
                  (\_ -> do
                     prg <- getProgName
                     hPutStrLn stderr (usageInfo (prg ++ " [OPTIONS] [FILES] - convert FILES from one markup format to another\nIf no OPTIONS specified, converts from markdown to html.\nIf no FILES specified, input is read from STDIN.\nOptions:") options)
                     exitWith ExitSuccess))
                 "Show help"

    , Option "fr" ["from","read"]
                 (ReqArg
                  (\arg opt -> case (lookup (map toLower arg) readers) of
                                 Just reader -> return opt { optReader = reader }
                                 Nothing     -> error ("Unknown reader: " ++ arg) )
                  "FORMAT")
                 ("Source format (" ++ (concatMap (\(name, fn) -> " " ++ name) readers) ++ " )")

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

    , Option "S" ["smartypants"]
                 (NoArg
                  (\opt -> return opt { optSmartypants = True }))
                 "Use smartypants for html output"

    , Option "m" ["asciimathml"]
                 (NoArg
                  (\opt -> return opt { optASCIIMathML = True, optStandalone = True }))
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
                  (\arg opt -> return opt { optCSS = arg, optStandalone = True })
                  "CSS")
                 "Link to CSS style sheet"

    , Option "H" ["include-in-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optIncludeInHeader = text, optStandalone = True })
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

    , Option "" ["custom-header"]
                 (ReqArg
                  (\arg opt -> do
                     text <- readFile arg
                     return opt { optCustomHeader = text, optStandalone = True })
                  "FILENAME")
                 "File to use for custom header (implies -s)"

    , Option "T" ["title-prefix"]
                 (ReqArg
                  (\arg opt -> return opt { optTitlePrefix = arg, optStandalone = True })
                  "STRING")
                 "String to prefix to HTML window title"
                 
    , Option "D" ["print-default-header"]
                 (ReqArg
                  (\arg opt -> do
                     let header = case (lookup arg writers) of
                                    Just (writer, head) -> head
                                    Nothing     -> error ("Unknown reader: " ++ arg) 
                     hPutStrLn stdout header
                     exitWith ExitSuccess)
                  "FORMAT")
                 "Print default header for FORMAT"
    ]
main = do

  args <- getArgs
  let (actions, sources, errors) = getOpt RequireOrder options args

  -- thread option data structure through all supplied option actions
  opts <- foldl (>>=) (return startOpt) actions

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
              , optNumberSections    = numberSections
              , optIncremental       = incremental
              , optSmartypants       = smartypants
              , optASCIIMathML       = asciiMathML
             } = opts

  let writingS5 = (defaultHeader == defaultS5Header)
  let tabFilter = if preserveTabs then id else (tabsToSpaces tabStop)
  let addBlank str = str ++ "\n\n"
  let removeCRs str = filter (/= '\r') str  -- remove DOS-style line endings
  let filter = tabFilter . addBlank . removeCRs
  let startParserState = defaultParserState { stateParseRaw     = parseRaw,
                                              stateTabStop      = tabStop, 
                                              stateStandalone   = standalone }
  let csslink = if (css == "") then 
                    "" 
                else 
                    "<link rel=\"stylesheet\" href=\"" ++ css ++ 
                    "\" type=\"text/css\" media=\"all\" />\n"
  let asciiMathMLLink = if asciiMathML then asciiMathMLScript else ""
  let header = (if (customHeader == "DEFAULT") then defaultHeader else customHeader) ++ 
               csslink ++ asciiMathMLLink ++ includeHeader
  let writerOptions = WriterOptions { writerStandalone     = standalone, 
                                      writerHeader         = header, 
                                      writerTitlePrefix    = titlePrefix,
                                      writerSmartypants    = smartypants, 
                                      writerTabStop        = tabStop, 
                                      writerS5             = writingS5,
                                      writerIncremental    = incremental, 
                                      writerNumberSections = numberSections,
                                      writerIncludeBefore  = includeBefore, 
                                      writerIncludeAfter   = includeAfter }

  (readSources sources) >>= (putStrLn . encodeUTF8 . (writer writerOptions) . 
                             (reader startParserState) .  filter .
                             decodeUTF8 . (joinWithSep "\n"))

  where 
    readSources [] = mapM readSource ["-"]
    readSources sources = mapM readSource sources
    readSource "-" = getContents 
    readSource source = readFile source


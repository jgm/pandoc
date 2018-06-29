{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App (
            convertWithOpts
          , Opt(..)
          , LineEnding(..)
          , Filter(..)
          , defaultOpts
          , parseOptions
          , options
          , applyFilters
          ) where
import Prelude
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower, toUpper, isAscii, ord)
import Data.List (find, intercalate, isPrefixOf, isSuffixOf, sort)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.YAML as YAML
import GHC.Generics
import Network.URI (URI (..), parseURI)
#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data (dataFiles)
#else
import System.Directory (getDirectoryContents)
import Paths_pandoc (getDataDir)
#endif
import Data.Aeson.Encode.Pretty (encodePretty', Config(..), keyOrder,
         defConfig, Indent(..), NumberFormat(..))
import Skylighting (Style, Syntax (..), defaultSyntaxMap, parseTheme,
                    pygments)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.Console.GetOpt
import System.Directory (getAppUserDataDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import System.IO.Error (isDoesNotExistError)
import Text.Pandoc
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Builder (setMeta, deleteMeta)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), applyFilters)
import Text.Pandoc.Highlighting (highlightingStyles)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained (makeDataURI, makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift, isURI, ordNub, safeRead, tabFilter, uriPathToPath)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.Math (defaultKaTeXURL, defaultMathJaxURL)
import Text.Pandoc.XML (toEntities)
import Text.Printf
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

data LineEnding = LF | CRLF | Native deriving (Show, Generic)

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
  return (opts{ optInputFiles = args })

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

pdfWriterAndProg :: Maybe String              -- ^ user-specified writer name
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> IO (String, Maybe String) -- ^ IO (writerName, maybePdfEngineProg)
pdfWriterAndProg mWriter mEngine = do
  let panErr msg = liftIO $ E.throwIO $ PandocAppError msg
  case go mWriter mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left err           -> panErr err
    where
      go Nothing Nothing       = Right ("latex", "pdflatex")
      go (Just writer) Nothing = (writer,) <$> engineForWriter writer
      go Nothing (Just engine) = (,engine) <$> writerForEngine (takeBaseName engine)
      go (Just writer) (Just engine) =
           case find (== (baseWriterName writer, takeBaseName engine)) engines of
                Just _  -> Right (writer, engine)
                Nothing -> Left $ "pdf-engine " ++ engine ++
                           " is not compatible with output format " ++ writer

      writerForEngine eng = case [f | (f,e) <- engines, e == eng] of
                                 fmt : _ -> Right fmt
                                 []      -> Left $
                                   "pdf-engine " ++ eng ++ " not known"

      engineForWriter w = case [e |  (f,e) <- engines, f == baseWriterName w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " ++ w

convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
       exitSuccess

  epubMetadata <- case optEpubMetadata opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> UTF8.readFile fp

  let isPandocCiteproc (JSONFilter f) = takeBaseName f == "pandoc-citeproc"
      isPandocCiteproc _              = False
  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      all (not . isPandocCiteproc) filters
  let filters' = if needsCiteproc then JSONFilter "pandoc-citeproc" : filters
                                  else filters

  let sources = case optInputFiles opts of
                     []  -> ["-"]
                     xs | optIgnoreArgs opts -> ["-"]
                        | otherwise  -> xs

  datadir <- case optDataDir opts of
                  Nothing   -> E.catch
                                 (Just <$> getAppUserDataDirectory "pandoc")
                                 (\e -> let _ = (e :: E.SomeException)
                                        in  return Nothing)
                  Just _    -> return $ optDataDir opts

  -- assign reader and writer based on options and filenames
  let readerName =  fromMaybe ( defaultReaderName
                  (if any isURI sources
                      then "html"
                      else "markdown") sources) (optReader opts)

  let nonPdfWriterName Nothing  = defaultWriterName outputFile
      nonPdfWriterName (Just x) = x

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"
  (writerName, maybePdfProg) <-
    if pdfOutput
       then pdfWriterAndProg (optWriter opts) (optPdfEngine opts)
       else return (nonPdfWriterName $ optWriter opts, Nothing)

  let format = map toLower $ baseWriterName
                 $ takeFileName writerName  -- in case path to lua script

  -- disabling the custom writer for now
  (writer, writerExts) <-
            if ".lua" `isSuffixOf` format
               then return (TextWriter
                       (\o d -> writeCustom writerName o d)
                               :: Writer PandocIO, mempty)
               else case getWriter (map toLower writerName) of
                         Left e  -> E.throwIO $ PandocAppError $
                           if format == "pdf"
                              then e ++
                               "\nTo create a pdf using pandoc, use " ++
                               "-t latex|beamer|context|ms|html5" ++
                               "\nand specify an output file with " ++
                               ".pdf extension (-o filename.pdf)."
                              else e
                         Right (w, es) -> return (w :: Writer PandocIO, es)

  -- TODO: we have to get the input and the output into the state for
  -- the sake of the text2tags reader.
  (reader, readerExts) <-
           case getReader readerName of
                Right (r, es) -> return (r :: Reader PandocIO, es)
                Left e   -> E.throwIO $ PandocAppError e'
                  where e' = case readerName of
                                  "pdf" -> e ++
                                     "\nPandoc can convert to PDF, but not from PDF."
                                  "doc" -> e ++
                                     "\nPandoc can convert from DOCX, but not from DOC.\nTry using Word to save your DOC file as DOCX, and convert that with pandoc."
                                  _ -> e

  let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput
  let addStringAsVariable varname s vars = return $ (varname, s) : vars

  highlightStyle <- lookupHighlightStyle $ optHighlightStyle opts
  let addSyntaxMap existingmap f = do
        res <- parseSyntaxDefinition f
        case res of
              Left errstr -> E.throwIO $ PandocSyntaxMapError errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  -- We don't want to send output to the terminal if the user
  -- does 'pandoc -t docx input.txt'; though we allow them to
  -- force this with '-o -'.  On posix systems, we detect
  -- when stdout is being piped and allow output to stdout
  -- in that case, but on Windows we can't.
#ifdef _WINDOWS
  let istty = True
#else
  istty <- queryTerminal stdOutput
#endif
  when (not (isTextFormat format) && istty && isNothing ( optOutputFile opts)) $
    E.throwIO $ PandocAppError $
            "Cannot write " ++ format ++ " output to terminal.\n" ++
            "Specify an output file using the -o option, or " ++
            "use '-o -' to force output to stdout."

  let convertTabs = tabFilter (if optPreserveTabs opts || readerName == "t2t"
                                  then 0
                                  else optTabStop opts)

      readSources :: [FilePath] -> PandocIO Text
      readSources srcs = convertTabs . T.intercalate (T.pack "\n") <$>
                            mapM readSource srcs

  let runIO' :: PandocIO a -> IO a
      runIO' f = do
        (res, reports) <- runIOorExplode $ do
                             setTrace (optTrace opts)
                             setVerbosity verbosity
                             x <- f
                             rs <- getLog
                             return (x, rs)
        case optLogFile opts of
             Nothing      -> return ()
             Just logfile -> B.writeFile logfile (encodeLogMessages reports)
        let isWarning msg = messageVerbosity msg == WARNING
        when (optFailIfWarnings opts && any isWarning reports) $
            E.throwIO PandocFailOnWarningError
        return res

  let eol = case optEol opts of
                 CRLF   -> IO.CRLF
                 LF     -> IO.LF
                 Native -> nativeNewline

  -- note: this reverses the list constructed in option parsing,
  -- which in turn was reversed from the command-line order,
  -- so we end up with the correct order in the variable list:
  let withList _ [] vars     = return vars
      withList f (x:xs) vars = f x vars >>= withList f xs

  let addContentsAsVariable varname fp vars = do
             s <- UTF8.toString <$> readFileStrict fp
             return $ (varname, s) : vars

  runIO' $ do
    setUserDataDir datadir
    setInputFiles (optInputFiles opts)
    setOutputFile (optOutputFile opts)

    variables <-
        withList (addStringAsVariable "sourcefile")
                 (reverse $ optInputFiles opts)
                 (("outputfile", fromMaybe "-" (optOutputFile opts))
                  : optVariables opts)
                 -- we reverse this list because, unlike
                 -- the other option lists here, it is
                 -- not reversed when parsed from CLI arguments.
                 -- See withList, above.
        >>=
        withList (addContentsAsVariable "include-before")
                 (optIncludeBeforeBody opts)
        >>=
        withList (addContentsAsVariable "include-after")
                 (optIncludeAfterBody opts)
        >>=
        withList (addContentsAsVariable "header-includes")
                 (optIncludeInHeader opts)
        >>=
        withList (addStringAsVariable "css") (optCss opts)
        >>=
        maybe return (addStringAsVariable "title-prefix")
                     (optTitlePrefix opts)
        >>=
        maybe return (addStringAsVariable "epub-cover-image")
                     (optEpubCoverImage opts)
        >>=
        (\vars ->  if format == "dzslides"
                      then do
                          dztempl <- UTF8.toString <$> readDataFile
                                       ("dzslides" </> "template.html")
                          let dzline = "<!-- {{{{ dzslides core"
                          let dzcore = unlines
                                     $ dropWhile (not . (dzline `isPrefixOf`))
                                     $ lines dztempl
                          return $ ("dzslides-core", dzcore) : vars
                      else return vars)

    abbrevs <- (Set.fromList . filter (not . null) . lines) <$>
               case optAbbreviations opts of
                    Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                    Just f  -> UTF8.toString <$> readFileStrict f

    templ <- case optTemplate opts of
                    _ | not standalone -> return Nothing
                    Nothing -> Just <$> getDefaultTemplate format
                    Just tp -> do
                      -- strip off extensions
                      let tp' = case takeExtension tp of
                                     "" -> tp <.> format
                                     _  -> tp
                      Just . UTF8.toString <$>
                            ((fst <$> fetchItem tp') `catchError`
                             (\e ->
                                 case e of
                                      PandocIOError _ e' |
                                        isDoesNotExistError e' ->
                                         readDataFile ("templates" </> tp')
                                      _ -> throwError e))

    metadata <- if format == "jats" &&
                   isNothing (lookup "csl" (optMetadata opts)) &&
                   isNothing (lookup "citation-style" (optMetadata opts))
                   then do
                     jatsCSL <- readDataFile "jats.csl"
                     let jatsEncoded = makeDataURI
                                         ("application/xml", jatsCSL)
                     return $ ("csl", jatsEncoded) : optMetadata opts
                   else return $ optMetadata opts

    case lookup "lang" (optMetadata opts) of
           Just l  -> case parseBCP47 l of
                           Left _   -> return ()
                           Right l' -> setTranslations l'
           Nothing -> setTranslations $ Lang "en" "" "US" []

    let writerOptions = def {
            writerTemplate         = templ
          , writerVariables        = variables
          , writerTabStop          = optTabStop opts
          , writerTableOfContents  = optTableOfContents opts
          , writerHTMLMathMethod   = optHTMLMathMethod opts
          , writerIncremental      = optIncremental opts
          , writerCiteMethod       = optCiteMethod opts
          , writerNumberSections   = optNumberSections opts
          , writerNumberOffset     = optNumberOffset opts
          , writerSectionDivs      = optSectionDivs opts
          , writerExtensions       = writerExts
          , writerReferenceLinks   = optReferenceLinks opts
          , writerReferenceLocation = optReferenceLocation opts
          , writerDpi              = optDpi opts
          , writerWrapText         = optWrapText opts
          , writerColumns          = optColumns opts
          , writerEmailObfuscation = optEmailObfuscation opts
          , writerIdentifierPrefix = optIdentifierPrefix opts
          , writerHtmlQTags        = optHtmlQTags opts
          , writerTopLevelDivision = optTopLevelDivision opts
          , writerListings         = optListings opts
          , writerSlideLevel       = optSlideLevel opts
          , writerHighlightStyle   = highlightStyle
          , writerSetextHeaders    = optSetextHeaders opts
          , writerEpubSubdirectory = optEpubSubdirectory opts
          , writerEpubMetadata     = epubMetadata
          , writerEpubFonts        = optEpubFonts opts
          , writerEpubChapterLevel = optEpubChapterLevel opts
          , writerTOCDepth         = optTOCDepth opts
          , writerReferenceDoc     = optReferenceDoc opts
          , writerSyntaxMap        = syntaxMap
          }

    let readerOpts = def{
            readerStandalone = standalone
          , readerColumns = optColumns opts
          , readerTabStop = optTabStop opts
          , readerIndentedCodeClasses = optIndentedCodeClasses opts
          , readerDefaultImageExtension =
             optDefaultImageExtension opts
          , readerTrackChanges = optTrackChanges opts
          , readerAbbreviations = abbrevs
          , readerExtensions = readerExts
          , readerStripComments = optStripComments opts
          }

    let transforms = (case optBaseHeaderLevel opts of
                          x | x > 1     -> (headerShift (x - 1) :)
                            | otherwise -> id) .
                     (if optStripEmptyParagraphs opts
                         then (stripEmptyParagraphs :)
                         else id) .
                     (if extensionEnabled Ext_east_asian_line_breaks
                            readerExts &&
                         not (extensionEnabled Ext_east_asian_line_breaks
                              writerExts &&
                              writerWrapText writerOptions == WrapPreserve)
                         then (eastAsianLineBreakFilter :)
                         else id) $
                     []

    let sourceToDoc :: [FilePath] -> PandocIO Pandoc
        sourceToDoc sources' =
           case reader of
                TextReader r
                  | optFileScope opts || readerName == "json" ->
                      mconcat <$> mapM (readSource >=> r readerOpts) sources
                  | otherwise ->
                      readSources sources' >>= r readerOpts
                ByteStringReader r ->
                  mconcat <$> mapM (readFile' >=> r readerOpts) sources


    when (readerName == "markdown_github" ||
          writerName == "markdown_github") $
      report $ Deprecated "markdown_github" "Use gfm instead."

    setResourcePath (optResourcePath opts)
    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    doc <- sourceToDoc sources >>=
              (   (if isJust (optExtractMedia opts)
                      then fillMediaBag
                      else return)
              >=> return . addMetadata metadata
              >=> applyTransforms transforms
              >=> applyFilters readerOpts filters' [format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    case writer of
      ByteStringWriter f -> f writerOptions doc >>= writeFnBinary outputFile
      TextWriter f -> case maybePdfProg of
        Just pdfProg -> do
                res <- makePDF pdfProg (optPdfEngineArgs opts) f
                        writerOptions doc
                case res of
                     Right pdf -> writeFnBinary outputFile pdf
                     Left err' -> liftIO $
                       E.throwIO $ PandocPDFError $
                                     TL.unpack (TE.decodeUtf8With TE.lenientDecode err')

        Nothing -> do
                let htmlFormat = format `elem`
                      ["html","html4","html5","s5","slidy",
                       "slideous","dzslides","revealjs"]
                    escape
                      | optAscii opts
                      , htmlFormat || format == "docbook4" ||
                        format == "docbook5" || format == "docbook" ||
                        format == "jats" || format == "opml" ||
                        format == "icml" = toEntities
                      | optAscii opts
                      , format == "ms" || format == "man" = groffEscape
                      | otherwise = id
                    addNl = if standalone
                               then id
                               else (<> T.singleton '\n')
                output <- (addNl . escape) <$> f writerOptions doc
                writerFn eol outputFile =<<
                  if optSelfContained opts && htmlFormat
                     -- TODO not maximally efficient; change type
                     -- of makeSelfContained so it works w/ Text
                     then T.pack <$> makeSelfContained (T.unpack output)
                     else return output

groffEscape :: Text -> Text
groffEscape = T.concatMap toUchar
  where toUchar c
         | isAscii c = T.singleton c
         | otherwise = T.pack $ printf "\\[u%04X]" (ord c)

type Transform = Pandoc -> Pandoc

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop               :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs          :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone            :: Bool    -- ^ Include header, footer
    , optReader                :: Maybe String  -- ^ Reader format
    , optWriter                :: Maybe String  -- ^ Writer format
    , optTableOfContents       :: Bool    -- ^ Include table of contents
    , optBaseHeaderLevel       :: Int     -- ^ Base header level
    , optTemplate              :: Maybe FilePath  -- ^ Custom template
    , optVariables             :: [(String,String)] -- ^ Template variables to set
    , optMetadata              :: [(String, String)] -- ^ Metadata fields to set
    , optOutputFile            :: Maybe FilePath  -- ^ Name of output file
    , optInputFiles            :: [FilePath] -- ^ Names of input files
    , optNumberSections        :: Bool    -- ^ Number sections in LaTeX
    , optNumberOffset          :: [Int]   -- ^ Starting number for sections
    , optSectionDivs           :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental           :: Bool    -- ^ Use incremental lists in Slidy/Slideous/S5
    , optSelfContained         :: Bool    -- ^ Make HTML accessible offline
    , optHtmlQTags             :: Bool    -- ^ Use <q> tags in HTML
    , optHighlightStyle        :: Maybe String -- ^ Style to use for highlighted code
    , optSyntaxDefinitions     :: [FilePath]  -- ^ xml syntax defs to load
    , optTopLevelDivision      :: TopLevelDivision -- ^ Type of the top-level divisions
    , optHTMLMathMethod        :: HTMLMathMethod -- ^ Method to print HTML math
    , optAbbreviations         :: Maybe FilePath -- ^ Path to abbrevs file
    , optReferenceDoc          :: Maybe FilePath -- ^ Path of reference doc
    , optEpubSubdirectory      :: String -- ^ EPUB subdir in OCF container
    , optEpubMetadata          :: Maybe FilePath   -- ^ EPUB metadata
    , optEpubFonts             :: [FilePath] -- ^ EPUB fonts to embed
    , optEpubChapterLevel      :: Int     -- ^ Header level at which to split chapters
    , optEpubCoverImage        :: Maybe FilePath -- ^ Cover image for epub
    , optTOCDepth              :: Int     -- ^ Number of levels to include in TOC
    , optDumpArgs              :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs            :: Bool    -- ^ Ignore command-line arguments
    , optVerbosity             :: Verbosity  -- ^ Verbosity of diagnostic output
    , optTrace                 :: Bool  -- ^ Enable tracing
    , optLogFile               :: Maybe FilePath -- ^ File to write JSON log output
    , optFailIfWarnings        :: Bool    -- ^ Fail on warnings
    , optReferenceLinks        :: Bool    -- ^ Use reference links in writing markdown, rst
    , optReferenceLocation     :: ReferenceLocation -- ^ location for footnotes and link references in markdown output
    , optDpi                   :: Int     -- ^ Dpi
    , optWrapText              :: WrapOption  -- ^ Options for wrapping text
    , optColumns               :: Int     -- ^ Line length in characters
    , optFilters               :: [Filter] -- ^ Filters to apply
    , optEmailObfuscation      :: ObfuscationMethod
    , optIdentifierPrefix      :: String
    , optStripEmptyParagraphs  :: Bool -- ^ Strip empty paragraphs
    , optIndentedCodeClasses   :: [String] -- ^ Default classes for indented code blocks
    , optDataDir               :: Maybe FilePath
    , optCiteMethod            :: CiteMethod -- ^ Method to output cites
    , optListings              :: Bool       -- ^ Use listings package for code blocks
    , optPdfEngine             :: Maybe String -- ^ Program to use for latex/html -> pdf
    , optPdfEngineArgs         :: [String]   -- ^ Flags to pass to the engine
    , optSlideLevel            :: Maybe Int  -- ^ Header level that creates slides
    , optSetextHeaders         :: Bool       -- ^ Use atx headers for markdown level 1-2
    , optAscii                 :: Bool       -- ^ Use ascii characters only in html
    , optDefaultImageExtension :: String -- ^ Default image extension
    , optExtractMedia          :: Maybe FilePath -- ^ Path to extract embedded media
    , optTrackChanges          :: TrackChanges -- ^ Accept or reject MS Word track-changes.
    , optFileScope             :: Bool         -- ^ Parse input files before combining
    , optTitlePrefix           :: Maybe String     -- ^ Prefix for title
    , optCss                   :: [FilePath]       -- ^ CSS files to link to
    , optIncludeBeforeBody     :: [FilePath]       -- ^ Files to include before
    , optIncludeAfterBody      :: [FilePath]       -- ^ Files to include after body
    , optIncludeInHeader       :: [FilePath]       -- ^ Files to include in header
    , optResourcePath          :: [FilePath] -- ^ Path to search for images etc
    , optRequestHeaders        :: [(String, String)] -- ^ Headers for HTTP requests
    , optEol                   :: LineEnding -- ^ Style of line-endings to use
    , optStripComments         :: Bool       -- ^ Skip HTML comments
    } deriving (Generic, Show)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop               = 4
    , optPreserveTabs          = False
    , optStandalone            = False
    , optReader                = Nothing
    , optWriter                = Nothing
    , optTableOfContents       = False
    , optBaseHeaderLevel       = 1
    , optTemplate              = Nothing
    , optVariables             = []
    , optMetadata              = []
    , optOutputFile            = Nothing
    , optInputFiles            = []
    , optNumberSections        = False
    , optNumberOffset          = [0,0,0,0,0,0]
    , optSectionDivs           = False
    , optIncremental           = False
    , optSelfContained         = False
    , optHtmlQTags             = False
    , optHighlightStyle        = Just "pygments"
    , optSyntaxDefinitions     = []
    , optTopLevelDivision      = TopLevelDefault
    , optHTMLMathMethod        = PlainMath
    , optAbbreviations         = Nothing
    , optReferenceDoc          = Nothing
    , optEpubSubdirectory      = "EPUB"
    , optEpubMetadata          = Nothing
    , optEpubFonts             = []
    , optEpubChapterLevel      = 1
    , optEpubCoverImage        = Nothing
    , optTOCDepth              = 3
    , optDumpArgs              = False
    , optIgnoreArgs            = False
    , optVerbosity             = WARNING
    , optTrace                 = False
    , optLogFile               = Nothing
    , optFailIfWarnings        = False
    , optReferenceLinks        = False
    , optReferenceLocation     = EndOfDocument
    , optDpi                   = 96
    , optWrapText              = WrapAuto
    , optColumns               = 72
    , optFilters               = []
    , optEmailObfuscation      = NoObfuscation
    , optIdentifierPrefix      = ""
    , optStripEmptyParagraphs  = False
    , optIndentedCodeClasses   = []
    , optDataDir               = Nothing
    , optCiteMethod            = Citeproc
    , optListings              = False
    , optPdfEngine             = Nothing
    , optPdfEngineArgs         = []
    , optSlideLevel            = Nothing
    , optSetextHeaders         = True
    , optAscii                 = False
    , optDefaultImageExtension = ""
    , optExtractMedia          = Nothing
    , optTrackChanges          = AcceptChanges
    , optFileScope             = False
    , optTitlePrefix           = Nothing
    , optCss                   = []
    , optIncludeBeforeBody     = []
    , optIncludeAfterBody      = []
    , optIncludeInHeader       = []
    , optResourcePath          = ["."]
    , optRequestHeaders        = []
    , optEol                   = Native
    , optStripComments          = False
    }

addMetadata :: [(String, String)] -> Pandoc -> Pandoc
addMetadata kvs pdc = foldr addMeta (removeMetaKeys kvs pdc) kvs

addMeta :: (String, String) -> Pandoc -> Pandoc
addMeta (k, v) (Pandoc meta bs) = Pandoc meta' bs
  where meta' = case lookupMeta k meta of
                      Nothing -> setMeta k v' meta
                      Just (MetaList xs) ->
                                 setMeta k (MetaList (xs ++ [v'])) meta
                      Just x  -> setMeta k (MetaList [x, v']) meta
        v' = readMetaValue v

removeMetaKeys :: [(String,String)] -> Pandoc -> Pandoc
removeMetaKeys kvs pdc = foldr (deleteMeta . fst) pdc kvs

readMetaValue :: String -> MetaValue
readMetaValue s = case YAML.decodeStrict (UTF8.fromString s) of
                       Right [YAML.Scalar (YAML.SStr t)]
                                             -> MetaString $ T.unpack t
                       Right [YAML.Scalar (YAML.SBool b)]
                                             -> MetaBool b
                       _                     -> MetaString s

-- Determine default reader based on source file extensions
defaultReaderName :: String -> [FilePath] -> String
defaultReaderName fallback [] = fallback
defaultReaderName fallback (x:xs) =
  case takeExtension (map toLower x) of
    ".xhtml"    -> "html"
    ".html"     -> "html"
    ".htm"      -> "html"
    ".md"       -> "markdown"
    ".markdown" -> "markdown"
    ".muse"     -> "muse"
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
    ".fb2"      -> "fb2"
    _           -> defaultReaderName fallback xs

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
    ".muse"     -> "muse"
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
    ".fb2"      -> "fb2"
    ".opml"     -> "opml"
    ".icml"     -> "icml"
    ".tei.xml"  -> "tei"
    ".tei"      -> "tei"
    ".ms"       -> "ms"
    ".roff"     -> "ms"
    ".pptx"     -> "pptx"
    ['.',y]     | y `elem` ['1'..'9'] -> "man"
    _           -> "html"

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

readSource :: FilePath -> PandocIO Text
readSource "-" = liftIO (UTF8.toText <$> BS.getContents)
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" ->
                                 liftIO $ UTF8.toText <$>
                                    BS.readFile (uriPathToPath $ uriPath u)
                      _       -> liftIO $ UTF8.toText <$>
                                    BS.readFile src

readURI :: FilePath -> PandocIO Text
readURI src = UTF8.toText . fst <$> openURL src

readFile' :: MonadIO m => FilePath -> m B.ByteString
readFile' "-" = liftIO B.getContents
readFile' f   = liftIO $ B.readFile f

writeFnBinary :: MonadIO m => FilePath -> B.ByteString -> m ()
writeFnBinary "-" = liftIO . B.putStr
writeFnBinary f   = liftIO . B.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => IO.Newline -> FilePath -> Text -> m ()
-- TODO this implementation isn't maximally efficient:
writerFn eol "-" = liftIO . UTF8.putStrWith eol . T.unpack
writerFn eol f   = liftIO . UTF8.writeFileWith eol f . T.unpack

lookupHighlightStyle :: Maybe String -> IO (Maybe Style)
lookupHighlightStyle Nothing = return Nothing
lookupHighlightStyle (Just s)
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
                  (\arg opt -> return opt { optOutputFile = Just arg })
                  "FILE")
                 "" -- "Name of output file"

    , Option "" ["data-dir"]
                 (ReqArg
                  (\arg opt -> return opt { optDataDir = Just arg })
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
                                    JSONFilter arg : optFilters opt })
                  "PROGRAM")
                 "" -- "External JSON filter"

    , Option "" ["lua-filter"]
                 (ReqArg
                  (\arg opt -> return opt { optFilters =
                                    LuaFilter arg : optFilters opt })
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
                  "FILE")
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
                     templ <- runIO $ do
                                setUserDataDir Nothing
                                getDefaultTemplate arg
                     case templ of
                          Right "" -> do -- e.g. for docx, odt, json:
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
                     sty <- fromMaybe pygments <$>
                              lookupHighlightStyle (Just arg)
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
                 (\arg opt -> return opt{ optHighlightStyle = Just arg })
                 "STYLE|FILE")
                 "" -- "Style for highlighted code"

    , Option "" ["syntax-definition"]
                (ReqArg
                 (\arg opt -> return opt{ optSyntaxDefinitions = arg :
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
  "Copyright (C) 2006-2018 John MacFarlane",
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

baseWriterName :: String -> String
baseWriterName = takeWhile (\c -> c /= '+' && c /= '-')

deprecatedOption :: String -> String -> IO ()
deprecatedOption o msg =
  runIO (report $ Deprecated o msg) >>=
    \r -> case r of
       Right () -> return ()
       Left e   -> E.throwIO e

-- see https://github.com/jgm/pandoc/pull/4083
-- using generic deriving caused long compilation times
$(deriveJSON defaultOptions ''LineEnding)
$(deriveJSON defaultOptions ''Opt)

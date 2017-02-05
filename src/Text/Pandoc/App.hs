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
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App (
            convertWithOpts
          , Opt(..)
          , defaultOpts
          ) where
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Shared ( tabFilter, readDataFileUTF8,
                            headerShift, err, openURL )
import Text.Pandoc.MediaBag ( mediaDirectory, extractMediaBag, MediaBag )
import Text.Pandoc.XML ( toEntities )
import Text.Pandoc.Highlighting (highlightingStyles)
import Text.Pandoc.SelfContained ( makeSelfContained )
import Text.Pandoc.Process (pipeProcess)
import Skylighting ( Style )
import System.Environment ( getEnvironment )
import System.Exit ( ExitCode (..), exitSuccess )
import System.FilePath
import Data.Char ( toLower )
import Data.List ( intercalate, isPrefixOf, isSuffixOf )
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
import Data.Aeson (eitherDecode', encode)
import Data.Yaml (decode)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
#ifndef _WINDOWS
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdOutput)
#endif
import Control.Monad.Trans
import Text.Pandoc.Class (withMediaBag, PandocIO, getLog)

convertWithOpts :: Opt -> [FilePath] -> IO ()
convertWithOpts opts args = do
  let outputFile = optOutputFile opts
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) args
       exitSuccess

  epubStylesheet <- case optEpubStylesheet opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> UTF8.readFile fp

  epubMetadata <- case optEpubMetadata opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> UTF8.readFile fp

  let csscdn = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.6.0/katex.min.css"
  let mathMethod =
        case (optKaTeXJS opts, optKaTeXStylesheet opts) of
            (Nothing, _) -> optHTMLMathMethod opts
            (Just js, ss) -> KaTeX js (fromMaybe csscdn ss)


  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      "pandoc-citeproc" `notElem` map takeBaseName filters
  let filters' = if needsCiteproc then "pandoc-citeproc" : filters
                                  else filters

  let sources = case args of
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
  let readerName = case optReader opts of
                          Nothing -> defaultReaderName
                                      (if any isURI sources
                                          then "html"
                                          else "markdown") sources
                          Just x  -> map toLower x

  let writerName = case optWriter opts of
                          Nothing   -> defaultWriterName outputFile
                          Just x    -> map toLower x
  let format = takeWhile (`notElem` ['+','-'])
                       $ takeFileName writerName  -- in case path to lua script

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

  let laTeXOutput = format `elem` ["latex", "beamer"]
  let conTeXtOutput = format == "context"
  let html5Output = format == "html5" || format == "html"

  -- disabling the custom writer for now
  writer <- if ".lua" `isSuffixOf` format
               -- note:  use non-lowercased version writerName
               then error "custom writers disabled for now"
               else case getWriter writerName of
                         Left e  -> err 9 $
                           if format == "pdf"
                              then e ++
                               "\nTo create a pdf with pandoc, use " ++
                               "the latex or beamer writer and specify\n" ++
                               "an output file with .pdf extension " ++
                               "(pandoc -t latex -o filename.pdf)."
                              else e
                         Right w -> return (w :: Writer PandocIO)

  -- TODO: we have to get the input and the output into the state for
  -- the sake of the text2tags reader.
  reader <-  case getReader readerName of
                Right r  -> return (r :: Reader PandocIO)
                Left e   -> err 7 e'
                  where e' = case readerName of
                                  "pdf" -> e ++
                                     "\nPandoc can convert to PDF, but not from PDF."
                                  "doc" -> e ++
                                     "\nPandoc can convert from DOCX, but not from DOC.\nTry using Word to save your DOC file as DOCX, and convert that with pandoc."
                                  _ -> e

  let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput

  templ <- case optTemplate opts of
                _ | not standalone -> return Nothing
                Nothing -> do
                           deftemp <- getDefaultTemplate datadir format
                           case deftemp of
                                 Left e   -> throwIO e
                                 Right t  -> return (Just t)
                Just tp -> do
                           -- strip off extensions
                           let tp' = case takeExtension tp of
                                          ""   -> tp <.> format
                                          _    -> tp
                           Just <$> E.catch (UTF8.readFile tp')
                             (\e -> if isDoesNotExistError e
                                       then E.catch
                                             (readDataFileUTF8 datadir
                                                ("templates" </> tp'))
                                             (\e' -> let _ = (e' :: E.SomeException)
                                                     in throwIO e')
                                       else throwIO e)

  let addStringAsVariable varname s vars = return $ (varname, s) : vars

  let addContentsAsVariable varname fp vars = do
             s <- UTF8.readFile fp
             return $ (varname, s) : vars

  -- note: this reverses the list constructed in option parsing,
  -- which in turn was reversed from the command-line order,
  -- so we end up with the correct order in the variable list:
  let withList _ [] vars = return vars
      withList f (x:xs) vars = f x vars >>= withList f xs

  variables <- return (optVariables opts)
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
      maybe return (addStringAsVariable "title-prefix") (optTitlePrefix opts)
      >>=
      maybe return (addStringAsVariable "epub-cover-image")
                   (optEpubCoverImage opts)
      >>=
      (\vars -> case mathMethod of
                     LaTeXMathML Nothing -> do
                        s <- readDataFileUTF8 datadir "LaTeXMathML.js"
                        return $ ("mathml-script", s) : vars
                     _ -> return vars)
      >>=
      (\vars ->  if format == "dzslides"
                    then do
                        dztempl <- readDataFileUTF8 datadir
                                     ("dzslides" </> "template.html")
                        let dzline = "<!-- {{{{ dzslides core"
                        let dzcore = unlines
                                   $ dropWhile (not . (dzline `isPrefixOf`))
                                   $ lines dztempl
                        return $ ("dzslides-core", dzcore) : vars
                    else return vars)

  let sourceURL = case sources of
                    []    -> Nothing
                    (x:_) -> case parseURI x of
                                Just u
                                  | uriScheme u `elem` ["http:","https:"] ->
                                      Just $ show u{ uriQuery = "",
                                                     uriFragment = "" }
                                _ -> Nothing

  let readerOpts = def{ readerStandalone = standalone
                      , readerParseRaw = optParseRaw opts
                      , readerColumns = optColumns opts
                      , readerTabStop = optTabStop opts
                      , readerIndentedCodeClasses = optIndentedCodeClasses opts
                      , readerApplyMacros = not laTeXOutput
                      , readerDefaultImageExtension =
                         optDefaultImageExtension opts
                      , readerTrackChanges = optTrackChanges opts
                      }

  highlightStyle <- lookupHighlightStyle $ optHighlightStyle opts

  let writerOptions = def { writerTemplate         = templ,
                            writerVariables        = variables,
                            writerTabStop          = optTabStop opts,
                            writerTableOfContents  = optTableOfContents opts,
                            writerHTMLMathMethod   = mathMethod,
                            writerIncremental      = optIncremental opts,
                            writerCiteMethod       = optCiteMethod opts,
                            writerNumberSections   = optNumberSections opts,
                            writerNumberOffset     = optNumberOffset opts,
                            writerSectionDivs      = optSectionDivs opts,
                            writerReferenceLinks   = optReferenceLinks opts,
                            writerReferenceLocation = optReferenceLocation opts,
                            writerDpi              = optDpi opts,
                            writerWrapText         = optWrapText opts,
                            writerColumns          = optColumns opts,
                            writerEmailObfuscation = optEmailObfuscation opts,
                            writerIdentifierPrefix = optIdentifierPrefix opts,
                            writerSourceURL        = sourceURL,
                            writerUserDataDir      = datadir,
                            writerHtmlQTags        = optHtmlQTags opts,
                            writerTopLevelDivision = optTopLevelDivision opts,
                            writerListings         = optListings opts,
                            writerSlideLevel       = optSlideLevel opts,
                            writerHighlightStyle   = highlightStyle,
                            writerSetextHeaders    = optSetextHeaders opts,
                            writerEpubMetadata     = epubMetadata,
                            writerEpubStylesheet   = epubStylesheet,
                            writerEpubFonts        = optEpubFonts opts,
                            writerEpubChapterLevel = optEpubChapterLevel opts,
                            writerTOCDepth         = optTOCDepth opts,
                            writerReferenceDoc     = optReferenceDoc opts,
                            writerLaTeXArgs        = optLaTeXEngineArgs opts
                          }


#ifdef _WINDOWS
  let istty = True
#else
  istty <- queryTerminal stdOutput
#endif
  when (istty && not (isTextFormat format) && outputFile == "-") $
    err 5 $ "Cannot write " ++ format ++ " output to stdout.\n" ++
            "Specify an output file using the -o option."


  let transforms = case optBaseHeaderLevel opts of
                        x | x > 1 -> [headerShift (x - 1)]
                          | otherwise -> []

  let convertTabs = tabFilter (if optPreserveTabs opts || readerName == "t2t"
                                 then 0
                                 else optTabStop opts)

      readSources :: MonadIO m => [FilePath] -> m String
      readSources srcs = convertTabs . intercalate "\n" <$>
                            mapM readSource srcs

  let runIO' :: PandocIO a -> IO a
      runIO' f = do
        (res, reports) <- runIOorExplode $ do
                             setVerbosity verbosity
                             x <- f
                             rs <- getLog
                             return (x, rs)
        let isWarning (WARNING, _) = True
            isWarning _            = False
        when (optFailIfWarnings opts && any isWarning reports) $
            err 3 "Failing because there were warnings."
        return res

  let sourceToDoc :: [FilePath] -> PandocIO (Pandoc, MediaBag)
      sourceToDoc sources' =
         case reader of
              StringReader r
                | optFileScope opts || readerName == "json" -> do
                    pairs <- mapM
                      (readSource >=> withMediaBag . r readerOpts) sources
                    return (mconcat (map fst pairs), mconcat (map snd pairs))
                | otherwise ->
                     readSources sources' >>= withMediaBag . r readerOpts
              ByteStringReader r -> do
                pairs <- mapM (readFile' >=>
                                 withMediaBag . r readerOpts) sources
                return (mconcat (map fst pairs), mconcat (map snd pairs))

  runIO' $ do
    (doc, media) <- sourceToDoc sources
    doc' <- (maybe return (extractMedia media) (optExtractMedia opts) >=>
              return . flip (foldr addMetadata) (optMetadata opts) >=>
              applyTransforms transforms >=>
              applyFilters datadir filters' [format]) doc

    case writer of
      -- StringWriter f -> f writerOptions doc' >>= writerFn outputFile
      ByteStringWriter f -> f writerOptions doc' >>= writeFnBinary outputFile
      StringWriter f
        | pdfOutput -> do
                -- make sure writer is latex or beamer or context or html5
                unless (laTeXOutput || conTeXtOutput || html5Output) $
                  err 47 $ "cannot produce pdf output with " ++ format ++
                           " writer"

                let pdfprog = case () of
                                _ | conTeXtOutput -> "context"
                                _ | html5Output   -> "wkhtmltopdf"
                                _                 -> optLaTeXEngine opts
                -- check for pdf creating program
                mbPdfProg <- liftIO $ findExecutable pdfprog
                when (isNothing mbPdfProg) $
                     err 41 $ pdfprog ++ " not found. " ++
                       pdfprog ++ " is needed for pdf output."

                res <- makePDF pdfprog f writerOptions verbosity media doc'
                case res of
                     Right pdf -> writeFnBinary outputFile pdf
                     Left err' -> liftIO $ do
                       B.hPutStr stderr err'
                       B.hPut stderr $ B.pack [10]
                       err 43 "Error producing PDF"
        | otherwise -> do
                let htmlFormat = format `elem`
                      ["html","html4","html5","s5","slidy","slideous","dzslides","revealjs"]
                    selfcontain = if optSelfContained opts && htmlFormat
                                  then makeSelfContained writerOptions media
                                  else return
                    handleEntities = if htmlFormat && optAscii opts
                                     then toEntities
                                     else id
                output <- f writerOptions doc'
                selfcontain (output ++ ['\n' | not standalone]) >>=
                    writerFn outputFile . handleEntities

type Transform = Pandoc -> Pandoc

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub","epub3"]

externalFilter :: MonadIO m => FilePath -> [String] -> Pandoc -> m Pandoc
externalFilter f args' d = liftIO $ do
  exists <- doesFileExist f
  isExecutable <- if exists
                     then executable <$> getPermissions f
                     else return True
  let (f', args'') = if exists
                        then case map toLower (takeExtension f) of
                                  _ | isExecutable -> ("." </> f, args')
                                  ".py"  -> ("python", f:args')
                                  ".hs"  -> ("runhaskell", f:args')
                                  ".pl"  -> ("perl", f:args')
                                  ".rb"  -> ("ruby", f:args')
                                  ".php" -> ("php", f:args')
                                  ".js"  -> ("node", f:args')
                                  _      -> (f, args')
                        else (f, args')
  unless (exists && isExecutable) $ do
    mbExe <- findExecutable f'
    when (isNothing mbExe) $
      err 83 $ "Error running filter " ++ f ++  ":\n" ++
               "Could not find executable '" ++ f' ++ "'."
  env <- getEnvironment
  let env' = Just $ ("PANDOC_VERSION", pandocVersion) : env
  (exitcode, outbs) <- E.handle filterException $
                              pipeProcess env' f' args'' $ encode d
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
    , optReader            :: Maybe String  -- ^ Reader format
    , optWriter            :: Maybe String  -- ^ Writer format
    , optParseRaw          :: Bool    -- ^ Parse unconvertable HTML and TeX
    , optTableOfContents   :: Bool    -- ^ Include table of contents
    , optBaseHeaderLevel   :: Int     -- ^ Base header level
    , optTemplate          :: Maybe FilePath  -- ^ Custom template
    , optVariables         :: [(String,String)] -- ^ Template variables to set
    , optMetadata          :: [(String, String)] -- ^ Metadata fields to set
    , optOutputFile        :: FilePath  -- ^ Name of output file
    , optNumberSections    :: Bool    -- ^ Number sections in LaTeX
    , optNumberOffset      :: [Int]   -- ^ Starting number for sections
    , optSectionDivs       :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental       :: Bool    -- ^ Use incremental lists in Slidy/Slideous/S5
    , optSelfContained     :: Bool    -- ^ Make HTML accessible offline
    , optHtmlQTags         :: Bool    -- ^ Use <q> tags in HTML
    , optHighlightStyle    :: Maybe String -- ^ Style to use for highlighted code
    , optTopLevelDivision  :: TopLevelDivision -- ^ Type of the top-level divisions
    , optHTMLMathMethod    :: HTMLMathMethod -- ^ Method to print HTML math
    , optReferenceDoc      :: Maybe FilePath -- ^ Path of reference doc
    , optEpubStylesheet    :: Maybe FilePath   -- ^ EPUB stylesheet
    , optEpubMetadata      :: Maybe FilePath   -- ^ EPUB metadata
    , optEpubFonts         :: [FilePath] -- ^ EPUB fonts to embed
    , optEpubChapterLevel  :: Int     -- ^ Header level at which to split chapters
    , optEpubCoverImage    :: Maybe FilePath -- ^ Cover image for epub
    , optTOCDepth          :: Int     -- ^ Number of levels to include in TOC
    , optDumpArgs          :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs        :: Bool    -- ^ Ignore command-line arguments
    , optVerbosity         :: Verbosity  -- ^ Verbosity of diagnostic output
    , optFailIfWarnings    :: Bool    -- ^ Fail on warnings
    , optReferenceLinks    :: Bool    -- ^ Use reference links in writing markdown, rst
    , optReferenceLocation :: ReferenceLocation -- ^ location for footnotes and link references in markdown output
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
    , optDefaultImageExtension :: String -- ^ Default image extension
    , optExtractMedia      :: Maybe FilePath -- ^ Path to extract embedded media
    , optTrackChanges      :: TrackChanges -- ^ Accept or reject MS Word track-changes.
    , optFileScope        :: Bool         -- ^ Parse input files before combining
    , optKaTeXStylesheet   :: Maybe String     -- ^ Path to stylesheet for KaTeX
    , optKaTeXJS           :: Maybe String     -- ^ Path to js file for KaTeX
    , optTitlePrefix       :: Maybe String     -- ^ Prefix for title
    , optCss               :: [FilePath]       -- ^ CSS files to link to
    , optIncludeBeforeBody :: [FilePath]       -- ^ Files to include before
    , optIncludeAfterBody  :: [FilePath]       -- ^ Files to include after body
    , optIncludeInHeader   :: [FilePath]       -- ^ Files to include in header
    }

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop               = 4
    , optPreserveTabs          = False
    , optStandalone            = False
    , optReader                = Nothing
    , optWriter                = Nothing
    , optParseRaw              = False
    , optTableOfContents       = False
    , optBaseHeaderLevel       = 1
    , optTemplate              = Nothing
    , optVariables             = []
    , optMetadata              = []
    , optOutputFile            = "-"    -- "-" means stdout
    , optNumberSections        = False
    , optNumberOffset          = [0,0,0,0,0,0]
    , optSectionDivs           = False
    , optIncremental           = False
    , optSelfContained         = False
    , optHtmlQTags             = False
    , optHighlightStyle        = Just "pygments"
    , optTopLevelDivision      = TopLevelDefault
    , optHTMLMathMethod        = PlainMath
    , optReferenceDoc          = Nothing
    , optEpubStylesheet        = Nothing
    , optEpubMetadata          = Nothing
    , optEpubFonts             = []
    , optEpubChapterLevel      = 1
    , optEpubCoverImage        = Nothing
    , optTOCDepth              = 3
    , optDumpArgs              = False
    , optIgnoreArgs            = False
    , optVerbosity             = WARNING
    , optFailIfWarnings        = False
    , optReferenceLinks        = False
    , optReferenceLocation     = EndOfDocument
    , optDpi                   = 96
    , optWrapText              = WrapAuto
    , optColumns               = 72
    , optFilters               = []
    , optEmailObfuscation      = NoObfuscation
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
    , optDefaultImageExtension = ""
    , optExtractMedia          = Nothing
    , optTrackChanges          = AcceptChanges
    , optFileScope             = False
    , optKaTeXStylesheet       = Nothing
    , optKaTeXJS               = Nothing
    , optTitlePrefix           = Nothing
    , optCss                   = []
    , optIncludeBeforeBody     = []
    , optIncludeAfterBody      = []
    , optIncludeInHeader       = []
    }

addMetadata :: (String, String) -> Pandoc -> Pandoc
addMetadata (k, v) (Pandoc meta bs) = Pandoc meta' bs
  where meta' = case lookupMeta k meta of
                      Nothing -> setMeta k v' meta
                      Just (MetaList xs) ->
                                 setMeta k (MetaList (xs ++ [v'])) meta
                      Just x  -> setMeta k (MetaList [x, v']) meta
        v' = readMetaValue v

readMetaValue :: String -> MetaValue
readMetaValue s = case decode (UTF8.fromString s) of
                       Just (Yaml.String t) -> MetaString $ T.unpack t
                       Just (Yaml.Bool b)   -> MetaBool b
                       _                    -> MetaString s

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

extractMedia :: MonadIO m => MediaBag -> FilePath -> Pandoc -> m Pandoc
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

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

  -- First we check to see if a filter is found.  If not, and if it's
  -- not an absolute path, we check to see whether it's in `userdir/filters`.
  -- If not, we leave it unchanged.
expandFilterPath :: MonadIO m => Maybe FilePath -> FilePath -> m FilePath
expandFilterPath mbDatadir fp = liftIO $ do
  fpExists <- doesFileExist fp
  if fpExists
     then return fp
     else case mbDatadir of
               Just datadir | isRelative fp -> do
                 let filterPath = (datadir </> "filters" </> fp)
                 filterPathExists <- doesFileExist filterPath
                 if filterPathExists
                    then return filterPath
                    else return fp
               _ -> return fp

applyFilters :: MonadIO m
             => Maybe FilePath -> [FilePath] -> [String] -> Pandoc -> m Pandoc
applyFilters mbDatadir filters args d = do
  expandedFilters <- mapM (expandFilterPath mbDatadir) filters
  foldrM ($) d $ map (flip externalFilter args) expandedFilters

readSource :: MonadIO m => FilePath -> m String
readSource "-" = liftIO UTF8.getContents
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" ->
                                 liftIO $ UTF8.readFile (uriPath u)
                      _       -> liftIO $ UTF8.readFile src

readURI :: MonadIO m => FilePath -> m String
readURI src = do
  res <- liftIO $ openURL src
  case res of
       Left e        -> liftIO $ throwIO e
       Right (bs,_)  -> return $ UTF8.toString bs

readFile' :: MonadIO m => FilePath -> m B.ByteString
readFile' "-" = liftIO $ B.getContents
readFile' f   = liftIO $ B.readFile f

writeFnBinary :: MonadIO m => FilePath -> B.ByteString -> m ()
writeFnBinary "-" = liftIO . B.putStr
writeFnBinary f   = liftIO . B.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => FilePath -> String -> m ()
writerFn "-" = liftIO . UTF8.putStr
writerFn f   = liftIO . UTF8.writeFile f

lookupHighlightStyle :: Maybe String -> IO (Maybe Style)
lookupHighlightStyle Nothing = return Nothing
lookupHighlightStyle (Just s) =
  case lookup (map toLower s) highlightingStyles of
       Just sty -> return (Just sty)
       Nothing  -> err 68 $ "Unknown highlight-style " ++ s

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{- |
   Module      : Text.Pandoc.App.Opt
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Options for pandoc when used as an app.
-}
module Text.Pandoc.App.Opt (
            Opt(..)
          , LineEnding (..)
          , IpynbOutput (..)
          , DefaultsState (..)
          , defaultOpts
          , applyDefaults
          , fullDefaultsPath
          ) where
import Control.Monad.Except (MonadIO, liftIO, throwError, (>=>), foldM)
import Control.Monad.State.Strict (StateT, modify, gets)
import System.FilePath ( addExtension, (</>), takeExtension, takeDirectory )
import System.Directory ( canonicalizePath )
import Data.Char (isLower, toLower)
import Data.Maybe (fromMaybe)
import GHC.Generics hiding (Meta)
import Text.Pandoc.Filter (Filter (..))
import Text.Pandoc.Logging (Verbosity (WARNING), LogMessage(..))
import Text.Pandoc.Options (TopLevelDivision (TopLevelDefault),
                            TrackChanges (AcceptChanges),
                            WrapOption (WrapAuto), HTMLMathMethod (PlainMath),
                            ReferenceLocation (EndOfDocument),
                            ObfuscationMethod (NoObfuscation),
                            CiteMethod (Citeproc))
import Text.Pandoc.Class (readFileLazy, fileExists, setVerbosity, report,
                          PandocMonad(lookupEnv), getUserDataDir)
import Text.Pandoc.Error (PandocError (PandocParseError, PandocSomeError))
import Text.Pandoc.Shared (camelCaseStrToHyphenated, defaultUserDataDir,
                           findM, ordNub)
import qualified Text.Pandoc.Parsing as P
import Text.Pandoc.Readers.Metadata (yamlMap)
import Text.Pandoc.Class.PandocPure
import Text.DocTemplates (Context(..))
import Data.Text (Text, unpack)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.Pandoc.Definition (Meta(..), MetaValue(..))
import Data.Aeson (defaultOptions, Options(..))
import Data.Aeson.TH (deriveJSON)
import Control.Applicative ((<|>))
import Data.YAML

-- | The type of line-endings to be used when writing plain-text.
data LineEnding = LF | CRLF | Native deriving (Show, Generic)

instance FromYAML LineEnding where
  parseYAML = withStr "LineEnding" $ \t ->
    case T.toLower t of
      "lf"     -> return LF
      "crlf"   -> return CRLF
      "native" -> return Native
      _        -> fail $ "Unknown line ending type " ++ show t

-- | How to handle output blocks in ipynb.
data IpynbOutput =
    IpynbOutputAll
  | IpynbOutputNone
  | IpynbOutputBest
  deriving (Show, Generic)

instance FromYAML IpynbOutput where
  parseYAML = withStr "LineEnding" $ \t ->
    case t of
      "none"  -> return IpynbOutputNone
      "all"   -> return IpynbOutputAll
      "best"  -> return IpynbOutputBest
      _       -> fail $ "Unknown ipynb output type " ++ show t

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop               :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs          :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone            :: Bool    -- ^ Include header, footer
    , optFrom                  :: Maybe Text  -- ^ Reader format
    , optTo                    :: Maybe Text  -- ^ Writer format
    , optTableOfContents       :: Bool    -- ^ Include table of contents
    , optShiftHeadingLevelBy   :: Int     -- ^ Shift heading level by
    , optTemplate              :: Maybe FilePath  -- ^ Custom template
    , optVariables             :: Context Text    -- ^ Template variables to set
    , optMetadata              :: Meta -- ^ Metadata fields to set
    , optMetadataFiles         :: [FilePath]  -- ^ Name of YAML metadata files
    , optOutputFile            :: Maybe FilePath  -- ^ Name of output file
    , optInputFiles            :: Maybe [FilePath] -- ^ Names of input files
    , optNumberSections        :: Bool    -- ^ Number sections in LaTeX
    , optNumberOffset          :: [Int]   -- ^ Starting number for sections
    , optSectionDivs           :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental           :: Bool    -- ^ Use incremental lists in Slidy/Slideous/S5
    , optSelfContained         :: Bool    -- ^ Make HTML accessible offline
    , optHtmlQTags             :: Bool    -- ^ Use <q> tags in HTML
    , optHighlightStyle        :: Maybe Text -- ^ Style to use for highlighted code
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
    , optWrap                  :: WrapOption  -- ^ Options for wrapping text
    , optColumns               :: Int     -- ^ Line length in characters
    , optFilters               :: [Filter] -- ^ Filters to apply
    , optEmailObfuscation      :: ObfuscationMethod
    , optIdentifierPrefix      :: Text
    , optStripEmptyParagraphs  :: Bool -- ^ Strip empty paragraphs
    , optIndentedCodeClasses   :: [Text] -- ^ Default classes for indented code blocks
    , optDataDir               :: Maybe FilePath
    , optCiteMethod            :: CiteMethod -- ^ Method to output cites
    , optListings              :: Bool       -- ^ Use listings package for code blocks
    , optPdfEngine             :: Maybe String -- ^ Program to use for latex/html -> pdf
    , optPdfEngineOpts         :: [String]   -- ^ Flags to pass to the engine
    , optSlideLevel            :: Maybe Int  -- ^ Header level that creates slides
    , optSetextHeaders         :: Bool       -- ^ Use atx headers for markdown level 1-2
    , optAscii                 :: Bool       -- ^ Prefer ascii output
    , optDefaultImageExtension :: Text       -- ^ Default image extension
    , optExtractMedia          :: Maybe FilePath -- ^ Path to extract embedded media
    , optTrackChanges          :: TrackChanges -- ^ Accept or reject MS Word track-changes.
    , optFileScope             :: Bool         -- ^ Parse input files before combining
    , optTitlePrefix           :: Maybe Text     -- ^ Prefix for title
    , optCss                   :: [FilePath]       -- ^ CSS files to link to
    , optIpynbOutput           :: IpynbOutput      -- ^ How to treat ipynb output blocks
    , optIncludeBeforeBody     :: [FilePath]       -- ^ Files to include before
    , optIncludeAfterBody      :: [FilePath]       -- ^ Files to include after body
    , optIncludeInHeader       :: [FilePath]       -- ^ Files to include in header
    , optResourcePath          :: [FilePath] -- ^ Path to search for images etc
    , optRequestHeaders        :: [(Text, Text)] -- ^ Headers for HTTP requests
    , optNoCheckCertificate    :: Bool       -- ^ Disable certificate validation
    , optEol                   :: LineEnding -- ^ Style of line-endings to use
    , optStripComments         :: Bool       -- ^ Skip HTML comments
    , optCSL                   :: Maybe FilePath -- ^ CSL stylesheet
    , optBibliography          :: [FilePath]  -- ^ Bibliography files
    , optCitationAbbreviations :: Maybe FilePath -- ^ Citation abbreviations
    } deriving (Generic, Show)

instance FromYAML (Opt -> Opt) where
  parseYAML (Mapping _ _ m) = chain doOpt (M.toList m)
  parseYAML n = failAtNode n "Expected a mapping"

data DefaultsState = DefaultsState
    {
      curDefaults      :: Maybe FilePath -- currently parsed file
    , inheritanceGraph :: [[FilePath]]   -- defaults file inheritance graph
    } deriving (Show)

instance (PandocMonad m, MonadIO m)
      => FromYAML (Opt -> StateT DefaultsState m Opt) where
  parseYAML (Mapping _ _ m) = do
    let opts = M.mapKeys toText m
    dataDir <- case M.lookup "data-dir" opts of
      Nothing -> return Nothing
      Just v -> Just . unpack <$> parseYAML v
    f <- parseOptions (M.toList m)
    case M.lookup "defaults" opts of
      Just v -> do
        g <- parseDefaults v dataDir
        return  $ g >=> f >=> resolveVarsInOpt
      Nothing -> return $ f >=> resolveVarsInOpt
    where
      toText (Scalar _ (SStr s)) = s
      toText _ = ""
  parseYAML n = failAtNode n "Expected a mapping"

resolveVarsInOpt :: forall m. (PandocMonad m, MonadIO m)
                 => Opt -> StateT DefaultsState m Opt
resolveVarsInOpt
    opt@Opt
    { optTemplate              = oTemplate
    , optMetadataFiles         = oMetadataFiles
    , optOutputFile            = oOutputFile
    , optInputFiles            = oInputFiles
    , optSyntaxDefinitions     = oSyntaxDefinitions
    , optAbbreviations         = oAbbreviations
    , optReferenceDoc          = oReferenceDoc
    , optEpubMetadata          = oEpubMetadata
    , optEpubFonts             = oEpubFonts
    , optEpubCoverImage        = oEpubCoverImage
    , optLogFile               = oLogFile
    , optFilters               = oFilters
    , optDataDir               = oDataDir
    , optExtractMedia          = oExtractMedia
    , optCss                   = oCss
    , optIncludeBeforeBody     = oIncludeBeforeBody
    , optIncludeAfterBody      = oIncludeAfterBody
    , optIncludeInHeader       = oIncludeInHeader
    , optResourcePath          = oResourcePath
    , optCSL                   = oCSL
    , optBibliography          = oBibliography
    , optCitationAbbreviations = oCitationAbbreviations
    }
  = do
      oTemplate' <- mapM resolveVars oTemplate
      oMetadataFiles' <- mapM resolveVars oMetadataFiles
      oOutputFile' <- mapM resolveVars oOutputFile
      oInputFiles' <- mapM (mapM resolveVars) oInputFiles
      oSyntaxDefinitions' <- mapM resolveVars oSyntaxDefinitions
      oAbbreviations' <- mapM resolveVars oAbbreviations
      oReferenceDoc' <- mapM resolveVars oReferenceDoc
      oEpubMetadata' <- mapM resolveVars oEpubMetadata
      oEpubFonts' <- mapM resolveVars oEpubFonts
      oEpubCoverImage' <- mapM resolveVars oEpubCoverImage
      oLogFile' <- mapM resolveVars oLogFile
      oFilters' <- mapM resolveVarsInFilter oFilters
      oDataDir' <- mapM resolveVars oDataDir
      oExtractMedia' <- mapM resolveVars oExtractMedia
      oCss' <- mapM resolveVars oCss
      oIncludeBeforeBody' <- mapM resolveVars oIncludeBeforeBody
      oIncludeAfterBody' <- mapM resolveVars oIncludeAfterBody
      oIncludeInHeader' <- mapM resolveVars oIncludeInHeader
      oResourcePath' <- mapM resolveVars oResourcePath
      oCSL' <- mapM resolveVars oCSL
      oBibliography' <- mapM resolveVars oBibliography
      oCitationAbbreviations' <- mapM resolveVars oCitationAbbreviations
      return opt{ optTemplate              = oTemplate'
                , optMetadataFiles         = oMetadataFiles'
                , optOutputFile            = oOutputFile'
                , optInputFiles            = oInputFiles'
                , optSyntaxDefinitions     = oSyntaxDefinitions'
                , optAbbreviations         = oAbbreviations'
                , optReferenceDoc          = oReferenceDoc'
                , optEpubMetadata          = oEpubMetadata'
                , optEpubFonts             = oEpubFonts'
                , optEpubCoverImage        = oEpubCoverImage'
                , optLogFile               = oLogFile'
                , optFilters               = oFilters'
                , optDataDir               = oDataDir'
                , optExtractMedia          = oExtractMedia'
                , optCss                   = oCss'
                , optIncludeBeforeBody     = oIncludeBeforeBody'
                , optIncludeAfterBody      = oIncludeAfterBody'
                , optIncludeInHeader       = oIncludeInHeader'
                , optResourcePath          = oResourcePath'
                , optCSL                   = oCSL'
                , optBibliography          = oBibliography'
                , optCitationAbbreviations = oCitationAbbreviations'
                }

 where
  resolveVars :: FilePath -> StateT DefaultsState m FilePath
  resolveVars [] = return []
  resolveVars ('$':'{':xs) =
    let (ys, zs) = break (=='}') xs
     in if null zs
           then return $ '$':'{':xs
           else do
             val <- lookupEnv' ys
             (val ++) <$> resolveVars (drop 1 zs)
  resolveVars (c:cs) = (c:) <$> resolveVars cs
  lookupEnv' :: String -> StateT DefaultsState m String
  lookupEnv' "." = do
    mbCurDefaults <- gets curDefaults
    maybe (return "")
          (fmap takeDirectory . liftIO . canonicalizePath)
          mbCurDefaults
  lookupEnv' "USERDATA" = do
    mbodatadir <- mapM resolveVars oDataDir
    mbdatadir  <- getUserDataDir
    defdatadir <- liftIO defaultUserDataDir
    return $ fromMaybe defdatadir (mbodatadir <|> mbdatadir)
  lookupEnv' v = do
    mbval <- fmap T.unpack <$> lookupEnv (T.pack v)
    case mbval of
      Nothing -> do
        report $ EnvironmentVariableUndefined (T.pack v)
        return mempty
      Just x  -> return x
  resolveVarsInFilter (JSONFilter fp) =
    JSONFilter <$> resolveVars fp
  resolveVarsInFilter (LuaFilter fp) =
    LuaFilter <$> resolveVars fp
  resolveVarsInFilter CiteprocFilter = return CiteprocFilter



parseDefaults :: (PandocMonad m, MonadIO m)
              => Node Pos
              -> Maybe FilePath
              -> Parser (Opt -> StateT DefaultsState m Opt)
parseDefaults n dataDir = parseDefsNames n >>= \ds -> return $ \o -> do
  -- get parent defaults:
  defsParent <- gets $ fromMaybe "" . curDefaults
  -- get child defaults:
  defsChildren <- mapM (fullDefaultsPath dataDir) ds
  -- expand parent in defaults inheritance graph by children:
  defsGraph <- gets inheritanceGraph
  let defsGraphExp = expand defsGraph defsChildren defsParent
  modify $ \defsState -> defsState{ inheritanceGraph = defsGraphExp }
  -- check for cyclic inheritance:
  if cyclic defsGraphExp
    then throwError $
      PandocSomeError $ T.pack $
        "Error: Circular defaults file reference in " ++
        "'" ++ defsParent ++ "'"
    else foldM applyDefaults o defsChildren
  where parseDefsNames x = (parseYAML x >>= \xs -> return $ map unpack xs)
                       <|> (parseYAML x >>= \x' -> return [unpack x'])

parseOptions :: Monad m
             => [(Node Pos, Node Pos)]
             -> Parser (Opt -> StateT DefaultsState m Opt)
parseOptions ns = do
  f <- chain doOpt' ns
  return $ return . f

chain :: Monad m => (a -> m (b -> b)) -> [a] -> m (b -> b)
chain f = foldM g id
  where g o n = f n >>= \o' -> return $ o' . o

doOpt' :: (Node Pos, Node Pos) -> Parser (Opt -> Opt)
doOpt' (k',v) = do
  k <- parseStringKey k'
  case k of
    "defaults" -> return id
    _ -> doOpt (k',v)

doOpt :: (Node Pos, Node Pos) -> Parser (Opt -> Opt)
doOpt (k',v) = do
  k <- parseStringKey k'
  case k of
    "tab-stop" ->
      parseYAML v >>= \x -> return (\o -> o{ optTabStop = x })
    "preserve-tabs" ->
      parseYAML v >>= \x -> return (\o -> o{ optPreserveTabs = x })
    "standalone" ->
      parseYAML v >>= \x -> return (\o -> o{ optStandalone = x })
    "table-of-contents" ->
      parseYAML v >>= \x -> return (\o -> o{ optTableOfContents = x })
    "toc" ->
      parseYAML v >>= \x -> return (\o -> o{ optTableOfContents = x })
    "from" ->
      parseYAML v >>= \x -> return (\o -> o{ optFrom = x })
    "reader" ->
      parseYAML v >>= \x -> return (\o -> o{ optFrom = x })
    "to" ->
      parseYAML v >>= \x -> return (\o -> o{ optTo = x })
    "writer" ->
      parseYAML v >>= \x -> return (\o -> o{ optTo = x })
    "shift-heading-level-by" ->
      parseYAML v >>= \x -> return (\o -> o{ optShiftHeadingLevelBy = x })
    "template" ->
      parseYAML v >>= \x -> return (\o -> o{ optTemplate = unpack <$> x })
    "variables" ->
      parseYAML v >>= \x -> return (\o -> o{ optVariables =
                                               x <> optVariables o })
      -- Note: x comes first because <> for Context is left-biased union
      -- and we want to favor later default files. See #5988.
    "metadata" ->
      yamlToMeta v >>= \x -> return (\o -> o{ optMetadata = optMetadata o <> x })
    "metadata-files" ->
      parseYAML v >>= \x ->
                        return (\o -> o{ optMetadataFiles =
                                           optMetadataFiles o <>
                                           map unpack x })
    "metadata-file" -> -- allow either a list or a single value
      (parseYAML v >>= \x -> return (\o -> o{ optMetadataFiles =
                                                optMetadataFiles o <>
                                                map unpack x }))
      <|>
      (parseYAML v >>= \x ->
                        return (\o -> o{ optMetadataFiles =
                                           optMetadataFiles o <>[unpack x] }))
    "output-file" ->
      parseYAML v >>= \x -> return (\o -> o{ optOutputFile = unpack <$> x })
    "input-files" ->
      parseYAML v >>= \x -> return (\o -> o{ optInputFiles =
                                              optInputFiles o <>
                                                (map unpack <$> x) })
    "input-file" -> -- allow either a list or a single value
      (parseYAML v >>= \x -> return (\o -> o{ optInputFiles =
                                                optInputFiles o <>
                                                  (map unpack <$> x) }))
      <|>
      (parseYAML v >>= \x -> return (\o -> o{ optInputFiles =
                                                optInputFiles o <>
                                                ((\z -> [unpack z]) <$> x)
                                            }))
    "number-sections" ->
      parseYAML v >>= \x -> return (\o -> o{ optNumberSections = x })
    "number-offset" ->
      parseYAML v >>= \x -> return (\o -> o{ optNumberOffset = x })
    "section-divs" ->
      parseYAML v >>= \x -> return (\o -> o{ optSectionDivs = x })
    "incremental" ->
      parseYAML v >>= \x -> return (\o -> o{ optIncremental = x })
    "self-contained" ->
      parseYAML v >>= \x -> return (\o -> o{ optSelfContained = x })
    "html-q-tags" ->
      parseYAML v >>= \x -> return (\o -> o{ optHtmlQTags = x })
    "highlight-style" ->
      parseYAML v >>= \x -> return (\o -> o{ optHighlightStyle = x })
    "syntax-definition" ->
      (parseYAML v >>= \x ->
                return (\o -> o{ optSyntaxDefinitions =
                                   optSyntaxDefinitions o <> map unpack x }))
      <|>
      (parseYAML v >>= \x ->
             return (\o -> o{ optSyntaxDefinitions =
                                 optSyntaxDefinitions o <> [unpack x] }))
    "syntax-definitions" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optSyntaxDefinitions =
                                optSyntaxDefinitions o <> map unpack x })
    "top-level-division" ->
      parseYAML v >>= \x -> return (\o -> o{ optTopLevelDivision = x })
    "html-math-method" ->
      parseYAML v >>= \x -> return (\o -> o{ optHTMLMathMethod = x })
    "abbreviations" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optAbbreviations = unpack <$> x })
    "reference-doc" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optReferenceDoc = unpack <$> x })
    "epub-subdirectory" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optEpubSubdirectory = unpack x })
    "epub-metadata" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optEpubMetadata = unpack <$> x })
    "epub-fonts" ->
      parseYAML v >>= \x -> return (\o -> o{ optEpubFonts = optEpubFonts o <>
                                               map unpack x })
    "epub-chapter-level" ->
      parseYAML v >>= \x -> return (\o -> o{ optEpubChapterLevel = x })
    "epub-cover-image" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optEpubCoverImage = unpack <$> x })
    "toc-depth" ->
      parseYAML v >>= \x -> return (\o -> o{ optTOCDepth = x })
    "dump-args" ->
      parseYAML v >>= \x -> return (\o -> o{ optDumpArgs = x })
    "ignore-args" ->
      parseYAML v >>= \x -> return (\o -> o{ optIgnoreArgs = x })
    "verbosity" ->
      parseYAML v >>= \x -> return (\o -> o{ optVerbosity = x })
    "trace" ->
      parseYAML v >>= \x -> return (\o -> o{ optTrace = x })
    "log-file" ->
      parseYAML v >>= \x -> return (\o -> o{ optLogFile = unpack <$> x })
    "fail-if-warnings" ->
      parseYAML v >>= \x -> return (\o -> o{ optFailIfWarnings = x })
    "reference-links" ->
      parseYAML v >>= \x -> return (\o -> o{ optReferenceLinks = x })
    "reference-location" ->
      parseYAML v >>= \x -> return (\o -> o{ optReferenceLocation = x })
    "dpi" ->
      parseYAML v >>= \x -> return (\o -> o{ optDpi = x })
    "wrap" ->
      parseYAML v >>= \x -> return (\o -> o{ optWrap = x })
    "columns" ->
      parseYAML v >>= \x -> return (\o -> o{ optColumns = x })
    "filters" ->
      parseYAML v >>= \x -> return (\o -> o{ optFilters = optFilters o <> x })
    "citeproc" ->
      parseYAML v >>= \x ->
        if x
           then return (\o -> o{ optFilters = CiteprocFilter : optFilters o })
           else return id
    "email-obfuscation" ->
      parseYAML v >>= \x -> return (\o -> o{ optEmailObfuscation = x })
    "identifier-prefix" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optIdentifierPrefix = x })
    "strip-empty-paragraphs" ->
      parseYAML v >>= \x -> return (\o -> o{ optStripEmptyParagraphs = x })
    "indented-code-classes" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optIndentedCodeClasses = x })
    "data-dir" ->
      parseYAML v >>= \x -> return (\o -> o{ optDataDir = unpack <$> x })
    "cite-method" ->
      parseYAML v >>= \x -> return (\o -> o{ optCiteMethod = x })
    "listings" ->
      parseYAML v >>= \x -> return (\o -> o{ optListings = x })
    "pdf-engine" ->
      parseYAML v >>= \x -> return (\o -> o{ optPdfEngine = unpack <$> x })
    "pdf-engine-opts" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optPdfEngineOpts = map unpack x })
    "pdf-engine-opt" ->
      (parseYAML v >>= \x ->
             return (\o -> o{ optPdfEngineOpts = map unpack x }))
      <|>
      (parseYAML v >>= \x ->
             return (\o -> o{ optPdfEngineOpts = [unpack x] }))
    "slide-level" ->
      parseYAML v >>= \x -> return (\o -> o{ optSlideLevel = x })
    "atx-headers" ->
      parseYAML v >>= \x -> return (\o -> o{ optSetextHeaders = not x })
    "markdown-headings" ->
      parseYAML v >>= \x -> return (\o ->
        case T.toLower x of
          "atx"    -> o{ optSetextHeaders = False }
          "setext" -> o{ optSetextHeaders = True }
          _        -> o)
    "ascii" ->
      parseYAML v >>= \x -> return (\o -> o{ optAscii = x })
    "default-image-extension" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optDefaultImageExtension = x })
    "extract-media" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optExtractMedia = unpack <$> x })
    "track-changes" ->
      parseYAML v >>= \x -> return (\o -> o{ optTrackChanges = x })
    "file-scope" ->
      parseYAML v >>= \x -> return (\o -> o{ optFileScope = x })
    "title-prefix" ->
      parseYAML v >>= \x -> return (\o -> o{ optTitlePrefix = x,
                                             optStandalone = True })
    "css" ->
      (parseYAML v >>= \x -> return (\o -> o{ optCss = optCss o <>
                                                 map unpack x }))
      <|>
      (parseYAML v >>= \x -> return (\o -> o{ optCss = optCss o <>
                                                [unpack x] }))
    "bibliography" ->
      (parseYAML v >>= \x -> return (\o ->
                               o{ optBibliography = optBibliography o <>
                                                      map unpack x }))
      <|>
      (parseYAML v >>= \x -> return (\o ->
                               o{ optBibliography = optBibliography o <>
                                                       [unpack x] }))
    "csl" ->
      parseYAML v >>= \x -> return (\o -> o{ optCSL = unpack <$> x })
    "citation-abbreviations" ->
      parseYAML v >>= \x -> return (\o -> o{ optCitationAbbreviations =
                                                  unpack <$> x })
    "ipynb-output" ->
      parseYAML v >>= \x -> return (\o -> o{ optIpynbOutput = x })
    "include-before-body" ->
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeBeforeBody =
                                optIncludeBeforeBody o <> map unpack x }))
      <|>
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeBeforeBody =
                                optIncludeBeforeBody o <> [unpack x] }))
    "include-after-body" ->
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeAfterBody =
                                optIncludeAfterBody o <> map unpack x }))
      <|>
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeAfterBody =
                                optIncludeAfterBody o <> [unpack x] }))
    "include-in-header" ->
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeInHeader =
                                optIncludeInHeader o <> map unpack x }))
      <|>
      (parseYAML v >>= \x ->
             return (\o -> o{ optIncludeInHeader =
                                optIncludeInHeader o <> [unpack x] }))
    "resource-path" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optResourcePath = map unpack x <>
                                 optResourcePath o })
    "request-headers" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optRequestHeaders = x })
    "no-check-certificate" ->
      parseYAML v >>= \x ->
             return (\o -> o{ optNoCheckCertificate = x })
    "eol" ->
      parseYAML v >>= \x -> return (\o -> o{ optEol = x })
    "strip-comments" ->
      parseYAML v >>= \x -> return (\o -> o  { optStripComments = x })
    _ -> failAtNode k' $ "Unknown option " ++ show k

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop               = 4
    , optPreserveTabs          = False
    , optStandalone            = False
    , optFrom                  = Nothing
    , optTo                    = Nothing
    , optTableOfContents       = False
    , optShiftHeadingLevelBy   = 0
    , optTemplate              = Nothing
    , optVariables             = mempty
    , optMetadata              = mempty
    , optMetadataFiles         = []
    , optOutputFile            = Nothing
    , optInputFiles            = Nothing
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
    , optWrap                  = WrapAuto
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
    , optPdfEngineOpts         = []
    , optSlideLevel            = Nothing
    , optSetextHeaders         = False
    , optAscii                 = False
    , optDefaultImageExtension = ""
    , optExtractMedia          = Nothing
    , optTrackChanges          = AcceptChanges
    , optFileScope             = False
    , optTitlePrefix           = Nothing
    , optCss                   = []
    , optIpynbOutput           = IpynbOutputBest
    , optIncludeBeforeBody     = []
    , optIncludeAfterBody      = []
    , optIncludeInHeader       = []
    , optResourcePath          = ["."]
    , optRequestHeaders        = []
    , optNoCheckCertificate    = False
    , optEol                   = Native
    , optStripComments         = False
    , optCSL                   = Nothing
    , optBibliography          = []
    , optCitationAbbreviations = Nothing
    }

parseStringKey ::  Node Pos -> Parser Text
parseStringKey k = case k of
  Scalar _ (SStr t) -> return t
  Scalar _ _ -> failAtNode k "Non-string key"
  _ -> failAtNode k "Non-scalar key"

yamlToMeta :: Node Pos -> Parser Meta
yamlToMeta (Mapping _ _ m) =
    either (fail . show) return $ runEverything (yamlMap pMetaString m)
  where
    pMetaString = pure . MetaString <$> P.manyChar P.anyChar
    runEverything p =
      runPure (P.readWithM p (def :: P.ParserState) ("" :: Text))
      >>= fmap (Meta . flip P.runF def)
yamlToMeta _ = return mempty

-- | Apply defaults from --defaults file.
applyDefaults :: (PandocMonad m, MonadIO m)
              => Opt
              -> FilePath
              -> StateT DefaultsState m Opt
applyDefaults opt file = do
  setVerbosity $ optVerbosity opt
  modify $ \defsState -> defsState{ curDefaults = Just file }
  inp <- readFileLazy file
  case decode1 inp of
      Right f -> f opt
      Left (errpos, errmsg)  -> throwError $
         PandocParseError $ T.pack $
         "Error parsing " ++ file ++ " line " ++
          show (posLine errpos) ++ " column " ++
          show (posColumn errpos) ++ ":\n" ++ errmsg

fullDefaultsPath :: (PandocMonad m, MonadIO m)
                 => Maybe FilePath
                 -> FilePath
                 -> m FilePath
fullDefaultsPath dataDir file = do
  let fp = if null (takeExtension file)
              then addExtension file "yaml"
              else file
  defaultDataDir <- liftIO defaultUserDataDir
  let defaultFp = fromMaybe defaultDataDir dataDir </> "defaults" </> fp
  fromMaybe fp <$> findM fileExists [fp, defaultFp]

-- | In a list of lists, append another list in front of every list which
-- starts with specific element.
expand :: Ord a => [[a]] -> [a] -> a -> [[a]]
expand [] ns n = fmap (\x -> x : [n]) ns
expand ps ns n = concatMap (ext n ns) ps
  where
    ext x xs p = case p of
      (l : _) | x == l -> fmap (: p) xs
      _ -> [p]

cyclic :: Ord a => [[a]] -> Bool
cyclic = any hasDuplicate
  where
    hasDuplicate xs = length (ordNub xs) /= length xs

-- see https://github.com/jgm/pandoc/pull/4083
-- using generic deriving caused long compilation times
$(deriveJSON
   defaultOptions{ fieldLabelModifier = drop 11 . map toLower } ''IpynbOutput)
$(deriveJSON
   defaultOptions{ fieldLabelModifier = map toLower } ''LineEnding)
$(deriveJSON
   defaultOptions{ fieldLabelModifier =
                      camelCaseStrToHyphenated . dropWhile isLower
                 } ''Opt)

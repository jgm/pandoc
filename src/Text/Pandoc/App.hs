{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App (
            convertWithOpts
          , handleOptInfo
          , Opt(..)
          , OptInfo(..)
          , LineEnding(..)
          , IpynbOutput (..)
          , Filter(..)
          , defaultOpts
          , parseOptions
          , parseOptionsFromArgs
          , options
          , applyFilters
          ) where
import qualified Control.Exception as E
import Control.Monad ( (>=>), when, forM, forM_ )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.Directory (doesDirectoryExist, createDirectory)
import Codec.Archive.Zip (toArchiveOrFail,
                          extractFilesFromArchive, ZipOption(..))
import System.Exit (exitSuccess)
import System.FilePath ( takeBaseName, takeExtension)
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.MediaBag (mediaItems)
import Text.Pandoc.Image (svgToPng)
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), defaultOpts,
                            IpynbOutput (..), OptInfo(..))
import Text.Pandoc.App.CommandLineOptions (parseOptions, parseOptionsFromArgs,
                                           options, handleOptInfo)
import Text.Pandoc.App.Input (InputParameters (..), readInput)
import Text.Pandoc.App.OutputSettings (OutputSettings (..), optToOutputSettings)
import Text.Collate.Lang (Lang (..), parseLang)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), Environment (..),
                           applyFilters)
import qualified Text.Pandoc.Format as Format
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Scripting (ScriptingEngine (..), CustomComponents(..))
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter,
         headerShift, filterIpynbOutput, tshow)
import Text.Pandoc.URI (isURI)
import Text.Pandoc.Writers.Shared (lookupMetaString)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

convertWithOpts :: ScriptingEngine -> Opt -> IO ()
convertWithOpts scriptingEngine opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)
  datadir <- case optDataDir opts of
                  Nothing   -> do
                    d <- defaultUserDataDir
                    exists <- doesDirectoryExist d
                    return $ if exists
                                then Just d
                                else Nothing
                  mdatadir  -> return mdatadir

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout (T.pack outputFile)
       mapM_ (UTF8.hPutStrLn stdout . T.pack)
             (fromMaybe ["-"] $ optInputFiles opts)
       exitSuccess

#ifdef _WINDOWS
  let istty = True
#else
  istty <- liftIO $ queryTerminal stdOutput
#endif

  res <- runIO $ convertWithOpts' scriptingEngine istty datadir opts
  case res of
    Left e -> E.throwIO e
    Right (output, reports) -> do
      case optLogFile opts of
           Nothing      -> return ()
           Just logfile -> BL.writeFile logfile (encodeLogMessages reports)
      let isWarning msg = messageVerbosity msg == WARNING
      when (optFailIfWarnings opts && any isWarning reports) $
          E.throwIO PandocFailOnWarningError
      let eol = case optEol opts of
                     CRLF   -> IO.CRLF
                     LF     -> IO.LF
                     Native -> nativeNewline
      case output of
        TextOutput t    -> writerFn eol outputFile t
        BinaryOutput bs -> writeFnBinary outputFile bs
        ZipOutput bs
          | null (takeExtension outputFile) -> do
             -- create directory and unzip
             createDirectory outputFile -- will fail if directory exists
             let zipopts = [OptRecursive, OptDestination outputFile] ++
                           [OptVerbose | optVerbosity opts == INFO]
             case toArchiveOrFail bs of
               Right archive -> extractFilesFromArchive zipopts archive
               Left e -> E.throwIO $ PandocShouldNeverHappenError $ T.pack e
          | otherwise -> writeFnBinary outputFile bs

convertWithOpts' :: (PandocMonad m, MonadIO m, MonadMask m)
                 => ScriptingEngine
                 -> Bool
                 -> Maybe FilePath
                 -> Opt
                 -> m (PandocOutput, [LogMessage])
convertWithOpts' scriptingEngine istty datadir opts = do
  configureCommonState datadir opts
  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let sources = case optInputFiles opts of
                     Just xs | not (optIgnoreArgs opts) -> xs
                     _ -> ["-"]

  let defFlavor fmt = Format.FlavoredFormat fmt mempty
  -- assign reader and writer based on options and filenames
  flvrd@(Format.FlavoredFormat readerNameBase _extsDiff) <-
    case optFrom opts of
      Just f  -> Format.parseFlavoredFormat f
      Nothing -> case Format.formatFromFilePaths sources of
        Just f' -> return f'
        Nothing | sources == ["-"] -> return $ defFlavor "markdown"
                | any (isURI . T.pack) sources -> return $ defFlavor "html"
                | otherwise -> do
                    report $ CouldNotDeduceFormat
                      (map (T.pack . takeExtension) sources) "markdown"
                    return $ defFlavor "markdown"

  let makeSandboxed pureReader =
        let files = maybe id (:) (optReferenceDoc opts) .
                    maybe id (:) (optEpubMetadata opts) .
                    maybe id (:) (optEpubCoverImage opts) .
                    maybe id (:) (optCSL opts) .
                    maybe id (:) (optCitationAbbreviations opts) $
                    optEpubFonts opts ++
                    optBibliography opts
         in  case pureReader of
               TextReader r -> TextReader $ \o t -> sandbox files (r o t)
               ByteStringReader r
                          -> ByteStringReader $ \o t -> sandbox files (r o t)

  (reader, readerExts) <-
    if ".lua" `T.isSuffixOf` readerNameBase
       then do
            let scriptPath = T.unpack readerNameBase
            components <- engineLoadCustom scriptingEngine scriptPath
            r <- case customReader components of
                   Nothing -> throwError $ PandocAppError $
                               readerNameBase <> " does not contain a custom reader"
                   Just r -> return r
            let extsConf = fromMaybe mempty (customExtensions components)
            rexts <- Format.applyExtensionsDiff extsConf flvrd
            return (r, rexts)
       else if optSandbox opts
               then case runPure (getReader flvrd) of
                      Left e -> throwError e
                      Right (r, rexts) -> return (makeSandboxed r, rexts)
               else getReader flvrd

  outputSettings <- optToOutputSettings scriptingEngine opts
  let format = outputFormat outputSettings
  let writer = outputWriter outputSettings
  let writerOptions = outputWriterOptions outputSettings

  -- whether we are targeting PDF.
  let pdfOutput = isJust $ outputPdfProgram outputSettings
  -- whether standalone output should be produced.
  let bibOutput = format `elem` ["bibtex", "biblatex", "csljson"]
  let standalone = isJust (writerTemplate writerOptions) || bibOutput

  --
  -- Sanity checks
  --
  when (pdfOutput && readerNameBase == "latex") $
    case optInputFiles opts of
      Just (inputFile:_) -> report $ UnusualConversion $ T.pack $
        "to convert a .tex file to PDF, you get better results by using pdflatex "
          <> "(or lualatex or xelatex) directly, try `pdflatex " <> inputFile
          <> "` instead of `pandoc " <> inputFile <> " -o " <> outputFile <> "`."
      _ -> return ()

  -- We don't want to send output to the terminal if the user
  -- does 'pandoc -t docx input.txt'; though we allow them to
  -- force this with '-o -'.  On posix systems, we detect
  -- when stdout is being piped and allow output to stdout
  -- in that case, but on Windows we can't.
  when ((pdfOutput || not (isTextFormat format)) &&
           istty && isNothing ( optOutputFile opts)) $
    throwError $ PandocAppError $
            "Cannot write " <> (if pdfOutput then "pdf" else format) <>
            " output to terminal.\n" <>
            "Specify an output file using the -o option, or " <>
            "use '-o -' to force output to stdout."

  when (readerNameBase == "markdown_github" ||
        format == "markdown_github") $
    report $ Deprecated "markdown_github" "Use gfm instead."

  abbrevs <- readAbbreviations (optAbbreviations opts)
  let readerOpts = def{
          readerStandalone = standalone
        , readerColumns = optColumns opts
        , readerTabStop = optTabStop opts
        , readerIndentedCodeClasses = optIndentedCodeClasses opts
        , readerDefaultImageExtension = optDefaultImageExtension opts
        , readerTrackChanges = optTrackChanges opts
        , readerAbbreviations = abbrevs
        , readerExtensions = readerExts
        , readerStripComments = optStripComments opts
        }

  metadataFromFile <- getMetadataFromFiles readerNameBase readerOpts
                         (optMetadataFiles opts)

  let transforms = (case optShiftHeadingLevelBy opts of
                        0             -> id
                        x             -> (headerShift x :)) .
                   (if extensionEnabled Ext_east_asian_line_breaks
                          readerExts &&
                       not (extensionEnabled Ext_east_asian_line_breaks
                            (writerExtensions writerOptions) &&
                            writerWrapText writerOptions == WrapPreserve)
                       then (eastAsianLineBreakFilter :)
                       else id) .
                   (case optIpynbOutput opts of
                     _ | readerNameBase /= "ipynb" -> id
                     IpynbOutputAll  -> id
                     IpynbOutputNone -> (filterIpynbOutput Nothing :)
                     IpynbOutputBest -> (filterIpynbOutput (Just $
                                   if htmlFormat format
                                      then Format "html"
                                      else
                                        case format of
                                          "latex"  -> Format "latex"
                                          "beamer" -> Format "latex"
                                          _        -> Format format) :))
                   $ []

  let isPandocCiteproc (JSONFilter f) = takeBaseName f == "pandoc-citeproc"
      isPandocCiteproc _              = False

  when (any isPandocCiteproc filters) $
    report $ Deprecated "pandoc-citeproc filter"
             "Use --citeproc instead."

  let cslMetadata =
        maybe id (setMeta "csl") (optCSL opts) .
        (case optBibliography opts of
           [] -> id
           xs -> setMeta "bibliography" xs) .
        maybe id (setMeta "citation-abbreviations")
                       (optCitationAbbreviations opts) $ mempty

  let filterEnv = Environment readerOpts writerOptions

  let inputParams = InputParameters
        { inputReader = reader
        , inputReaderName = readerNameBase
        , inputReaderOptions = readerOpts
        , inputSources = sources
        , inputFileScope = optFileScope opts
        , inputSpacesPerTab = if optPreserveTabs opts
                              then Nothing
                              else Just (optTabStop opts)
        }

  doc <- readInput inputParams
          >>= ( return . adjustMetadata (metadataFromFile <>)
            >=> return . adjustMetadata (<> optMetadata opts)
            >=> return . adjustMetadata (<> cslMetadata)
            >=> applyTransforms transforms
            >=> applyFilters scriptingEngine filterEnv filters [T.unpack format]
            >=> (if not (optSandbox opts) &&
                    (isJust (optExtractMedia opts)
                     || format == "docx") -- for fallback pngs
                 then fillMediaBag
                 else return)
            >=> maybe return extractMedia (optExtractMedia opts)
            )

  when (format == "docx" && not (optSandbox opts)) $ do
    createPngFallbacks (writerDpi writerOptions)

  output <- case writer of
    ByteStringWriter f
      | format == "chunkedhtml" -> ZipOutput <$> f writerOptions doc
      | otherwise -> BinaryOutput <$> f writerOptions doc
    TextWriter f -> case outputPdfProgram outputSettings of
      Just pdfProg -> do
              res <- makePDF pdfProg (optPdfEngineOpts opts) f
                      writerOptions doc
              case res of
                   Right pdf -> return $ BinaryOutput pdf
                   Left err' -> throwError $ PandocPDFError $
                                   TL.toStrict (TE.decodeUtf8With TE.lenientDecode err')

      Nothing -> do
              let ensureNl t
                    | standalone = t
                    | T.null t || T.last t /= '\n' = t <> T.singleton '\n'
                    | otherwise = t
              textOutput <- ensureNl <$> f writerOptions doc
              if (optSelfContained opts || optEmbedResources opts) && htmlFormat format
                 then TextOutput <$> makeSelfContained textOutput
                 else return $ TextOutput textOutput
  reports <- getLog
  return (output, reports)

data PandocOutput =
      TextOutput Text
    | BinaryOutput BL.ByteString
    | ZipOutput BL.ByteString
  deriving (Show)

type Transform = Pandoc -> Pandoc

-- | Configure the common state
configureCommonState :: PandocMonad m => Maybe FilePath -> Opt -> m ()
configureCommonState datadir opts = do
  setUserDataDir datadir
  setTrace (optTrace opts)
  setVerbosity (optVerbosity opts)
  setResourcePath (optResourcePath opts)
  setInputFiles (fromMaybe ["-"] (optInputFiles opts))
  setOutputFile (optOutputFile opts)
  setNoCheckCertificate (optNoCheckCertificate opts)

  mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

  case lookupMetaString "lang" (optMetadata opts) of
    ""      -> setTranslations $ Lang "en" Nothing (Just "US") [] [] []
    l       -> case parseLang l of
                 Left _   -> report $ InvalidLang l
                 Right l' -> setTranslations l'

-- | Retrieves the set of abbreviations to be used by pandoc. These currently
-- only affect the Markdown reader.
readAbbreviations :: PandocMonad m => Maybe FilePath -> m (Set.Set Text)
readAbbreviations mbfilepath =
  (case mbfilepath of
    Nothing -> readDataFile "abbreviations"
    Just f  -> readFileStrict f)
    >>= fmap (Set.fromList . filter (not . T.null) . T.lines) .
         toTextM (fromMaybe mempty mbfilepath)

createPngFallbacks :: (PandocMonad m, MonadIO m) => Int -> m ()
createPngFallbacks dpi = do
  -- create fallback pngs for svgs
  items <- mediaItems <$> getMediaBag
  forM_ items $ \(fp, mt, bs) ->
    case T.takeWhile (/=';') mt of
      "image/svg+xml" -> do
        res <- svgToPng dpi bs
        case res of
          Right bs' -> do
            let fp' = fp <> ".png"
            insertMedia fp' (Just "image/png") bs'
          Left e -> report $ CouldNotConvertImage (T.pack fp) (tshow e)
      _ -> return ()

getMetadataFromFiles :: PandocMonad m
                     => Text -> ReaderOptions -> [FilePath] -> m Meta
getMetadataFromFiles readerFormat readerOpts = \case
  []    -> return mempty
  paths -> mconcat <$> do
    -- If format is markdown or commonmark, use the enabled extensions,
    -- otherwise treat metadata as pandoc markdown (see #7926, #6832)
    let readerOptsMeta =
          if readerFormat `elem` ["markdown", "commonmark"]
          then readerOpts
          else readerOpts{ readerExtensions = pandocExtensions }
    forM paths $ \path -> do
      raw <- readMetadataFile path
      yamlToMeta readerOptsMeta (Just path) raw

htmlFormat :: Text -> Bool
htmlFormat = (`elem` ["html","html4","html5","s5","slidy",
                      "slideous","dzslides","revealjs"])

isTextFormat :: Text -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]

adjustMetadata :: (Meta -> Meta) -> Pandoc -> Pandoc
adjustMetadata f (Pandoc meta bs) = Pandoc (f meta) bs

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

writeFnBinary :: FilePath -> BL.ByteString -> IO ()
writeFnBinary "-" = BL.putStr
writeFnBinary f   = BL.writeFile (UTF8.encodePath f)

writerFn :: IO.Newline -> FilePath -> Text -> IO ()
writerFn eol "-" = UTF8.putStrWith eol
writerFn eol f   = UTF8.writeFileWith eol f

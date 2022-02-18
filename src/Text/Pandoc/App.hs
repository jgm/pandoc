{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
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
          , parseOptionsFromArgs
          , options
          , applyFilters
          ) where
import qualified Control.Exception as E
import Control.Monad ( (>=>), when, forM_ )
import Control.Monad.Trans ( MonadIO(..) )
import Control.Monad.Except (throwError, catchError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding as TSE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Encoding.Error as TSE
import Network.URI (URI (..), parseURI)
import System.Directory (doesDirectoryExist)
import System.Exit (exitSuccess)
import System.FilePath ( takeBaseName, takeExtension)
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import Text.Pandoc
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.MediaBag (mediaItems)
import Text.Pandoc.MIME (getCharset, MimeType)
import Text.Pandoc.Image (svgToPng)
import Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), defaultOpts,
                            IpynbOutput (..))
import Text.Pandoc.App.CommandLineOptions (parseOptions, parseOptionsFromArgs,
                                           options)
import Text.Pandoc.App.OutputSettings (OutputSettings (..), optToOutputSettings)
import Text.Collate.Lang (Lang (..), parseLang)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), Environment (..),
                           applyFilters)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift, isURI, tabFilter, uriPathToPath, filterIpynbOutput,
         defaultUserDataDir, tshow)
import Text.Pandoc.Writers.Shared (lookupMetaString)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.Readers.Custom (readCustom)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  datadir <- case optDataDir opts of
                  Nothing   -> do
                    d <- defaultUserDataDir
                    exists <- doesDirectoryExist d
                    return $ if exists
                                then Just d
                                else Nothing
                  Just _    -> return $ optDataDir opts

  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout (T.pack outputFile)
       mapM_ (UTF8.hPutStrLn stdout . T.pack)
             (fromMaybe ["-"] $ optInputFiles opts)
       exitSuccess

  let sources = case optInputFiles opts of
                     Just xs | not (optIgnoreArgs opts) -> xs
                     _ -> ["-"]
#ifdef _WINDOWS
  let istty = True
#else
  istty <- liftIO $ queryTerminal stdOutput
#endif

  res <- runIO $ do

    setTrace (optTrace opts)
    setVerbosity verbosity
    setUserDataDir datadir
    setResourcePath (optResourcePath opts)

    setInputFiles (fromMaybe ["-"] (optInputFiles opts))
    setOutputFile (optOutputFile opts)

    -- assign reader and writer based on options and filenames
    readerName <- case optFrom opts of
                       Just f  -> return f
                       Nothing -> case formatFromFilePaths sources of
                           Just f' -> return f'
                           Nothing | sources == ["-"] -> return "markdown"
                                   | any (isURI . T.pack) sources -> return "html"
                                   | otherwise -> do
                             report $ CouldNotDeduceFormat
                                 (map (T.pack . takeExtension) sources) "markdown"
                             return "markdown"

    let readerNameBase = T.takeWhile (\c -> c /= '+' && c /= '-') readerName

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
      if ".lua" `T.isSuffixOf` readerName
         then return (TextReader (readCustom (T.unpack readerName)), mempty)
         else if optSandbox opts
                 then case runPure (getReader readerName) of
                        Left e -> throwError e
                        Right (r, rexts) -> return (makeSandboxed r, rexts)
                 else getReader readerName

    outputSettings <- optToOutputSettings opts
    let format = outputFormat outputSettings
    let writer = outputWriter outputSettings
    let writerName = outputWriterName outputSettings
    let writerNameBase = T.takeWhile (\c -> c /= '+' && c /= '-') writerName
    let writerOptions = outputWriterOptions outputSettings

    let pdfOutput = isJust $ outputPdfProgram outputSettings

    let bibOutput = writerNameBase == "bibtex" ||
                    writerNameBase == "biblatex" ||
                    writerNameBase == "csljson"

    let standalone = optStandalone opts ||
                     not (isTextFormat format) ||
                     pdfOutput ||
                     bibOutput

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


    abbrevs <- Set.fromList . filter (not . T.null) . T.lines . UTF8.toText <$>
               case optAbbreviations opts of
                    Nothing -> readDataFile "abbreviations"
                    Just f  -> readFileStrict f

    case lookupMetaString "lang" (optMetadata opts) of
           ""      -> setTranslations $ Lang "en" Nothing (Just "US") [] [] []
           l       -> case parseLang l of
                           Left _   -> report $ InvalidLang l
                           Right l' -> setTranslations l'

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

    metadataFromFile <-
      case optMetadataFiles opts of
        []    -> return mempty
        paths -> do
          -- If format is markdown or commonmark, use the enabled extensions,
          -- otherwise treat metadata as pandoc markdown (see #7926, #6832)
          let readerOptsMeta =
                if readerNameBase == "markdown" || readerNameBase == "commonmark"
                   then readerOpts
                   else readerOpts{ readerExtensions = pandocExtensions }
          mconcat <$> mapM
                (\path -> do raw <- readMetadataFile path
                             yamlToMeta readerOptsMeta (Just path) raw) paths

    let transforms = (case optShiftHeadingLevelBy opts of
                          0             -> id
                          x             -> (headerShift x :)) .
                     (if optStripEmptyParagraphs opts
                         then (stripEmptyParagraphs :)
                         else id) .
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

    let convertTabs = tabFilter (if optPreserveTabs opts ||
                                      readerNameBase == "t2t" ||
                                      readerNameBase == "man"
                                    then 0
                                    else optTabStop opts)


    when (readerNameBase == "markdown_github" ||
          writerNameBase == "markdown_github") $
      report $ Deprecated "markdown_github" "Use gfm instead."

    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    setNoCheckCertificate (optNoCheckCertificate opts)

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

    inputs <- readSources sources

    doc <- (case reader of
             TextReader r
               | readerNameBase == "json" ->
                   mconcat <$>
                      mapM (inputToText convertTabs
                             >=> r readerOpts . (:[])) inputs
               | optFileScope opts  ->
                   mconcat <$> mapM
                      (inputToText convertTabs
                             >=> r readerOpts . (:[]))
                      inputs
               | otherwise -> mapM (inputToText convertTabs) inputs
                                >>= r readerOpts
             ByteStringReader r ->
               mconcat <$> mapM (r readerOpts . inputToLazyByteString) inputs)
            >>=
              (   (if not (optSandbox opts) &&
                      (isJust (optExtractMedia opts)
                       || writerNameBase == "docx") -- for fallback pngs
                      then fillMediaBag
                      else return)
              >=> return . adjustMetadata (metadataFromFile <>)
              >=> return . adjustMetadata (<> optMetadata opts)
              >=> return . adjustMetadata (<> cslMetadata)
              >=> applyTransforms transforms
              >=> applyFilters filterEnv filters [T.unpack format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    when (writerNameBase == "docx" && not (optSandbox opts)) $ do
      -- create fallback pngs for svgs
      items <- mediaItems <$> getMediaBag
      forM_ items $ \(fp, mt, bs) ->
        case T.takeWhile (/=';') mt of
          "image/svg+xml" -> do
            res <- svgToPng (writerDpi writerOptions) bs
            case res of
              Right bs' -> do
                let fp' = fp <> ".png"
                insertMedia fp' (Just "image/png") bs'
              Left e -> report $ CouldNotConvertImage (T.pack fp) (tshow e)
          _ -> return ()

    output <- case writer of
      ByteStringWriter f -> BinaryOutput <$> f writerOptions doc
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
                if optSelfContained opts && htmlFormat format
                   then TextOutput <$> makeSelfContained textOutput
                   else return $ TextOutput textOutput
    reports <- getLog
    return (output, reports)

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

data PandocOutput = TextOutput Text | BinaryOutput BL.ByteString
  deriving (Show)

type Transform = Pandoc -> Pandoc

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

readSources :: (PandocMonad m, MonadIO m)
            => [FilePath] -> m [(FilePath, (BS.ByteString, Maybe MimeType))]
readSources srcs =
  mapM (\fp -> do t <- readSource fp
                  return (if fp == "-" then "" else fp, t)) srcs

readSource :: (PandocMonad m, MonadIO m)
           => FilePath -> m (BS.ByteString, Maybe MimeType)
readSource "-" = (,Nothing) <$> readStdinStrict
readSource src =
  case parseURI src of
    Just u | uriScheme u `elem` ["http:","https:"] -> openURL (T.pack src)
           | uriScheme u == "file:" ->
               (,Nothing) <$>
                 readFileStrict (uriPathToPath $ T.pack $ uriPath u)
    _       -> (,Nothing) <$> readFileStrict src

utf8ToText :: PandocMonad m => FilePath -> BS.ByteString -> m Text
utf8ToText fp bs =
  case TSE.decodeUtf8' . dropBOM $ bs of
    Left (TSE.DecodeError _ (Just w)) ->
      case BS.elemIndex w bs of
        Just offset -> throwError $ PandocUTF8DecodingError (T.pack fp) offset w
        Nothing -> throwError $ PandocUTF8DecodingError (T.pack fp) 0 w
    Left e -> throwError $ PandocAppError (tshow e)
    Right t -> return t
 where
   dropBOM bs' =
     if "\xEF\xBB\xBF" `BS.isPrefixOf` bs'
        then BS.drop 3 bs'
        else bs'


inputToText :: PandocMonad m
            => (Text -> Text)
            -> (FilePath, (BS.ByteString, Maybe MimeType))
            -> m (FilePath, Text)
inputToText convTabs (fp, (bs,mt)) =
  (fp,) . convTabs . T.filter (/='\r') <$>
  case mt >>= getCharset of
    Just "UTF-8"      -> utf8ToText fp bs
    Just "ISO-8859-1" -> return $ T.pack $ B8.unpack bs
    Just charset      -> throwError $ PandocUnsupportedCharsetError charset
    Nothing           -> catchError
                           (utf8ToText fp bs)
                           (\case
                              PandocUTF8DecodingError{} -> do
                                report $ NotUTF8Encoded
                                  (if null fp
                                      then "input"
                                      else fp)
                                return $ T.pack $ B8.unpack bs
                              e -> throwError e)

inputToLazyByteString :: (FilePath, (BS.ByteString, Maybe MimeType))
                      -> BL.ByteString
inputToLazyByteString (_, (bs,_)) = BL.fromStrict bs

writeFnBinary :: FilePath -> BL.ByteString -> IO ()
writeFnBinary "-" = BL.putStr
writeFnBinary f   = BL.writeFile (UTF8.encodePath f)

writerFn :: IO.Newline -> FilePath -> Text -> IO ()
writerFn eol "-" = UTF8.putStrWith eol
writerFn eol f   = UTF8.writeFileWith eol f

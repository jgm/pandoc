{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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
import Control.Monad.Trans
import Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Encoding.Error as TSE
import Network.URI (URI (..), parseURI)
import System.Directory (doesDirectoryExist)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import Text.Pandoc
import Text.Pandoc.App.FormatHeuristics (formatFromFilePaths)
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), defaultOpts)
import Text.Pandoc.App.CommandLineOptions (parseOptions, options)
import Text.Pandoc.App.OutputSettings (OutputSettings (..), optToOutputSettings)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Builder (setMeta, deleteMeta)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), applyFilters)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.SelfContained (makeDataURI, makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift, isURI, tabFilter, uriPathToPath, filterIpynbOutput,
         defaultUserDataDirs)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif


convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
       exitSuccess

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
                  Nothing   -> do
                    ds <- defaultUserDataDirs
                    let selectUserDataDir [] = return Nothing
                        selectUserDataDir (dir:dirs) = do
                              exists <- doesDirectoryExist dir
                              if exists
                                 then return (Just dir)
                                 else selectUserDataDir dirs
                    selectUserDataDir ds
                  Just _    -> return $ optDataDir opts

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
             Just logfile -> BL.writeFile logfile (encodeLogMessages reports)
        let isWarning msg = messageVerbosity msg == WARNING
        when (optFailIfWarnings opts && any isWarning reports) $
            E.throwIO PandocFailOnWarningError
        return res

  let eol = case optEol opts of
                 CRLF   -> IO.CRLF
                 LF     -> IO.LF
                 Native -> nativeNewline
#ifdef _WINDOWS
  let istty = True
#else
  istty <- liftIO $ queryTerminal stdOutput
#endif

  runIO' $ do
    setUserDataDir datadir
    setInputFiles (optInputFiles opts)
    setOutputFile (optOutputFile opts)

    -- assign reader and writer based on options and filenames
    readerName <- case optReader opts of
                       Just f  -> return f
                       Nothing -> case formatFromFilePaths sources of
                           Just f' -> return f'
                           Nothing | sources == ["-"] -> return "markdown"
                                   | any isURI sources -> return "html"
                                   | otherwise -> do
                             report $ CouldNotDeduceFormat
                                 (map takeExtension sources) "markdown"
                             return "markdown"

    let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

    when (pdfOutput && readerName == "latex") $
      case (optInputFiles opts) of
        (inputFile:_) -> report $ UnusualConversion $
          "to convert a .tex file to PDF, you get better results by using pdflatex "
            <> "(or lualatex or xelatex) directly, try `pdflatex " <> inputFile
            <> "` instead of `pandoc " <> inputFile <> " -o " <> outputFile <> "`."
        _ -> return ()

    (reader :: Reader PandocIO, readerExts) <- getReader readerName

    let convertTabs = tabFilter (if optPreserveTabs opts ||
                                      readerName == "t2t" ||
                                      readerName == "man"
                                    then 0
                                    else optTabStop opts)


    let readSources :: [FilePath] -> PandocIO Text
        readSources srcs = convertTabs . T.intercalate (T.pack "\n") <$>
                              mapM readSource srcs


    outputSettings <- optToOutputSettings opts
    let format = outputFormat outputSettings
    let writer = outputWriter outputSettings
    let writerName = outputWriterName outputSettings
    let writerOptions = outputWriterOptions outputSettings

    let standalone = optStandalone opts || not (isTextFormat format) || pdfOutput

    -- We don't want to send output to the terminal if the user
    -- does 'pandoc -t docx input.txt'; though we allow them to
    -- force this with '-o -'.  On posix systems, we detect
    -- when stdout is being piped and allow output to stdout
    -- in that case, but on Windows we can't.
    when (not (isTextFormat format) && istty && isNothing ( optOutputFile opts)) $
      throwError $ PandocAppError $
              "Cannot write " ++ format ++ " output to terminal.\n" ++
              "Specify an output file using the -o option, or " ++
              "use '-o -' to force output to stdout."


    abbrevs <- Set.fromList . filter (not . null) . lines <$>
               case optAbbreviations opts of
                    Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                    Just f  -> UTF8.toString <$> readFileStrict f

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
      case optMetadataFile opts of
        []    -> return mempty
        paths -> mapM readFileLazy paths >>= mapM (yamlToMeta readerOpts)
                   >>= return . (foldr1 (<>))

    let transforms = (case optBaseHeaderLevel opts of
                          x | x > 1     -> (headerShift (x - 1) :)
                            | otherwise -> id) .
                     (case optShiftHeadingLevelBy opts of
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
                       "all"    -> id
                       "none"   -> (filterIpynbOutput Nothing :)
                       "best"   -> (filterIpynbOutput (Just $
                                     if htmlFormat format
                                        then Format "html"
                                        else
                                          case format of
                                            "latex"  -> Format "latex"
                                            "beamer" -> Format "latex"
                                            _        -> Format format) :)
                       _  -> id)  -- should not happen
                     $ []

    let sourceToDoc :: [FilePath] -> PandocIO Pandoc
        sourceToDoc sources' =
           case reader of
                TextReader r
                  | optFileScope opts || readerName == "json" ->
                      mconcat <$> mapM (readSource >=> r readerOpts) sources'
                  | otherwise ->
                      readSources sources' >>= r readerOpts
                ByteStringReader r ->
                  mconcat <$> mapM (readFile' >=> r readerOpts) sources'


    when (readerName == "markdown_github" ||
          writerName == "markdown_github") $
      report $ Deprecated "markdown_github" "Use gfm instead."

    setResourcePath (optResourcePath opts)
    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    doc <- sourceToDoc sources >>=
              (   (if isJust (optExtractMedia opts)
                      then fillMediaBag
                      else return)
              >=> return . addNonPresentMetadata metadataFromFile
              >=> return . addMetadata metadata
              >=> applyTransforms transforms
              >=> applyFilters readerOpts filters' [format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    case writer of
      ByteStringWriter f -> f writerOptions doc >>= writeFnBinary outputFile
      TextWriter f -> case outputPdfProgram outputSettings of
        Just pdfProg -> do
                res <- makePDF pdfProg (optPdfEngineArgs opts) f
                        writerOptions doc
                case res of
                     Right pdf -> writeFnBinary outputFile pdf
                     Left err' -> throwError $ PandocPDFError $
                                     TL.unpack (TE.decodeUtf8With TE.lenientDecode err')

        Nothing -> do
                let ensureNl t
                      | standalone = t
                      | T.null t || T.last t /= '\n' = t <> T.singleton '\n'
                      | otherwise = t
                output <- ensureNl <$> f writerOptions doc
                writerFn eol outputFile =<<
                  if optSelfContained opts && htmlFormat format
                     -- TODO not maximally efficient; change type
                     -- of makeSelfContained so it works w/ Text
                     then T.pack <$> makeSelfContained (T.unpack output)
                     else return output

type Transform = Pandoc -> Pandoc

htmlFormat :: String -> Bool
htmlFormat = (`elem` ["html","html4","html5","s5","slidy",
                      "slideous","dzslides","revealjs"])

isTextFormat :: String -> Bool
isTextFormat s = s `notElem` ["odt","docx","epub2","epub3","epub","pptx"]

addNonPresentMetadata :: Text.Pandoc.Meta -> Pandoc -> Pandoc
addNonPresentMetadata newmeta (Pandoc meta bs) = Pandoc (meta <> newmeta) bs

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
readMetaValue s
  | s == "true"  = MetaBool True
  | s == "True"  = MetaBool True
  | s == "TRUE"  = MetaBool True
  | s == "false" = MetaBool False
  | s == "False" = MetaBool False
  | s == "FALSE" = MetaBool False
  | otherwise    = MetaString s

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

readSource :: FilePath -> PandocIO Text
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" -> liftIO $
                                 readTextFile (uriPathToPath $ uriPath u)
                      _       -> liftIO $ readTextFile src
  where readTextFile :: FilePath -> IO Text
        readTextFile fp = do
          bs <- if src == "-"
                   then BS.getContents
                   else BS.readFile fp
          E.catch (return $! UTF8.toText bs)
             (\e -> case e of
                         TSE.DecodeError _ (Just w) -> do
                           case BS.elemIndex w bs of
                             Just offset -> E.throwIO $
                                  PandocUTF8DecodingError fp offset w
                             _ -> E.throwIO $ PandocUTF8DecodingError fp 0 w
                         _ -> E.throwIO $ PandocAppError (show e))

readURI :: FilePath -> PandocIO Text
readURI src = UTF8.toText . fst <$> openURL src

readFile' :: MonadIO m => FilePath -> m BL.ByteString
readFile' "-" = liftIO BL.getContents
readFile' f   = liftIO $ BL.readFile f

writeFnBinary :: MonadIO m => FilePath -> BL.ByteString -> m ()
writeFnBinary "-" = liftIO . BL.putStr
writeFnBinary f   = liftIO . BL.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => IO.Newline -> FilePath -> Text -> m ()
-- TODO this implementation isn't maximally efficient:
writerFn eol "-" = liftIO . UTF8.putStrWith eol . T.unpack
writerFn eol f   = liftIO . UTF8.writeFileWith eol f . T.unpack

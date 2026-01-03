{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{- |
   Module      : Text.Pandoc.PDF
   Copyright   : Copyright (C) 2012-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( makePDF ) where

import qualified Codec.Picture as JP
import qualified Control.Exception as E
import Control.Monad.Trans (MonadIO (..))
import Control.Monad (foldM_)
import Crypto.Hash (hashWith, SHA1(SHA1))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8')
import Text.Printf (printf)
import Data.Char (ord, isAscii, isSpace)
import System.Directory
import System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory,
                       withTempFile)
import qualified System.IO.Error as IE
import Text.DocLayout (literal, render, hsep)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocPDFProgramNotFoundError))
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.Options (HTMLMathMethod (..), WriterOptions (..))
import Text.Pandoc.Extensions (disableExtension, Extension(Ext_smart))
import Text.Pandoc.Process (pipeProcess)
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Shared (inDirectory, stringify, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Writers.Shared (getField, metaToContext)
import Control.Monad.Catch (MonadMask)
#ifdef _WINDOWS
import Data.List (intercalate)
#endif
import Data.List (isPrefixOf, find)
import Text.Pandoc.MediaBag (mediaItems)
import Text.Pandoc.Class (fillMediaBag, getMediaBag, getVerbosity, setVerbosity,
                          readFileStrict, fileExists,
                          report, extractMedia, PandocMonad, runIOorExplode)
import Text.Pandoc.Logging
import Text.DocTemplates ( FromContext(lookupContext) )

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators =
  -- We filter out backslashes because an initial `C:\` gets
  -- retained by `splitDirectories`, see #6173:
  intercalate "/" . map (filter (/='\\')) . splitDirectories
#endif

makePDF :: (PandocMonad m, MonadIO m, MonadMask m)
        => String              -- ^ pdf creator (pdflatex, lualatex, xelatex,
                               -- wkhtmltopdf, weasyprint, prince, context,
                               -- pdfroff, pagedjs,
                               -- or path to executable)
        -> [String]            -- ^ arguments to pass to pdf creator
        -> (WriterOptions -> Pandoc -> m Text)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> m (Either ByteString ByteString)
makePDF program pdfargs writer opts doc = withTempDir (program == "typst") "media" $ \mediaDir -> do
#ifdef _WINDOWS
  -- note:  we want / even on Windows, for TexLive
  let tmpdir = changePathSeparators mediaDir
#else
  let tmpdir = mediaDir
#endif
  let isTeXFormat "context" = True
      isTeXFormat "tectonic" = True
      isTeXFormat "latexmk" = True
      isTeXFormat "lualatex" = True
      isTeXFormat "lualatex-dev" = True
      isTeXFormat "pdflatex" = True
      isTeXFormat "pdflatex-dev" = True
      isTeXFormat "xelatex" = True
      isTeXFormat _ = False
  let opts' = if isTeXFormat program
                 then -- disable quote ligatures to avoid bad ligatures like ?`
                      opts{ writerExtensions = disableExtension Ext_smart
                                (writerExtensions opts) }
                 else opts
  source <- fillMediaBag doc
              >>= extractMedia tmpdir
              >>= (if isTeXFormat program
                      then walkM (convertImages opts' tmpdir)
                      else return)
              >>= writer opts'

  verbosity <- getVerbosity
  let compileHTML mkOutArgs = do
        -- check to see if there is anything in mediabag, and if so,
        -- make the HTML self contained
        mediabag <- getMediaBag
        source' <- case mediaItems mediabag of
                      [] -> pure source
                      _ -> makeSelfContained source
        liftIO $
          toPdfViaTempFile verbosity program pdfargs mkOutArgs ".html" source'
  case takeBaseName program of
    "wkhtmltopdf" -> makeWithWkhtmltopdf program pdfargs writer opts' doc
    "pagedjs-cli" -> compileHTML (\f -> ["-o", f])
    "prince"      -> compileHTML (\f -> ["-o", f])
    "weasyprint"  -> compileHTML (:[])
    "typst" -> liftIO $
        toPdfViaTempFile verbosity program ("compile":pdfargs) (:[]) ".typ" source
    "pdfroff" -> do
      let paperargs =
            case lookupContext "papersize" (writerVariables opts') of
              Just s
                | T.takeEnd 1 s == "l" -> ["-P-p" <>
                                           T.unpack (T.dropEnd 1 s), "-P-l"]
                | otherwise -> ["-P-p" <> T.unpack s]
              Nothing -> []
      let args   = ["-ms", "-mpdfmark", "-mspdf",
                    "-e", "-t", "-k", "-KUTF-8", "-i"] ++
                   ["-U" | ".PDFPIC" `T.isInfixOf` source] ++
                    paperargs ++ pdfargs
      generic2pdf program args source
    "groff" -> do
      let paperargs =
            case lookupContext "papersize" (writerVariables opts') of
              Just s
                | T.takeEnd 1 s == "l" -> ["-P-p" <>
                                           T.unpack (T.dropEnd 1 s), "-P-l"]
                | otherwise -> ["-P-p" <> T.unpack s]
              Nothing -> []
      let args   = ["-ms", "-Tpdf",
                    "-e", "-t", "-k", "-KUTF-8"] ++
                   ["-U" | ".PDFPIC" `T.isInfixOf` source] ++
                    paperargs ++ pdfargs
      generic2pdf program args source
    "context"      -> context2pdf program pdfargs tmpdir source
    "tectonic"     -> tectonic2pdf program pdfargs tmpdir source
    "latexmk"      -> tex2pdf program pdfargs tmpdir source
    "lualatex"     -> tex2pdf program pdfargs tmpdir source
    "lualatex-dev" -> tex2pdf program pdfargs tmpdir source
    "pdflatex"     -> tex2pdf program pdfargs tmpdir source
    "pdflatex-dev" -> tex2pdf program pdfargs tmpdir source
    "xelatex"      -> tex2pdf program pdfargs tmpdir source
    _ -> return $ Left $ UTF8.fromStringLazy $ "Unknown program " ++ program

-- latex has trouble with tildes in paths, which
-- you find in Windows temp dir paths with longer
-- user names (see #777)
withTempDir :: (PandocMonad m, MonadMask m, MonadIO m)
            => Bool -> FilePath -> (FilePath -> m a) -> m a
withTempDir useWorkingDirectory templ action = do
  tmp <- liftIO getTemporaryDirectory
  uname <- liftIO $ E.catch
    (do (ec, sout, _) <- readProcessWithExitCode "uname" ["-o"] ""
        if ec == ExitSuccess
           then return $ Just $ filter (not . isSpace) sout
           else return Nothing)
    (\(_  :: E.SomeException) -> return Nothing)
  if useWorkingDirectory || '~' `elem` tmp || uname == Just "Cygwin" -- see #5451
     then withTempDirectory "." templ action
     else withSystemTempDirectory templ action

makeWithWkhtmltopdf :: (PandocMonad m, MonadIO m)
                    => String              -- ^ wkhtmltopdf or path
                    -> [String]            -- ^ arguments
                    -> (WriterOptions -> Pandoc -> m Text)  -- ^ writer
                    -> WriterOptions       -- ^ options
                    -> Pandoc              -- ^ document
                    -> m (Either ByteString ByteString)
makeWithWkhtmltopdf program pdfargs writer opts doc@(Pandoc meta _) = do
  let mathArgs = case writerHTMLMathMethod opts of
                 -- with MathJax, wait til all math is rendered:
                      MathJax _ -> ["--run-script", "MathJax.Hub.Register.StartupHook('End Typeset', function() { window.status = 'mathjax_loaded' });",
                                    "--window-status", "mathjax_loaded"]
                      _ -> []
  meta' <- metaToContext opts
             (return . literal . stringify)
             (return . literal . stringify)
             meta
  let toArgs (f, mbd) = maybe [] (\d -> ["--" <> f, T.unpack d]) mbd
  let args   = mathArgs ++ concatMap toArgs
                 [("page-size", getField "papersize" meta')
                 ,("title", getField "title" meta')
                 ,("margin-bottom", Just $ fromMaybe "1.2in"
                            (getField "margin-bottom" meta'))
                 ,("margin-top", Just $ fromMaybe "1.25in"
                            (getField "margin-top" meta'))
                 ,("margin-right", Just $ fromMaybe "1.25in"
                            (getField "margin-right" meta'))
                 ,("margin-left", Just $ fromMaybe "1.25in"
                            (getField "margin-left" meta'))
                 ,("footer-html", getField "footer-html" meta')
                 ,("header-html", getField "header-html" meta')
                 ] ++ ("--enable-local-file-access" : pdfargs)
                 -- see #6474
  source <- writer opts doc
  verbosity <- getVerbosity
  liftIO $ toPdfViaTempFile verbosity program args (:[]) ".html" source

-- convert SVG to PDF, and pixel formats to PNG or JPEG, for TeX formats
convertImages :: (PandocMonad m, MonadIO m)
              => WriterOptions -> FilePath -> Inline -> m Inline
convertImages opts tmpdir (Image attr ils (src, tit)) = do
  img <- liftIO $ convertImage opts tmpdir $ T.unpack src
  newPath <-
    case img of
      Left e -> do
        report $ CouldNotConvertImage src e
        return src
      Right fp -> return $ T.pack fp
  return (Image attr ils (newPath, tit))
convertImages _ _ x = return x

-- Convert formats which do not work well in pdf to png
convertImage :: WriterOptions -> FilePath -> FilePath
             -> IO (Either Text FilePath)
convertImage opts tmpdir fname = do
  let dpi = show $ writerDpi opts
  case mime of
    Just "image/png" -> doNothing
    Just "image/jpeg" -> doNothing
    Just "application/pdf" -> doNothing
    -- Note: eps is converted by pdflatex using epstopdf.pl
    Just "application/eps" -> doNothing
    Just "image/svg+xml" -> E.catch (do
      (exit, _) <- pipeProcess Nothing "rsvg-convert"
                     ["-f","pdf","-a","--dpi-x",dpi,"--dpi-y",dpi,
                      "-o",pdfOut,svgIn] BL.empty
      if exit == ExitSuccess
         then return $ Right pdfOut
         else return $ Left "conversion from SVG failed")
      (\(e :: E.SomeException) -> return $ Left $
          "check that rsvg-convert is in path.\n" <>
          tshow e)
    _ -> JP.readImage fname >>= \case
               Left e    -> return $ Left $ T.pack e
               Right img ->
                 E.catch (Right pngOut <$ JP.savePngImage pngOut img) $
                     \(e :: E.SomeException) -> return (Left (tshow e))
  where
    sha = show (hashWith SHA1 (UTF8.fromString fname))
    pngOut = normalise $ tmpdir </> sha <.> "png"
    pdfOut = normalise $ tmpdir </> sha <.> "pdf"
    svgIn = normalise fname
    mime = getMimeType fname
    doNothing = return (Right fname)

tectonic2pdf :: (PandocMonad m, MonadIO m)
             => String                          -- ^ tex program
             -> [String]                        -- ^ Arguments to the latex-engine
             -> FilePath                        -- ^ temp directory for output
             -> Text                            -- ^ tex source
             -> m (Either ByteString ByteString)
tectonic2pdf program args tmpDir source = do
  (exit, log', mbPdf) <- runTectonic program args tmpDir source
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> return $ Left $ extractMsg log'
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> do
          missingCharacterWarnings log'
          return $ Right pdf

tex2pdf :: (PandocMonad m, MonadIO m)
        => String                          -- ^ tex program
        -> [String]                        -- ^ Arguments to the latex-engine
        -> FilePath                        -- ^ temp directory for output
        -> Text                            -- ^ tex source
        -> m (Either ByteString ByteString)
tex2pdf program args tmpDir source = do
  let isOutdirArg x = "-outdir=" `isPrefixOf` x ||
                      "-output-directory=" `isPrefixOf` x
  let outDir =
        case find isOutdirArg args of
          Just x  -> drop 1 $ dropWhile (/='=') x
          Nothing -> tmpDir
  let file = tmpDir ++ "/input.tex"  -- note: tmpDir has / path separators
  liftIO $ BS.writeFile file $ UTF8.fromText source
  (exit, log', mbPdf) <- runTeXProgram program args tmpDir outDir
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> do
          let logmsg = extractMsg log'
          let extramsg =
                case logmsg of
                     x | "! Package inputenc Error" `BC.isPrefixOf` x
                           && program /= "xelatex"
                       -> "\nTry running pandoc with --pdf-engine=xelatex."
                     _ -> ""
          return $ Left $ logmsg <> extramsg
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> do
          latexWarnings log'
          missingCharacterWarnings log'
          return $ Right pdf

missingCharacterWarnings :: PandocMonad m => ByteString -> m ()
missingCharacterWarnings log' = do
  let ls = BC.lines log'
  let isMissingCharacterWarning = BC.isPrefixOf "Missing character:"
  let toCodePoint c
        | isAscii c   = T.singleton c
        | otherwise   = T.pack $ c : " (U+" ++ printf "%04X" (ord c) ++ ")"
  let addCodePoint = T.concatMap toCodePoint
  let warnings = [ addCodePoint (utf8ToText (BC.drop 19 l))
                 | l <- ls
                 , isMissingCharacterWarning l
                 ]
  mapM_ (report . MissingCharacter) warnings

latexWarnings :: PandocMonad m => ByteString -> m ()
latexWarnings log' = foldM_ go Nothing (BC.lines log')
 where
   go Nothing ln
     | BC.isPrefixOf "LaTeX Warning:" ln =
       pure $ Just ln
     | otherwise = pure Nothing
   go (Just msg) ln
     | ln == "" = do -- emit report and reset accumulator
         report $ MakePDFWarning $ render (Just 60) $
            hsep $ map literal $ T.words $ utf8ToText msg
         pure Nothing
     | otherwise = pure $ Just (msg <> ln)

-- parsing output

extractMsg :: ByteString -> ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)

extractConTeXtMsg :: ByteString -> ByteString
extractConTeXtMsg log' = do
  let msg'  = take 1 $
              dropWhile (not . ("tex error" `BC.isPrefixOf`)) $ BC.lines log'
  if null msg'
     then log'
     else BC.unlines msg'

-- running tex programs

runTectonic :: (PandocMonad m, MonadIO m)
            => String -> [String] -> FilePath
              -> Text -> m (ExitCode, ByteString, Maybe ByteString)
runTectonic program args' tmpDir' source = do
    let getOutDir acc (a:b:xs) = if a `elem` ["-o", "--outdir"]
                                    then (reverse acc ++ xs, Just b)
                                    else getOutDir (b:a:acc) xs
        getOutDir acc xs = (reverse acc ++ xs, Nothing)
        (args, outDir) = getOutDir [] args'
        tmpDir = fromMaybe tmpDir' outDir
    liftIO $ createDirectoryIfMissing True tmpDir
    -- run tectonic on stdin so it reads \include commands from $PWD instead of a temp directory
    let sourceBL = BL.fromStrict $ UTF8.fromText source
    let programArgs = ["--outdir", tmpDir] ++ args ++ ["-"]
    env <- liftIO getEnvironment
    showVerboseInfo (Just tmpDir) program programArgs env (utf8ToText sourceBL)
    (exit, out) <- liftIO $ E.catch
      (pipeProcess (Just env) program programArgs sourceBL)
      (handlePDFProgramNotFound program)
    report $ MakePDFInfo "tectonic output" (utf8ToText out)
    let pdfFile = tmpDir ++ "/texput.pdf"
    (_, pdf) <- getResultingPDF Nothing pdfFile
    return (exit, out, pdf)

-- read a pdf that has been written to a temporary directory, and optionally read
-- logs
getResultingPDF :: (PandocMonad m, MonadIO m)
                => Maybe String -> String
                -> m (Maybe ByteString, Maybe ByteString)
getResultingPDF logFile pdfFile = do
    pdfExists <- fileExists pdfFile
    pdf <- if pdfExists
              -- We read PDF as a strict bytestring to make sure that the
              -- temp directory is removed on Windows.
              -- See https://github.com/jgm/pandoc/issues/1192.
              then (Just . BL.fromChunks . (:[])) `fmap`
                   (readFileStrict pdfFile)
              else return Nothing
    -- Note that some things like Missing character warnings
    -- appear in the log but not on stderr, so we prefer the log:
    log' <- case logFile of
              Just logFile' -> do
                logExists <- fileExists logFile'
                if logExists
                  then Just . BL.fromStrict <$> readFileStrict logFile'
                  else return Nothing
              Nothing -> return Nothing
    return (log', pdf)

-- Run a TeX program once in a temp directory (on input.tex) and return (exit code,
-- contents of stdout, contents of produced PDF if any).
runTeXProgram :: (PandocMonad m, MonadIO m)
              => String -> [String] -> FilePath -> FilePath
              -> m (ExitCode, ByteString, Maybe ByteString)
runTeXProgram program args tmpDir outDir = do
    let isLatexMk = takeBaseName program == "latexmk"
        programArgs | isLatexMk =
                      ["-interaction=batchmode", "-halt-on-error", "-pdf",
                       "-quiet", "-outdir=" ++ outDir] ++ args ++ [file]
                    | otherwise =
                      ["-halt-on-error", "-interaction", "nonstopmode",
                       "-output-directory", outDir] ++ args ++ [file]
    env' <- liftIO getEnvironment
    let sep = [searchPathSeparator]
    let texinputs = maybe (tmpDir ++ sep) ((tmpDir ++ sep) ++)
          $ lookup "TEXINPUTS" env'
    let env'' = ("TEXINPUTS", texinputs) :
                ("TEXMFOUTPUT", outDir) :
                  [(k,v) | (k,v) <- env'
                         , k /= "TEXINPUTS" && k /= "TEXMFOUTPUT"]
    liftIO (UTF8.readFile file) >>=
      showVerboseInfo (Just tmpDir) program programArgs env''
    go env'' programArgs (1 :: Int)
 where
   file = tmpDir ++ "/input.tex"
   outfile = outDir ++ "/input.pdf"
   go env'' programArgs runNumber = do
     let maxruns = 4 -- stop if warnings present after 4 runs
     report $ MakePDFInfo ("LaTeX run number " <> tshow runNumber) mempty
     (exit, out) <- liftIO $ E.catch
       (pipeProcess (Just env'') program programArgs BL.empty)
       (handlePDFProgramNotFound program)
     report $ MakePDFInfo "LaTeX output" (utf8ToText out)
     -- parse log to see if we need to rerun LaTeX
     let logFile = replaceExtension outfile ".log"
     logExists <- fileExists logFile
     logContents <- if logExists
                       then BL.fromStrict <$> readFileStrict logFile
                       else return mempty
     let rerunWarnings = checkForRerun logContents
     tocFileExists <- fileExists (replaceExtension outfile ".toc")
       -- if we have a TOC we always need 3 runs, see #10308
     let rerunWarnings' = rerunWarnings ++ ["TOC is present" | tocFileExists]
     if not (null rerunWarnings') && runNumber < maxruns
        then do
          report $ MakePDFInfo "Rerun needed"
                    (T.intercalate "\n" (map utf8ToText rerunWarnings'))
          go env'' programArgs (runNumber + 1)
       else do
          (log', pdf) <- getResultingPDF (Just logFile) outfile
          return (exit, fromMaybe out log', pdf)

   checkForRerun log' = filter isRerunWarning $ BC.lines log'

   isRerunWarning ln =
     let ln' = BL.toStrict ln
       in BS.isInfixOf "Warning:" ln' && BS.isInfixOf "Rerun" ln'

generic2pdf :: (PandocMonad m, MonadIO m)
            => String
            -> [String]
            -> Text
            -> m (Either ByteString ByteString)
generic2pdf program args source = do
  env' <- liftIO getEnvironment
  showVerboseInfo Nothing program args env' source
  (exit, out) <- liftIO $ E.catch
    (pipeProcess (Just env') program args
                     (BL.fromStrict $ UTF8.fromText source))
    (handlePDFProgramNotFound program)
  return $ case exit of
             ExitFailure _ -> Left out
             ExitSuccess   -> Right out

toPdfViaTempFile  ::
             Verbosity    -- ^ Verbosity level
          -> String       -- ^ Program (program name or path)
          -> [String]     -- ^ Args to program
          -> (String -> [String]) -- ^ Construct args for output file
          -> String       -- ^ extension to use for input file (e.g. '.html')
          -> Text         -- ^ Source
          -> IO (Either ByteString ByteString)
toPdfViaTempFile verbosity program args mkOutArgs extension source =
  withTempFile "." ("toPdfViaTempFile" <> extension) $ \file h1 ->
    withTempFile "." "toPdfViaTempFile.pdf" $ \pdfFile h2 -> do
      hClose h1
      hClose h2
      BS.writeFile file $ UTF8.fromText source
      let programArgs = args ++ [file] ++ mkOutArgs pdfFile
      env' <- getEnvironment
      fileContents <- UTF8.readFile file
      runIOorExplode $ do
        setVerbosity verbosity
        showVerboseInfo Nothing program programArgs env' fileContents
      (exit, out) <- E.catch
        (pipeProcess (Just env') program programArgs BL.empty)
        (handlePDFProgramNotFound program)
      runIOorExplode $ do
        setVerbosity verbosity
        report $ MakePDFInfo "pdf-engine output" (utf8ToText out)
      pdfExists <- doesFileExist pdfFile
      mbPdf <- if pdfExists
                -- We read PDF as a strict bytestring to make sure that the
                -- temp directory is removed on Windows.
                -- See https://github.com/jgm/pandoc/issues/1192.
                then Just . BL.fromChunks . (:[]) <$> BS.readFile pdfFile
                else return Nothing
      return $ case (exit, mbPdf) of
                 (ExitFailure _, _)      -> Left out
                 (ExitSuccess, Nothing)  -> Left ""
                 (ExitSuccess, Just pdf) -> Right pdf

context2pdf :: (PandocMonad m, MonadIO m)
            => String       -- ^ "context" or path to it
            -> [String]     -- ^ extra arguments
            -> FilePath     -- ^ temp directory for output
            -> Text         -- ^ ConTeXt source
            -> m (Either ByteString ByteString)
context2pdf program pdfargs tmpDir source = do
  let file = "input.tex"
  let programArgs = "--batchmode" : pdfargs ++ [file]
  env' <- liftIO getEnvironment
  verbosity <- getVerbosity
  liftIO $ BS.writeFile (tmpDir </> file) $ UTF8.fromText source
  liftIO (UTF8.readFile (tmpDir </> file)) >>=
    showVerboseInfo (Just tmpDir) program programArgs env'
  liftIO $ inDirectory tmpDir $ do
    (exit, out) <- E.catch
      (pipeProcess (Just env') program programArgs BL.empty)
      (handlePDFProgramNotFound program)
    runIOorExplode $ do
      setVerbosity verbosity
      report $ MakePDFInfo "ConTeXt run output" (utf8ToText out)
    let pdfFile = replaceExtension file ".pdf"
    pdfExists <- doesFileExist pdfFile
    mbPdf <- if pdfExists
              -- We read PDF as a strict bytestring to make sure that the
              -- temp directory is removed on Windows.
              -- See https://github.com/jgm/pandoc/issues/1192.
              then (Just . BL.fromChunks . (:[])) `fmap` BS.readFile pdfFile
              else return Nothing
    case (exit, mbPdf) of
         (ExitFailure _, _)      -> do
            let logmsg = extractConTeXtMsg out
            return $ Left logmsg
         (ExitSuccess, Nothing)  -> return $ Left ""
         (ExitSuccess, Just pdf) -> return $ Right pdf


showVerboseInfo :: PandocMonad m
                => Maybe FilePath
                -> String
                -> [String]
                -> [(String, String)]
                -> Text
                -> m ()
showVerboseInfo mbTmpDir program programArgs env source = do
  case mbTmpDir of
    Just tmpDir -> report $ MakePDFInfo "Temp dir:" (T.pack tmpDir)
    Nothing -> return ()
  report $ MakePDFInfo "Command line:"
           (T.pack program <> " " <> T.pack (unwords (map show programArgs)))
  -- we filter out irrelevant stuff to avoid leaking passwords and keys!
  let isRelevant e = (e `elem` [ "PKFONTS"
                               , "AFMFONTS"
                               , "BIBINPUTS"
                               , "BLTXMLINPUTS"
                               , "BSTINPUTS"
                               , "CLUAINPUTS"
                               , "CMAPFONTS"
                               , "CWEBINPUTS"
                               , "DVIPSHEADERS"
                               , "ENCFONTS"
                               , "FONTCIDMAPS"
                               , "FONTFEATURES"
                               , "GFFONTS"
                               , "GLYPHFONTS"
                               , "HOME"
                               , "INDEXSTYLE"
                               , "KPATHSEA_DEBUG"
                               , "KPATHSEA_WARNING"
                               , "LANG"
                               , "LIGFONTS"
                               , "LUAINPUTS"
                               , "LUA_CPATH"
                               , "LUA_PATH"
                               , "MFBASES"
                               , "MFINPUTS"
                               , "MFPOOL"
                               , "MFTINPUTS"
                               , "MISCFONTS"
                               , "MISSFONT_LOG"
                               , "MLBIBINPUTS"
                               , "MLBSTINPUTS"
                               , "MPINPUTS"
                               , "MPMEMS"
                               , "MPPOOL"
                               , "MPSUPPORT"
                               , "OCPINPUTS"
                               , "OFMFONTS"
                               , "OPENTYPEFONTS"
                               , "OPLFONTS"
                               , "OTPINPUTS"
                               , "OVFFONTS"
                               , "OVPFONTS"
                               , "PATH"
                               , "PDFTEXCONFIG"
                               , "PROGRAMFONTS"
                               , "PSHEADERS"
                               , "PWD"
                               , "RISINPUTS"
                               , "SELFAUTODIR"
                               , "SELFAUTOLOC"
                               , "SELFAUTOPARENT"
                               , "SFDFONTS"
                               , "SHELL"
                               , "SOURCE_DATE_EPOCH"
                               , "T1FONTS"
                               , "T1INPUTS"
                               , "T42FONTS"
                               , "TEXBIB"
                               , "TEXCONFIG"
                               , "TEXDOCS"
                               , "TEXFONTMAPS"
                               , "TEXFONTS"
                               , "TEXFORMATS"
                               , "TEXINDEXSTYLE"
                               , "TEXINPUTS"
                               , "TEXMFCNF"
                               , "TEXMFDBS"
                               , "TEXMFINI"
                               , "TEXMFSCRIPTS"
                               , "TEXMFVAR"
                               , "TEXPICTS"
                               , "TEXPKS"
                               , "TEXPOOL"
                               , "TEXPSHEADERS"
                               , "TEXSOURCES"
                               , "TEX_HUSH"
                               , "TFMFONTS"
                               , "TMPDIR"
                               , "TRFONTS"
                               , "TTFONTS"
                               , "USERPROFILE"
                               , "USE_TEXMFVAR"
                               , "USE_VARTEXFONTS"
                               , "VARTEXFONTS"
                               , "VFFONTS"
                               , "WEB2C"
                               , "WEBINPUTS"
                               ]) || "TEXMF" `isPrefixOf` e
  report $ MakePDFInfo "Relevant environment variables:"
             (T.intercalate "\n" $ map tshow $ filter (isRelevant . fst) env)
  report $ MakePDFInfo "Source:" source

handlePDFProgramNotFound :: String -> IE.IOError -> IO a
handlePDFProgramNotFound program e
  | IE.isDoesNotExistError e =
      E.throwIO $ PandocPDFProgramNotFoundError $ T.pack program
  | otherwise = E.throwIO e

utf8ToText :: ByteString -> Text
utf8ToText lbs =
  case decodeUtf8' lbs of
    Left _  -> T.pack $ BC.unpack lbs  -- if decoding fails, treat as latin1
    Right t -> TL.toStrict t

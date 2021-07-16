{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.PDF
   Copyright   : Copyright (C) 2012-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( makePDF ) where

import qualified Codec.Picture as JP
import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Trans (MonadIO (..))
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
import System.IO (stderr, hClose)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory,
                       withTempFile)
import qualified System.IO.Error as IE
import Text.DocLayout (literal)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocPDFProgramNotFoundError))
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.Options (HTMLMathMethod (..), WriterOptions (..))
import Text.Pandoc.Extensions (disableExtension, Extension(Ext_smart))
import Text.Pandoc.Process (pipeProcess)
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Shared (inDirectory, stringify, tshow)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Writers.Shared (getField, metaToContext)
#ifdef _WINDOWS
import Data.List (intercalate)
#endif
import Data.List (isPrefixOf, find)
import Text.Pandoc.Class.PandocIO (PandocIO, extractMedia, runIOorExplode)
import Text.Pandoc.Class.PandocMonad (fillMediaBag, getCommonState, getVerbosity,
                                      putCommonState, report, setVerbosity)
import Text.Pandoc.Logging

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators =
  -- We filter out backslashes because an initial `C:\` gets
  -- retained by `splitDirectories`, see #6173:
  intercalate "/" . map (filter (/='\\')) . splitDirectories
#endif

makePDF :: String              -- ^ pdf creator (pdflatex, lualatex, xelatex,
                               -- wkhtmltopdf, weasyprint, prince, context, pdfroff,
                               -- or path to executable)
        -> [String]            -- ^ arguments to pass to pdf creator
        -> (WriterOptions -> Pandoc -> PandocIO Text)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> PandocIO (Either ByteString ByteString)
makePDF program pdfargs writer opts doc =
  case takeBaseName program of
    "wkhtmltopdf" -> makeWithWkhtmltopdf program pdfargs writer opts doc
    prog | prog `elem` ["weasyprint", "prince"] -> do
      source <- writer opts doc
      verbosity <- getVerbosity
      liftIO $ html2pdf verbosity program pdfargs source
    "pdfroff" -> do
      source <- writer opts doc
      let args   = ["-ms", "-mpdfmark", "-mspdf",
                    "-e", "-t", "-k", "-KUTF-8", "-i"] ++ pdfargs
      verbosity <- getVerbosity
      liftIO $ generic2pdf verbosity program args source
    baseProg -> do
      commonState <- getCommonState
      verbosity <- getVerbosity
      -- latex has trouble with tildes in paths, which
      -- you find in Windows temp dir paths with longer
      -- user names (see #777)
      let withTempDir templ action = do
            tmp <- getTemporaryDirectory
            uname <- E.catch
              (do (ec, sout, _) <- readProcessWithExitCode "uname" ["-o"] ""
                  if ec == ExitSuccess
                     then return $ Just $ filter (not . isSpace) sout
                     else return Nothing)
              (\(_ :: E.SomeException) -> return Nothing)
            if '~' `elem` tmp || uname == Just "Cygwin" -- see #5451
                   then withTempDirectory "." templ action
                   else withSystemTempDirectory templ action
      (newCommonState, res) <- liftIO $ withTempDir "tex2pdf." $ \tmpdir' -> do
#ifdef _WINDOWS
        -- note:  we want / even on Windows, for TexLive
        let tmpdir = changePathSeparators tmpdir'
#else
        let tmpdir = tmpdir'
#endif
        runIOorExplode $ do
          putCommonState commonState
          doc' <- handleImages opts tmpdir doc
          source <- writer opts{ writerExtensions = -- disable use of quote
                                    -- ligatures to avoid bad ligatures like ?`
                                    disableExtension Ext_smart
                                     (writerExtensions opts) } doc'
          res <- case baseProg of
            "context" -> context2pdf verbosity program pdfargs tmpdir source
            "tectonic" -> tectonic2pdf verbosity program pdfargs tmpdir source
            prog | prog `elem` ["pdflatex", "lualatex", "xelatex", "latexmk"]
                -> tex2pdf verbosity program pdfargs tmpdir source
            _ -> return $ Left $ UTF8.fromStringLazy
                               $ "Unknown program " ++ program
          cs <- getCommonState
          return (cs, res)
      putCommonState newCommonState
      return res

makeWithWkhtmltopdf :: String              -- ^ wkhtmltopdf or path
                    -> [String]            -- ^ arguments
                    -> (WriterOptions -> Pandoc -> PandocIO Text)  -- ^ writer
                    -> WriterOptions       -- ^ options
                    -> Pandoc              -- ^ document
                    -> PandocIO (Either ByteString ByteString)
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
  liftIO $ html2pdf verbosity program args source

handleImages :: WriterOptions
             -> FilePath      -- ^ temp dir to store images
             -> Pandoc        -- ^ document
             -> PandocIO Pandoc
handleImages opts tmpdir doc =
  fillMediaBag doc >>=
    extractMedia tmpdir >>=
    walkM (convertImages opts tmpdir)

convertImages :: WriterOptions -> FilePath -> Inline -> PandocIO Inline
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
    pngOut = normalise $ replaceDirectory (replaceExtension fname ".png") tmpdir
    pdfOut = normalise $ replaceDirectory (replaceExtension fname ".pdf") tmpdir
    svgIn = normalise fname
    mime = getMimeType fname
    doNothing = return (Right fname)

tectonic2pdf :: Verbosity                       -- ^ Verbosity level
             -> String                          -- ^ tex program
             -> [String]                        -- ^ Arguments to the latex-engine
             -> FilePath                        -- ^ temp directory for output
             -> Text                            -- ^ tex source
             -> PandocIO (Either ByteString ByteString)
tectonic2pdf verbosity program args tmpDir source = do
  (exit, log', mbPdf) <- runTectonic verbosity program args tmpDir source
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> return $ Left $ extractMsg log'
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> do
          missingCharacterWarnings verbosity log'
          return $ Right pdf

tex2pdf :: Verbosity                       -- ^ Verbosity level
        -> String                          -- ^ tex program
        -> [String]                        -- ^ Arguments to the latex-engine
        -> FilePath                        -- ^ temp directory for output
        -> Text                            -- ^ tex source
        -> PandocIO (Either ByteString ByteString)
tex2pdf verbosity program args tmpDir source = do
  let numruns | takeBaseName program == "latexmk"        = 1
              | "\\tableofcontents" `T.isInfixOf` source = 3  -- to get page numbers
              | otherwise                                = 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- runTeXProgram verbosity program args numruns
                          tmpDir source
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
          missingCharacterWarnings verbosity log'
          return $ Right pdf

missingCharacterWarnings :: Verbosity -> ByteString -> PandocIO ()
missingCharacterWarnings verbosity log' = do
  let ls = BC.lines log'
  let isMissingCharacterWarning = BC.isPrefixOf "Missing character: "
  let toCodePoint c
        | isAscii c   = T.singleton c
        | otherwise   = T.pack $ c : " (U+" ++ printf "%04X" (ord c) ++ ")"
  let addCodePoint = T.concatMap toCodePoint
  let warnings = [ addCodePoint (utf8ToText (BC.drop 19 l))
                 | l <- ls
                 , isMissingCharacterWarning l
                 ]
  setVerbosity verbosity
  mapM_ (report . MissingCharacter) warnings

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

runTectonic :: Verbosity -> String -> [String] -> FilePath
              -> Text -> PandocIO (ExitCode, ByteString, Maybe ByteString)
runTectonic verbosity program args' tmpDir' source = do
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
    when (verbosity >= INFO) $ liftIO $
      showVerboseInfo (Just tmpDir) program programArgs env
         (utf8ToText sourceBL)
    (exit, out) <- liftIO $ E.catch
      (pipeProcess (Just env) program programArgs sourceBL)
      (handlePDFProgramNotFound program)
    when (verbosity >= INFO) $ liftIO $ do
      UTF8.hPutStrLn stderr "[makePDF] Running"
      BL.hPutStr stderr out
      UTF8.hPutStr stderr "\n"
    let pdfFile = tmpDir ++ "/texput.pdf"
    (_, pdf) <- getResultingPDF Nothing pdfFile
    return (exit, out, pdf)

-- read a pdf that has been written to a temporary directory, and optionally read
-- logs
getResultingPDF :: Maybe String -> String -> PandocIO (Maybe ByteString, Maybe ByteString)
getResultingPDF logFile pdfFile = do
    pdfExists <- liftIO $ doesFileExist pdfFile
    pdf <- if pdfExists
              -- We read PDF as a strict bytestring to make sure that the
              -- temp directory is removed on Windows.
              -- See https://github.com/jgm/pandoc/issues/1192.
              then (Just . BL.fromChunks . (:[])) `fmap`
                   liftIO (BS.readFile pdfFile)
              else return Nothing
    -- Note that some things like Missing character warnings
    -- appear in the log but not on stderr, so we prefer the log:
    log' <- case logFile of
              Just logFile' -> do
                logExists <- liftIO $ doesFileExist logFile'
                if logExists
                  then liftIO $ Just <$> BL.readFile logFile'
                  else return Nothing
              Nothing -> return Nothing
    return (log', pdf)

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
runTeXProgram :: Verbosity -> String -> [String] -> Int -> FilePath
              -> Text -> PandocIO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram verbosity program args numRuns tmpDir' source = do
    let isOutdirArg x = "-outdir=" `isPrefixOf` x ||
                        "-output-directory=" `isPrefixOf` x
    let tmpDir =
          case find isOutdirArg args of
            Just x  -> drop 1 $ dropWhile (/='=') x
            Nothing -> tmpDir'
    liftIO $ createDirectoryIfMissing True tmpDir
    let file = tmpDir ++ "/input.tex"  -- note: tmpDir has / path separators
    liftIO $ BS.writeFile file $ UTF8.fromText source
    let isLatexMk = takeBaseName program == "latexmk"
        programArgs | isLatexMk = ["-interaction=batchmode", "-halt-on-error", "-pdf",
                                   "-quiet", "-outdir=" ++ tmpDir] ++ args ++ [file]
                    | otherwise = ["-halt-on-error", "-interaction", "nonstopmode",
                                   "-output-directory", tmpDir] ++ args ++ [file]
    env' <- liftIO getEnvironment
    let sep = [searchPathSeparator]
    let texinputs = maybe (tmpDir ++ sep) ((tmpDir ++ sep) ++)
          $ lookup "TEXINPUTS" env'
    let env'' = ("TEXINPUTS", texinputs) :
                ("TEXMFOUTPUT", tmpDir) :
                  [(k,v) | (k,v) <- env'
                         , k /= "TEXINPUTS" && k /= "TEXMFOUTPUT"]
    when (verbosity >= INFO) $ liftIO $
        UTF8.readFile file >>=
         showVerboseInfo (Just tmpDir) program programArgs env''
    let runTeX runNumber = do
          (exit, out) <- liftIO $ E.catch
            (pipeProcess (Just env'') program programArgs BL.empty)
            (handlePDFProgramNotFound program)
          when (verbosity >= INFO) $ liftIO $ do
            UTF8.hPutStrLn stderr $ "[makePDF] Run #" <> tshow runNumber
            BL.hPutStr stderr out
            UTF8.hPutStr stderr "\n"
          if runNumber < numRuns
             then runTeX (runNumber + 1)
             else do
               let logFile = replaceExtension file ".log"
               let pdfFile = replaceExtension file ".pdf"
               (log', pdf) <- getResultingPDF (Just logFile) pdfFile
               return (exit, fromMaybe out log', pdf)
    runTeX 1

generic2pdf :: Verbosity
            -> String
            -> [String]
            -> Text
            -> IO (Either ByteString ByteString)
generic2pdf verbosity program args source = do
  env' <- getEnvironment
  when (verbosity >= INFO) $
    showVerboseInfo Nothing program args env' source
  (exit, out) <- E.catch
    (pipeProcess (Just env') program args
                     (BL.fromStrict $ UTF8.fromText source))
    (handlePDFProgramNotFound program)
  return $ case exit of
             ExitFailure _ -> Left out
             ExitSuccess   -> Right out


html2pdf  :: Verbosity    -- ^ Verbosity level
          -> String       -- ^ Program (wkhtmltopdf, weasyprint, prince, or path)
          -> [String]     -- ^ Args to program
          -> Text         -- ^ HTML5 source
          -> IO (Either ByteString ByteString)
html2pdf verbosity program args source =
  -- write HTML to temp file so we don't have to rewrite
  -- all links in `a`, `img`, `style`, `script`, etc. tags,
  -- and piping to weasyprint didn't work on Windows either.
  withTempFile "." "html2pdf.html" $ \file h1 ->
    withTempFile "." "html2pdf.pdf" $ \pdfFile h2 -> do
      hClose h1
      hClose h2
      BS.writeFile file $ UTF8.fromText source
      let pdfFileArgName = ["-o" | takeBaseName program == "prince"]
      let programArgs = args ++ [file] ++ pdfFileArgName ++ [pdfFile]
      env' <- getEnvironment
      when (verbosity >= INFO) $
        UTF8.readFile file >>=
          showVerboseInfo Nothing program programArgs env'
      (exit, out) <- E.catch
        (pipeProcess (Just env') program programArgs BL.empty)
        (handlePDFProgramNotFound program)
      when (verbosity >= INFO) $ do
        BL.hPutStr stderr out
        UTF8.hPutStr stderr "\n"
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

context2pdf :: Verbosity    -- ^ Verbosity level
            -> String       -- ^ "context" or path to it
            -> [String]     -- ^ extra arguments
            -> FilePath     -- ^ temp directory for output
            -> Text         -- ^ ConTeXt source
            -> PandocIO (Either ByteString ByteString)
context2pdf verbosity program pdfargs tmpDir source =
  liftIO $ inDirectory tmpDir $ do
    let file = "input.tex"
    BS.writeFile file $ UTF8.fromText source
    let programArgs = "--batchmode" : pdfargs ++ [file]
    env' <- getEnvironment
    when (verbosity >= INFO) $
      UTF8.readFile file >>=
        showVerboseInfo (Just tmpDir) program programArgs env'
    (exit, out) <- E.catch
      (pipeProcess (Just env') program programArgs BL.empty)
      (handlePDFProgramNotFound program)
    when (verbosity >= INFO) $ do
      BL.hPutStr stderr out
      UTF8.hPutStr stderr "\n"
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


showVerboseInfo :: Maybe FilePath
                -> String
                -> [String]
                -> [(String, String)]
                -> Text
                -> IO ()
showVerboseInfo mbTmpDir program programArgs env source = do
  case mbTmpDir of
    Just tmpDir -> do
      UTF8.hPutStrLn stderr "[makePDF] temp dir:"
      UTF8.hPutStrLn stderr (T.pack tmpDir)
    Nothing -> return ()
  UTF8.hPutStrLn stderr "[makePDF] Command line:"
  UTF8.hPutStrLn stderr $
       T.pack program <> " " <> T.pack (unwords (map show programArgs))
  UTF8.hPutStr stderr "\n"
  UTF8.hPutStrLn stderr "[makePDF] Relevant environment variables:"
  -- we filter out irrelevant stuff to avoid leaking passwords and keys!
  let isRelevant ("PATH",_) = True
      isRelevant ("TMPDIR",_) = True
      isRelevant ("PWD",_) = True
      isRelevant ("LANG",_) = True
      isRelevant ("HOME",_) = True
      isRelevant ("LUA_PATH",_) = True
      isRelevant ("LUA_CPATH",_) = True
      isRelevant ("SHELL",_) = True
      isRelevant ("TEXINPUTS",_) = True
      isRelevant ("TEXMFOUTPUT",_) = True
      isRelevant _ = False
  mapM_ (UTF8.hPutStrLn stderr . tshow) (filter isRelevant env)
  UTF8.hPutStr stderr "\n"
  UTF8.hPutStrLn stderr "[makePDF] Source:"
  UTF8.hPutStrLn stderr source

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

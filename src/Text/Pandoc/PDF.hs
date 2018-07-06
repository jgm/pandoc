{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2012-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.PDF
   Copyright   : Copyright (C) 2012-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( makePDF ) where

import Prelude
import qualified Codec.Picture as JP
import qualified Control.Exception as E
import Control.Monad (unless, when)
import Control.Monad.Trans (MonadIO (..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (stdout)
import System.IO.Temp (withTempDirectory, withTempFile)
#if MIN_VERSION_base(4,8,3)
import System.IO.Error (IOError, isDoesNotExistError)
#else
import System.IO.Error (isDoesNotExistError)
#endif
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocPDFProgramNotFoundError))
import Text.Pandoc.MIME (getMimeType)
import Text.Pandoc.Options (HTMLMathMethod (..), WriterOptions (..))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Shared (inDirectory, stringify)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Writers.Shared (getField, metaToJSON)
#ifdef _WINDOWS
import Data.List (intercalate)
#endif
import Text.Pandoc.Class (PandocIO, extractMedia, fillMediaBag, getCommonState,
                          getVerbosity, putCommonState, report, runIO,
                          runIOorExplode, setVerbosity)
import Text.Pandoc.Logging

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators = intercalate "/" . splitDirectories
#endif

makePDF :: String              -- ^ pdf creator (pdflatex, lualatex, xelatex,
                               -- wkhtmltopdf, weasyprint, prince, context, pdfroff,
                               -- or path to executable)
        -> [String]            -- ^ arguments to pass to pdf creator
        -> (WriterOptions -> Pandoc -> PandocIO Text)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> PandocIO (Either ByteString ByteString)
makePDF program pdfargs writer opts doc = do
  case takeBaseName program of
    "wkhtmltopdf" -> makeWithWkhtmltopdf program pdfargs writer opts doc
    prog | prog `elem` ["weasyprint", "prince"] -> do
      source <- writer opts doc
      verbosity <- getVerbosity
      liftIO $ html2pdf verbosity program pdfargs source
    "pdfroff" -> do
      source <- writer opts doc
      let args   = ["-ms", "-mpdfmark", "-e", "-t", "-k", "-KUTF-8", "-i",
                    "--no-toc-relocation"] ++ pdfargs
      verbosity <- getVerbosity
      liftIO $ ms2pdf verbosity program args source
    baseProg -> do
      -- With context and latex, we create a temp directory within
      -- the working directory, since pdflatex sometimes tries to
      -- use tools like epstopdf.pl, which are restricted if run
      -- on files outside the working directory.
      let withTemp = withTempDirectory "."
      commonState <- getCommonState
      verbosity <- getVerbosity
      liftIO $ withTemp "tex2pdf." $ \tmpdir -> do
        source <- runIOorExplode $ do
                    putCommonState commonState
                    doc' <- handleImages tmpdir doc
                    writer opts doc'
        case baseProg of
           "context" -> context2pdf verbosity program tmpdir source
           prog | prog `elem` ["pdflatex", "lualatex", "xelatex"]
               -> tex2pdf verbosity program pdfargs tmpdir source
           _ -> return $ Left $ UTF8.fromStringLazy $ "Unknown program " ++ program

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
  meta' <- metaToJSON opts (return . stringify) (return . stringify) meta
  let toArgs (f, mbd) = maybe [] (\d -> ['-':'-':f, d]) mbd
  let args   = pdfargs ++ mathArgs ++ concatMap toArgs
                 [("page-size", getField "papersize" meta')
                 ,("title", getField "title" meta')
                 ,("margin-bottom", fromMaybe (Just "1.2in")
                            (getField "margin-bottom" meta'))
                 ,("margin-top", fromMaybe (Just "1.25in")
                            (getField "margin-top" meta'))
                 ,("margin-right", fromMaybe (Just "1.25in")
                            (getField "margin-right" meta'))
                 ,("margin-left", fromMaybe (Just "1.25in")
                            (getField "margin-left" meta'))
                 ,("footer-html", fromMaybe Nothing
                            (getField "footer-html" meta'))
                 ,("header-html", fromMaybe Nothing
                            (getField "header-html" meta'))
                 ]
  source <- writer opts doc
  verbosity <- getVerbosity
  liftIO $ html2pdf verbosity program args source

handleImages :: FilePath      -- ^ temp dir to store images
             -> Pandoc        -- ^ document
             -> PandocIO Pandoc
handleImages tmpdir doc =
  fillMediaBag doc >>=
    extractMedia tmpdir >>=
    walkM (convertImages tmpdir)

convertImages :: FilePath -> Inline -> PandocIO Inline
convertImages tmpdir (Image attr ils (src, tit)) = do
  img <- liftIO $ convertImage tmpdir src
  newPath <-
    case img of
      Left e -> do
        report $ CouldNotConvertImage src e
        return src
      Right fp -> return fp
  return (Image attr ils (newPath, tit))
convertImages _ x = return x

-- Convert formats which do not work well in pdf to png
convertImage :: FilePath -> FilePath -> IO (Either String FilePath)
convertImage tmpdir fname =
  case mime of
    Just "image/png" -> doNothing
    Just "image/jpeg" -> doNothing
    Just "application/pdf" -> doNothing
    -- Note: eps is converted by pdflatex using epstopdf.pl
    Just "application/eps" -> doNothing
    Just "image/svg+xml" -> E.catch (do
      (exit, _) <- pipeProcess Nothing "rsvg-convert"
                     ["-f","pdf","-a","-o",pdfOut,fname] BL.empty
      if exit == ExitSuccess
         then return $ Right pdfOut
         else return $ Left "conversion from SVG failed")
      (\(e :: E.SomeException) -> return $ Left $
          "check that rsvg2pdf is in path.\n" ++
          show e)
    _ -> JP.readImage fname >>= \res ->
          case res of
               Left e    -> return $ Left e
               Right img ->
                 E.catch (Right pngOut <$ JP.savePngImage pngOut img) $
                     \(e :: E.SomeException) -> return (Left (show e))
  where
    pngOut = replaceDirectory (replaceExtension fname ".png") tmpdir
    pdfOut = replaceDirectory (replaceExtension fname ".pdf") tmpdir
    mime = getMimeType fname
    doNothing = return (Right fname)

tex2pdf :: Verbosity                       -- ^ Verbosity level
        -> String                          -- ^ tex program
        -> [String]                        -- ^ Arguments to the latex-engine
        -> FilePath                        -- ^ temp directory for output
        -> Text                            -- ^ tex source
        -> IO (Either ByteString ByteString)
tex2pdf verbosity program args tmpDir source = do
  let numruns = if "\\tableofcontents" `T.isInfixOf` source
                   then 3  -- to get page numbers
                   else 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- E.catch
    (runTeXProgram verbosity program args 1 numruns tmpDir source)
    (\(e :: IOError) -> if isDoesNotExistError e
                           then E.throwIO $
                                 PandocPDFProgramNotFoundError program
                           else E.throwIO e)
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

missingCharacterWarnings :: Verbosity -> ByteString -> IO ()
missingCharacterWarnings verbosity log' = do
  let ls = BC.lines log'
  let isMissingCharacterWarning = BC.isPrefixOf "Missing character: "
  let warnings = [ UTF8.toStringLazy (BC.drop 19 l)
                 | l <- ls
                 , isMissingCharacterWarning l
                 ]
  runIO $ do
    setVerbosity verbosity
    mapM_ (report . MissingCharacter) warnings
  return ()

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

-- Run a TeX program on an input bytestring and return (exit code,
-- contents of stdout, contents of produced PDF if any).  Rerun
-- a fixed number of times to resolve references.
runTeXProgram :: Verbosity -> String -> [String] -> Int -> Int -> FilePath
              -> Text -> IO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram verbosity program args runNumber numRuns tmpDir source = do
    let file = tmpDir </> "input.tex"
    exists <- doesFileExist file
    unless exists $ BS.writeFile file $ UTF8.fromText source
#ifdef _WINDOWS
    -- note:  we want / even on Windows, for TexLive
    let tmpDir' = changePathSeparators tmpDir
    let file' = changePathSeparators file
#else
    let tmpDir' = tmpDir
    let file' = file
#endif
    let programArgs = ["-halt-on-error", "-interaction", "nonstopmode",
         "-output-directory", tmpDir'] ++ args ++ [file']
    env' <- getEnvironment
    let sep = [searchPathSeparator]
    let texinputs = maybe (tmpDir' ++ sep) ((tmpDir' ++ sep) ++)
          $ lookup "TEXINPUTS" env'
    let env'' = ("TEXINPUTS", texinputs) :
                  [(k,v) | (k,v) <- env', k /= "TEXINPUTS"]
    when (verbosity >= INFO && runNumber == 1) $ do
      putStrLn "[makePDF] temp dir:"
      putStrLn tmpDir'
      putStrLn "[makePDF] Command line:"
      putStrLn $ program ++ " " ++ unwords (map show programArgs)
      putStr "\n"
      putStrLn "[makePDF] Environment:"
      mapM_ print env''
      putStr "\n"
      putStrLn $ "[makePDF] Contents of " ++ file' ++ ":"
      BL.readFile file' >>= BL.putStr
      putStr "\n"
    (exit, out) <- pipeProcess (Just env'') program programArgs BL.empty
    when (verbosity >= INFO) $ do
      putStrLn $ "[makePDF] Run #" ++ show runNumber
      BL.hPutStr stdout out
      putStr "\n"
    if runNumber < numRuns
       then runTeXProgram verbosity program args (runNumber + 1) numRuns tmpDir source
       else do
         let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
         pdfExists <- doesFileExist pdfFile
         pdf <- if pdfExists
                   -- We read PDF as a strict bytestring to make sure that the
                   -- temp directory is removed on Windows.
                   -- See https://github.com/jgm/pandoc/issues/1192.
                   then (Just . BL.fromChunks . (:[])) `fmap` BS.readFile pdfFile
                   else return Nothing
         -- Note that some things like Missing character warnings
         -- appear in the log but not on stderr, so we prefer the log:
         let logFile = replaceExtension file ".log"
         logExists <- doesFileExist logFile
         log' <- if logExists
                    then BL.readFile logFile
                    else return out
         return (exit, log', pdf)

ms2pdf :: Verbosity
       -> String
       -> [String]
       -> Text
       -> IO (Either ByteString ByteString)
ms2pdf verbosity program args source = do
  env' <- getEnvironment
  when (verbosity >= INFO) $ do
    putStrLn "[makePDF] Command line:"
    putStrLn $ program ++ " " ++ unwords (map show args)
    putStr "\n"
    putStrLn "[makePDF] Environment:"
    mapM_ print env'
    putStr "\n"
    putStrLn "[makePDF] Contents:\n"
    putStr $ T.unpack source
    putStr "\n"
  (exit, out) <- E.catch
    (pipeProcess (Just env') program args
                     (BL.fromStrict $ UTF8.fromText source))
    (\(e :: IOError) -> if isDoesNotExistError e
                           then E.throwIO $
                                  PandocPDFProgramNotFoundError program
                           else E.throwIO e)
  when (verbosity >= INFO) $ do
    BL.hPutStr stdout out
    putStr "\n"
  return $ case exit of
             ExitFailure _ -> Left out
             ExitSuccess   -> Right out

html2pdf  :: Verbosity    -- ^ Verbosity level
          -> String       -- ^ Program (wkhtmltopdf, weasyprint, prince, or path)
          -> [String]     -- ^ Args to program
          -> Text         -- ^ HTML5 source
          -> IO (Either ByteString ByteString)
html2pdf verbosity program args source = do
  -- write HTML to temp file so we don't have to rewrite
  -- all links in `a`, `img`, `style`, `script`, etc. tags,
  -- and piping to weasyprint didn't work on Windows either.
  file    <- withTempFile "." "html2pdf.html" $ \fp _ -> return fp
  pdfFile <- withTempFile "." "html2pdf.pdf" $ \fp _ -> return fp
  BS.writeFile file $ UTF8.fromText source
  let pdfFileArgName = ["-o" | takeBaseName program == "prince"]
  let programArgs = args ++ [file] ++ pdfFileArgName ++ [pdfFile]
  env' <- getEnvironment
  when (verbosity >= INFO) $ do
    putStrLn "[makePDF] Command line:"
    putStrLn $ program ++ " " ++ unwords (map show programArgs)
    putStr "\n"
    putStrLn "[makePDF] Environment:"
    mapM_ print env'
    putStr "\n"
    putStrLn $ "[makePDF] Contents of " ++ file ++ ":"
    BL.readFile file >>= BL.putStr
    putStr "\n"
  (exit, out) <- E.catch
    (pipeProcess (Just env') program programArgs BL.empty)
    (\(e :: IOError) -> if isDoesNotExistError e
                           then E.throwIO $
                                  PandocPDFProgramNotFoundError program
                           else E.throwIO e)
  removeFile file
  when (verbosity >= INFO) $ do
    BL.hPutStr stdout out
    putStr "\n"
  pdfExists <- doesFileExist pdfFile
  mbPdf <- if pdfExists
            -- We read PDF as a strict bytestring to make sure that the
            -- temp directory is removed on Windows.
            -- See https://github.com/jgm/pandoc/issues/1192.
            then do
              res <- (Just . BL.fromChunks . (:[])) `fmap` BS.readFile pdfFile
              removeFile pdfFile
              return res
            else return Nothing
  return $ case (exit, mbPdf) of
             (ExitFailure _, _)      -> Left out
             (ExitSuccess, Nothing)  -> Left ""
             (ExitSuccess, Just pdf) -> Right pdf

context2pdf :: Verbosity    -- ^ Verbosity level
            -> String       -- ^ "context" or path to it
            -> FilePath     -- ^ temp directory for output
            -> Text         -- ^ ConTeXt source
            -> IO (Either ByteString ByteString)
context2pdf verbosity program tmpDir source = inDirectory tmpDir $ do
  let file = "input.tex"
  BS.writeFile file $ UTF8.fromText source
#ifdef _WINDOWS
  -- note:  we want / even on Windows, for TexLive
  let tmpDir' = changePathSeparators tmpDir
#else
  let tmpDir' = tmpDir
#endif
  let programArgs = "--batchmode" : [file]
  env' <- getEnvironment
  when (verbosity >= INFO) $ do
    putStrLn "[makePDF] temp dir:"
    putStrLn tmpDir'
    putStrLn "[makePDF] Command line:"
    putStrLn $ program ++ " " ++ unwords (map show programArgs)
    putStr "\n"
    putStrLn "[makePDF] Environment:"
    mapM_ print env'
    putStr "\n"
    putStrLn $ "[makePDF] Contents of " ++ file ++ ":"
    BL.readFile file >>= BL.putStr
    putStr "\n"
  (exit, out) <- E.catch
    (pipeProcess (Just env') program programArgs BL.empty)
    (\(e :: IOError) -> if isDoesNotExistError e
                           then E.throwIO $
                                  PandocPDFProgramNotFoundError "context"
                           else E.throwIO e)
  when (verbosity >= INFO) $ do
    BL.hPutStr stdout out
    putStr "\n"
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

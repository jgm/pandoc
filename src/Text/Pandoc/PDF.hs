{-# LANGUAGE OverloadedStrings, CPP, ScopedTypeVariables #-}
{-
Copyright (C) 2012-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( makePDF ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (stderr, stdout)
import System.IO.Temp (withTempFile)
import System.Directory
import Data.Digest.Pure.SHA (showDigest, sha1)
import System.Environment
import Control.Monad (unless, when, (<=<))
import qualified Control.Exception as E
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Shared (fetchItem', warn, withTempDir, inDirectory,
                           stringify)
import Text.Pandoc.Writers.Shared (getField, metaToJSON)
import Text.Pandoc.Options (WriterOptions(..), HTMLMathMethod(..))
import Text.Pandoc.MIME (extensionFromMimeType, getMimeType)
import Text.Pandoc.Process (pipeProcess)
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Picture as JP
#ifdef _WINDOWS
import Data.List (intercalate)
#endif

#ifdef _WINDOWS
changePathSeparators :: FilePath -> FilePath
changePathSeparators = intercalate "/" . splitDirectories
#endif

makePDF :: String              -- ^ pdf creator (pdflatex, lualatex,
                               -- xelatex, context, wkhtmltopdf)
        -> (WriterOptions -> Pandoc -> String)  -- ^ writer
        -> WriterOptions       -- ^ options
        -> Pandoc              -- ^ document
        -> IO (Either ByteString ByteString)
makePDF "wkhtmltopdf" writer opts doc@(Pandoc meta _) = do
  let mathArgs = case writerHTMLMathMethod opts of
                 -- with MathJax, wait til all math is rendered:
                      MathJax _ -> ["--run-script", "MathJax.Hub.Register.StartupHook('End Typeset', function() { window.status = 'mathjax_loaded' });",
                                    "--window-status", "mathjax_loaded"]
                      _ -> []
  meta' <- metaToJSON opts (return . stringify) (return . stringify) meta
  let toArgs (f, mbd) = maybe [] (\d -> ['-':'-':f, d]) mbd
  let args   = mathArgs ++
               concatMap toArgs
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
                 ]
  let source = writer opts doc
  html2pdf (writerVerbose opts) args source
makePDF program writer opts doc = withTempDir "tex2pdf." $ \tmpdir -> do
  doc' <- handleImages opts tmpdir doc
  let source = writer opts doc'
      args   = writerLaTeXArgs opts
  case takeBaseName program of
     "context" -> context2pdf (writerVerbose opts) tmpdir source
     prog | prog `elem` ["pdflatex", "lualatex", "xelatex"]
         -> tex2pdf' (writerVerbose opts) args tmpdir program source
     _ -> return $ Left $ UTF8.fromStringLazy $ "Unknown program " ++ program

handleImages :: WriterOptions
             -> FilePath      -- ^ temp dir to store images
             -> Pandoc        -- ^ document
             -> IO Pandoc
handleImages opts tmpdir = walkM (convertImages tmpdir) <=< walkM (handleImage' opts tmpdir)

handleImage' :: WriterOptions
             -> FilePath
             -> Inline
             -> IO Inline
handleImage' opts tmpdir (Image attr ils (src,tit)) = do
    exists <- doesFileExist src
    if exists
       then return $ Image attr ils (src,tit)
       else do
         res <- fetchItem' (writerMediaBag opts) (writerSourceURL opts) src
         case res of
              Right (contents, Just mime) -> do
                let ext = fromMaybe (takeExtension src) $
                          extensionFromMimeType mime
                let basename = showDigest $ sha1 $ BL.fromChunks [contents]
                let fname = tmpdir </> basename <.> ext
                BS.writeFile fname contents
                return $ Image attr ils (fname,tit)
              _ -> do
                warn $ "Could not find image `" ++ src ++ "', skipping..."
                -- return alt text
                return $ Emph ils
handleImage' _ _ x = return x

convertImages :: FilePath -> Inline -> IO Inline
convertImages tmpdir (Image attr ils (src, tit)) = do
  img <- convertImage tmpdir src
  newPath <-
    case img of
      Left e -> src <$ warn e
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
    _ -> JP.readImage fname >>= \res ->
          case res of
               Left _    -> return $ Left $ "Unable to convert `" ++
                               fname ++ "' for use with pdflatex."
               Right img ->
                 E.catch (Right fileOut <$ JP.savePngImage fileOut img) $
                     \(e :: E.SomeException) -> return (Left (show e))
  where
    fileOut = replaceDirectory (replaceExtension fname ".png") tmpdir
    mime = getMimeType fname
    doNothing = return (Right fname)

tex2pdf' :: Bool                            -- ^ Verbose output
         -> [String]                        -- ^ Arguments to the latex-engine
         -> FilePath                        -- ^ temp directory for output
         -> String                          -- ^ tex program
         -> String                          -- ^ tex source
         -> IO (Either ByteString ByteString)
tex2pdf' verbose args tmpDir program source = do
  let numruns = if "\\tableofcontents" `isInfixOf` source
                   then 3  -- to get page numbers
                   else 2  -- 1 run won't give you PDF bookmarks
  (exit, log', mbPdf) <- runTeXProgram verbose program args 1 numruns tmpDir source
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> do
          let logmsg = extractMsg log'
          let extramsg =
                case logmsg of
                     x | "! Package inputenc Error" `BC.isPrefixOf` x
                           && program /= "xelatex"
                       -> "\nTry running pandoc with --latex-engine=xelatex."
                     _ -> ""
          return $ Left $ logmsg <> extramsg
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> return $ Right pdf

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
runTeXProgram :: Bool -> String -> [String] -> Int -> Int -> FilePath -> String
              -> IO (ExitCode, ByteString, Maybe ByteString)
runTeXProgram verbose program args runNumber numRuns tmpDir source = do
    let file = tmpDir </> "input.tex"
    exists <- doesFileExist file
    unless exists $ UTF8.writeFile file source
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
    when (verbose && runNumber == 1) $ do
      putStrLn "[makePDF] temp dir:"
      putStrLn tmpDir'
      putStrLn "[makePDF] Command line:"
      putStrLn $ program ++ " " ++ unwords (map show programArgs)
      putStr "\n"
      putStrLn "[makePDF] Environment:"
      mapM_ print env''
      putStr "\n"
      putStrLn $ "[makePDF] Contents of " ++ file' ++ ":"
      B.readFile file' >>= B.putStr
      putStr "\n"
    (exit, out, err) <- pipeProcess (Just env'') program programArgs BL.empty
    when verbose $ do
      putStrLn $ "[makePDF] Run #" ++ show runNumber
      B.hPutStr stdout out
      B.hPutStr stderr err
      putStr "\n"
    if runNumber <= numRuns
       then runTeXProgram verbose program args (runNumber + 1) numRuns tmpDir source
       else do
         let pdfFile = replaceDirectory (replaceExtension file ".pdf") tmpDir
         pdfExists <- doesFileExist pdfFile
         pdf <- if pdfExists
                   -- We read PDF as a strict bytestring to make sure that the
                   -- temp directory is removed on Windows.
                   -- See https://github.com/jgm/pandoc/issues/1192.
                   then (Just . B.fromChunks . (:[])) `fmap` BS.readFile pdfFile
                   else return Nothing
         return (exit, out <> err, pdf)

html2pdf  :: Bool         -- ^ Verbose output
          -> [String]     -- ^ Args to wkhtmltopdf
          -> String       -- ^ HTML5 source
          -> IO (Either ByteString ByteString)
html2pdf verbose args source = do
  file <- withTempFile "." "html2pdf.html" $ \fp _ -> return fp
  pdfFile <- withTempFile "." "html2pdf.pdf" $ \fp _ -> return fp
  UTF8.writeFile file source
  let programArgs = args ++ [file, pdfFile]
  env' <- getEnvironment
  when verbose $ do
    putStrLn "[makePDF] Command line:"
    putStrLn $ "wkhtmltopdf" ++ " " ++ unwords (map show programArgs)
    putStr "\n"
    putStrLn "[makePDF] Environment:"
    mapM_ print env'
    putStr "\n"
    putStrLn $ "[makePDF] Contents of " ++ file ++ ":"
    B.readFile file >>= B.putStr
    putStr "\n"
  (exit, out, err) <- pipeProcess (Just env') "wkhtmltopdf"
                              programArgs BL.empty
  removeFile file
  when verbose $ do
    B.hPutStr stdout out
    B.hPutStr stderr err
    putStr "\n"
  pdfExists <- doesFileExist pdfFile
  mbPdf <- if pdfExists
            -- We read PDF as a strict bytestring to make sure that the
            -- temp directory is removed on Windows.
            -- See https://github.com/jgm/pandoc/issues/1192.
            then do
              res <- (Just . B.fromChunks . (:[])) `fmap` BS.readFile pdfFile
              removeFile pdfFile
              return res
            else return Nothing
  let log' = out <> err
  return $ case (exit, mbPdf) of
             (ExitFailure _, _)      -> Left log'
             (ExitSuccess, Nothing)  -> Left ""
             (ExitSuccess, Just pdf) -> Right pdf

context2pdf :: Bool         -- ^ Verbose output
            -> FilePath     -- ^ temp directory for output
            -> String       -- ^ ConTeXt source
            -> IO (Either ByteString ByteString)
context2pdf verbose tmpDir source = inDirectory tmpDir $ do
  let file = "input.tex"
  UTF8.writeFile file source
#ifdef _WINDOWS
  -- note:  we want / even on Windows, for TexLive
  let tmpDir' = changePathSeparators tmpDir
#else
  let tmpDir' = tmpDir
#endif
  let programArgs = "--batchmode" : [file]
  env' <- getEnvironment
  let sep = [searchPathSeparator]
  let texinputs = maybe (".." ++ sep) ((".." ++ sep) ++)
        $ lookup "TEXINPUTS" env'
  let env'' = ("TEXINPUTS", texinputs) :
                [(k,v) | (k,v) <- env', k /= "TEXINPUTS"]
  when verbose $ do
    putStrLn "[makePDF] temp dir:"
    putStrLn tmpDir'
    putStrLn "[makePDF] Command line:"
    putStrLn $ "context" ++ " " ++ unwords (map show programArgs)
    putStr "\n"
    putStrLn "[makePDF] Environment:"
    mapM_ print env''
    putStr "\n"
    putStrLn $ "[makePDF] Contents of " ++ file ++ ":"
    B.readFile file >>= B.putStr
    putStr "\n"
  (exit, out, err) <- pipeProcess (Just env'') "context" programArgs BL.empty
  when verbose $ do
    B.hPutStr stdout out
    B.hPutStr stderr err
    putStr "\n"
  let pdfFile = replaceExtension file ".pdf"
  pdfExists <- doesFileExist pdfFile
  mbPdf <- if pdfExists
            -- We read PDF as a strict bytestring to make sure that the
            -- temp directory is removed on Windows.
            -- See https://github.com/jgm/pandoc/issues/1192.
            then (Just . B.fromChunks . (:[])) `fmap` BS.readFile pdfFile
            else return Nothing
  let log' = out <> err
  case (exit, mbPdf) of
       (ExitFailure _, _)      -> do
          let logmsg = extractConTeXtMsg log'
          return $ Left logmsg
       (ExitSuccess, Nothing)  -> return $ Left ""
       (ExitSuccess, Just pdf) -> return $ Right pdf


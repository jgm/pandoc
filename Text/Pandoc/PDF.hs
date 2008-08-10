{-# LANGUAGE CPP #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for producing a PDF file from LaTeX.
-}
module Text.Pandoc.PDF ( saveLaTeXAsPDF ) where
import Data.List ( isInfixOf )
import System.Directory
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.Process ( runProcess, waitForProcess )
import System.Exit
import System.Environment ( getEnvironment )
import Text.Pandoc.Shared ( withTempDir )
import Prelude hiding ( writeFile, readFile, putStrLn )
import System.IO ( stderr, openFile, IOMode (..) )
#ifdef _UTF8STRING
import System.IO.UTF8
#else
import Text.Pandoc.UTF8
#endif

-- | Produce an PDF file from LaTeX.
saveLaTeXAsPDF :: FilePath    -- ^ Pathname of PDF file to be produced.
                -> FilePath    -- ^ Relative directory of source file.
                -> String      -- ^ LaTeX document.
                -> IO ()
saveLaTeXAsPDF destinationPDFPath sourceDirRelative latex = do
  -- check for pdflatex and bibtex in path:
  latexPathMaybe <- findExecutable "pdflatex"
  bibtexPathMaybe <- findExecutable "bibtex"
  let latexPath = case latexPathMaybe of
                  Nothing -> error $ "The 'pdflatex' command, which is needed to build an PDF file, was not found."
                  Just x  -> x
  let bibtexPath = case bibtexPathMaybe of
                  Nothing -> error $ "The 'bibtex' command, which is needed to build an PDF file, was not found."
                  Just x  -> x
  sourceDirAbsolute <- getCurrentDirectory >>= return . (</> sourceDirRelative) >>= canonicalizePath
  withTempDir "pandoc-pdf" $ \tempDir -> do
    env <- getEnvironment
    let env' = ("TEXINPUTS", ".:" ++ sourceDirAbsolute ++ ":") : env
    let baseName = "input"
    writeFile (tempDir </> baseName <.> "tex") latex
    let runLatex = runProgram latexPath ["-interaction=nonstopmode", baseName] tempDir env'
    let runBibtex = runProgram bibtexPath [baseName] tempDir env'
    messages1 <- runLatex
    let logPath = tempDir </> baseName <.> "log"
    tocExists <- doesFileExist (tempDir </> baseName <.> "toc")
    logContents <- readFile logPath
    let undefinedRefs = "There were undefined references" `isInfixOf` logContents
    let needsBibtex = "itation" `isInfixOf` logContents
    if needsBibtex
       then runBibtex >>= hPutStr stderr . unlines
       else return ()
    if tocExists || undefinedRefs
       then do
         messages2 <- runLatex
         logContents' <- readFile logPath
         let stillUndefinedRefs = "There were undefined references" `isInfixOf` logContents'
         if stillUndefinedRefs
            then runLatex >>= hPutStr stderr . unlines
            else hPutStr stderr $ unlines messages2
       else
         hPutStr stderr $ unlines messages1
    let pdfPath = tempDir </> baseName <.> "pdf"
    pdfExists <- doesFileExist pdfPath
    if pdfExists
       then copyFile pdfPath destinationPDFPath
       else error "The PDF could not be created."

runProgram :: FilePath             -- ^ pathname of executable
           -> [String]             -- ^ arguments
           -> FilePath             -- ^ working directory
           -> [(String, String)]   -- ^ environment
           -> IO [String]
runProgram cmdPath arguments workingDir env = do
   let runOutputPath = workingDir </> "output" <.> "tmp"
   runOutput <- openFile runOutputPath WriteMode
   ph <- runProcess cmdPath arguments (Just workingDir) (Just env) Nothing (Just runOutput) (Just runOutput)
   ec <- waitForProcess ph   -- requires compilation with -threaded
   case ec of
         ExitSuccess -> return []
         _           -> do
                          output <- readFile runOutputPath
                          if (takeBaseName cmdPath) == "bibtex"
                             then return $! lines output
                             else do
                               return $!
                                   (if "`ucs.sty' not found" `isInfixOf` output
                                       then ["Please install the 'unicode' package from CTAN:",
                                             "  http://www.ctan.org/tex-archive/macros/latex/contrib/unicode/"]
                                       else []) ++
                                   (if "`ulem.sty' not found" `isInfixOf` output
                                       then ["Please install the 'ulem' package from CTAN:",
                                             "  http://www.ctan.org/tex-archive/macros/latex/contrib/misc/"]
                                       else []) ++
                                   (if "`graphicx.sty' not found" `isInfixOf` output
                                       then ["Please install the 'graphicx' package from CTAN:",
                                             "  http://www.ctan.org/tex-archive/macros/latex/required/graphics/"]
                                       else []) ++
                                   (if "`fancyhdr.sty' not found" `isInfixOf` output
                                       then ["Please install the 'fancyhdr' package from CTAN:",
                                             "  http://www.ctan.org/tex-archive/macros/latex/contrib/fancyhdr/"]
                                       else []) ++
                                   (if "`array.sty' not found" `isInfixOf` output
                                       then ["Please install the 'array' package from CTAN:",
                                             "  http://www.ctan.org/tex-archive/macros/latex/required/tools/"]
                                       else []) ++
                                   (filter isUseful $ lines output)
                                     where isUseful ln = take 1 ln == "!"  ||
                                                         take 2 ln == "l." ||
                                                         "Error"   `isInfixOf` ln ||
                                                         "error"   `isInfixOf` ln


{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX documents to PDF.
-}
module Text.Pandoc.PDF ( tex2pdf, tex2pdf' ) where

import System.IO.Temp
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Exit (ExitCode (..))
import System.FilePath
import System.Directory
import System.Process
import Control.Exception (evaluate, bracket)
import System.IO (hClose)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Text.Pandoc.UTF8 as UTF8
import Control.Monad (liftM)
import Data.List (nub, isPrefixOf)


-- | Convert latex without bibliography to pdf.
tex2pdf :: FilePath                          -- ^ program (pdflatex, lualatex, xelatex)
        -> String                            -- ^ latex source
        -> IO (Either ByteString ByteString) -- ^ either error or pdf contents
tex2pdf prog tex = tex2pdf' prog tex ""


-- | Convert latex with bibliography to pdf.
-- Currently, only Biblatex with Biber backend supported.
tex2pdf' :: FilePath                          -- ^ program (pdflatex, lualatex, xelatex)
         -> String                            -- ^ latex source
         -> String                            -- ^ bibl source
         -> IO (Either ByteString ByteString) -- ^ either error or pdf contents
tex2pdf' prog tex bib = inTempDir "tex2pdf" (tex2pdf'' prog tex bib)


tex2pdf'' :: FilePath
          -> String
          -> String
          -> IO (Either ByteString ByteString)
tex2pdf'' prog tex bib = do

  UTF8.writeFile fileTex (prepareTex tex)
  UTF8.writeFile fileBib bib

  choose Again

  where choose Again = runTex >>= checkTex
        choose Biber = runBib >>= checkBib
        choose Done  = liftM Right (BC.readFile filePdf)

        checkTex (ExitSuccess, o) = choose $ toChoice o
        checkTex (ExitFailure _, e) = return $ Left $ extractMsg e

        checkBib (ExitSuccess, _) = choose Again
        checkBib (ExitFailure _, e) = return $ Left e

        runTex = readCommand prog
                  [ "-halt-on-error"
                  , "-interaction", "nonstopmode"
                  , fileBase
                  ]

        -- TODO : implement bibtex support.
        runBib = readCommand "biber"
                  [ "--quiet"
                  , fileBase
                  ]

        fileBase = "tex2pdf"
        fileTex = fileBase <.> "tex"
        fileBib = fileBase <.> "bib"
        filePdf = fileBase <.> "pdf"

        -- Remove any \addbibresource commands in the preamble
        -- and append one for the provided bib source.
        -- TODO : implement natbib support.
        prepareTex s
          | begDoc `isPrefixOf` s = bibCmd ++ "{" ++ fileBib ++ "}\n" ++ s
          | bibCmd `isPrefixOf` s = prepareTex $ mySkip s
          | null s = []
          | otherwise = head s : prepareTex (tail s)
        begDoc = "\\begin{document}"
        bibCmd = "\\addbibresource"
        mySkip = tail . dropWhile (/= '}')


data Choice
  = Done
  | Again
  | Biber
  deriving (Eq)

-- Transform LaTeX log into a 'Choice'.
toChoice :: ByteString -> Choice
toChoice stdout
  | Biber `elem` choices = Biber
  | Again `elem` choices = Again
  | otherwise = Done
  where choices = nub $ map toCh (BC.lines stdout)
        toCh line
          | "Package biblatex Warning: Please (re)run Biber" `BC.isPrefixOf` line = Biber
          | "Package biblatex Warning: Please rerun LaTeX" `BC.isPrefixOf` line = Again
          | "Package rerunfilecheck Warning: File `" `BC.isPrefixOf` line = Again
          | otherwise = Done


extractMsg :: ByteString -> ByteString
extractMsg log' = do
  let msg'  = dropWhile (not . ("!" `BC.isPrefixOf`)) $ BC.lines log'
  let (msg'',rest) = break ("l." `BC.isPrefixOf`) msg'
  let lineno = take 1 rest
  if null msg'
     then log'
     else BC.unlines (msg'' ++ lineno)


-- utility functions

-- Run a command and return exitcode and contents of stdout (stderr is
-- ignored). Based on 'readProcessWithExitCode' from 'System.Process'.
readCommand :: FilePath                 -- ^ command to run
            -> [String]                 -- ^ arguments
            -> IO (ExitCode,ByteString) -- ^ exit, stdout
readCommand cmd args = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar
    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh
    _ <- forkIO $ evaluate (B.length out) >> putMVar outMVar ()
    hClose inh -- done with stdin
    -- wait on the output
    takeMVar outMVar
    hClose outh
    hClose errh
    -- wait on the process
    ex <- waitForProcess pid
    return (ex, out)


-- Execute 'action' inside a temporary working dir.
inTempDir :: String     -- ^ Directory name template. See 'openTempFile'.
          -> IO a       -- ^ Callback that can use the directory
          -> IO a       -- ^ Callback result
inTempDir template action = getCurrentDirectory >>= go
  where go from = bracket alloc (dealloc from) go'
        go' tmp = setCurrentDirectory tmp >> action
        alloc = getTemporaryDirectory >>= flip createTempDirectory template
        dealloc from tmp = setCurrentDirectory from >> removeDirectoryRecursive tmp

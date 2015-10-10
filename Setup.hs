{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.PackageDescription (PackageDescription(..), FlagName(..))
import Distribution.Simple.Utils ( rawSystemExitCode, findProgramVersion )
import System.Exit
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Utils (info, notice, installOrdinaryFiles)
import Distribution.Simple.Setup
import Distribution.Simple.Program (simpleProgram, Program(..))
import Distribution.Simple.LocalBuildInfo
import Data.Version
import Control.Monad (when)
import qualified Control.Exception as E

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
      -- enable hsb2hs preprocessor for .hsb files
      hookedPreProcessors = [ppBlobSuffixHandler]
    , hookedPrograms = [(simpleProgram "hsb2hs"){
                           programFindVersion = \verbosity fp ->
                             findProgramVersion "--version" id verbosity fp }]
    , postCopy = installManPage
    }

ppBlobSuffixHandler :: PPSuffixHandler
ppBlobSuffixHandler = ("hsb", \_ lbi ->
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
      do let embedData = case lookup (FlagName "embed_data_files")
                              (configConfigurationsFlags (configFlags lbi)) of
                              Just True -> True
                              _         -> False
         when embedData $
            do info verbosity $ "Preprocessing " ++ infile ++ " to " ++ outfile
               ec <- rawSystemExitCode verbosity "hsb2hs"
                          [infile, infile, outfile]
               case ec of
                    ExitSuccess   -> return ()
                    ExitFailure _ -> error "hsb2hs is needed to build this program"
  })

installManPage :: Args -> CopyFlags
               -> PackageDescription -> LocalBuildInfo -> IO ()
installManPage _ flags pkg lbi = do
  let verbosity = fromFlag (copyVerbosity flags)
  let copydest  = fromFlag (copyDest flags)
  let mandest   = mandir (absoluteInstallDirs pkg lbi copydest)
                     ++ "/man1"
  notice verbosity $ "Copying man page to " ++ mandest
  installOrdinaryFiles verbosity mandest [("man", "pandoc.1")]

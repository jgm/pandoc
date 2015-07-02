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
import Distribution.Simple.InstallDirs (mandir)
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import System.Process ( rawSystem )
import System.FilePath ( (</>) )
import System.Directory ( findExecutable )
import Distribution.Simple.Utils (info, notice, rawSystemExit, installOrdinaryFile)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
      -- enable hsb2hs preprocessor for .hsb files
      hookedPreProcessors = [ppBlobSuffixHandler]
    , postBuild = \args bf pkgdescr lbi ->
                  makeManPage args bf pkgdescr lbi

    , postCopy = \_ flags pkg lbi ->
                installManpage pkg lbi (fromFlag $ copyVerbosity flags)
                NoCopyDest
    }

ppBlobSuffixHandler :: PPSuffixHandler
ppBlobSuffixHandler = ("hsb", \_ _ ->
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
      do info verbosity $ "Preprocessing " ++ infile ++ " to " ++ outfile
         hsb2hsPath <- findExecutable "hsb2hs"
         case hsb2hsPath of
            Just p  -> rawSystem p [infile, infile, outfile]
            Nothing -> error "hsb2hs is needed to build this program: cabal install hsb2hs"
         return ()
  })

makeManPage :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo
            -> IO ()
makeManPage _ bf _ LocalBuildInfo{buildDir=buildDir}
  = do notice verbosity "Creating man/pandoc.1"
       rawSystemExit verbosity progPath args
  where verbosity = fromFlagOrDefault normal $ buildVerbosity bf
        progPath = buildDir </> "pandoc" </> "pandoc"
        args = ["README", "-t", "man", "-s",
                "--template", "man/pandoc.1.template",
                "--filter", "man/capitalizeHeaders.hs",
                "--filter", "man/removeNotes.hs",
                "--filter", "man/removeLinks.hs",
                "-o", "man/pandoc.1"]

installManpage :: PackageDescription -> LocalBuildInfo
               -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy = do
  let mandest = mandir (absoluteInstallDirs pkg lbi copy) </>
                "man1" </> "pandoc.1"
  notice verbosity $ "Copying man page to " ++ mandest
  installOrdinaryFile verbosity ("man" </> "pandoc.1") mandest

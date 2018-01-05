{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
import Distribution.Simple.Setup (CopyFlags(..), fromFlag)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Utils (notice, installOrdinaryFiles)
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
      postCopy = installManPage
    }

installManPage :: Args -> CopyFlags
               -> PackageDescription -> LocalBuildInfo -> IO ()
installManPage _ flags pkg lbi = do
  let verbosity = fromFlag (copyVerbosity flags)
  let copydest  = fromFlag (copyDest flags)
  let mandest   = mandir (absoluteInstallDirs pkg lbi copydest)
                     ++ "/man1"
  notice verbosity $ "Copying man page to " ++ mandest
  installOrdinaryFiles verbosity mandest [("man", "pandoc.1")]

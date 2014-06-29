-- Get an interactive shell with the right packages to load
-- pandoc modules.

-- To use:
-- runghc Interact.hs
-- then,
-- :l Text/Pandoc.hs
-- (or whichever package you like)

-- You must have first done a 'cabal configure' or 'cabal install'

-- Note:  Interact.hs doesn't work with Cabal >= 1.18.  I recommend
-- using cabal sandboxes and the new 'cabal repl' command if you are
-- using a recent version.

import System.Process
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.Version
import Data.List (intercalate)

main = do
  setupConfig' <- readFile "dist/setup-config"
  let setupConfig = read $ unlines $ drop 1 $ lines setupConfig'
  let (Just (ComponentLocalBuildInfo { componentPackageDeps = deps })) = libraryConfig setupConfig
  let packageSpecs = map (toPackageSpec . snd) deps
  let args = ["-optP-include", "-optP../dist/build/autogen/cabal_macros.h","-cpp","-I../dist/build/autogen","-i../dist/build/autogen"] ++ concatMap (\p -> ["-package",p]) packageSpecs
  print args
  ph <- runProcess "ghci" args (Just "src") Nothing Nothing Nothing Nothing
  waitForProcess ph

toPackageSpec pkg = pkgN ++ "-" ++ pkgV
  where (PackageName pkgN) = pkgName pkg
        pkgV = intercalate "." $ map show $ versionBranch $ pkgVersion pkg

#!/usr/bin/env runghc

-- NB: This code deliberately avoids relying on non-standard packages

import Control.Monad
import Data.List
import Data.Version
import System.Environment
import System.Exit
import System.IO

import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription (packageDescription, testedWith)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Version
import Distribution.Text

putStrLnErr :: String -> IO ()
putStrLnErr m = hPutStrLn stderr ("*ERROR* " ++ m) >> exitFailure

putStrLnWarn :: String -> IO ()
putStrLnWarn m = hPutStrLn stderr ("*WARNING* " ++ m)

putStrLnInfo :: String -> IO ()
putStrLnInfo m = hPutStrLn stderr ("*INFO* " ++ m)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (cabfn:xpkgs) -> do genTravisFromCabalFile cabfn xpkgs
        _ -> putStrLnErr (unlines $ [ "expected .cabal file as command-line argument"
                                    , "Usage: make_travis_yml.hs <cabal-file> <extra-apt-packages...>"
                                    , ""
                                    , "Example: make_travis_yml.hs someProject.cabal alex-3.1.4 liblzma-dev > .travis.yml"
                                    ])

genTravisFromCabalFile :: FilePath -> [String] -> IO ()
genTravisFromCabalFile fn xpkgs = do
    gpd <- readPackageDescription maxBound fn

    let compilers = testedWith $ packageDescription $ gpd

    let unknownComps = nub [ c | (c,_) <- compilers, c /= GHC ]
        ghcVerConstrs = [ vc | (GHC,vc) <- compilers ]
        ghcVerConstrs' = simplifyVersionRange $ foldr unionVersionRanges noVersion ghcVerConstrs

    when (null compilers) $ do
        putStrLnErr "empty or missing 'tested-with:' definition in .cabal file"

    unless (null unknownComps) $ do
        putStrLnWarn $ "ignoring unsupported compilers mentioned in tested-with: " ++ show unknownComps

    when (null ghcVerConstrs) $ do
        putStrLnErr "'tested-with:' doesn't mention any 'GHC' version"

    when (isNoVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' describes an empty version range for 'GHC'"

    when (isAnyVersion ghcVerConstrs') $ do
        putStrLnErr "'tested-with:' allows /any/ 'GHC' version"

    let testedGhcVersions = filter (`withinRange` ghcVerConstrs') knownGhcVersions

    when (null testedGhcVersions) $ do
        putStrLnErr "no known GHC version is allowed by the 'tested-with' specification"

    putStrLnInfo $ "Generating Travis-CI config for testing for GHC versions: " ++ (unwords $ map disp' $ testedGhcVersions)

    ----------------------------------------------------------------------------
    -- travis.yml generation starts here

    putStrLn "# This file has been generated -- see https://github.com/hvr/multi-ghc-travis"
    putStrLn "language: c"
    putStrLn "sudo: false"
    putStrLn ""
    putStrLn "cache:"
    putStrLn "  directories:"
    putStrLn "    - $HOME/.cabsnap"
    putStrLn "    - $HOME/.cabal/packages"
    putStrLn ""
    putStrLn "before_cache:"
    putStrLn "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/build-reports.log"
    putStrLn "  - rm -fv $HOME/.cabal/packages/hackage.haskell.org/00-index.tar"
    putStrLn ""
    putStrLn "matrix:"
    putStrLn "  include:"

    forM_ testedGhcVersions $ \gv -> do
        let cvs = disp' (lookupCabVer gv)
            gvs = disp' gv
            ghcopts = if gv >= Version [7,10,0] []
                         then ""
                         else "-Werror"

            xpkgs' = concatMap (',':) xpkgs

        putStrLn $ concat [ "    - env: CABALVER=", cvs, " GHCVER=", gvs,
                                 " GHCOPTS=", ghcopts]
        putStrLn $ concat [ "      compiler: \": #GHC ", gvs, "\"" ]
        putStrLn $ concat [ "      addons: {apt: {packages: [cabal-install-", cvs, ",ghc-", gvs, xpkgs'
                          , "], sources: [hvr-ghc]}}" ]
        return ()

    let headGhcVers = filter isHead testedGhcVersions

    unless (null headGhcVers) $ do
        putStrLn ""
        putStrLn "  allow_failures:"

    forM_ headGhcVers $ \gv -> do
        let cvs = disp' (lookupCabVer gv)
            gvs = disp' gv
        putStrLn $ concat [ "    - env: CABALVER=", cvs, " GHCVER=", gvs ]

    putStrLn ""
    putStrLn "before_install:"
    putStrLn " - unset CC"
    putStrLn " - export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$PATH"

    putStrLn ""

    putStr $ unlines
        [ "install:"
        , " - cabal --version"
        , " - echo \"$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]\""
        , " - if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ];"
        , "   then"
        , "     zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz >"
        , "          $HOME/.cabal/packages/hackage.haskell.org/00-index.tar;"
        , "   fi"
        , " - travis_retry cabal update -v"
        , " - sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config"
        , " - cabal install --only-dependencies --enable-tests --enable-benchmarks --dry -v > installplan.txt"
        , " - sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt"
        , ""
        , "# check whether current requested install-plan matches cached package-db snapshot"
        , " - if diff -u installplan.txt $HOME/.cabsnap/installplan.txt;"
        , "   then"
        , "     echo \"cabal build-cache HIT\";"
        , "     rm -rfv .ghc;"
        , "     cp -a $HOME/.cabsnap/ghc $HOME/.ghc;"
        , "     cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;"
        , "   else"
        , "     echo \"cabal build-cache MISS\";"
        , "     rm -rf $HOME/.cabsnap;"
        , "     mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;"
        , "     cabal install --only-dependencies --enable-tests --enable-benchmarks;"
        , "   fi"
        , " "
        , "# snapshot package-db on cache miss"
        , " - if [ ! -d $HOME/.cabsnap ];"
        , "   then"
        , "      echo \"snapshotting package-db to build-cache\";"
        , "      mkdir $HOME/.cabsnap;"
        , "      cp -a $HOME/.ghc $HOME/.cabsnap/ghc;"
        , "      cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;"
        , "   fi"
        , ""
        , "# Here starts the actual work to be performed for the package under test;"
        , "# any command which exits with a non-zero exit code causes the build to fail."
        , "script:"
        , " - if [ -f configure.ac ]; then autoreconf -i; fi"
        , " - cabal configure --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging"
        , " - cabal build --ghc-options=$GHCOPTS  # this builds all libraries and executables (including tests/benchmarks)"
        , " - cabal test"
        , " - cabal check"
        , "# Test that a source-distribution can be generated"
        , "# (with cabal >= 1.18 'cabal sdist' would work too):"
        , " - ./dist/setup/setup sdist"
        , ""
        , "# Check that the resulting source distribution can be built & installed."
        , "# If there are no other `.tar.gz` files in `dist`, this can be even simpler:"
        , "# `cabal install --force-reinstalls dist/*-*.tar.gz`"
        , " - SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&"
        , "   (cd dist && cabal install --force-reinstalls \"$SRC_TGZ\")"
        , ""
        , "# EOF"
        ]

    return ()
  where
    knownGhcVersions :: [Version]
    knownGhcVersions = fmap (`Version` [])
                       [ [7,0,1],  [7,0,2], [7,0,3], [7,0,4]
                       , [7,2,1],  [7,2,2]
                       , [7,4,1],  [7,4,2]
                       , [7,6,1],  [7,6,2], [7,6,3]
                       , [7,8,1],  [7,8,2], [7,8,3], [7,8,4]
                       , [7,10,1], [7,10,2]
                       , [7,11] -- HEAD
                       ]

    lookupCabVer :: Version -> Version
    lookupCabVer (Version (x:y:_) _) = maybe (error "internal error") id $ lookup (x,y) cabalVerMap
      where
        cabalVerMap = fmap (fmap (`Version` []))
                      [ ((7, 0),  [1,16])
                      , ((7, 2),  [1,16])
                      , ((7, 4),  [1,16])
                      , ((7, 6),  [1,16])
                      , ((7, 8),  [1,18])
                      , ((7,10), [1,22])
                      , ((7,11), [1,23]) -- HEAD
                      ]

    isHead (Version (_:y:_) _) = odd (y :: Int)

    disp' v | isHead v = "head"
            | otherwise = display v

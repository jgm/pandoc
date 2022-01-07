#!/bin/sh
set -e

dir=haddocks-dir

cabal v2-haddock --builddir="$dir" --haddock-for-hackage --haddock-hyperlinked-source
# Starting with cabal 2.0, `--publish` is needed for uploading to non-candidate releases
cabal upload -d $dir/*-docs.tar.gz

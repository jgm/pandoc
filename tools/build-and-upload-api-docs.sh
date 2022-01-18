#!/bin/sh
set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal v2-haddock --builddir="$dir" --haddock-for-hackage
cabal upload -d --publish $dir/*-docs.tar.gz

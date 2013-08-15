# This Makefile is for development only.  It requires cabal-dev.
# To get started, do 'make prep' and then 'make' or 'make quick'.

.PHONY: prep submodules all quick bench clean veryclean install sdist

all:
	cabal-dev configure --enable-tests --enable-benchmarks && cabal-dev build

test: all
	cabal test

prof:
	cabal-dev configure --disable-tests --enable-library-profiling --enable-executable-profiling && cabal-dev build

prep: submodules pandoc-types citeproc-hs
	(cabal-dev --version || (cabal update && cabal install cabal-dev)) && \
	cabal-dev update && \
	cabal-dev install-deps --enable-library-profiling --enable-tests --enable-benchmarks

submodules:
	git submodule update --init

quick:
	cabal-dev configure --enable-tests --disable-optimization && cabal-dev build

relocatable:
	cabal-dev configure -fembed_data_files && cabal-dev build

bench:
	cabal-dev configure --enable-benchmarks && cabal-dev build

sdist:
	dist/setup/setup sdist
	# cabal sdist won't work, see https://github.com/haskell/cabal/issues/403

clean:
	cabal-dev clean

veryclean: clean
	rm -rf pandoc-types citeproc-hs dist cabal-dev

pandoc-types:
	git clone https://github.com/jgm/pandoc-types && \
 	  cabal-dev add-source pandoc-types

citeproc-hs: pandoc-types
	darcs get --lazy http://gorgias.mine.nu/repos/citeproc-hs && \
 	cabal-dev add-source citeproc-hs

install:
	cabal-dev install --enable-tests

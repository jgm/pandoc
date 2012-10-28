# This Makefile is for development only.  It requires cabal-dev.
# To get started, do 'make prep' and then 'make' or 'make quick'.

.PHONY: prep, all, quick, bench, clean, veryclean, install

all:
	cabal-dev configure --enable-tests --enable-benchmarks && cabal-dev build

prof:
	cabal-dev configure --enable-tests --enable-library-profiling --enable-executable-profiling && cabal-dev build

prep: pandoc-types
	cabal-dev update && \
	cabal-dev install-deps --enable-library-profiling --enable-tests --enable-benchmarks

quick:
	cabal-dev configure --enable-tests --disable-optimization && cabal-dev build

bench:
	cabal-dev configure --enable-benchmarks && cabal-dev build

clean:
	cabal-dev clean

veryclean: clean
	cabal-dev clean && rm -rf pandoc-types citeproc-hs

pandoc-types:
	git clone https://github.com/jgm/pandoc-types && \
	  cabal-dev add-source pandoc-types

citeproc-hs: pandoc-types
	darcs get --lazy http://gorgias.mine.nu/repos/citeproc-hs && \
	cabal-dev add-source citeproc-hs

install:
	cabal-dev install --enable-tests --enable-benchmarks

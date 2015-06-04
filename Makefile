version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
setup=dist/setup/setup
PREFIX ?= /usr/local

quick:
	cabal configure --enable-tests --disable-optimization
	cabal build

full:
	cabal configure --enable-tests --enable-optimization -ftrypandoc -fmake-pandoc-man-pages -fembed_data_files --enable-benchmarks
	cabal build
	cabal haddock

deps:
	cabal install --only-dependencies --enable-tests -ftrypandoc -fmake-pandoc-man-pages -fembed_data_files --enable-benchmarks

prof:
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-optimization --enable-tests
	cabal build

test:
	cabal test

bench:
	cabal bench

install: full
	cabal copy
	cabal register

dist: man
	cabal sdist
	rm -rf "pandoc-${version}"
	tar xvzf dist/pandoc-${version}.tar.gz
	cd pandoc-${version}
	cabal configure ${CABALARGS} && cabal build && cabal test && cd .. && rm -rf "pandoc-${version}"

debpkg:
	./make_deb.sh

osxpkg:
	./make_osx_package.sh

%.1: %.1.template README
	${makemanpages}

%.5: %.5.template README
	${makemanpages}

clean:
	cabal clean
	-rm -rf $(BINDIST) $(BINDIST).tar.gz

.PHONY: deps quick full install man clean test bench haddock osxpkg dist bindist prof

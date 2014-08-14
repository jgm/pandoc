version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
makemanpages=$(shell find dist -type f -name make-pandoc-man-pages)
ifeq "${makemanpages}" ""
	makemanpages=@echo "You need to 'cabal configure -fmake-pandoc-man-pages && cabal build'" && exit 1
endif
setup=$(shell find dist -type f -name setup)
MANPAGES=man/man1/pandoc.1 man/man5/pandoc_markdown.5
CABALARGS=-fmake-pandoc-man-pages --enable-tests --enable-benchmarks

all: build test

deps:
	cabal install ${OPTIONS} ${CABALARGS} --only-dependencies

build:
	cabal configure ${OPTIONS} ${CABALARGS}
	cabal build

test:
	cabal test

bench:
	cabal bench

install:
	cabal install

haddock:
	cabal haddock

sdist: man haddock
	# note: cabal sdist doesn't work well with preprocessors for some cabal versions
	${setup} sdist

# sanity check on sdist
dist: sdist
	rm -rf "pandoc-${version}"
	tar xvzf dist/pandoc-${version}.tar.gz
	cd pandoc-${version}
	cabal configure ${CABALARGS} && cabal build && cabal test && cd .. && rm -rf "pandoc-${version}"

man: ${MANPAGES}

%.1: %.1.template
	${makemanpages}

%.5: %.5.template
	${makemanpages}

clean:
	-rm ${MANPAGES}

.PHONY: all man clean test build bench haddock sdist

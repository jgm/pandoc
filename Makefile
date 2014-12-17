version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
makemanpages=$(shell find dist -type f -name make-pandoc-man-pages)
ifeq "${makemanpages}" ""
	makemanpages=@echo "You need to 'cabal configure -fmake-pandoc-man-pages && cabal build'" && exit 1
endif
setup=dist/setup/setup
MANPAGES=man/man1/pandoc.1 man/man5/pandoc_markdown.5
PREFIX ?= /usr/local
BINDIST ?= pandoc-$(version)-$(shell uname -m)

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

bindist: $(MANPAGES)
	cabal install --only-dependencies -fembed_data_files
	cabal configure --prefix=/usr/local --datadir=share/data --enable-tests -fembed_data_files
	cabal build
	cabal test
	cabal copy --destdir=$(BINDIST)
	mkdir -p $(BINDIST)$(PREFIX)/share/man/man1 $(BINDIST)$(PREFIX)/share/man/man5
	for x in $(MANPAGES); do cp $$x $(BINDIST)$(PREFIX)/share/$$x; done
	mkdir -p $(BINDIST)$(PREFIX)/share/doc/pandoc
	cp COPYING $(BINDIST)$(PREFIX)/share/doc/pandoc/
	tar cvzf $(BINDIST).tar.gz $(BINDIST)$(PREFIX)/bin $(BINDIST)$(PREFIX)/share

man: $(MANPAGES)

osxpkg:
	./make_osx_package.sh

%.1: %.1.template README
	${makemanpages}

%.5: %.5.template README
	${makemanpages}

clean:
	cabal clean
	-rm $(MANPAGES)
	-rm -rf $(BINDIST) $(BINDIST).tar.gz

.PHONY: deps quick full install man clean test bench haddock osxpkg dist bindist prof

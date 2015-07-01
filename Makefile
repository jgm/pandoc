version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)

quick:
	cabal configure --enable-tests --disable-optimization
	cabal build

full:
	cabal configure --enable-tests --enable-optimization -ftrypandoc -fembed_data_files --enable-benchmarks
	cabal build
	cabal haddock

deps:
	cabal install --only-dependencies --enable-tests -ftrypandoc -fembed_data_files --enable-benchmarks

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

dist:
	cabal sdist
	rm -rf "pandoc-${version}"
	tar xvzf dist/pandoc-${version}.tar.gz
	cd pandoc-${version}
	cabal configure ${CABALARGS} && cabal build && cabal test && cd .. && rm -rf "pandoc-${version}"

debpkg:
	./make_deb.sh

osxpkg:
	./make_osx_package.sh

man/man1/pandoc.1: README man/pandoc.1.template
	@[ -n "$(pandoc)" ] || \
		(echo "Could not find pandoc in dist/" && exit 1)
	$(pandoc) $< -t man -s --template man/pandoc.1.template \
	   --filter man/capitalizeHeaders.hs \
	   --filter man/removeNotes.hs \
	   --filter man/removeLinks.hs \
	   -o $@

download_stats:
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq '[.[] | .assets | .[] | {name: .name, download_count: .download_count}]'

clean:
	cabal clean
	-rm man/man1/pandoc.1

.PHONY: deps quick full install man clean test bench haddock osxpkg dist bindist prof download_stats

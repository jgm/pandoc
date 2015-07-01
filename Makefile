version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)

quick:
	cabal --ignore-sandbox configure --enable-tests -fembed_data_files --disable-optimization
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

download_stats:
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq '[.[] | .assets | .[] | {name: .name, download_count: .download_count}]'

clean:
	cabal clean

.PHONY: deps quick full install clean test bench osxpkg dist prof download_stats

version=$(shell grep '^Version:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)

quick:
	stack install --flag 'pandoc:embed_data_files' --fast --test --test-arguments='-j4'

full:
	stack install --flag 'pandoc:embed_data_files' --test --test-arguments='-j4' --pedantic
	stack haddock

test:
	stack test --test-arguments='-j4'

bench:
	stack bench

changes_github:
	pandoc --filter extract-changes.hs changelog -t markdown_github | pbcopy

dist: man/pandoc.1
	cabal sdist
	rm -rf "pandoc-${version}"
	tar xvzf dist/pandoc-${version}.tar.gz
	cd pandoc-${version}
	stack setup && stack test && cd .. && rm -rf "pandoc-${version}"

debpkg: man/pandoc.1
	make -C deb

osxpkg: man/pandoc.1
	./osx/make_osx_package.sh

winpkg: pandoc-$(version)-windows.msi

pandoc-$(version)-windows.msi:
	wget 'https://ci.appveyor.com/api/projects/jgm/pandoc/artifacts/windows/pandoc.msi?branch=master' -O pandoc.msi && \
	osslsigncode sign -pkcs12 ~/Private/ComodoCodeSigning.exp2017.p12 -in pandoc.msi -i http://johnmacfarlane.net/ -t http://timestamp.comodoca.com/ -out $@ -askpass
	rm pandoc.msi

man/pandoc.1: MANUAL.txt man/pandoc.1.template
	pandoc $< -t man -s --template man/pandoc.1.template \
		--filter man/capitalizeHeaders.hs \
		--filter man/removeNotes.hs \
		--filter man/removeLinks.hs \
		--variable version="pandoc $(version)" \
		-o $@

download_stats:
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq -r '.[] | .assets | .[] | "\(.download_count)\t\(.name)"'

clean:
	stack clean

.PHONY: deps quick full install clean test bench changes_github osxpkg dist prof download_stats

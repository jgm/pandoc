version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)
SOURCEFILES?=$(shell git ls-tree -r master --name-only | grep "\.hs$$")
BRANCH?=master
RESOLVER?=lts-13
GHCOPTS=-fdiagnostics-color=always
WEBSITE=../../web/pandoc.org
REVISION?=1

quick:
	stack install --ghc-options='$(GHCOPTS)' --install-ghc --flag 'pandoc:embed_data_files' --fast --test --ghc-options='-j +RTS -A64m -RTS' --test-arguments='-j4 --hide-successes $(TESTARGS)'

quick-cabal:
	cabal new-configure . --ghc-options '$(GHCOPTS)' --disable-optimization --enable-tests
	cabal new-build . --disable-optimization
	cabal new-run test-pandoc --disable-optimization -- --hide-successes $(TESTARGS)

full-cabal:
	cabal new-configure . --ghc-options '$(GHCOPTS)' --flags '+embed_data_files +trypandoc' --enable-tests --enable-benchmarks
	cabal new-build . --disable-optimization
	cabal new-run test-pandoc --disable-optimization -- --hide-successes $(TESTARGS)

full:
	stack install --flag 'pandoc:embed_data_files' --flag 'pandoc:trypandoc' --bench --no-run-benchmarks --test --test-arguments='-j4 --hide-successes' --ghc-options '-Wall -Werror -fno-warn-unused-do-bind -O0 -j4 $(GHCOPTS)'

ghci:
	stack ghci --flag 'pandoc:embed_data_files'

haddock:
	stack haddock

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
test:
	stack test --flag 'pandoc:embed_data_files' --fast --test-arguments='-j4 --hide-successes $(TESTARGS)' --ghc-options '$(GHCOPTS)'

ghcid:
	ghcid -c "stack repl --flag 'pandoc:embed_data_files'"

bench:
	stack bench --benchmark-arguments='$(BENCHARGS)' --ghc-options '$(GHCOPTS)'

weigh:
	stack build --ghc-options '$(GHCOPTS)' pandoc:weigh-pandoc && stack exec weigh-pandoc

reformat:
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done

lint: hlint fix_spacing

hlint:
	for f in $(SOURCEFILES); do echo $$f; hlint --verbose --refactor --refactor-options='-s -o -' $$f; done

fix_spacing:
	# Fix trailing newlines and spaces at ends of lines
	for f in $(SOURCEFILES); do printf '%s\n' "`cat $$f`" | sed -e 's/  *$$//' > $$f.tmp; mv $$f.tmp $$f; done

changes_github:
	pandoc --filter tools/extract-changes.hs changelog.md -t gfm --wrap=none --template tools/changes_template.html | sed -e 's/\\#/#/g' | pbcopy

dist: man/pandoc.1
	cabal sdist
	rm -rf "pandoc-${version}"
	tar xvzf dist/pandoc-${version}.tar.gz
	cd pandoc-${version}
	stack setup && stack test && cd .. && rm -rf "pandoc-${version}"

checkdocs: README.md
	! grep -n -e "\t" MANUAL.txt changelog

debpkg: man/pandoc.1
	docker run -v `pwd`:/mnt \
                   -v `pwd`/linux/artifacts:/artifacts \
		   -e REVISION=$(REVISION) \
		   -w /mnt \
	           utdemir/ghc-musl:v12-libgmp-ghc8101 bash \
		   /mnt/linux/make_artifacts.sh

macospkg:
	rm -rf macos-release-candidate
	aws s3 sync s3://travis-jgm-pandoc macos-release-candidate
	make -C macos-release-candidate

man/pandoc.1: MANUAL.txt man/pandoc.1.before man/pandoc.1.after
	pandoc $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--include-before-body man/pandoc.1.before \
		--include-after-body man/pandoc.1.after \
		--metadata author="" \
		--variable footer="pandoc $(version)" \
		-o $@

README.md: README.template MANUAL.txt tools/update-readme.lua
	pandoc --lua-filter tools/update-readme.lua \
	      --reference-location=section -t gfm $< -o $@

download_stats:
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq -r '.[] | .assets | .[] | "\(.download_count)\t\(.name)"'

pandoc-templates:
	rm ../pandoc-templates/default.* ; \
	cp data/templates/* ../pandoc-templates/ ; \
	pushd ../pandoc-templates/ && \
	git add * && \
	git commit -m "Updated templates for pandoc $(version)" && \
	popd

trypandoc:
	ssh -t macfarlane 'cd src/pandoc && git pull && stack install --flag pandoc:trypandoc --flag pandoc:embed_data_files && cd trypandoc && sudo make install'

update-website:
	make -C $(WEBSITE) update
	make -C $(WEBSITE)
	make -C $(WEBSITE) upload

clean:
	stack clean

.PHONY: deps quick full haddock install clean test bench changes_github macospkg dist prof download_stats reformat lint weigh doc/lua-filters.md pandoc-templates trypandoc update-website debpkg macospkg checkdocs ghcid ghci fix_spacing hlint

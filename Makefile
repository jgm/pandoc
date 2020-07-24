version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)
SOURCEFILES?=$(shell git ls-tree -r master --name-only | grep "\.hs$$")
BRANCH?=master
RESOLVER?=lts-13
GHCOPTS=-fdiagnostics-color=always
WEBSITE=../../web/pandoc.org

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

packages: checkdocs winpkg debpkg macospkg

checkdocs: README.md
	! grep -n -e "\t" MANUAL.txt changelog

debpkg: man/pandoc.1
	make -C linux && \
	cp linux/artifacts/pandoc-$(version)-*.* .

macospkg: man/pandoc.1
	./macos/make_macos_package.sh

winpkg: pandoc-$(version)-windows-i386.msi pandoc-$(version)-windows-i386.zip pandoc-$(version)-windows-x86_64.msi pandoc-$(version)-windows-x86_64.zip

pandoc-$(version)-windows-%.zip: pandoc-$(version)-windows-%.msi
	ORIGDIR=`pwd` && \
	CONTAINER=$(basename $<) && \
	TEMPDIR=`mktemp -d` && \
	msiextract -C $$TEMPDIR/msi $< && \
	pushd $$TEMPDIR && \
	mkdir $$CONTAINER && \
	find msi -type f -exec cp {} $$CONTAINER/ \; && \
	zip -r $$ORIGDIR/$@ $$CONTAINER && \
	popd & \
	rm -rf $$TEMPDIR

pandoc-$(version)-windows-%.msi: pandoc-windows-%.msi
	osslsigncode sign -pkcs12 ~/Private/SectigoCodeSigning.exp2023.p12 -in $< -i http://johnmacfarlane.net/ -t http://timestamp.comodoca.com/ -out $@ -askpass
	rm $<

.INTERMEDIATE: pandoc-windows-i386.msi pandoc-windows-x86_64.msi

pandoc-windows-i386.msi:
	JOBID=$(shell curl https://ci.appveyor.com/api/projects/jgm/pandoc | jq '.build.jobs[]| select(.name|test("i386")) | .jobId') && \
	wget "https://ci.appveyor.com/api/buildjobs/$$JOBID/artifacts/windows%2F$@" -O $@

pandoc-windows-x86_64.msi:
	JOBID=$(shell curl https://ci.appveyor.com/api/projects/jgm/pandoc | jq '.build.jobs[]| select(.name|test("x86_64")) | .jobId') && \
	wget "https://ci.appveyor.com/api/buildjobs/$$JOBID/artifacts/windows%2F$@" -O $@

man/pandoc.1: MANUAL.txt man/pandoc.1.before man/pandoc.1.after
	pandoc $< -f markdown-smart -t man -s \
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
	cp data/templates/default.* data/templates/README.markdown data/templates/styles.* ../pandoc-templates/ ; \
	pushd ../pandoc-templates/ && \
	git add default.* README.markdown styles.* && \
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

.PHONY: deps quick full haddock install clean test bench changes_github macospkg dist prof download_stats reformat lint weigh doc/lua-filters.md packages pandoc-templates trypandoc update-website debpkg macospkg winpkg checkdocs ghcid ghci fix_spacing hlint

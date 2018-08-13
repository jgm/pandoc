version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)
SOURCEFILES?=$(shell find pandoc.hs src test -name '*.hs')
BRANCH?=master
RESOLVER=lts-12
GHCOPTS=-fdiagnostics-color=always -Wall -fno-warn-unused-do-bind -Wincomplete-record-updates -Wnoncanonical-monad-instances -Wnoncanonical-monadfail-instances -Wincomplete-uni-patterns -Werror=missing-home-modules -Widentities -Wcpp-undef -j +RTS -A32M -RTS
# Later:
# -Wpartial-fields        (currently used in Powerpoint writer)
# -Wmissing-export-lists  (currently some Odt modules violate this)
# -Wredundant-constraints (problematic if we want to support older base)
WEBSITE=../../web/pandoc.org

quick:
	stack install --resolver=$(RESOLVER) --ghc-options='$(GHCOPTS)' --install-ghc --flag 'pandoc:embed_data_files' --fast --test --test-arguments='-j4 --hide-successes $(TESTARGS)'

full:
	stack install --resolver=$(RESOLVER) --flag 'pandoc:embed_data_files' --flag 'pandoc:weigh-pandoc' --flag 'pandoc:trypandoc' --bench --no-run-benchmarks --test --test-arguments='-j4 --hide-successes' --ghc-options '-Wall -Werror -fno-warn-unused-do-bind -O0 -j4 $(GHCOPTS)'

haddock:
	stack haddock --resolver=$(RESOLVER)

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
test:
	stack test --resolver=$(RESOLVER) --flag 'pandoc:embed_data_files' --fast --test-arguments='-j4 --hide-successes $(TESTARGS)' --ghc-options '$(GHCOPTS)'

bench:
	stack bench --benchmark-arguments='$(BENCHARGS)' --resolver=$(RESOLVER) --ghc-options '$(GHCOPTS)'

weigh:
	stack build --resolver=$(RESOLVER) --ghc-options '$(GHCOPTS)' --flag 'pandoc:weigh-pandoc' && stack exec weigh-pandoc

reformat:
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done

lint:
	for f in $(SOURCEFILES); do echo $$f; hlint --verbose --refactor --refactor-options='-i -s' $$f; done

changes_github:
	pandoc --filter tools/extract-changes.hs changelog -t gfm --wrap=none | sed -e 's/\\#/#/g' | pbcopy

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
	-rm -rf wintmp && \
	msiextract -C wintmp $< && \
	cd wintmp/"Program Files" && \
	mv Pandoc pandoc-$(version) && \
	zip -r $@ pandoc-$(version) && \
	mv $@ ../../ && \
	cd ../.. && \
	rm -rf wintmp

pandoc-$(version)-windows-%.msi: pandoc-windows-%.msi
	osslsigncode sign -pkcs12 ~/Private/ComodoCodeSigning.exp2019.p12 -in $< -i http://johnmacfarlane.net/ -t http://timestamp.comodoca.com/ -out $@ -askpass
	rm $<

.INTERMEDIATE: pandoc-windows-i386.msi pandoc-windows-x86_64.msi

pandoc-windows-i386.msi:
	JOBID=$(shell curl 'https://ci.appveyor.com/api/projects/jgm/pandoc' | jq -r '.build.jobs[0].jobId') && \
	wget "https://ci.appveyor.com/api/buildjobs/$$JOBID/artifacts/windows%2F$@" -O $@

pandoc-windows-x86_64.msi:
	JOBID=$(shell curl 'https://ci.appveyor.com/api/projects/jgm/pandoc' | jq -r '.build.jobs[1].jobId') && \
	wget "https://ci.appveyor.com/api/buildjobs/$$JOBID/artifacts/windows%2F$@" -O $@

man/pandoc.1: MANUAL.txt man/pandoc.1.template
	pandoc $< -f markdown-smart -t man -s --template man/pandoc.1.template \
		--lua-filter man/manfilter.lua \
		--variable version="pandoc $(version)" \
		-o $@

doc/lua-filters.md: tools/ldoc.ltp data/pandoc.lua tools/update-lua-docs.lua
	cp $@ $@.tmp
	pandoc -t markdown --columns=64 --atx-headers  \
	       -f markdown -t markdown --standalone\
         --lua-filter tools/update-lua-docs.lua \
	       -o $@ $@.tmp
	rm $@.tmp

README.md: README.template MANUAL.txt tools/update-readme.lua
	pandoc --lua-filter tools/update-readme.lua --reference-links \
	      --reference-location=section -t gfm $< -o $@

download_stats:
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq -r '.[] | .assets | .[] | "\(.download_count)\t\(.name)"'

pandoc-templates:
	rm ../pandoc-templates/default.* ; \
	cp data/templates/default.* ../pandoc-templates/ ; \
	pushd ../pandoc-templates/ && \
	git add default.* && \
	git commit -m "Updated templates for pandoc $(version)" && \
	popd

trypandoc:
	ssh -t macfarlane 'cd src/pandoc && git pull && ~/.local/bin/stack install --flag pandoc:trypandoc --flag pandoc:embed_data_files && cd trypandoc && sudo make install'

update-website:
	make -C $(WEBSITE) update
	make -C $(WEBSITE)
	make -C $(WEBSITE) upload

clean:
	stack clean

.PHONY: deps quick full haddock install clean test bench changes_github macospkg dist prof download_stats reformat lint weigh doc/lua-filters.md packages pandoc-templates trypandoc update-website debpkg macospkg winpkg checkdocs

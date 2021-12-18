version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)
SOURCEFILES?=$(shell git ls-tree -r master --name-only | grep "\.hs$$")
BRANCH?=master
ARCH=$(shell uname -m)
DOCKERIMAGE=registry.gitlab.b-data.ch/ghc/ghc4pandoc:8.10.4
COMMIT=$(shell git rev-parse --short HEAD)
TIMESTAMP=$(shell date "+%Y%m%d_%H%M")
LATESTBENCH=$(word 1,$(shell ls -t bench_*.csv 2>/dev/null))
BASELINE?=$(LATESTBENCH)
ifeq ($(BASELINE),)
BASELINECMD=
else
BASELINECMD=--baseline $(BASELINE)
endif
GHCOPTS=-fdiagnostics-color=always -j4 +RTS -A8m -RTS
WEBSITE=../../web/pandoc.org
REVISION?=1
# For gauge:
# BENCHARGS?=--small --ci=0.90 --match=pattern $(PATTERN)
# For tasty-bench:
BENCHARGS?=--csv bench_$(TIMESTAMP).csv $(BASELINECMD) --timeout=6 +RTS -T --nonmoving-gc -RTS $(if $(PATTERN),--pattern "$(PATTERN)",)

quick:
	stack install --ghc-options='$(GHCOPTS)' --system-ghc --flag 'pandoc:embed_data_files' --fast --test --test-arguments='-j4 --hide-successes --ansi-tricks=false $(TESTARGS)'

quick-cabal:
	cabal v2-build -j8 --ghc-options '$(GHCOPTS)' --disable-optimization --enable-tests
	cabal v2-test --disable-optimization --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)"
	echo "Path to built executable:" && cabal exec -- sh -c 'command -v pandoc' | sed -e 's!x/pandoc/build!x/pandoc/noopt/build!'

full-cabal:
	cabal v2-configure . --ghc-options '$(GHCOPTS)' --flags '+embed_data_files +trypandoc' --enable-tests --enable-benchmarks
	cabal v2-build . --disable-optimization
	cabal v2-run test-pandoc --disable-optimization -- --hide-successes --ansi-tricks=false $(TESTARGS)

full:
	stack install --flag 'pandoc:embed_data_files' --flag 'pandoc:trypandoc' --bench --no-run-benchmarks --test --test-arguments='-j4 --hide-successes--ansi-tricks-false' --ghc-options '-Wall -Werror -fno-warn-unused-do-bind -O0 $(GHCOPTS)'

ghci:
	stack ghci --flag 'pandoc:embed_data_files'

haddock:
	stack haddock

check: check-cabal checkdocs
	cabal check # check cabal file
	cabal outdated # check cabal dependencies
	lsd # check that stack.yaml dependencies are up to date

check-cabal: git-files.txt sdist-files.txt
	@echo "Checking to see if all committed test/data files are in sdist."
	diff -u $^

checkdocs:
	@echo "Checking for tabs in manual."
	! grep -q -n -e "\t" MANUAL.txt changelog.md

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
test:
	stack test --flag 'pandoc:embed_data_files' --fast --test-arguments='-j4 --hide-successes --ansi-tricks=false $(TESTARGS)' --ghc-options '$(GHCOPTS)'

ghcid:
	ghcid -c "stack repl --flag 'pandoc:embed_data_files'"

ghcid-test:
	ghcid -c "stack repl --ghc-options=-XNoImplicitPrelude --flag 'pandoc:embed_data_files' --ghci-options=-fobject-code pandoc:lib pandoc:test-pandoc"

bench:
	stack bench \
	  --ghc-options '$(GHCOPTS)' \
	  --benchmark-arguments='$(BENCHARGS)' 2>&1 | \
	  tee "bench_latest.txt"

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


debpkg:
	docker run -v `pwd`:/mnt \
                   -v `pwd`/linux/artifacts:/artifacts \
		   --user $(id -u):$(id -g) \
		   -e REVISION=$(REVISION) \
		   -w /mnt \
		   --memory=0 \
		   --rm \
		   $(DOCKERIMAGE) \
		   bash \
		   /mnt/linux/make_artifacts.sh 2>&1 > docker.log

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

sdist-files.txt: .FORCE
	cabal sdist --list-only | sed 's/\.\///' | grep '^\(test\|data\)\/' | sort > $@

git-files.txt: .FORCE
	git ls-tree -r --name-only HEAD | grep '^\(test\|data\)\/' | sort > $@

.PHONY: .FORCE deps quick full haddock install clean test bench changes_github dist prof download_stats reformat lint weigh doc/lua-filters.md pandoc-templates trypandoc update-website debpkg checkdocs ghcid ghci fix_spacing hlint check check-cabal check

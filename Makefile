version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc=$(shell find dist -name pandoc -type f -exec ls -t {} \; | head -1)
SOURCEFILES?=$(shell git ls-tree -r master --name-only | grep "\.hs$$")
BRANCH?=master
ARCH=$(shell uname -m)
DOCKERIMAGE=registry.gitlab.b-data.ch/ghc/ghc4pandoc:9.2.3
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
BENCHARGS?=--csv bench_$(TIMESTAMP).csv $(BASELINECMD) --timeout=6 +RTS -T --nonmoving-gc -RTS $(if $(PATTERN),--pattern "$(PATTERN)",)

quick-cabal: ## build & test with stack, no optimizations
	cabal v2-test --ghc-options='$(GHCOPTS)' --disable-optimization --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)" && cabal build --ghc-options='$(GHCOPTS)' --disable-optimization exe:pandoc

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
quick: ## build & test with stack, no optimizations
	stack install --ghc-options='$(GHCOPTS)' --system-ghc --flag 'pandoc:embed_data_files' --fast --test --test-arguments='-j4 --hide-successes --ansi-tricks=false $(TESTARGS)'

full: ## build with stack, including benchmarks
	stack install --flag 'pandoc:embed_data_files' --bench --no-run-benchmarks --test --test-arguments='-j4 --hide-successes--ansi-tricks-false' --ghc-options '-Wall -Werror -fno-warn-unused-do-bind -O0 $(GHCOPTS)'

ghci: ## start ghci session
	stack ghci --flag 'pandoc:embed_data_files'

haddock: ## build haddocks
	stack haddock

check: check-cabal checkdocs ## prerelease checks
	cabal check # check cabal file
	cabal outdated # check cabal dependencies
	stack-lint-extra-deps # check that stack.yaml dependencies are up to date
	! grep 'git:' stack.yaml # use only released versions
	! grep 'git:' cabal.project # use only released versions

check-cabal: git-files.txt sdist-files.txt
	@echo "Checking to see if all committed test/data files are in sdist."
	diff -u $^

checkdocs:
	@echo "Checking for tabs in manual."
	! grep -q -n -e "\t" MANUAL.txt changelog.md

ghcid: ## run ghcid/stack
	ghcid -c "stack repl --flag 'pandoc:embed_data_files'"

ghcid-test: ## run ghcid/stack with tests
	ghcid -c "stack repl --ghc-options=-XNoImplicitPrelude --flag 'pandoc:embed_data_files' --ghci-options=-fobject-code pandoc:lib pandoc:test-pandoc"

bench: ## build and run benchmarks
	cabal bench --benchmark-options='$(BENCHARGS)' 2>&1 | tee "bench_$(TIMESTAMP).txt"
#	stack bench \
#	  --ghc-options '$(GHCOPTS)' \
#	  --benchmark-arguments='$(BENCHARGS)' 2>&1 | \
#	  tee "bench_latest.txt"

reformat: ## reformat with stylish-haskell
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done

lint: hlint fix_spacing ## run linters

hlint: ## run hlint
	for f in $(SOURCEFILES); do echo $$f; hlint --verbose --refactor --refactor-options='-s -o -' $$f; done

fix_spacing: ## Fix trailing newlines and spaces
	for f in $(SOURCEFILES); do printf '%s\n' "`cat $$f`" | sed -e 's/  *$$//' > $$f.tmp; mv $$f.tmp $$f; done

changes_github: ## copy this release's changes in gfm
	pandoc --lua-filter tools/extract-changes.lua changelog.md -t gfm --wrap=none --template tools/changes_template.html | sed -e 's/\\#/#/g' | pbcopy

man: man/pandoc.1 man/pandoc-server.1

.PHONY: man

debpkg: ## create linux package
	docker run -v `pwd`:/mnt \
                   -v `pwd`/linux/artifacts:/artifacts \
		   --user $(id -u):$(id -g) \
		   -e REVISION=$(REVISION) \
		   -w /mnt \
		   --memory=0 \
		   --rm \
		   $(DOCKERIMAGE) \
		   bash \
		   /mnt/linux/make_artifacts.sh

man/pandoc.1: MANUAL.txt man/pandoc.1.before man/pandoc.1.after
	pandoc $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--include-before-body man/pandoc.1.before \
		--include-after-body man/pandoc.1.after \
		--metadata author="" \
		--variable footer="pandoc $(version)" \
		-o $@

man/pandoc-server.1: doc/pandoc-server.md
	pandoc $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--variable footer="pandoc-server $(version)" \
		-o $@

README.md: README.template MANUAL.txt tools/update-readme.lua
	pandoc --lua-filter tools/update-readme.lua \
	      --reference-location=section -t gfm $< -o $@

.PHONY: doc/lua-filters.md
doc/lua-filters.md: tools/update-lua-module-docs.lua
	cabal run pandoc -- --standalone \
		--reference-links \
		--lua-filter=$< \
		--columns=66 \
		--output=$@ \
		$@

download_stats: ## print download stats from GitHub releases
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq -r '.[] | .assets | .[] | "\(.download_count)\t\(.name)"'

pandoc-templates: ## update pandoc-templates repo
	rm ../pandoc-templates/default.* ; \
	cp data/templates/* ../pandoc-templates/ ; \
	pushd ../pandoc-templates/ && \
	git add * && \
	git commit -m "Updated templates for pandoc $(version)" && \
	popd

update-website: ## update website and upload
	make -C $(WEBSITE) update
	make -C $(WEBSITE)
	make -C $(WEBSITE) upload

clean: ## clean up
	stack clean

sdist-files.txt: .FORCE
	cabal sdist --list-only | sed 's/\.\///' | grep '^\(test\|data\)\/' | sort > $@

git-files.txt: .FORCE
	git ls-tree -r --name-only HEAD | grep '^\(test\|data\)\/' | sort > $@

.PHONY: help
help: ## Display this help
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'

.PHONY: .FORCE deps quick haddock install clean test bench changes_github download_stats reformat lint weigh pandoc-templates update-website debpkg checkdocs ghcid ghci fix_spacing hlint check check-cabal check

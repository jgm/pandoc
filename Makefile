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

quick-cabal: quick-cabal-test ## tests and build executable
	cabal build \
	  --ghc-options='$(GHCOPTS)' \
	  --disable-optimization pandoc-cli
.PHONY: quick-cabal

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
quick-cabal-test:  ## unoptimized build and run tests with cabal
	cabal v2-test \
	  --ghc-options='$(GHCOPTS)' \
	  --disable-optimization \
	  --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)"
.PHONY: quick-cabal-test

quick-stack: ## unoptimized build and tests with stack
	stack install \
	  --ghc-options='$(GHCOPTS)' \
	  --system-ghc --flag 'pandoc:embed_data_files' \
	  --fast \
	  --test \
	  --test-arguments='-j4 --hide-successes --ansi-tricks=false $(TESTARGS)'
.PHONY: quick-stack

check: check-cabal checkdocs ## prerelease checks
	stack-lint-extra-deps # check that stack.yaml dependencies are up to date
	! grep 'git:' stack.yaml # use only released versions
	! grep 'git:' cabal.project # use only released versions
.PHONY: check

check-cabal: git-files.txt sdist-files.txt
	@echo "Checking to see if all committed test/data files are in sdist."
	diff -u $^
	cabal check
	cabal outdated
.PHONY: check-cabal

checkdocs:
	@echo "Checking for tabs in manual."
	! grep -q -n -e "\t" MANUAL.txt changelog.md
.PHONY: checkdocs

bench: ## build and run benchmarks
	cabal bench --benchmark-options='$(BENCHARGS)' 2>&1 | tee "bench_$(TIMESTAMP).txt"
.PHONY: bench

reformat: ## reformat with stylish-haskell
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done
.PHONY: reformat

lint: hlint fix_spacing ## run linters
.PHONY: lint

hlint: ## run hlint
	for f in $(SOURCEFILES); do echo $$f; hlint --refactor --refactor-options='-s -o -' $$f; done
.PHONY: hlint

fix_spacing: ## Fix trailing newlines and spaces
	for f in $(SOURCEFILES); do printf '%s\n' "`cat $$f`" | sed -e 's/  *$$//' > $$f.tmp; mv $$f.tmp $$f; done
.PHONY: fix_spacing

changes_github: ## copy this release's changes in gfm
	pandoc --lua-filter tools/extract-changes.lua changelog.md -t gfm --wrap=none --template tools/changes_template.html | sed -e 's/\\#/#/g' | pbcopy
.PHONY: changes_github

man: man/pandoc.1 man/pandoc-server.1 man/pandoc-lua.1
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
.PHONY: debpkg

man/pandoc.1: MANUAL.txt man/pandoc.1.before man/pandoc.1.after
	pandoc $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--include-before-body man/pandoc.1.before \
		--include-after-body man/pandoc.1.after \
		--metadata author="" \
		--variable footer="pandoc $(version)" \
		-o $@

man/pandoc-%.1: doc/pandoc-%.md
	pandoc $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--variable footer="pandoc-$* $(version)" \
		-o $@

README.md: README.template MANUAL.txt tools/update-readme.lua
	pandoc --lua-filter tools/update-readme.lua \
	      --reference-location=section -t gfm $< -o $@

doc/lua-filters.md: tools/update-lua-module-docs.lua
	cabal run pandoc -- --standalone \
		--reference-links \
		--lua-filter=$< \
		--columns=66 \
		--output=$@ \
		$@
.PHONY: doc/lua-filters.md

download_stats: ## print download stats from GitHub releases
	curl https://api.github.com/repos/jgm/pandoc/releases | \
		jq -r '.[] | .assets | .[] | "\(.download_count)\t\(.name)"'
.PHONY: download_stats

pandoc-templates: ## update pandoc-templates repo
	rm ../pandoc-templates/default.* ; \
	cp data/templates/* ../pandoc-templates/ ; \
	pushd ../pandoc-templates/ && \
	git add * && \
	git commit -m "Updated templates for pandoc $(version)" && \
	popd
.PHONY: pandoc-templates

update-website: ## update website and upload
	make -C $(WEBSITE) update
	make -C $(WEBSITE)
	make -C $(WEBSITE) upload
.PHONY: update-website

clean: ## clean up
	cabal clean
	stack clean
.PHONY: clean

sdist-files.txt: .FORCE
	cabal sdist --list-only | sed 's/\.\///' | grep '^\(test\|data\)\/' | sort > $@

git-files.txt: .FORCE
	git ls-tree -r --name-only HEAD | grep '^\(test\|data\)\/' | sort > $@

help: ## Display this help
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-30s %s\n", $$1, $$2}'
.PHONY: help


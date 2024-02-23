version?=$(shell grep '^[Vv]ersion:' pandoc.cabal | awk '{print $$2;}')
pandoc-cli-version?=$(shell grep '^[Vv]ersion:' pandoc-cli/pandoc-cli.cabal | awk '{print $$2;}')
SOURCEFILES?=$(shell git ls-tree -r main --name-only src pandoc-cli pandoc-server pandoc-lua-engine | grep "\.hs$$")
PANDOCSOURCEFILES?=$(shell git ls-tree -r main --name-only src | grep "\.hs$$")
DOCKERIMAGE=quay.io/benz0li/ghc-musl:9.6
TIMESTAMP=$(shell date "+%Y%m%d_%H%M")
LATESTBENCH=$(word 1,$(shell ls -t bench_*.csv 2>/dev/null))
BASELINE?=$(LATESTBENCH)
ROOT?=Text.Pandoc
ifeq ($(BASELINE),)
BASELINECMD=
else
BASELINECMD=--baseline $(BASELINE)
endif
GHCOPTS=-fwrite-ide-info -fdiagnostics-color=always -j +RTS -A8m -RTS
CABALOPTS?=--disable-optimization -f-export-dynamic
WEBSITE=../../web/pandoc.org
REVISION?=1
BENCHARGS?=--csv bench_$(TIMESTAMP).csv $(BASELINECMD) --timeout=6 +RTS -T --nonmoving-gc -RTS $(if $(PATTERN),--pattern "$(PATTERN)",)
pandoc=$(shell cabal list-bin $(CABALOPTS) pandoc-cli)

all: build test binpath ## build executable and run tests
.PHONY: all

build: ## build executable
	cabal build \
	  --ghc-options='$(GHCOPTS)' \
	  $(CABALOPTS) pandoc-cli
.PHONY: build

binpath: ## print path of built pandoc executable
	@cabal list-bin $(CABALOPTS) --ghc-options='$(GHCOPTS)' pandoc-cli
.PHONY: binpath

ghcid: ## run ghcid
	ghcid -c 'cabal repl pandoc'
.PHONY: ghcid

repl:  ## run cabal repl
	cabal repl $(CABALOPTS) pandoc
.PHONY: repl

linecounts: ## print line counts for each module
	@wc -l $(SOURCEFILES) | sort -n
.PHONY: linecounts

# Note:  to accept current results of golden tests,
# make test TESTARGS='--accept'
test:  ## unoptimized build and run tests with cabal
	cabal test \
	  --ghc-options='$(GHCOPTS)' \
	  $(CABALOPTS) \
	  --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)" all
.PHONY: test

quick-stack: ## unoptimized build and tests with stack
	stack install \
	  --ghc-options='$(GHCOPTS)' \
	  --system-ghc --flag 'pandoc:embed_data_files' \
	  --fast \
	  --test \
	  --test-arguments='-j4 --hide-successes --ansi-tricks=false $(TESTARGS)'
.PHONY: quick-stack

prerelease: validate-epub README.md fix_spacing check-cabal check-stack checkdocs man check-version-sync check-changelog check-manversion uncommitted_changes ## prerelease checks
.PHONY: prerelease

uncommitted_changes:
	! git diff | grep '.'
.PHONY: uncommitted_changes

authors:  ## prints unique authors since LASTRELEASE (version)
	git log --pretty=format:"%an" $(LASTRELEASE)..HEAD | sort | uniq


check-stack:
	stack-lint-extra-deps # check that stack.yaml dependencies are up to date
	! grep 'git:' stack.yaml # use only released versions
.PHONY: check-stack

check-cabal: git-files.txt sdist-files.txt
	@echo "Checking to see if all committed test/data files are in sdist."
	diff -u $^
	@for pkg in . pandoc-lua-engine pandoc-server pandoc-cli; \
	do \
	     pushd $$pkg ; \
	     cabal check ; \
	     cabal outdated ; \
	     popd ; \
	done
	! grep 'git:' cabal.project # use only released versions

.PHONY: check-cabal

check-version-sync:
	@echo "Checking for match between pandoc and pandoc-cli versions"
	[ $(version) == $(pandoc-cli-version) ]
	@echo "Checking that pandoc-cli depends on this version of pandoc"
	grep 'pandoc == $(version)' pandoc-cli/pandoc-cli.cabal
.PHONY: check-version-sync

check-changelog:
	@echo "Checking for changelog entry for this version"
	grep '## pandoc $(version) (\d\d\d\d-\d\d-\d\d)' changelog.md
.PHONY: check-changelog

check-manversion:
	@echo "Checking version number in man pages"
	grep '"pandoc $(version)"' "pandoc-cli/man/pandoc.1"
	grep '"pandoc $(version)"' "pandoc-cli/man/pandoc-server.1"
	grep '"pandoc $(version)"' "pandoc-cli/man/pandoc-lua.1"
.PHONY: check-manversion

checkdocs:
	@echo "Checking for tabs in manual."
	! grep -q -n -e "\t" \
	   MANUAL.txt changelog.md doc/pandoc-server.md doc/pandoc-lua.md
.PHONY: checkdocs

bench: ## build and run benchmarks
	cabal bench --benchmark-options='$(BENCHARGS)' 2>&1 | tee "bench_$(TIMESTAMP).txt"
.PHONY: bench

reformat: ## reformat with stylish-haskell
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done
.PHONY: reformat

lint: ## run hlint
	hlint --report=hlint.html $(SOURCEFILES) || open hlint.html
.PHONY: lint

fix_spacing: ## fix trailing newlines and spaces
	@ERRORS=0; echo "Checking for spacing errors..." && for f in $(SOURCEFILES); do printf '%s\n' "`cat $$f`" | sed -e 's/  *$$//' > $$f.tmp; diff -u $$f $$f.tmp || ERRORS=1; mv $$f.tmp $$f; done; [ $$ERRORS -eq 0 ] || echo "Spacing errors have been fixed; please commit the changes."; exit $$ERRORS
.PHONY: fix_spacing

changes_github: ## copy this release's changes in gfm
	$(pandoc) --lua-filter tools/extract-changes.lua changelog.md -t gfm --wrap=none --template tools/changes_template.html | sed -e 's/\\#/#/g' | pbcopy
.PHONY: changes_github

man: pandoc-cli/man/pandoc.1 pandoc-cli/man/pandoc-server.1 pandoc-cli/man/pandoc-lua.1 ## build man pages
.PHONY: man

latex-package-dependencies: ## print packages used by default latex template
	$(pandoc) lua tools=latex-package-dependencies.lua
.PHONY: latex-package-dependencies

coverage: ## code coverage information
	cabal test \
	  --ghc-options='-fhpc $(GHCOPTS)' \
	  $(CABALOPTS) \
	  --test-options="--hide-successes --ansi-tricks=false $(TESTARGS)"
	hpc markup --destdir=coverage test/test-pandoc.tix
	open coverage/hpc_index.html
.PHONY: coverage

weeder: ## run weeder to find dead code
	weeder
.PHONY: weeder

transitive-deps: ## print transitive dependencies
	cabal-plan topo | sort | sed -e 's/-[0-9]\..*//'
.PHONY: transitive-deps

debpkg: ## create linux package
	docker run -v `pwd`:/mnt \
                   -v `pwd`/linux/artifacts:/artifacts \
		   --user $(id -u):$(id -g) \
		   -e REVISION=$(REVISION) \
       -e GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread" \
       -e CABALOPTS="-f-export-dynamic -fembed_data_files -fserver -flua --enable-executable-static -j4" \
		   -w /mnt \
		   --memory=0 \
		   --rm \
		   $(DOCKERIMAGE) \
		   bash \
		   /mnt/linux/make_artifacts.sh
.PHONY: debpkg

pandoc-cli/man/pandoc.1: MANUAL.txt man/pandoc.1.before man/pandoc.1.after pandoc.cabal
	$(pandoc) $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--include-before-body man/pandoc.1.before \
		--include-after-body man/pandoc.1.after \
		--metadata author="" \
    --variable section="1" \
    --variable title="pandoc" \
    --variable header='Pandoc User\[cq]s Guide' \
		--variable footer="pandoc $(version)" \
		-o $@

pandoc-cli/man/%.1: doc/%.md pandoc.cabal
	$(pandoc) $< -f markdown -t man -s \
		--lua-filter man/manfilter.lua \
		--metadata author="" \
    --variable section="1" \
    --variable title="$(basename $(notdir $@))" \
    --variable header='Pandoc User\[cq]s Guide' \
		--variable footer="pandoc $(version)" \
    --include-after-body man/pandoc.1.after \
		-o $@


README.md: README.template MANUAL.txt tools/update-readme.lua
	$(pandoc) --lua-filter tools/update-readme.lua \
	      --reference-location=section -t gfm $< -o $@

doc/lua-filters.md: tools/update-lua-module-docs.lua  ## update lua-filters.md module docs
	cabal run pandoc-cli -- \
		--standalone \
		--reference-links \
		--columns=66 \
		--from=$< \
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

validate-docx-golden-tests: ## validate docx golden tests against schema
	which xmllint || ("xmllint is required" && exit 1)
	test -d ./docx-validator || \
		(git clone https://github.com/devoidfury/docx-validator && \
		cd docx-validator && patch -p1 <../wml.xsd.patch)
	sh ./tools/validate-docx.sh test/docx/golden/*.docx
.PHONY: validate-docx-golden-tests

validate-docx-golden-tests2: ## validate docx golden tests using OOXMLValidator
	which dotnet || ("dotnet is required" && exit 1)
	which json_reformat || ("json_reformat is required" && exit 1)
	test -d ./OOXML-Validator || \
		(git clone https://github.com/mikeebowen/OOXML-Validator.git \
		&& cd OOXML-Validator && dotnet build --configuration=Release)
	sh ./tools/validate-docx2.sh test/docx/golden/
.PHONY: validate-docx-golden-tests2

validate-epub: ## generate an epub and validate it with epubcheck and ace
	which epubcheck || exit 1
	which ace || exit 1
	tmp=$$(mktemp -d) && \
  for epubver in 2 3; do \
    file=$$tmp/ver$$epubver.epub ; \
	  $(pandoc) test/epub/wasteland.epub --epub-cover=test/lalune.jpg -Mtitle="The Wasteland" --resource-path test/epub -t epub$$epubver -o $$file --number-sections --toc --quiet && \
	  echo $$file && \
	  epubcheck $$file || exit 1 ; \
  done && \
	ace $$tmp/ver3.epub -o ace-report-v2 --force

modules.csv: $(PANDOCSOURCEFILES)
	@rg '^import.*Text\.Pandoc\.' --with-filename $^ \
		| rg -v 'Text\.Pandoc\.(Definition|Builder|Walk|Generic)' \
		| sort \
		| uniq \
		| sed -e 's/src\///' \
	        | sed -e 's/\//\./g' \
		| sed -e 's/\.hs:import *\(qualified *\)*\([^ ]*\).*/,\2/' \
		> $@

modules.dot: modules.csv
	@echo "digraph G {" > $@
	@echo "overlap=\"scale\"" >> $@
	@sed -e 's/\([^,]*\),\(.*\)/  "\1" -> "\2";/' $< >> $@
	@echo "}" >> $@

# To get the module dependencies of Text.Pandoc.Parsing:
# make modules.pdf ROOT=Text.Pandoc.Parsing
modules.pdf: modules.dot
	gvpr -f tools/cliptree.gvpr -a '"$(ROOT)"' $< | dot -Tpdf > $@

# make moduledeps ROOT=Text.Pandoc.Parsing
moduledeps: modules.csv  ## Print transitive dependencies of a module ROOT
	@echo "$(ROOT)"
	@lua tools/moduledeps.lua transitive $(ROOT) | sort
.PHONY: moduledeps

clean: ## clean up
	cabal clean
.PHONY: clean

.PHONY: .FORCE

sdist-files.txt: .FORCE
	cabal sdist --list-only | sed 's/\.\///' | grep '^\(test\|data\)\/' | sort > $@

git-files.txt: .FORCE
	git ls-tree -r --name-only HEAD | grep '^\(test\|data\)\/' | sort > $@

help: ## display this help
	@echo "Targets:"
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "%-16s %s\n", $$1, $$2}'
	@echo
	@echo "Environment variables with default values:"
	@printf "%-16s%s\n" "CABALOPTS" "$(CABALOPTS)"
	@printf "%-16s%s\n" "GHCOPTS" "$(GHCOPTS)"
	@printf "%-16s%s\n" "TESTARGS" "$(TESTARGS)"
	@printf "%-16s%s\n" "BASELINE" "$(BASELINE)"
	@printf "%-16s%s\n" "REVISION" "$(REVISION)"
.PHONY: help

hie.yaml: ## regenerate hie.yaml
	gen-hie > $@
.PHONY: hie.yaml

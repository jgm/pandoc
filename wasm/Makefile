SHA1_PANDOC_WASM=$(shell openssl sha1 -r pandoc.wasm | sed 's/ .*$$//')
SHA1_PANDOC_JS=$(shell openssl sha1 -r pandoc.js | sed 's/ .*$$//')
SHA1_INDEX_JS=$(shell openssl sha1 -r index.js | sed 's/ .*$$//')
EXAMPLES=$(patsubst %,site/%.zip,$(shell find examples -mindepth 1 -type d))
SHA1_EXAMPLES=$(shell cat $(EXAMPLES) | openssl sha1 -r | sed 's/ .*$$//')

site: site/examples $(EXAMPLES) site/pandoc.js site/index.js site/index.html site/pandoc.wasm

site/examples:
	mkdir -p $@

site/pandoc.js: pandoc.js
	perl -p -e "s/SHA1_PANDOC_WASM/$(SHA1_PANDOC_WASM)/g" $< > $@

site/index.js: index.js
	perl -p -e "s/SHA1_PANDOC_JS/$(SHA1_PANDOC_JS)/g; s/SHA1_EXAMPLES/$(SHA1_EXAMPLES)/g" $< > $@

site/%.html: %.html
	perl -p -e "s/SHA1_INDEX_JS/$(SHA1_INDEX_JS)/g" $< > $@

site/examples/%.zip: examples/% $(wildcard examples/%/*)
	cd "$<" && zip -r "$(CURDIR)/$@" .

site/%: %
	cp $< $@

upload: site
	rsync -av site/ website:pandoc.org/app
.PHONY: upload

serve: site
	cd site && python3 -m http.server
.PHONY: serve

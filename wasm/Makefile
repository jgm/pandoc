SHA1=$(shell openssl sha1 -r pandoc.wasm | sed 's/ .*$$//')
EXAMPLES=$(patsubst %,%.tar,$(shell find examples -mindepth 1 -type d))

site/examples:
	mkdir -p $@

site: site/examples $(patsubst %,site/%,$(EXAMPLES)) site/pandoc.js site/index.html site/pandoc.wasm

site/pandoc.js: pandoc.js
	perl -p -e "s/pandoc.wasm\?sha1=XX/pandoc.wasm?sha1=$(SHA1)/" $< > $@

site/%.html: %.html
	perl -p -e "s/pandoc.js\?sha1=XX/pandoc.js?sha1=$(SHA1)/" $< > $@

site/examples/%.tar: examples/%.tar
	cp $< $@

site/%: %
	cp $< $@

upload: site
	rsync -av site/ website:pandoc.org/wasm-demo
.PHONY: upload

examples/%.tar: examples/%
	tar -cf "$@" -C "$<" .

serve: $(EXAMPLES)
	python3 -m http.server
.PHONY: serve

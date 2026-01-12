SHA1=$(shell openssl sha1 -r pandoc.wasm | sed 's/ .*$$//')

site:
	mkdir -p site

site/pandoc.js: pandoc.js
	perl -p -e "s/pandoc.wasm\?sha1=XX/pandoc.wasm?sha1=$(SHA1)/" $< > $@

site/%.html: %.html
	perl -p -e "s/pandoc.js\?sha1=XX/pandoc.js?sha1=$(SHA1)/" $< > $@

site/%: %
	cp $< $@

upload: site site/pandoc.js site/convert.html site/index.html site/pandoc.wasm
	rsync -av site/ website:pandoc.org/wasm-demo
.PHONY: upload


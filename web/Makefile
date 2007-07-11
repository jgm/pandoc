ALL := index.html osx-notes.html README.html INSTALL.html features.html changelog.html examples.html
PANDOC_PATH ?= $(dir $(shell which pandoc))
MAKEPAGE = $(PANDOC_PATH)/pandoc -s -S -B header.html -A footer.html -H css 
PANDOC_DEPS = header.html footer.html css

all : $(ALL)

.PHONY: clean
clean:
	for file in $(ALL); do rm $$file; done; \
	rm -r example*;

examples.txt : $(PANDOC_DEPS) mkdemos.pl config.xsl S5DEMO README demos
	PATH=$(PANDOC_PATH):$$PATH ./mkdemos.pl demos $@
	perl -pi -e 's!(href="(main|(my)?header|footer|example\d+)\.(html|tex|xml|css))"!\1.html"!g' $@
	for file in $$(ls | egrep '(main|(my)?header|footer|example[0-9]+)\.(html|tex|xml|css)$$'); \
		do highlight -u utf-8 --style emacs $$file > $$file.html; \
	done

%.html : %.txt $(PANDOC_DEPS)
	$(MAKEPAGE) $< > $@

%.html : % $(PANDOC_DEPS)
	$(MAKEPAGE) $< > $@

changelog.html : changelog.txt $(PANDOC_DEPS)
	$(MAKEPAGE) -T "Pandoc changelog" $< > $@

upload : 
	sitecopy --update macfarlane 

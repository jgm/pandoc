all: index.html osx-notes.html README.html INSTALL.html features.html changelog.html examples.html

PANDOC_PATH ?= $(dir $(shell which pandoc))
MAKEPAGE = $(PANDOC_PATH)/pandoc -s -S -B header.html -A footer.html -H css 
PANDOC_DEPS = header.html footer.html css

examples.txt : $(PANDOC_DEPS) mkdemos.sh
	./mkdemos.sh . $$PANDOC_PATH > $@

%.html : %.txt $(PANDOC_DEPS)
	$(MAKEPAGE) $< > $@

%.html : % $(PANDOC_DEPS)
	$(MAKEPAGE) $< > $@

changelog.html : changelog.txt $(PANDOC_DEPS)
	$(MAKEPAGE) -T "Pandoc changelog" $< > $@

upload : 
	sitecopy --update macfarlane 

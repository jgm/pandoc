ALL := index.html README.html INSTALL.html examples.html pandoc1.html markdown2pdf1.html html2markdown1.html hsmarkdown1.html
PANDOC_PATH ?= $(dir $(shell which pandoc))
MAKEPAGE = $(PANDOC_PATH)/pandoc -s -S -c pandoc.css -A footer.html
all : $(ALL) 

.PHONY: clean
clean:
	for file in $(ALL); do rm $$file; done; \
	rm -r example*;

examples.txt : $(PANDOC_DEPS) mkdemos.pl config.xsl S5DEMO README demos
	PATH=$(PANDOC_PATH):$$PATH ./mkdemos.pl demos $@
	perl -pi -e 's!(href="(main|(my)?header|footer|example\d+)\.(html|tex|xml|css))"!\1.html"!g' $@
	for file in $$(ls | egrep '(main|(my)?header|footer|example[0-9]+)\.(html|tex|xml|css)$$'); \
		do highlight -k monospace -u utf-8 --style emacs $$file > $$file.html; \
	done

index.html : index.txt
	$(MAKEPAGE) --toc $< > $@

README.html : README
	$(MAKEPAGE) --toc $< > $@

INSTALL.html : INSTALL
	$(MAKEPAGE) --toc $< > $@

%.html : %.txt
	$(MAKEPAGE) $< > $@

%1.html : %.1
	groff -man -T html $< > $@

%.html : %
	$(MAKEPAGE) $< > $@

upload : 
	sitecopy --update macfarlane 

ALL := index.html README.html INSTALL.html examples.html pandoc.1.html html2markdown.1.html hsmarkdown.1.html html2x.html
PANDOC_PATH ?= $(dir $(shell which pandoc))
MAKEPAGE = $(PANDOC_PATH)/pandoc -s -S -H css -A footer.html
all : $(ALL) 

.PHONY: clean
clean:
	for file in $(ALL); do rm $$file; done; \
	rm -r example*;

examples.txt : mkdemos.pl config.xsl S5DEMO README demos math.text
	PATH=$(PANDOC_PATH):$$PATH ./mkdemos.pl demos $@
	perl -pi -e 's!(href="(main|(my)?header|footer|example\d+)\.(html|tex|xml|css))"!\1.html"!g' $@
	for file in $$(ls | egrep '(main|(my)?header|footer|example[0-9]+)\.(html|tex|xml|css)$$'); \
		do highlight -k monospace -u utf-8 --style emacs $$file > $$file.html; \
	done

examples.html : examples.txt
	$(PANDOC_PATH)/pandoc -s -H css -A footer.html $< > $@

index.html : index.txt css
	$(MAKEPAGE) --toc $< > $@

README.html : README css
	$(MAKEPAGE) --toc $< > $@

INSTALL.html : INSTALL css
	$(MAKEPAGE) --toc $< > $@

%.html : %.txt css
	$(MAKEPAGE) $< > $@

%.1.html : %.1
	groff -man -T html $< > $@

%.html : % css
	$(MAKEPAGE) $< > $@

upload : 
	make -C .. upload

VERSION=$(shell cat version.txt)
BASE=pandoc-$(VERSION)
DEST=pandoc/usr/local
ALL=$(BASE)-macOS.pkg $(BASE)-macOS.zip

all: $(ALL) notarize

signed.txt: $(DEST)
	chmod +x $(DEST)/bin/pandoc
	codesign --force --options runtime -s "5U2WKE6DES" $(DEST)/bin/pandoc
	echo "SIGNED" > signed.txt

$(DEST)/bin/pandoc-server:
	cd $(DEST)/bin && ln -s pandoc pandoc-server

$(DEST)/bin/pandoc-lua:
	cd $(DEST)/bin && ln -s pandoc pandoc-lua

pandoc.pkg: $(DEST)/bin/pandoc-server $(DEST)/bin/pandoc-lua signed.txt
	pkgbuild --root pandoc --identifier net.johnmacfarlane.pandoc --version $(VERSION) --ownership recommended $@

$(BASE)-macOS.pkg: pandoc.pkg
	productbuild --distribution distribution.xml --resources Resources --package-path $< --version $(VERSION) --sign 'Developer ID Installer: John Macfarlane' $@

$(BASE)-macOS.zip: $(DEST)/bin/pandoc-server $(DEST)/bin/pandoc-lua signed.txt
	mv $(DEST) $(BASE)
	zip --symlinks -r $@ $(BASE)

gon.hcl: version.txt
	echo "notarize {\n  path = \"$(BASE)-macOS.pkg\"\n  bundle_id = \"org.pandoc.pandoc\"\n  staple = true\n}\napple_id {\n  username = \"jgm@berkeley.edu\"\n  password = \"@env:AC_PASSWORD\"\n}" > $@

notarize: gon.hcl $(BASE)-macOS.pkg
	gon -log-level=warn ./gon.hcl

clean:
	rm signed.txt $(BASE) pandoc.pkg gon.hcl

distclean: clean
	rm $(ALL)

.PHONY: all clean distclean

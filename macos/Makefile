VERSION=$(shell cat version.txt)
BASE=pandoc-$(VERSION)
DEST=pandoc/usr/local
ALL=$(BASE)-macOS.pkg $(BASE)-macOS.zip

all: $(ALL) notarize

signed.txt: $(DEST)
	chmod +x $(DEST)/bin/pandoc $(DEST)/bin/pandoc-citeproc
	codesign --force --options runtime -s "5U2WKE6DES" $(DEST)/bin/pandoc
	codesign --force --options runtime -s "5U2WKE6DES" $(DEST)/bin/pandoc-citeproc
	echo "SIGNED" > signed.txt

pandoc.pkg: signed.txt
	pkgbuild --root pandoc --identifier net.johnmacfarlane.pandoc --version $(VERSION) --ownership recommended $@

$(BASE)-macOS.pkg: pandoc.pkg
	productbuild --distribution distribution.xml --resources Resources --package-path $< --version $(VERSION) --sign 'Developer ID Installer: John Macfarlane' $@

$(BASE)-macOS.zip: signed.txt
	ln -s $(DEST) $(BASE)
	zip -r $@ $(BASE)

gon.hcl: version.txt
	echo "notarize {\n  path = \"$(BASE)-macOS.pkg\"\n  bundle_id = \"org.pandoc.pandoc\"\n  staple = true\n}\napple_id {\n  username = \"jgm@berkeley.edu\"\n  password = \"@env:AC_PASSWORD\"\n}" > $@

notarize: gon.hcl $(BASE)-macOS.pkg
	gon -log-level=warn ./gon.hcl

clean:
	rm signed.txt $(BASE) pandoc.pkg gon.hcl

distclean: clean
	rm $(ALL)

.PHONY: all clean distclean

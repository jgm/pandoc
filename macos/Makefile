ARCHITECTURE=$(shell cat architecture.txt)
VERSION=$(shell cat version.txt)
BASE=pandoc-$(VERSION)-$(ARCHITECTURE)
DEST=pandoc/usr/local
ALL=$(BASE)-macOS.pkg $(BASE)-macOS.zip
TOOLPATH=/Applications/XCode.app/Contents/Developer/usr/bin

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

notarize: $(BASE)-macOS.pkg
	$(TOOLPATH)/notarytool submit $< --keychain-profile=pandoc --wait && \
    $(TOOLPATH)/stapler staple $<

clean:
	rm signed.txt $(BASE) pandoc.pkg # gon.hcl

distclean: clean
	rm $(ALL)

.PHONY: all clean distclean

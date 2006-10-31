# Makefile for Pandoc.

CABAL     := Pandoc.cabal

#-------------------------------------------------------------------------------
# Cabal constants
#-------------------------------------------------------------------------------
NAME      := $(shell sed -ne 's/^[Nn]ame:[[:space:]]*//p' $(CABAL).in)
THIS      := $(shell echo $(NAME) | tr A-Z a-z)
VERSION   := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(CABAL).in)
BINS      := $(shell sed -ne 's/^[Ee]xecutable:[[:space:]]*//p' $(CABAL).in)

#-------------------------------------------------------------------------------
# Variables to setup through environment
#-------------------------------------------------------------------------------
PREFIX    ?= /usr/local
DESTDIR   ?=

#-------------------------------------------------------------------------------
# Constant names and commands in source tree
#-------------------------------------------------------------------------------
SRCDIR    := src
MANDIR    := man
BUILDDIR  := dist
BUILDCONF := .setup-config
BUILDCMD  := runhaskell Setup.hs
CONFIGURE := configure

#-------------------------------------------------------------------------------
# Installation paths
#-------------------------------------------------------------------------------
DESTPATH    := $(DESTDIR)$(PREFIX)
BINPATH     := $(DESTPATH)/bin
DATAPATH    := $(DESTPATH)/share
LIBPATH     := $(DESTPATH)/$(NAME)-$(VERSION)
DOCPATH     := $(DATAPATH)/doc/$(THIS)
LIBDOCPATH  := $(DATAPATH)/doc/$(THIS)-doc
MANPATH     := $(DATAPATH)/man
PKGPATH     := $(DATAPATH)/$(THIS)

#-------------------------------------------------------------------------------
# Generic Makefile variables
#-------------------------------------------------------------------------------
INSTALL         := install -c
INSTALL_PROGRAM := $(INSTALL) -m 755
INSTALL_DATA    := $(INSTALL) -m 644
STRIP           := strip
GHC             := ghc
GHC_PKG         := ghc-pkg

#-------------------------------------------------------------------------------
# Recipes
#-------------------------------------------------------------------------------

.PHONY: all
all: $(BINS)

# Document process rules.
%.html: % $(THIS)
	./$(THIS) -s $< >$@ || rm -f $@
%.tex: % $(THIS)
	./$(THIS) -s -w latex $< >$@ || rm -f $@
%.rtf: % $(THIS)
	./$(THIS) -s -w rtf $< >$@ || rm -f $@
%.pdf: % $(THIS)
	sh ./markdown2pdf $< || rm -f $@

.PHONY: templates
templates: $(SRCDIR)/templates
$(SRCDIR)/templates:
	$(MAKE) -C $(SRCDIR)/templates

cleanup_files+=$(CABAL)
$(CABAL): cabalize $(CABAL).in
	./cabalize <$(CABAL).in >$(CABAL)

.PHONY: configure
cleanup_files+=$(BUILDDIR) $(BUILDCONF)
configure: $(BUILDCONF)
$(BUILDCONF): $(CABAL)
	$(BUILDCMD) configure --prefix=$(PREFIX)

.PHONY: build
build: templates configure
	$(BUILDCMD) build

.PHONY: build-lib-doc haddock
build-lib-doc: html
haddock: build-lib-doc
cleanup_files+=html
html/: configure
	-rm -rf html
	$(BUILDCMD) haddock && mv $(BUILDDIR)/doc/html .

cleanup_files+=$(BINS)
$(BINS): build
	find $(BUILDDIR) -type f -name "$(BINS)" -perm +a=x -exec cp {} . \;

.PHONY: build-all
build-all: build $(BINS) build-lib-doc

# XXX: Note that we don't handle PREFIX correctly at the install-* stages,
# i.e. any PREFIX given at the configuration time is lost, unless it is
# also supplied (via environment) at these stages.

# User documents installation.
.PHONY: install-doc uninstall-doc
doc_all:=README.html README BUGS TODO
man_all:=$(patsubst $(MANDIR)/%,%,$(wildcard $(MANDIR)/man?/*.1))
cleanup_files+=README.html
install-doc: $(BINS) $(doc_all)
	$(INSTALL) -d $(DOCPATH) && $(INSTALL_DATA) $(doc_all) $(DOCPATH)/
	for f in $(man_all); do \
		$(INSTALL) -d $(MANPATH)/$$(dirname $$f); \
		$(INSTALL_DATA) $(MANDIR)/$$f $(MANPATH)/$$f; \
	done
uninstall-doc:
	-for f in $(doc_all); do rm -f $(DOCPATH)/$$f; done
	-for f in $(man_all); do rm -f $(MANPATH)/$$f; done
	-rmdir $(DOCPATH)

# Library documents installation.
.PHONY: install-lib-doc uninstall-lib-doc
install-lib-doc: build-lib-doc
	$(INSTALL) -d $(LIBDOCPATH) && cp -a html $(LIBDOCPATH)/
uninstall-lib-doc:
	-rm -rf $(LIBDOCPATH)/html
	-rmdir $(LIBDOCPATH)

# Program only installation.
.PHONY: install-exec uninstall-exec
bin_all:=$(BINS) html2markdown markdown2html latex2markdown markdown2latex markdown2pdf
install-exec: $(bin_all)
	$(INSTALL) -d $(BINPATH); \
	for f in $(bin_all); do $(INSTALL_PROGRAM) $$f $(BINPATH)/; done
uninstall-exec:
	-for f in $(bin_all); do rm -f $(BINPATH)/$$f; done

# Program + user documents installation.
.PHONY: install-program uninstall-program
install-program: install-exec install-doc
uninstall-program: uninstall-exec uninstall-doc

# Install everything.
.PHONY: install-all uninstall-all
install-all: install-doc install-lib-doc
	destdir=$(DESTDIR); destdir=$${destdir:-/}; \
	$(BUILDCMD) copy --destdir=$$destdir; \
	$(BUILDCMD) register
uninstall-all: uninstall-exec uninstall-doc uninstall-lib-doc
	-pkg_id="$(NAME)-$(VERSION)"; \
	libdir=$$($(GHC_PKG) field $$pkg_id library-dirs 2>/dev/null | \
		  sed 's/^library-dirs: *//'); \
	if [ -d "$$libdir" ]; then \
		$(BUILDCMD) unregister; \
		rm -rf $$libdir; \
		rmdir $$(dirname $$libdir); \
	else \
		echo "*** Couldn't locate library files for pkgid: $$pkg_id. ***"; \
	fi

# Default installation recipe for a common deployment scenario.
.PHONY: install uninstall
install: install-program
uninstall: uninstall-program

.PHONY: osx-pkg
osx_dest:=osx-pkg
doc_more:=README.rtf LICENSE.rtf OSX-Welcome.rtf
cleanup_files+=$(osx_dest) $(doc_more)
osx-pkg: $(osx_dest)
$(osx_dest): $(doc_more)
	-rm -rf $(osx_dest)
	$(INSTALL) -d $(osx_dest)
	DESTDIR=$(osx_dest)/Package_root $(MAKE) install-program
	find $(osx_dest) -type f -regex ".*bin/.*" | xargs chmod +x
	find $(osx_dest) -type f -regex ".*bin/$(THIS)" | xargs $(STRIP)
	find $(osx_dest) -type f | xargs chown root:wheel
	$(INSTALL) -d $(osx_dest)/Resources
	mv README.rtf $(osx_dest)/Resources/ReadMe.rtf
	mv LICENSE.rtf $(osx_dest)/Resources/License.rtf
	sed -e 's#@PREFIX@#$(PREFIX)#g' OSX-Welcome.rtf > $(osx_dest)/Resources/Welcome.rtf
	sed -e 's/@VERSION@/$(VERSION)/g' Info.plist > $(osx_dest)/Info.plist
	cp Description.plist $(osx_dest)/
	PackageMaker -build -p Pandoc_$(VERSION).pkg \
		            -f $(osx_dest)/Package_root \
			    -r $(osx_dest)/Resources \
			    -i $(osx_dest)/Info.plist \
			    -d $(osx_dest)/Description.plist

.PHONY: test test-markdown
test: $(BINS)
	@cd tests && perl runtests.pl -s $(PWD)/$(THIS)
test-markdown: $(BINS)
	@cd tests/MarkdownTest_1.0.3 && perl MarkdownTest.pl -s $(PWD)/$(THIS) -tidy

# Stolen and slightly improved from a GPLed Makefile.  Credits to John Meacham.
src_all:=$(shell find $(SRCDIR) -type f -name '*hs' | egrep -v '^\./(_darcs|lib|test)/')
cleanup_files+=$(patsubst %,$(SRCDIR)/%,tags tags.sorted)
tags: $(src_all)
	cd $(SRCDIR) && hasktags -c $(src_all:$(SRCDIR)/%=%); \
	LC_ALL=C sort tags >tags.sorted; mv tags.sorted tags

deb: debian
	[ -x /usr/bin/fakeroot ] || { \
		echo "*** Please install fakeroot package. ***"; \
		exit 1; \
	}
	[ -x /usr/bin/dpkg-buildpackage ] || { \
		echo "*** Please install dpkg-dev package. ***"; \
		exit 1; \
	}
	if [ -x /usr/bin/debuild ]; then \
		debuild -uc -us -i.svn -I.svn -i_darcs -I_darcs --lintian-opts -i; \
	else \
		echo "*** Please install devscripts package. ***"; \
		echo "*** Using dpkg-buildpackage for package building. ***"; \
		dpkg-buildpackage -rfakeroot -uc -us -i.svn -I.svn -i_darcs -I_darcs; \
	fi

.PHONY: distclean clean
distclean: clean
	if [ -d debian ]; then \
		chmod +x debian/rules; fakeroot debian/rules clean; \
	fi

clean:
	-if [ -f $(BUILDCONF) ]; then $(BUILDCMD) clean; fi
	-rm -rf $(cleanup_files)

# Makefile for Pandoc.

#-------------------------------------------------------------------------------
# Constant names and commands in source tree
#-------------------------------------------------------------------------------
CABAL     := Pandoc.cabal
SRCDIR    := src
MANDIR    := man
TESTDIR   := tests
BUILDDIR  := dist
BUILDCONF := .setup-config
BUILDCMD  := runhaskell Setup.hs
BUILDVARS := vars
CONFIGURE := configure

#-------------------------------------------------------------------------------
# Cabal constants
#-------------------------------------------------------------------------------
NAME      := $(shell sed -ne 's/^[Nn]ame:[[:space:]]*//p' $(CABAL).in)
THIS      := $(shell echo $(NAME) | tr A-Z a-z)
VERSION   := $(shell sed -ne 's/^version[[:space:]]*=[[:space:]]*"\([^"]*\)"/\1/p' $(SRCDIR)/Main.hs)
RELNAME   := $(THIS)-$(VERSION)
EXECSBASE := $(shell sed -ne 's/^[Ee]xecutable:[[:space:]]*//p' $(CABAL).in)

#-------------------------------------------------------------------------------
# Install targets
#-------------------------------------------------------------------------------
WRAPPERS  := html2markdown markdown2pdf hsmarkdown
# Add .exe extensions if we're running Windows/Cygwin.
EXTENSION := $(shell uname | tr '[:upper:]' '[:lower:]' | \
               sed -ne 's/^cygwin.*$$/\.exe/p')
EXECS     := $(addsuffix $(EXTENSION),$(EXECSBASE))
PROGS     := $(EXECS) $(WRAPPERS) 
# First entry in Cabal's executable stanza is the main executable.
MAIN      := $(firstword $(EXECS))
DOCS      := README.html README BUGS 

#-------------------------------------------------------------------------------
# Variables to setup through environment
#-------------------------------------------------------------------------------

# Specify default values.
prefix    := /usr/local
destdir   := 
# Attempt to set variables from a previous make session.
-include $(BUILDVARS)
# Fallback to defaults but allow to get the values from environment.
PREFIX    ?= $(prefix)
DESTDIR   ?= $(destdir)

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

# Default target.
.PHONY: all
all: build-program

# Document process rules.
%.html: % $(MAIN)
	./$(MAIN) -s $< >$@ || rm -f $@
%.tex: % $(MAIN)
	./$(MAIN) -s -w latex $< >$@ || rm -f $@
%.rtf: % $(MAIN)
	./$(MAIN) -s -w rtf $< >$@ || rm -f $@
%.pdf: % $(MAIN) markdown2pdf
	sh ./markdown2pdf $< || rm -f $@
%.txt: %
	perl -p -e 's/\n/\r\n/' $< > $@ || rm -f $@ # convert to DOS line endings

.PHONY: templates
templates: $(SRCDIR)/templates
	$(MAKE) -C $(SRCDIR)/templates

define generate-shell-script
echo "Generating $@...";                                 \
awk '                                                    \
	/^[ \t]*###+ / {                                 \
                lead = $$0; sub(/[^ \t].*$$/, "", lead); \
		t = "$(dir $<)/"$$2;                     \
		while (getline line < t > 0)             \
			print lead line;                 \
		next;                                    \
	}                                                \
	{ print }                                        \
' <$< >$@
chmod +x $@
endef

.PHONY: wrappers
wrappers: $(WRAPPERS)
cleanup_files+=$(WRAPPERS)
$(WRAPPERS): %: $(SRCDIR)/wrappers/%.in $(SRCDIR)/wrappers/*.sh
	@$(generate-shell-script)

cleanup_files+=$(CABAL)
$(CABAL): cabalize $(CABAL).in $(SRCDIR)/Main.hs
	./cabalize <$(CABAL).in >$(CABAL)

.PHONY: configure
cleanup_files+=$(BUILDDIR) $(BUILDCONF) $(BUILDVARS)
configure: $(BUILDCONF) templates
$(BUILDCONF): $(CABAL)
	$(BUILDCMD) configure --prefix=$(PREFIX)
	# Make configuration time settings persistent (definitely a hack).
	@echo "PREFIX?=$(PREFIX)" >$(BUILDVARS)
	@echo "DESTDIR?=$(DESTDIR)" >>$(BUILDVARS)

.PHONY: build
build: configure
	$(BUILDCMD) build

.PHONY: build-exec
build-exec: $(PROGS)
cleanup_files+=$(EXECS)
$(EXECS): build
	for f in $@; do \
		[ -f $$f ] || { \
        		find $(BUILDDIR) -type f -name "$$f" \
					 -perm +a=x -exec ln -s {} . \; ; \
		} \
        done

.PHONY: build-doc
cleanup_files+=README.html 
build-doc: $(DOCS)

.PHONY: build-program
build-program: build-exec build-doc

.PHONY: build-lib-doc haddock
build-lib-doc: html
haddock: build-lib-doc
cleanup_files+=html
html/: configure
	-rm -rf html
	$(BUILDCMD) haddock && mv $(BUILDDIR)/doc/html .

.PHONY: build-all
build-all: build-program build-lib-doc

# User documents installation.
.PHONY: install-doc uninstall-doc
man_all:=$(patsubst $(MANDIR)/%,%,$(wildcard $(MANDIR)/man?/*.1))
install-doc: build-doc
	$(INSTALL) -d $(DOCPATH) && $(INSTALL_DATA) $(DOCS) $(DOCPATH)/
	for f in $(man_all); do \
		$(INSTALL) -d $(MANPATH)/$$(dirname $$f); \
		$(INSTALL_DATA) $(MANDIR)/$$f $(MANPATH)/$$f; \
	done
uninstall-doc:
	-for f in $(DOCS); do rm -f $(DOCPATH)/$$f; done
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
install-exec: build-exec
	$(INSTALL) -d $(BINPATH); \
	for f in $(PROGS); do \
		if [ -L $$f ]; then \
			f=$$(readlink $$f); \
		fi; \
		$(INSTALL_PROGRAM) $$f $(BINPATH)/; \
	done
uninstall-exec:
	-for f in $(notdir $(PROGS)); do rm -f $(BINPATH)/$$f; done ;

# Program + user documents installation.
.PHONY: install-program uninstall-program
install-program: install-exec install-doc
uninstall-program: uninstall-exec uninstall-doc

# Install everything.
.PHONY: install-all uninstall-all
install-all: install-doc install-lib-doc
	destdir=$(DESTDIR); \
	# Older Cabal versions have no '--destdir' option.
	if $(BUILDCMD) copy --help | grep -q '\-\-destdir'; then \
		opt="--destdir=$${destdir-/}"; \
	else \
		opt="--copy-prefix=$${destdir}$(PREFIX)"; \
	fi; \
	$(BUILDCMD) copy $$opt; \
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

# OSX packages:  make osx-pkg-prep, then (as root) make osx-pkg
.PHONY: osx-pkg osx-pkg-prep
osx_dest:=osx-pkg-tmp
osx_src:=osx
doc_more:=README.rtf COPYRIGHT.rtf $(osx_src)/Welcome.rtf
osx_pkg_name:=$(RELNAME).pkg
cleanup_files+=$(osx_dest) $(doc_more) $(osx_pkg_name)
osx-pkg-prep: $(osx_dest)
$(osx_dest)/: build-program $(doc_more)
	-rm -rf $(osx_dest)
	$(INSTALL) -d $(osx_dest)
	DESTDIR=$(osx_dest)/Package_root $(MAKE) install-program
	cp $(osx_src)/uninstall-pandoc $(osx_dest)/Package_root/usr/local/bin/
	find $(osx_dest) -type f -regex ".*bin/.*" | xargs chmod +x
	find $(osx_dest) -type f -regex ".*bin/$(notdir $(MAIN))" | xargs $(STRIP)
	$(INSTALL) -d $(osx_dest)/Resources
	cp README.rtf $(osx_dest)/Resources/ReadMe.rtf
	cp COPYRIGHT.rtf $(osx_dest)/Resources/License.rtf
	sed -e 's#@PREFIX@#$(PREFIX)#g' $(osx_src)/Welcome.rtf > $(osx_dest)/Resources/Welcome.rtf
	sed -e 's/@VERSION@/$(VERSION)/g' $(osx_src)/Info.plist > $(osx_dest)/Info.plist
	cp $(osx_src)/Description.plist $(osx_dest)/
osx-pkg: $(osx_pkg_name)
$(osx_pkg_name)/: $(osx_dest)
	if [ "`id -u`" != 0 ]; then \
		echo "Root permissions needed to create OSX package!"; \
		exit 1; \
	fi
	find $(osx_dest) | xargs chown root:wheel
	PackageMaker -build -p $(osx_pkg_name) \
	                    -f $(osx_dest)/Package_root \
	                    -r $(osx_dest)/Resources \
	                    -i $(osx_dest)/Info.plist \
	                    -d $(osx_dest)/Description.plist
	chgrp admin $(osx_pkg_name)
	-rm -rf $(osx_dest)

.PHONY: osx-dmg
osx_dmg_name:=$(RELNAME).dmg
cleanup_files+=$(osx_dmg_name)
osx_udzo_name:=$(RELNAME).udzo.dmg
osx_dmg_volume:="$(NAME) $(VERSION)"
osx-dmg: $(osx_dmg_name)
$(osx_dmg_name): $(osx_pkg_name)
	-rm -f $(osx_dmg_name)
	hdiutil create $(osx_dmg_name) -size 05m -fs HFS+ -volname $(osx_dmg_volume)
	dev_handle=`hdid $(osx_dmg_name) | grep Apple_HFS | \
		    perl -e '\$$_=<>; /^\\/dev\\/(disk.)/; print \$$1'`; \
	ditto $(osx_pkg_name) /Volumes/$(osx_dmg_volume)/$(osx_pkg_name); \
	hdiutil detach $$dev_handle
	hdiutil convert $(osx_dmg_name) -format UDZO -o $(osx_udzo_name)
	-rm -f $(osx_dmg_name)
	mv $(osx_udzo_name) $(osx_dmg_name)


.PHONY: win-pkg
win_pkg_name:=$(RELNAME).zip
win_docs:=COPYING.txt COPYRIGHT.txt BUGS.txt README.txt README.html
cleanup_files+=$(win_pkg_name) $(win_docs)
win-pkg: $(win_pkg_name)
$(win_pkg_name): $(THIS).exe  $(win_docs)
	zip -r $(win_pkg_name) $(THIS).exe $(win_docs)

.PHONY: test test-markdown
test: $(MAIN)
	@cd $(TESTDIR) && perl runtests.pl -s $(PWD)/$(MAIN)
compat:=$(PWD)/hsmarkdown
markdown_test_dirs:=$(wildcard $(TESTDIR)/MarkdownTest_*)
test-markdown: $(MAIN) $(compat)
	@for suite in $(markdown_test_dirs); do \
		( \
			suite_version=$$(echo $$suite | sed -e 's/.*_//');\
			echo "-----------------------------------------";\
			echo "Running Markdown test suite version $${suite_version}.";\
			PATH=$(PWD):$$PATH; export PATH; cd $$suite && \
			perl MarkdownTest.pl -s $(compat) -tidy ; \
		) \
	done

# Stolen and slightly improved from a GPLed Makefile.  Credits to John Meacham.
src_all:=$(shell find $(SRCDIR) -type f -name '*hs' | egrep -v '^\./(_darcs|lib|test)/')
cleanup_files+=$(patsubst %,$(SRCDIR)/%,tags tags.sorted)
tags: $(src_all)
	cd $(SRCDIR) && hasktags -c $(src_all:$(SRCDIR)/%=%); \
	LC_ALL=C sort tags >tags.sorted; mv tags.sorted tags

.PHONY: tarball
tarball_name:=$(RELNAME).tar.gz
cleanup_files+=$(tarball_name)
tarball: $(tarball_name)
$(tarball_name):
	svn export . $(RELNAME)
	tar cvzf $(tarball_name) $(RELNAME)
	-rm -rf $(fullname)

.PHONY: deb
deb_name:=$(shell grep ^Package debian/control | cut -d' ' -f2 | head -n 1)
deb_version:=$(shell head -n 1 debian/changelog | cut -f2 -d' ' | tr -d '()')
deb_arch:=i386
deb_main:=$(deb_name)_$(deb_version)_$(deb_arch).deb
deb: debian
	@[ -x /usr/bin/fakeroot ] || { \
		echo "*** Please install fakeroot package. ***"; \
		exit 1; \
	}
	@[ -x /usr/bin/dpkg-buildpackage ] || { \
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

.PHONY: website
web_src:=web
web_dest:=web/pandoc
make_page:=./$(MAIN) -s -B $(web_src)/header.html \
                        -A $(web_src)/footer.html \
	                -H $(web_src)/css 
cleanup_files+=$(web_dest)
website: $(web_dest)
$(web_dest)/: $(MAIN) html
	-rm -rf $(web_dest)
	( \
		mkdir $(web_dest); \
		cp -r html $(web_dest)/doc; \
		cp $(web_src)/*.css $(web_dest)/; \
		sed -e 's#@PREFIX@#$(PREFIX)#g' $(osx_src)/Welcome | \
			$(make_page) > $(web_dest)/osx-notes.html; \
		sed -e '/^ --.*/d' -e 's#^#    #g' changelog | \
			sed -e 's/    \(pandoc (.*\)/## \1/g' | \
			$(make_page) -T "Pandoc changelog" > $(web_dest)/changelog.html; \
		$(make_page) README > $(web_dest)/README.html; \
		$(make_page) INSTALL > $(web_dest)/INSTALL.html; \
		sed -e 's/@TARBALL_NAME@/$(tarball_name)/g' $(web_src)/index.txt | \
			sed -e 's/@OSX_DMG_NAME@/$(osx_dmg_name)/g' | \
			sed -e 's/@WINDOWS_PKG_NAME@/$(win_pkg_name)/g' | \
			sed -e 's/@VERSION@/$(VERSION)/g' | \
			$(make_page) > $(web_dest)/index.html; \
		cp README $(web_dest)/ ; \
		./$(MAIN) -s -w latex README > $(web_dest)/README.tex ; \
		cp $(web_src)/myheader.tex $(web_dest)/ ; \
		cp $(web_src)/S5DEMO $(web_dest)/ ; \
		cp $(web_src)/header.html $(web_dest)/ ; \
		cp $(web_src)/footer.html $(web_dest)/ ; \
		sh $(web_src)/mkdemos.sh $(web_dest) $(shell pwd) | \
		    $(make_page) > $(web_dest)/examples.html; \
	) || { rm -rf $(web_dest); exit 1; }

.PHONY: distclean clean
distclean: clean
	if [ -d debian ]; then \
		chmod +x debian/rules; fakeroot debian/rules clean; \
	fi

clean:
	make -C $(SRCDIR)/templates clean
	-if [ -f $(BUILDCONF) ]; then $(BUILDCMD) clean; fi
	-rm -rf $(cleanup_files)

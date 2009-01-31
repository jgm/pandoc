# Makefile for Pandoc.

#-------------------------------------------------------------------------------
# Constant names and commands in source tree
#-------------------------------------------------------------------------------
CABAL     := pandoc.cabal
SRCDIR    := src
MANDIR    := man
TESTDIR   := tests
BUILDDIR  := dist
BUILDCONF := $(BUILDDIR)/setup-config
BUILDVARS := vars
CONFIGURE := configure

#-------------------------------------------------------------------------------
# Cabal constants
#-------------------------------------------------------------------------------
PKG       := $(shell sed -ne 's/^[Nn]ame:[[:space:]]*//p' $(CABAL) | tr A-Z a-z)
VERSION   := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(CABAL))
PKGID     := $(PKG)-$(VERSION)
EXECSBASE := $(shell sed -ne 's/^[Ee]xecutable:\{0,1\}[[:space:]]*//p' $(CABAL))

#-------------------------------------------------------------------------------
# Install targets
#-------------------------------------------------------------------------------
WRAPPERS  := html2markdown hsmarkdown markdown2pdf
# Add .exe extensions if we're running Windows/Cygwin.
EXTENSION := $(shell uname | tr '[:upper:]' '[:lower:]' | \
               sed -ne 's/^cygwin.*$$/\.exe/p')
BUILDCMD  := $(addsuffix $(EXTENSION), ./setup)
EXECS     := $(addsuffix $(EXTENSION),$(EXECSBASE))
PROGS     := $(EXECS) $(WRAPPERS)
MAIN      := $(firstword $(EXECS))
DOCS      := README.html README BUGS
MANPAGES  := $(patsubst %.md,%,$(wildcard $(MANDIR)/man?/*.?.md))

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
DATADIR   ?= $(PKGID)
DOCDIR    ?= $(PKGID)/doc

#-------------------------------------------------------------------------------
# Installation paths
#-------------------------------------------------------------------------------
DESTPATH    := $(DESTDIR)$(PREFIX)
BINPATH     := $(DESTPATH)/bin
DATAPATH    := $(DESTPATH)/share
MANPATH     := $(DATAPATH)/man
PKGDATAPATH := $(DATAPATH)/$(DATADIR)
PKGDOCPATH  := $(DATAPATH)/$(DOCDIR)

#-------------------------------------------------------------------------------
# Generic Makefile variables
#-------------------------------------------------------------------------------
INSTALL         := install -c
INSTALL_PROGRAM := $(INSTALL) -m 755
INSTALL_DATA    := $(INSTALL) -m 644
STRIP           := strip
GHC             ?= ghc
GHC_VERSION     := $(shell $(GHC) --version | sed -e 's/[^0-9]*//')

#-------------------------------------------------------------------------------
# Recipes
#-------------------------------------------------------------------------------

# Default target.
.PHONY: all
all: build-program

# Document process rules.
%.html: % $(MAIN)
	./$(MAIN) -s -S --toc $< >$@ || rm -f $@
%.tex: % $(MAIN)
	./$(MAIN) -s -w latex $< >$@ || rm -f $@
%.rtf: % $(MAIN)
	./$(MAIN) -s -w rtf $< >$@ || rm -f $@
%.pdf: % $(MAIN) markdown2pdf
	sh ./markdown2pdf $< || rm -f $@
%.txt: %
	perl -p -e 's/\n/\r\n/' $< > $@ || rm -f $@ # convert to DOS line endings

.PHONY: configure
cleanup_files+=Setup.hi Setup.o $(BUILDCMD) $(BUILDVARS)
ifdef GHC_PKG
  hc_pkg = --with-hc-pkg=$(GHC_PKG)
else
  hc_pkg =
endif
templates=$(wildcard templates/*.* templates/headers/*.* templates/ui/default/*.*)
configure: $(BUILDCONF)
$(BUILDCMD): Setup.hs
	$(GHC) -package Cabal Setup.hs -o $(BUILDCMD)
$(BUILDCONF): $(CABAL) $(CABAL_BACKUP) $(BUILDCMD) $(templates)
	$(BUILDCMD) configure --prefix=$(PREFIX) --with-compiler=$(GHC) $(hc_pkg) $(CABALOPTS)
	@# Make configuration time settings persistent (definitely a hack).
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
		find $(BUILDDIR) -type f -name "$$f" -perm +a=x \
		                 -exec ln -s -f {} . \; ; \
	done

.PHONY: build-doc
cleanup_files+=README.html $(MANPAGES)
build-doc: $(DOCS) $(MANPAGES)

.PHONY: build-program
build-program: build-exec build-doc

.PHONY: build-lib-doc haddock
build-lib-doc: html
haddock: build-lib-doc
cleanup_files+=html
html/: configure
	-rm -rf html
	$(BUILDCMD) haddock && cp -r $(BUILDDIR)/doc/html .

.PHONY: build-all
build-all: build-program build-lib-doc

# User documents installation.
.PHONY: install-doc uninstall-doc
man_all:=$(patsubst $(MANDIR)/%,%,$(MANPAGES))
install-doc: build-doc
	$(INSTALL) -d $(PKGDOCPATH) && $(INSTALL_DATA) $(DOCS) $(PKGDOCPATH)/
	for f in $(man_all); do \
		$(INSTALL) -d $(MANPATH)/$$(dirname $$f); \
		$(INSTALL_DATA) $(MANDIR)/$$f $(MANPATH)/$$f; \
	done
uninstall-doc:
	-for f in $(DOCS); do rm -f $(PKGDOCPATH)/$$f; done
	-for f in $(man_all); do rm -f $(MANPATH)/$$f; done
	rmdir $(PKGDOCPATH) 2>/dev/null ||:

# Program only installation.
.PHONY: install-exec uninstall-exec
install-exec: build-exec
	$(STRIP) $(EXECS)
	$(INSTALL) -d $(BINPATH); \
	for f in $(PROGS); do \
		if [ -L $$f ]; then \
			f=$$(readlink $$f); \
		fi; \
		$(INSTALL_PROGRAM) $$f $(BINPATH)/; \
	done
uninstall-exec:
	-for f in $(notdir $(PROGS)); do rm -f $(BINPATH)/$$f; done

# Program + user documents installation.
.PHONY: install-program uninstall-program
install-program: install-exec install-doc
uninstall-program: uninstall-exec uninstall-doc

.PHONY: install-all uninstall-all
# Full installation through Cabal: main + wrappers + user docs + lib + lib docs
install-all: build-all install-program
	destdir=$(DESTDIR); \
	# Older Cabal versions have no '--destdir' option.
	if $(BUILDCMD) copy --help | grep -q '\-\-destdir'; then \
		opt="--destdir=$${destdir-/}"; \
	else \
		opt="--copy-prefix=$${destdir}$(PREFIX)"; \
	fi; \
	$(BUILDCMD) copy $$opt; $(BUILDCMD) register
# Cabal lacks an 'uninstall' command.  We have to remove some cruft manually.
uninstall-all: uninstall-program configure
	@libdir=$$($(GHC_PKG) field $(PKGID) library-dirs 2>/dev/null | \
		  sed 's/^library-dirs: *//'); \
	htmldir=$$($(GHC_PKG) field $(PKGID) haddock-html 2>/dev/null | \
		  sed 's/^haddock-html: *//'); \
	if [ -d $$libdir ]; then \
		$(BUILDCMD) unregister ||:; \
	else \
		echo >&2 "*** Couldn't locate library for pkgid: $(PKGID). ***"; \
	fi; \
	for d in $$libdir $$htmldir; do \
		[ -d $$d ] && { \
			rm -rf $$d; rmdir $$(dirname $$d) 2>/dev/null ||:; \
		} \
	done; \
	rmdir $(PKGDOCPATH) $(PKGDATAPATH) 2>/dev/null ||:

# Default installation recipe for a common deployment scenario.
.PHONY: install uninstall
install: install-program
uninstall: uninstall-program

# FreeBSD port
.PHONY: freebsd
freebsd_dest:=freebsd
freebsd_makefile:=$(freebsd_dest)/Makefile
freebsd_template:=$(freebsd_makefile).in
cleanup_files+=$(freebsd_makefile)
freebsd : $(freebsd_makefile)
$(freebsd_makefile) : $(freebsd_template)
	sed -e 's/@VERSION@/$(VERSION)/' $< > $@

# MacPort
.PHONY: macport
macport_dest:=macports
portfile:=$(macport_dest)/Portfile
portfile_template:=$(portfile).in
cleanup_files+=$(portfile)
macport : $(portfile)
$(portfile) : $(portfile_template) tarball
	sed -e 's/@VERSION@/$(VERSION)/' $(portfile_template) | \
	sed -e 's/@TARBALLMD5SUM@/$(word 2, $(shell openssl md5 $(tarball)))/' > \
	$(portfile)

.PHONY: win-pkg
win_pkg_name:=$(PKGID).zip
win_docs:=COPYING.txt COPYRIGHT.txt BUGS.txt README.txt README.html
cleanup_files+=$(win_pkg_name) $(win_docs)
win-pkg: $(win_pkg_name)
$(win_pkg_name): $(PKG).exe $(win_docs)
	zip -r $(win_pkg_name) $(PKG).exe $(win_docs)

.PHONY: test test-markdown
test: $(MAIN)
	$(BUILDCMD) test
compat:=$(PWD)/hsmarkdown
markdown_test_dirs:=$(wildcard $(TESTDIR)/MarkdownTest_*)
test-markdown: $(MAIN) $(compat)
	@for suite in $(markdown_test_dirs); do \
		( \
			suite_version=$$(echo $$suite | sed -e 's/.*_//');\
			echo >&2 "-----------------------------------------";\
			echo >&2 "Running Markdown test suite version $${suite_version}.";\
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
tarball:=$(PKGID).tar.gz
cleanup_files+=$(tarball)
tarball: $(tarball)
$(tarball):
	$(BUILDCMD) sdist
	cp $(BUILDDIR)/$(tarball) .

.PHONY: website
web_src:=web
web_dest:=pandoc-website
make_page:=./$(MAIN) -s -S -B $(web_src)/header.html \
                        -A $(web_src)/footer.html \
	                -H $(web_src)/css
cleanup_files+=$(web_dest)
$(web_dest) : html $(wildcard $(web_src)/*) changelog \
    INSTALL $(MANPAGES) $(MANDIR)/man1/pandoc.1.md README
	rm -rf $(web_dest) && { \
		mkdir $(web_dest); \
		cp -r html $(web_dest)/doc; \
		cp $(web_src)/* $(web_dest)/; \
		sed -e 's#@VERSION@#$(VERSION)#g' $(web_src)/index.txt.in > \
			$(web_dest)/index.txt; \
		cp changelog $(web_dest)/changelog.txt ; \
		cp README $(web_dest)/ ; \
		cp INSTALL $(web_dest)/ ; \
		cp $(MANDIR)/man1/pandoc.1.md $(web_dest)/ ; \
		cp $(MANPAGES) $(web_dest)/ ; \
	} || { rm -rf $(web_dest); exit 1; }
website: $(MAIN) $(web_dest)
	PANDOC_PATH=$(shell pwd) make -C $(web_dest)

.PHONY: distclean clean
distclean: clean
	if [ -d debian ]; then \
		chmod +x debian/rules; fakeroot debian/rules clean; \
	fi

clean:
	-if [ -f $(BUILDCONF) ]; then $(BUILDCMD) clean; fi
	-rm -rf $(cleanup_files)

VPATH     := ..
PROCESSOR := ./fillTemplates.pl
TARGETS   := Text/Pandoc/ASCIIMathML.hs \
	     Text/Pandoc/Writers/S5.hs \
	     Text/Pandoc/Writers/DefaultHeaders.hs

all: $(TARGETS)

Text/Pandoc/ASCIIMathML.hs: ASCIIMathML.hs $(PROCESSOR) $(VPATH)/ASCIIMathML.js
	perl $(PROCESSOR) $@ $(VPATH)

Text/Pandoc/Writers/S5.hs: S5.hs $(PROCESSOR) $(VPATH)/ui/default/*
	perl $(PROCESSOR) $@ $(VPATH)

Text/Pandoc/Writers/DefaultHeaders.hs: DefaultHeaders.hs $(PROCESSOR) $(VPATH)/headers/*
	perl $(PROCESSOR) $@ $(VPATH)

.PHONY: clean
clean:
	cd $(VPATH); rm -f $(TARGETS)

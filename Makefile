makemanpages=$(shell find dist -type f -name make-pandoc-man-pages | head -1)
MANPAGES=man/man1/pandoc.1 man/man5/pandoc_markdown.5

all: ${MANPAGES}

%.1: %.1.template
	${makemanpages}

%.5: %.5.template
	${makemanpages}

clean:
	-rm ${MANPAGES}

.PHONY: all clean

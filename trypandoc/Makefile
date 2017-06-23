CGIBIN=/home/website/cgi-bin
TRYPANDOC=/home/website/pandoc.org/try/
CGI=${CGIBIN}/trypandoc
BIN=/home/jgm/.local/bin/trypandoc

install: ${CGI} ${TRYPANDOC}/index.html

${TRYPANDOC}/%: %
	cp $< $@ && chown website:www-data $@ && chmod a+r $@

${CGI}: ${BIN}
	cp $< $@ && chown website:www-data $@ && chmod a+rx $@

.PHONY: install

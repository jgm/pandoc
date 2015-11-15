CGIBIN=/home/website/cgi-bin
TRYPANDOC=/home/website/html/pandoc/try/
CGI=${CGIBIN}/trypandoc
BIN=../.cabal-sandbox/bin/trypandoc

install: ${CGI} ${TRYPANDOC}/index.html

${TRYPANDOC}/%: %
	cp $< $@ && chown website:www-data $@ && chmod a+r $@

${CGI}: ${BIN}
	cp $< $@ && chown website:www-data $@ && chmod a+rx $@

.PHONY: install

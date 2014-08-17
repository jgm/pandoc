CGIBIN=/home/website/cgi-bin
TRYPANDOC=/home/website/html/pandoc/try/
CGI=${CGIBIN}/trypandoc
BIN=../dist/build/trypandoc/trypandoc

install: ${CGI} ${TRYPANDOC}/index.html

${TRYPANDOC}/%: %
	cp $< $@ && chown website:www-data $@ && chmod a+r $@

${CGI}: ${BIN}
	cp $< $@ && chown website:www-data $@ && chmod a+rx $@

.PHONY: install

# 'mingw32-make prep' prepares for building the Inno Setup installer
# note: before running this, build pandoc using:
# cabal install -f-library -fwrappers
# If the library is built, the data file paths will not be relocatable!

.PHONY: prep
prep:
	strip ..\dist\build\pandoc\pandoc.exe
	strip ..\dist\build\hsmarkdown\hsmarkdown.exe
	strip ..\dist\build\markdown2pdf\markdown2pdf.exe
	pandoc -s -S ..\README -o ..\README.html
	copy ..\COPYING ..\COPYING.txt
	copy ..\COPYRIGHT ..\COPYRIGHT.txt


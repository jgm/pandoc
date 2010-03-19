@echo off
cd ..
cabal clean
rem note: we use -f-library in building pandoc, because
rem if the library is built, the data file paths will not be relocatable!
cabal configure -f-library -fwrappers -fhighlighting --datasubdir=
cabal build
strip dist\build\pandoc\pandoc.exe
strip dist\build\markdown2pdf\markdown2pdf.exe
dist\build\pandoc\pandoc.exe -s --template templates\html.template -S README -o README.html
copy COPYING COPYING.txt
copy COPYRIGHT COPYRIGHT.txt
cd windows
ISCC pandoc-setup.iss

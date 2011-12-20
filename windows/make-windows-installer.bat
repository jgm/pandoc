@echo off
cd ..
cabal-dev install --disable-library-for-ghci highlighting-kate
cabal-dev install --flags="embed_data_files" citeproc-hs
cabal-dev install --flags="executable wrappers -library highlighting" --datasubdir=
rem note: we use -f-library in building pandoc, because
rem if the library is built, the data file paths will not be relocatable!
strip cabal-dev\bin\pandoc.exe
strip cabal-dev\bin\markdown2pdf.exe
cabal-dev\bin\pandoc.exe -s --template templates\html.template -S README -o README.html
copy COPYING COPYING.txt
copy COPYRIGHT COPYRIGHT.txt
cd windows
ISCC pandoc-setup.iss

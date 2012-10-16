@echo off
cd ..
cabal update
cabal-dev clean
cabal-dev install --disable-library-for-ghci highlighting-kate
cabal-dev install --flags="embed_data_files" citeproc-hs
cabal-dev install --flags="executable -library blaze_html_0_5" --datasubdir=
rem note: we use -f-library in building pandoc, because
rem if the library is built, the data file paths will not be relocatable!
strip cabal-dev\bin\pandoc.exe
cabal-dev\bin\pandoc.exe -s --template templates\default.html -S README -o README.html
copy COPYING COPYING.txt
copy COPYRIGHT COPYRIGHT.txt
cd windows
ISCC pandoc-setup.iss

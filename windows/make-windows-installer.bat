@echo off
cd ..
cabal update
cabal-dev clean
cabal-dev install --disable-library-for-ghci highlighting-kate
cabal-dev install --flags="embed_data_files" citeproc-hs
cabal-dev install --flags="embed_data_files"
strip cabal-dev\bin\pandoc.exe
cabal-dev\bin\pandoc.exe -s --template data\templates\default.html -S README -o README.html
copy COPYING COPYING.txt
copy COPYRIGHT COPYRIGHT.txt
cd windows
ISCC pandoc-setup.iss

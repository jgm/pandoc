@echo off
cd ..
rem cabal update
cabal-dev clean
cabal-dev install --reinstall --force-reinstall --flags="embed_data_files" citeproc-hs
if %errorlevel% neq 0 exit /b %errorlevel%
cabal-dev install --reinstall --force-reinstall --flags="embed_data_files"
if %errorlevel% neq 0 exit /b %errorlevel%
strip cabal-dev\bin\pandoc.exe
cabal-dev\bin\pandoc.exe -s --template data\templates\default.html -S README -o README.html
if %errorlevel% neq 0 exit /b %errorlevel%
copy COPYING COPYING.txt
copy COPYRIGHT COPYRIGHT.txt
cd windows
echo Creating msi...
"C:\Program Files\WiX Toolset v3.7\bin\candle.exe" pandoc.wxs
if %errorlevel% neq 0 exit /b %errorlevel%
"C:\Program Files\WiX Toolset v3.7\bin\light.exe" pandoc.wixobj
if %errorlevel% neq 0 exit /b %errorlevel%
"C:\Program Files\kSign\kSign.exe"

@echo off
cd ..
cabal update
cabal-dev clean
cabal install hsb2hs cabal-dev
if %errorlevel% neq 0 exit /b %errorlevel%
cabal-dev install -v1 --force --reinstall --flags="embed_data_files"
if %errorlevel% neq 0 exit /b %errorlevel%
cabal-dev install -v1 --reinstall --flags="embed_data_files" pandoc-citeproc
if %errorlevel% neq 0 exit /b %errorlevel%
strip cabal-dev\bin\pandoc.exe
strip cabal-dev\bin\pandoc-citeproc.exe
strip cabal-dev\bin\biblio2yaml.exe
cabal-dev\bin\pandoc.exe -s --template data\templates\default.html -S README -o README.html
if %errorlevel% neq 0 exit /b %errorlevel%
cabal-dev\bin\pandoc.exe -s --template data\templates\default.rtf COPYING -t rtf -S -o COPYING.rtf
if %errorlevel% neq 0 exit /b %errorlevel%
copy COPYRIGHT COPYRIGHT.txt
for /f "tokens=1-2 delims= " %%a in ('cabal-dev\bin\pandoc --version') do (
  @set VERSION=%%b
  goto :next
  )
:next
if "%VERSION%" == "" (
  echo Error: could not determine version number.
  exit /b 1 
)
echo Detected version %VERSION%
cd windows
echo Creating msi...
candle -dVERSION=%VERSION% pandoc.wxs
if %errorlevel% neq 0 exit /b %errorlevel%
light  -sw1076 -ext WixUIExtension -out pandoc-%VERSION%.msi pandoc.wixobj
if %errorlevel% neq 0 exit /b %errorlevel%
echo Starting kSign: sign, then quit kSign to complete the build...
kSign

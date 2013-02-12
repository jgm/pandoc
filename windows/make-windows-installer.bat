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
"C:\Program Files\WiX Toolset v3.7\bin\candle.exe" -dVERSION=%VERSION% -ext WixUIExtension pandoc.wxs
if %errorlevel% neq 0 exit /b %errorlevel%
"C:\Program Files\WiX Toolset v3.7\bin\light.exe"  -sw1076 -ext WixUIExtension -out pandoc-%VERSION%.msi pandoc.wixobj
if %errorlevel% neq 0 exit /b %errorlevel%
echo Starting kSign: sign, then quit kSign to complete the build...
"C:\Program Files\kSign\kSign.exe"

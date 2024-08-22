choco install -y ghc
choco install -y wixtoolset
choco install -y 7zip.install
$env:Arch = arm64
$env:TheDate = Get-Date -Format "yyyy-MM-dd"
cabal build -fembed_data_files -fserver -flua pandoc-cli
$env:BinPath = cabal list-bin -fembed_data_files -fserver -flua pandoc-cli
Write-Output "BinPath is $env:BinPath"
$env:PandocVersion = $env:BinPath\pandoc --version | Select-String -Pattern "pandoc" | ForEach-Object { $_.Line.Split()[1] }
if (-not $env:PandocVersion) {
    throw "Error: The PandocVersion environment variable is empty."
}
Write-Output "Date is $env:TheDate"
Write-Output "Detected pandoc version $env:PandocVersion"
$env:Windows = Join-Path -Path (Get-Location) -ChildPath "windows"
$env:Release = Join-Path -Path $env:windows -ChildPath "pandoc-$env:PandocVersion"
New-Item -Path $env:Release -ItemType Directory
Copy-Item -Path $env:BinPath\pandoc -Destination $env:Release
$env:BinPath\pandoc -s --toc MANUAL.txt -o $env:Release\MANUAL.html
$env:BinPath\pandoc -s COPYING.md -t rtf -o $env:Release\COPYING.rtf
Copy-Item -Path $env:Release\COPYING.rtf -Destination $env:Windows
Copy-Item -Path $env:COPYRIGHT -Destination -o $env:Release\COPYRIGHT.txt
Set-Location -Path $env:Windows
Write-Output "Creating msi..."
ls
$env:WorkDir = Get-Location
Write-Output "Running candle..."
candle -arch $env:Arch -dVERSION=$env:PandocVersion -dBINPATH=$env:Release *.wxs -out wixobj\
if (-not $?) {
  throw "candle failed"
}
Write-Output "Running light..."
light -sw1076 -ext WixUIExtension -ext WixUtilExtension -cultures:en-us -loc Pandoc-en-us.wxl -out $env:WorkDir\pandoc-$env:PandocVersion-$env:Arch-UNSIGNED.msi wixobj\*.wixobj
if (-not $?) {
  throw "light failed"
}
Write-Output "Zipping..."
7z a "pandoc-$env:PandocVersion-$env:Arch.zip" pandoc-$env:PandocVersion
cd ..
New-Item -Path windows-release-candidate -ItemType Directory
Copy-Item -Path windows\pandoc-$env:PandcoVersion-$env:Arch-UNSIGNED.msi -Destination windows-release-candidate
Copy-Item -Path windows\pandoc-$env:PandcoVersion-$env:Arch.zip -Destination windows-release-candidate
Copy-Item -Path windows\Makefile -Destination windows-release-candidate

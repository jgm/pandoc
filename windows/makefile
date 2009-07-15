# 'mingw32-make prep' prepares for building the Inno Setup
# installer

.PHONY: prep
prep:
	strip ..\dist\build\pandoc\pandoc.exe
	pandoc -s -S ..\README -o ..\README.html
	copy ..\COPYING ..\COPYING.txt
	copy ..\COPYRIGHT ..\COPYRIGHT.txt


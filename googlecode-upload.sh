#!/bin/sh
VERSION=$1
googlecode_upload.py -s "Source tarball" -p pandoc -u fiddlosopher --labels=Featured,Type-Source,OpSys-All  dist/pandoc-$VERSION.tar.gz
googlecode_upload.py -s "Windows installer" -p pandoc -u fiddlosopher --labels=Featured,Type-Installer,OpSys-Windows  pandoc-$VERSION-setup.exe
googlecode_upload.py -s "Mac OS X installer" -p pandoc -u fiddlosopher --labels=Featured,Type-Installer,OpSys-OSX  pandoc-$VERSION.dmg

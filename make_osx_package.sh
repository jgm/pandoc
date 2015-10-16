#!/bin/bash -e

LOCALBIN=$HOME/.local/bin
DIST=`pwd`/osx_package
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
DEST=$ROOT/usr/local
OSX=osx
SCRIPTS=$OSX/osx-resources
BASE=pandoc-$VERSION
ME=$(whoami)
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker

# echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

echo Building pandoc...
stack install --stack-yaml=stack.pkg.yaml

echo Getting man pages...
make man/pandoc.1
PANDOC_CITEPROC_PATH=`cabal unpack -d $DIST pandoc-citeproc | awk '{print $3;}'`

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/man/man5
for f in pandoc pandoc-citeproc; do
  cp $LOCALBIN/$f $DEST/bin/;
done
cp $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
cp man/pandoc.1 $DEST/share/man/man1/

chown -R $ME:staff $DIST

echo Copying license...
$LOCALBIN/pandoc --data data -t html5 -s COPYING -o $RESOURCES/license.html

echo Signing pandoc executable...

codesign --force --sign "Developer ID Application: John Macfarlane" $DEST/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
spctl --assess --type execute $DEST/bin/pandoc

echo Creating OSX package...
# remove old package first
rm -rf $BASE.pkg

pkgbuild --root $DIST/pandoc --identifier net.johnmacfarlane.pandoc --version 1.13 --ownership recommended $DIST/pandoc.pkg
productbuild --distribution osx/distribution.xml --resources $DIST/Resources --package-path $DIST --version 1.13 --sign "Developer ID Installer: John Macfarlane" $BASE-osx.pkg

# verify signature
spctl --assess --type install $BASE-osx.pkg

# cleanup
rm -r $DIST

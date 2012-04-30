#!/bin/sh -e

DIST=osx_package
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
SCRIPTS=osx-resources
BASE=pandoc-$VERSION
ME=jgm

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

echo Building pandoc...
sudo cabal-dev install-deps
sudo cabal-dev install --flags="embed_data_files" citeproc-hs
sudo cabal-dev install --disable-library-for-ghci highlighting-kate
sudo cabal-dev install --prefix=/usr/local --datasubdir=$BASE --docdir=/usr/local/doc/$BASE --flags="executable -library"
sudo cabal-dev copy --destdir=$ROOT
sudo chown -R $ME:staff $DIST

gzip $ROOT/usr/local/share/man/man?/*.*
# cabal gives man pages the wrong permissions
chmod +r $ROOT/usr/local/share/man/man?/*.*

echo Copying license...
cp COPYING $RESOURCES/License.txt

PACKAGEMAKER=/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker

echo Creating OSX package...

$PACKAGEMAKER \
    --root $ROOT \
    --id net.johnmacfarlane.pandoc \
    --resources $RESOURCES \
    --version $VERSION \
    --no-relocate \
    --scripts $SCRIPTS \
    --out $BASE.pkg

echo Creating disk image...

hdiutil create "$BASE.dmg" \
    -format UDZO -ov \
    -volname "pandoc $VERSION" \
    -srcfolder $BASE.pkg
hdiutil internet-enable "$BASE.dmg"


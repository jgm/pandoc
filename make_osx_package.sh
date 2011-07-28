#!/bin/sh -e

DIST=osx
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
BASE=pandoc-$VERSION
PREFIX=$ROOT/usr/local

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

echo Building pandoc...
runghc Setup.hs configure --user --prefix=/usr/local --flags="executable -library highlighting"
runghc Setup.hs build
runghc Setup.hs copy --destdir=$ROOT

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
    --out $BASE.pkg

echo Creating disk image...

hdiutil create "$BASE.dmg" \
    -format UDZO -ov \
    -volname "pandoc $VERSION" \
    -srcfolder $BASE.pkg
hdiutil internet-enable "$BASE.dmg"


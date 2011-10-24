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
cabal-dev install --disable-library-for-ghci highlighting-kate
cabal-dev install --flags="embed_data_files" citeproc-hs
cabal-dev install --flags="executable -library highlighting"
cabal-dev copy --destdir=$ROOT

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


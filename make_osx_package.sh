#!/bin/sh -e

DIST=osx_package
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
SCRIPTS=osx-resources
BASE=pandoc-$VERSION
ME=jgm
CODESIGNID="Developer ID Application: John Macfarlane"
PACKAGEMAKER=/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

echo Building pandoc...
cabal-dev install-deps
cabal-dev configure --prefix=/usr/local --datasubdir=$BASE --docdir=/usr/local/doc/$BASE
cabal-dev build
cabal-dev copy --destdir=$ROOT
# remove library files
rm -r $ROOT/usr/local/lib
chown -R $ME:staff $DIST

gzip $ROOT/usr/local/share/man/man?/*.*
# cabal gives man pages the wrong permissions
chmod +r $ROOT/usr/local/share/man/man?/*.*

echo Copying license...
dist/build/pandoc/pandoc --data data -t rtf -s COPYING -o $RESOURCES/License.rtf

echo Signing pandoc executable...

codesign --force --sign "$CODESIGNID" $ROOT/usr/local/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
spctl --assess --type execute $ROOT/usr/local/bin/pandoc

echo Creating OSX package...

sudo $PACKAGEMAKER \
    --root $ROOT \
    --id net.johnmacfarlane.pandoc \
    --resources $RESOURCES \
    --version $VERSION \
    --no-relocate \
    --scripts $SCRIPTS \
    --out $BASE.pkg

echo Signing package...

sudo codesign --force --sign "$CODESIGNID" $BASE.pkg
# make sure it's valid...
spctl --assess --type install $BASE.pkg

echo Creating disk image...

sudo hdiutil create "$BASE.dmg" \
    -format UDZO -ov \
    -volname "pandoc $VERSION" \
    -srcfolder $BASE.pkg
sudo hdiutil internet-enable "$BASE.dmg"


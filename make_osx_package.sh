#!/bin/sh -e

DIST=`pwd`/osx_package
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
SCRIPTS=osx-resources
BASE=pandoc-$VERSION
ICU=/usr/local/Cellar/icu4c/51.1
ME=jgm
CODESIGNID="Developer ID Application: John Macfarlane"
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

# echo Updating database
# cabal update

echo Building pandoc...
cabal-dev install hsb2hs

cabal-dev install --reinstall -v1 --prefix $ROOT/tmp  --flags="embed_data_files unicode_collation" --extra-lib-dirs=$ICU/lib --extra-include-dirs=$ICU/include pandoc-citeproc
cabal-dev install -v1 --prefix $ROOT/tmp --flags="embed_data_files"

mkdir -p $ROOT/usr/local/share
cp -r $ROOT/tmp/bin $ROOT/usr/local/
cp -r $ROOT/tmp/share/man $ROOT/usr/local/share/
rm -rf $ROOT/tmp

chown -R $ME:staff $DIST
# gzip $ROOT/usr/local/share/man/man?/*.*
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
    --scripts $SCRIPTS \
    --out $BASE.pkg

    # --no-relocate

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


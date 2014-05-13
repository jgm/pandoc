#!/bin/bash -e

DIST=`pwd`/osx_package
SANDBOX=`pwd`/.cabal-sandbox
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
DEST=$ROOT/usr/local
SCRIPTS=osx-resources
BASE=pandoc-$VERSION
ME=$(whoami)
CODESIGNID="Developer ID Application: John Macfarlane"
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
EXES="pandoc pandoc-citeproc"

read -s -p "sudo password: " PASSWORD
echo $PASSWORD | sudo -S echo "Password valid, continuing."

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

cabal sandbox init
echo Updating database
cabal update

echo Building pandoc...
cabal clean
# Use cpphs to avoid problems with clang cpp on ghc 7.8 osx:
cabal install cpphs alex happy hsb2hs
cabal install --reinstall --flags="embed_data_files" --ghc-options '-pgmPcpphs -optP--cpp'
cabal install --reinstall --flags="embed_data_files" pandoc-citeproc --ghc-options '-pgmPcpphs -optP--cpp'

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/man/man5
for f in $EXES; do
  cp $SANDBOX/bin/$f $DEST/bin/;
  cp $SANDBOX/share/man/man1/$f.1 $DEST/share/man/man1/
done
cp $SANDBOX/share/man/man5/pandoc_markdown.5 $DEST/share/man/man5/
cp $SCRIPTS/uninstall-pandoc.pl $DEST/bin/

chown -R $ME:staff $DIST
# gzip $DEST/share/man/man?/*.*
# cabal gives man pages the wrong permissions
chmod +r $DEST/share/man/man?/*.*

echo Copying license...
$SANDBOX/bin/pandoc --data data -t rtf -s COPYING -o $RESOURCES/License.rtf

echo Signing pandoc executable...

codesign --force --sign "$CODESIGNID" $DEST/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
spctl --assess --type execute $DEST/bin/pandoc

echo Creating OSX package...
# remove old package first
echo $PASSWORD | sudo -S rm -rf $BASE.pkg $BASE.dmg

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

echo Creating zip...
zip -9 -r $BASE.pkg.zip $BASE.pkg

# echo Creating disk image...
# sudo hdiutil create "$BASE.dmg" \
#     -format UDZO -ov \
#     -volname "pandoc $VERSION" \
#     -srcfolder $BASE.pkg
# sudo hdiutil internet-enable "$BASE.dmg"


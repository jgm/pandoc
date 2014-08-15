#!/bin/bash -e

DIST=`pwd`/osx_package
SANDBOX=`pwd`/.cabal-sandbox
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
MANDIR=`pwd`/man
DEST=$ROOT/usr/local
OSX=osx
SCRIPTS=$OSX/osx-resources
BASE=pandoc-$VERSION
ME=$(whoami)
CODESIGNID="3rd Party Mac Developer Application: John Macfarlane"
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
EXES="pandoc pandoc-citeproc"
CPPHS=$SANDBOX/bin/cpphs

read -s -p "sudo password: " PASSWORD
echo $PASSWORD | sudo -S echo "Password valid, continuing."

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

cabal sandbox init
# echo Updating database
# cabal update

echo Building pandoc...
cabal clean
# Use cpphs to avoid problems with clang cpp on ghc 7.8 osx:
cabal install cpphs hsb2hs
cabal install --ghc-options="-optl-mmacosx-version-min=10.6" --reinstall --flags="embed_data_files make-pandoc-man-pages" --ghc-options "-pgmP$CPPHS -optP--cpp"
cabal install --ghc-options="-optl-mmacosx-version-min=10.6" --reinstall --flags="embed_data_files" pandoc-citeproc --ghc-options "-pgmP$CPPHS -optP--cpp"

make man

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/man/man5
for f in $EXES; do
  cp $SANDBOX/bin/$f $DEST/bin/;
  cp $MANDIR/man1/$f.1 $DEST/share/man/man1/
done
cp $MANDIR/man5/pandoc_markdown.5 $DEST/share/man/man5/

chown -R $ME:staff $DIST

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
zip -9 -r $BASE-osx.zip $BASE.pkg
zip -9 -j -r $BASE-osx.zip $OSX/uninstall-pandoc.pl

# echo Creating disk image...
# sudo hdiutil create "$BASE.dmg" \
#     -format UDZO -ov \
#     -volname "pandoc $VERSION" \
#     -srcfolder $BASE.pkg
# sudo hdiutil internet-enable "$BASE.dmg"


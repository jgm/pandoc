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
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
CPPHS=$SANDBOX/bin/cpphs

# echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

cabal sandbox init
echo Updating database
cabal update

echo Building pandoc...
cabal clean
# Use cpphs to avoid problems with clang cpp on ghc 7.8 osx:
cabal install cpphs hsb2hs
cabal install --ghc-options="-optl-mmacosx-version-min=10.6" --reinstall --flags="embed_data_files make-pandoc-man-pages" --ghc-options "-pgmP$CPPHS -optP--cpp" . pandoc-citeproc

make man
# get pandoc-citeproc man page:
PANDOC_CITEPROC_PATH=`cabal unpack -d $DIST pandoc-citeproc | awk '{print $3;}'`
cp $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $MANDIR/man1/

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/man/man5
for f in pandoc pandoc-citeproc; do
  cp $SANDBOX/bin/$f $DEST/bin/;
  cp $MANDIR/man1/$f.1 $DEST/share/man/man1/
done
cp $MANDIR/man5/pandoc_markdown.5 $DEST/share/man/man5/

chown -R $ME:staff $DIST

echo Copying license...
$SANDBOX/bin/pandoc --data data -t html5 -s COPYING -o $RESOURCES/license.html

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

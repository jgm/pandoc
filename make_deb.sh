#!/bin/bash -e

SANDBOX=`pwd`/.cabal-sandbox
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
DEBPKGVER=?1
BASE=pandoc-$VERSION-$DEBPKGVER
DIST=`pwd`/$BASE
MANDIR=`pwd`/man
DEST=$DIST/usr/local
ME=$(whoami)

# echo Removing old files...
rm -rf $DIST

cabal sandbox init
echo Updating database
cabal update

echo Building pandoc...
cabal clean
cabal install --reinstall --flags="embed_data_files make-pandoc-man-pages" . pandoc-citeproc

make man
# get pandoc-citeproc man page:
PANDOC_CITEPROC_PATH=`cabal unpack -d make_binary_package.tmp.$$ pandoc-citeproc | awk '{print $3;}'`
strip $SANDBOX/bin/pandoc
strip $SANDBOX/bin/pandoc-citeproc
install -d $DEST/bin
install -d $DEST/share/man/man1
install -d $DEST/share/man/man5
install -d $DEST/share/doc/pandoc
install -d $DEST/share/doc/pandoc-citeproc
install $SANDBOX/bin/pandoc $DEST/bin/
install $SANDBOX/bin/pandoc-citeproc $DEST/bin/
install $MANDIR/man1/pandoc.1 $DEST/share/man/man1/
install $MANDIR/man5/pandoc_markdown.5 $DEST/share/man/man5/
install $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
install COPYING $DEST/share/doc/pandoc/COPYING
install $PANDOC_CITEPROC_PATH/LICENSE $DEST/share/doc/pandoc-citeproc/LICENSE
rm -rf $PANDOC_CITEPROC_PATH

mkdir $DIST/DEBIAN
perl -pe 's/VERSION/${VERSION}-${DEBPKGVER}/' deb/control.in > $DIST/DEBIAN/control

dpkg-deb --build $DIST

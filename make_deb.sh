#!/bin/bash -e

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

LOCAL=$HOME/.local
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
DEBPKGVER=1
DEBVER=$VERSION-$DEBPKGVER
BASE=pandoc-$DEBVER-$ARCHITECTURE
DIST=`pwd`/$BASE
DEST=$DIST/usr
ME=$(whoami)
COPYRIGHT=$DEST/share/doc/pandoc/copyright
TEMPDIR=make_binary_package.tmp.$$

# We need this for hsb2hs:
PATH=$LOCAL/bin:$PATH

stack setup
stack clean
which hsb2hs || stack install --stack-yaml stack.hsb2hs.yaml

stack install --stack-yaml deb/stack.yaml

make man/pandoc.1
# get pandoc-citeproc man page:
PANDOC_CITEPROC_VERSION=`pandoc-citeproc --version | awk '{print $2;}'`
PANDOC_CITEPROC_TARBALL=https://hackage.haskell.org/package/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}.tar.gz
mkdir $TEMPDIR
curl ${PANDOC_CITEPROC_TARBALL} | tar xzC $TEMPDIR
PANDOC_CITEPROC_PATH=$TEMPDIR/pandoc-citeproc/${PANDOC_CITEPROC_VERSION}

strip $LOCAL/bin/pandoc
strip $LOCAL/bin/pandoc-citeproc
mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/pandoc

mkdir -p $DEST/share/doc/pandoc-citeproc
find $DIST -type d | xargs chmod 755
cp $LOCAL/bin/pandoc $DEST/bin/
cp $LOCAL/bin/pandoc-citeproc $DEST/bin/
cp man/pandoc.1 $DEST/share/man/man1/pandoc.1
gzip -9 $DEST/share/man/man1/pandoc.1
cp ${PANDOC_CITEPROC_pATH}/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
gzip -9 $DEST/share/man/man1/pandoc-citeproc.1
cp COPYRIGHT $COPYRIGHT
echo "" >> $COPYRIGHT
echo "pandoc-citeproc" >> $COPYRIGHT
cat $PANDOC_CITEPROC_PATH/LICENSE >> $COPYRIGHT
rm -rf $TEMPDIR

INSTALLED_SIZE=$(du -B 1024 -s $DEST | awk '{print $1}')
mkdir $DIST/DEBIAN
perl -pe "s/VERSION/$DEBVER/" deb/control.in | \
  perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
  perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
  > $DIST/DEBIAN/control

fakeroot dpkg-deb --build $DIST
rm -rf $DIST

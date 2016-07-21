MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

LOCAL=$HOME/.local
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
DEBPKGVER=${DEBPKGVER:-1}
DEBVER=$VERSION-$DEBPKGVER
BASE=pandoc-$DEBVER-$ARCHITECTURE
DIST=`pwd`/$BASE
DEST=$DIST/usr
ME=$(whoami)
COPYRIGHT=$DEST/share/doc/pandoc/copyright

# We need this for hsb2hs:
PATH=$LOCAL/bin:$PATH

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/pandoc

stack install --install-ghc --stack-yaml deb/stack.yaml --local-bin-path . hsb2hs
stack install --install-ghc --stack-yaml deb/stack.yaml --local-bin-path . pandoc pandoc-citeproc

make man/pandoc.1
# get pandoc-citeproc man page:
PANDOC_CITEPROC_VERSION=`pandoc-citeproc --version | awk '{print $2;}'`
curl https://raw.githubusercontent.com/jgm/pandoc-citeproc/${PANDOC_CITEPROC_VERSION}/man/man1/pandoc-citeproc.1 > $DEST/share/man/man1/pandoc-citeproc.1

strip deb/pandoc
strip deb/pandoc-citeproc

mkdir -p $DEST/share/doc/pandoc-citeproc
find $DIST -type d | xargs chmod 755
cp deb/pandoc $DEST/bin/
cp deb/pandoc-citeproc $DEST/bin/
cp man/pandoc.1 $DEST/share/man/man1/pandoc.1
gzip -9 $DEST/share/man/man1/pandoc.1
gzip -9 $DEST/share/man/man1/pandoc-citeproc.1
cp COPYRIGHT $COPYRIGHT
echo "" >> $COPYRIGHT
echo "pandoc-citeproc" >> $COPYRIGHT
curl https://raw.githubusercontent.com/jgm/pandoc-citeproc/${PANDOC_CITEPROC_VERSION}/LICENSE >> $COPYRIGHT

INSTALLED_SIZE=$(du -B 1024 -s $DEST | awk '{print $1}')
mkdir $DIST/DEBIAN
perl -pe "s/VERSION/$DEBVER/" deb/control.in | \
  perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
  perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
  > $DIST/DEBIAN/control

fakeroot dpkg-deb --build $DIST
rm -rf $DIST

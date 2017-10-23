set -e

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

ARTIFACTS="${ARTIFACTS:-/artifacts}"

VERSION=`$ARTIFACTS/pandoc --version | awk '{print $2; exit;}'`
REVISION=${REVISION:-1}
DEBVER=$VERSION-$REVISION
BASE=pandoc-$DEBVER-$ARCHITECTURE
DIST=`pwd`/$BASE
DEST=$DIST/usr
COPYRIGHT=$DEST/share/doc/pandoc/copyright

PANDOC_CITEPROC_VERSION=`$ARTIFACTS/pandoc-citeproc --version | awk '{print $2;}'`

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/pandoc

mkdir -p $DEST/share/doc/pandoc-citeproc
find $DIST -type d | xargs chmod 755
cp $ARTIFACTS/pandoc $DEST/bin/
cp $ARTIFACTS/pandoc-citeproc $DEST/bin/
strip $DEST/bin/pandoc
strip $DEST/bin/pandoc-citeproc
cp man/pandoc.1 $DEST/share/man/man1/pandoc.1
$ARTIFACTS/pandoc-citeproc --man > $DEST/share/man/man1/pandoc-citeproc.1
gzip -9 $DEST/share/man/man1/pandoc.1
gzip -9 $DEST/share/man/man1/pandoc-citeproc.1

cp COPYRIGHT $COPYRIGHT
echo "" >> $COPYRIGHT
echo "pandoc-citeproc" >> $COPYRIGHT
$ARTIFACTS/pandoc-citeproc --license >> $COPYRIGHT

INSTALLED_SIZE=$(du -k -s $DEST | awk '{print $1}')
mkdir $DIST/DEBIAN
perl -pe "s/VERSION/$DEBVER/" linux/control.in | \
  perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
  perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
  > $DIST/DEBIAN/control

fakeroot dpkg-deb --build $DIST
rm -rf $DIST
cp $BASE.deb $ARTIFACTS/

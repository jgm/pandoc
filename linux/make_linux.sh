#!/bin/bash -e

ARTIFACTS=`pwd`/linux/artifacts

mkdir -p $ARTIFACTS
stack clean
stack install --stack-yaml stack.pkg.yaml \
  --flag 'pandoc:embed_data_files' \
  --local-bin-path $ARTIFACTS \
  pandoc pandoc-citeproc

# Make deb

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64) ARCHITECTURE=amd64;;
  i686)   ARCHITECTURE=i386;;
  i386)   ARCHITECTURE=i386;;
esac

VERSION=`$ARTIFACTS/pandoc --version | awk '{print $2; exit;}'`
PANDOC_CITEPROC_VERSION=`$ARTIFACTS/pandoc-citeproc --version | awk '{print $2; exit;}'`
REVISION=${REVISION:-1}
DEBVER=$VERSION-$REVISION
BASE=pandoc-$DEBVER-$ARCHITECTURE
DIST=`pwd`/$BASE
DEST=$DIST/usr
COPYRIGHT=$DEST/share/doc/pandoc/copyright


mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
mkdir -p $DEST/share/doc/pandoc

make man/pandoc.1

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

# Create tarball

TARGET=$ARTIFACTS/pandoc-$VERSION

rm -rf $TARGET
mkdir $TARGET
mkdir $TARGET/bin $TARGET/share $TARGET/share/man $TARGET/share/man/man1
$ARTIFACTS/pandoc-citeproc --man > $TARGET/share/man/man1/pandoc-citeproc.1
cp man/pandoc.1 $TARGET/share/man/man1
mv $ARTIFACTS/pandoc $ARTIFACTS/pandoc-citeproc $TARGET/bin
strip $TARGET/bin/pandoc
strip $TARGET/bin/pandoc-citeproc
gzip -9 $TARGET/share/man/man1/pandoc.1
gzip -9 $TARGET/share/man/man1/pandoc-citeproc.1
tar cvzf $TARGET.tar.gz $TARGET
rm -r $TARGET

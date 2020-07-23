set -e

ARTIFACTS="${ARTIFACTS:-/artifacts}"
VERSION=`$ARTIFACTS/pandoc --version | awk '{print $2; exit;}'`
TARGET=pandoc-$VERSION

cd $ARTIFACTS
rm -rf $TARGET
mkdir $TARGET
mkdir $TARGET/bin $TARGET/share $TARGET/share/man $TARGET/share/man/man1
./pandoc-citeproc --man > $TARGET/share/man/man1/pandoc-citeproc.1
cp /usr/src/pandoc/man/pandoc.1 $TARGET/share/man/man1
mv pandoc pandoc-citeproc $TARGET/bin
strip $TARGET/bin/pandoc
strip $TARGET/bin/pandoc-citeproc
gzip -9 $TARGET/share/man/man1/pandoc.1
gzip -9 $TARGET/share/man/man1/pandoc-citeproc.1
tar cvzf $TARGET-linux-amd64.tar.gz $TARGET
rm -r $TARGET

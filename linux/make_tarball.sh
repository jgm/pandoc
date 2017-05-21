set -e

VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
TARGET=pandoc-$VERSION

cd /artifacts
rm -rf $TARGET
mkdir $TARGET
mkdir $TARGET/bin $TARGET/share $TARGET/share/man $TARGET/share/man/man1
./pandoc-citeproc --man > $TARGET/share/man/man1/pandoc-citeproc.1
cp /usr/src/pandoc/man/pandoc.1 $TARGET/share/man/man1
mv pandoc pandoc-citeproc $TARGET/bin
gzip -9 $TARGET/share/man/man1/pandoc.1
gzip -9 $TARGET/share/man/man1/pandoc-citeproc.1
tar cvzf $TARGET.tar.gz $TARGET
rm -r $TARGET

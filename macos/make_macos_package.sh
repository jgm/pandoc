#!/bin/bash -e

LOCALBIN=$HOME/.local/bin
DIST=`pwd`/macos_package
MACOS=`pwd`/macos
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
DEST=$ROOT/usr/local
SCRIPTS=$MACOS/macos-resources
BASE=pandoc-$VERSION
ME=$(whoami)
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
DEVELOPER_ID_APPLICATION=${DEVELOPER_ID_APPLICATION:-Developer ID Application: John Macfarlane}
DEVELOPER_ID_INSTALLER=${DEVELOPER_ID_INSTALLER:-Developer ID Installer: John Macfarlane}

export MACMACOS_DEPLOYMENT_TARGET=10.7

# echo Removing old files...
rm -rf $DIST
mkdir -p $DIST
mkdir -p $RESOURCES
stack setup
which cpphs  || stack install cpphs

echo Building pandoc...
stack clean
stack install --stack-yaml=stack.pkg.yaml --local-bin-path . pandoc pandoc-citeproc

echo Getting man pages...
make man/pandoc.1

# get pandoc-citeproc man page:
PANDOC_CITEPROC_VERSION=`pandoc-citeproc --version | awk '{print $2;}'`
PANDOC_CITEPROC_TARBALL=https://hackage.haskell.org/package/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}.tar.gz
curl ${PANDOC_CITEPROC_TARBALL} | tar xzC $DIST
PANDOC_CITEPROC_PATH=$DIST/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}

mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1
for f in pandoc pandoc-citeproc; do
  cp $MACOS/$f $DEST/bin/;
done
cp $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
cp man/pandoc.1 $DEST/share/man/man1/

chown -R $ME:staff $DIST

echo Copying license...
$MACOS/pandoc --data data -t html5 -s COPYING.md -o $RESOURCES/license.html

# Removing executable signing because of a problem that arose in El Capitan
# "source=obsolete resource envelope"

#echo Signing pandoc executable...

#codesign --force --sign "${DEVELOPER_ID_APPLICATION}" $DEST/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
#spctl --assess --type execute $DEST/bin/pandoc

echo Creating MacOS package...

sed -e "s/PANDOCVERSION/$VERSION/" $MACOS/distribution.xml.in > $MACOS/distribution.xml

pkgbuild --root $DIST/pandoc --identifier net.johnmacfarlane.pandoc --version 1.13 --ownership recommended $DIST/pandoc.pkg
productbuild --distribution $MACOS/distribution.xml --resources $DIST/Resources --package-path $DIST --version $VERSION --sign "${DEVELOPER_ID_INSTALLER}" $BASE-MacOS.pkg

# verify signature
spctl --assess --type install $BASE-MacOS.pkg

# cleanup
rm -r $DIST $MACOS/pandoc $MACOS/pandoc-citeproc

#!/bin/bash -e

LOCALBIN=$HOME/.local/bin
BASEDIR=`pwd`
DIST=`pwd`/macos_package
MACOS=`pwd`/macos
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
DEST=$ROOT/usr/local
PANDOC=$DEST/bin/pandoc
SCRIPTS=$MACOS/macos-resources
ME=$(whoami)
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
DEVELOPER_ID_APPLICATION=${DEVELOPER_ID_APPLICATION:-Developer ID Application: John Macfarlane}
DEVELOPER_ID_INSTALLER=${DEVELOPER_ID_INSTALLER:-Developer ID Installer: John Macfarlane}

export MACMACOS_DEPLOYMENT_TARGET=10.7

# echo Removing old files...
rm -rf $DIST
mkdir -p $DIST
mkdir -p $RESOURCES
mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1

stack setup

echo Building pandoc...
stack clean
stack install --ghc-options="-O2" --local-bin-path $DEST/bin/ pandoc pandoc-citeproc

strip $DEST/bin/pandoc
strip $DEST/bin/pandoc-citeproc

echo Getting man pages...
make man/pandoc.1

# get pandoc-citeproc man page:
PANDOC_CITEPROC_VERSION=`$DEST/bin/pandoc-citeproc --version | awk '{print $2;exit;}'`
PANDOC_CITEPROC_TARBALL=https://hackage.haskell.org/package/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}.tar.gz
curl ${PANDOC_CITEPROC_TARBALL} | tar xzC $DIST
PANDOC_CITEPROC_PATH=$DIST/pandoc-citeproc-${PANDOC_CITEPROC_VERSION}

cp $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
cp man/pandoc.1 $DEST/share/man/man1/

chown -R $ME:staff $DIST

echo Copying license...
$PANDOC --data data -t html5 -s COPYING.md -Vpagetitle="License" -o $RESOURCES/license.html

# Removing executable signing because of a problem that arose in El Capitan
# "source=obsolete resource envelope"

#echo Signing pandoc executable...

#codesign --force --sign "${DEVELOPER_ID_APPLICATION}" $DEST/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
#spctl --assess --type execute $DEST/bin/pandoc

echo Creating macOS package...

VERSION=`$DEST/bin/pandoc --version | awk '{print $2;exit;}'`
BASE=pandoc-$VERSION

sed -e "s/PANDOCVERSION/$VERSION/" $MACOS/distribution.xml.in > $MACOS/distribution.xml

pkgbuild --root $ROOT --identifier net.johnmacfarlane.pandoc --version $VERSION --ownership recommended $DIST/pandoc.pkg
productbuild --distribution $MACOS/distribution.xml --resources $DIST/Resources --package-path $DIST --version $VERSION --sign "${DEVELOPER_ID_INSTALLER}" $BASE-macOS.pkg

# verify signature
spctl --assess --type install $BASE-macOS.pkg

echo "Created $BASE-macOS.pkg"

# create zip
cd $DEST
cd ..
mv local $BASE
zip -r $BASEDIR/$BASE-macOS.zip $BASE
cd $BASEDIR

echo "Created $BASE-macOS.zip"

# cleanup
rm -r $DIST

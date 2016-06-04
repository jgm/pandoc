#!/bin/bash -e

LOCALBIN=$HOME/.local/bin
DIST=`pwd`/osx_package
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/pandoc
DEST=$ROOT/usr/local
OSX=osx
SCRIPTS=$OSX/osx-resources
BASE=pandoc-$VERSION
ME=$(whoami)
PACKAGEMAKER=/Applications/PackageMaker.app/Contents/MacOS/PackageMaker
DEVELOPER_ID_APPLICATION=${DEVELOPER_ID_APPLICATION:-Developer ID Application: John Macfarlane}
DEVELOPER_ID_INSTALLER=${DEVELOPER_ID_INSTALLER:-Developer ID Installer: John Macfarlane}

# We need this for hsb2hs:
PATH=$LOCALBIN:$PATH
export MACOSX_DEPLOYMENT_TARGET=10.7

# echo Removing old files...
rm -rf $DIST
mkdir -p $DIST
mkdir -p $RESOURCES
stack setup
which hsb2hs || stack install --stack-yaml=stack.hsb2hs.yaml

echo Building pandoc...
stack clean
stack install --stack-yaml=osx/stack.yaml --local-bin-path osx pandoc pandoc-citeproc

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
  cp osx/$f $DEST/bin/;
done
cp $PANDOC_CITEPROC_PATH/man/man1/pandoc-citeproc.1 $DEST/share/man/man1/
cp man/pandoc.1 $DEST/share/man/man1/

chown -R $ME:staff $DIST

echo Copying license...
osx/pandoc --data data -t html5 -s COPYING -o $RESOURCES/license.html

echo Signing pandoc executable...

codesign --force --sign "${DEVELOPER_ID_APPLICATION}" $DEST/bin/pandoc
# make sure it's valid... returns nonzero exit code if it isn't:
spctl --assess --type execute $DEST/bin/pandoc

echo Creating OSX package...
# remove old package first
rm -rf $BASE.pkg

sed -e "s/PANDOCVERSION/$VERSION/" osx/distribution.xml.in > osx/distribution.xml

pkgbuild --root $DIST/pandoc --identifier net.johnmacfarlane.pandoc --version 1.13 --ownership recommended $DIST/pandoc.pkg
productbuild --distribution osx/distribution.xml --resources $DIST/Resources --package-path $DIST --version $VERSION --sign "${DEVELOPER_ID_INSTALLER}" $BASE-osx.pkg

# verify signature
spctl --assess --type install $BASE-osx.pkg

# cleanup
rm -r $DIST osx/pandoc osx/pandoc-citeproc

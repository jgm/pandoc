#!/bin/sh -e

DIST=`pwd`/osx
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
ROOT=$DIST/Package_Root
BASE=pandoc-$VERSION
PREFIX=$ROOT/usr/local

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES

echo Creating Info.plist...

cat > "$DIST/Info.plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<pkg-info version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleIdentifier</key>
    <string>net.johnmacfarlane.pandoc</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>pandoc</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>$VERSION</string>
    <key>CFBundleVersion</key>
    <string>$VERSION</string>
</dict>
</pkg-info>
EOF

echo Building pandoc...
runghc Setup.hs configure --user --prefix=/usr/local --flags="executable -library highlighting"
runghc Setup.hs build
runghc Setup.hs copy --destdir=$ROOT

cp COPYING $RESOURCES/License.txt

PACKAGEMAKER=/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker

echo Creating OSX package...

$PACKAGEMAKER \
    --title "pandoc" \
    --info "$DIST/Info.plist" \
    --root "$ROOT" \
    --resources "$RESOURCES" \
    --target "10.5" \
    --version "$VERSION" \
    --no-relocate \
    --out $BASE.pkg

echo Creating disk image...

hdiutil create "$BASE.dmg" \
    -format UDZO -ov \
    -volname "pandoc $VERSION" \
    -srcfolder $BASE.pkg


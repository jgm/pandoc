#!/bin/sh -e

DIST=`pwd`/osx
VERSION=$(grep -e '^Version' pandoc.cabal | awk '{print $2}')
RESOURCES=$DIST/Resources
SCRIPTS=$DIST/Scripts
ROOT=$DIST/Package_Root
BASE=pandoc-$VERSION
PREFIX=$ROOT/usr/local/$BASE

echo Removing old files...
rm -rf $DIST
mkdir -p $RESOURCES
mkdir -p $SCRIPTS

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
cabal install --prefix=$PREFIX -fexecutable -f-library -fhighlighting

cp COPYING $RESOURCES/License.txt

echo Creating postflight script...

PKG=/usr/local/$BASE
LOCAL=/usr/local

cat >$SCRIPTS/postflight <<EOF
#!/bin/sh -e

# Create symlinks
mkdir -p $LOCAL/bin
mkdir -p $LOCAL/share/man{1,5}
ln -f -s $PKG/bin/{pandoc,markdown2pdf} $LOCAL/bin
ln -f -s $PKG/share/man/man1/{pandoc.1,markdown2pdf.1} $LOCAL/share/man/man1
ln -f -s $PKG/share/man/man5/pandoc_markdown.5 $LOCAL/share/man/man5
EOF

chmod a+rx $SCRIPTS/postflight

PACKAGEMAKER=/Developer/Applications/Utilities/PackageMaker.app/Contents/MacOS/PackageMaker

echo Creating OSX package...

$PACKAGEMAKER \
    --title "pandoc" \
    --info "$DIST/Info.plist" \
    --root "$ROOT" \
    --resources "$RESOURCES" \
    --scripts "$SCRIPTS" \
    --target "10.5" \
    --version "$VERSION" \
    --no-relocate \
    --out $BASE.pkg

echo Creating disk image...

hdiutil create "$BASE.dmg" \
    -format UDZO -ov \
    -volname "pandoc $VERSION" \
    -srcfolder $BASE.pkg


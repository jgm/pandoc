#!/bin/sh -e

MACHINE=$(uname -m)

VERSION=$(grep '^[Vv]ersion:' pandoc.cabal | awk '{print $2;}')
ARTIFACTS=macos-$MACHINE
RESOURCES=$ARTIFACTS/Resources
ROOT=$ARTIFACTS/pandoc
DEST=$ROOT/usr/local
ME=$(whoami)

CABALOPTS="-fembed_data_files -fserver -flua"
# Build the pandoc binary and put it in .
cabal update
cabal build all $CABALOPTS
BINPATH=$(cabal list-bin $CABALOPTS pandoc-cli)
echo "Built executable..."

# Create directories
echo "Creating $ARTIFACTS directory..."
mkdir -p $ARTIFACTS
mkdir -p $RESOURCES
mkdir -p $DEST/bin
mkdir -p $DEST/share/man/man1

# Copy binary and strip it
echo "Copying executable..."
cp "$BINPATH" "$DEST/bin/"
strip "$DEST/bin/pandoc"

# Copy man pages and license
echo "Copying manuals and license..."
cp pandoc-cli/man/pandoc.1 "$DEST/share/man/man1/pandoc.1"
cp pandoc-cli/man/pandoc-server.1 "$DEST/share/man/man1/pandoc-server.1"
cp pandoc-cli/man/pandoc-lua.1 "$DEST/share/man/man1/pandoc-lua.1"
"$BINPATH" -s COPYING.md -Vpagetitle=License -o "$RESOURCES/license.html"

# Prepare distribution directory; after downloading, run 'make' to notarize
echo "Preparing distribution directory..."
chown -R "$ME:staff" "$ROOT"
sed -e "s/PANDOCVERSION/$VERSION/; s/ARCHITECTURE/$MACHINE/;" macos/distribution.xml.in > "$ARTIFACTS/distribution.xml"
cp macos/Makefile "$ARTIFACTS/"
echo "$VERSION" > "$ARTIFACTS/version.txt"
echo "$MACHINE" > "$ARTIFACTS/architecture.txt"

ls -R $ARTIFACTS
echo "Finished..."

#!/bin/bash
set -e

cabal update
cabal clean
cabal build $CABALOPTS --ghc-options="$GHCOPTS" pandoc-cli
BINPATH=$(cabal list-bin $CABALOPTS --ghc-options="$GHCOPTS" pandoc-cli)
echo "Built executable: $BINPATH"

WORK="$(pwd)"

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64)  ARCHITECTURE=amd64;;
  i686)    ARCHITECTURE=i386;;
  i386)    ARCHITECTURE=i386;;
  aarch64) ARCHITECTURE=arm64;;
  armv6l | armv7l) ARCHITECTURE=armhf;;
  *)       ARCHITECTURE=unknown;;
esac

ARTIFACTS="$WORK/linux-${ARCHITECTURE}"
echo "Creating $ARTIFACTS directory"
mkdir -p $ARTIFACTS

echo "Copying and stripping pandoc binary"
cp "$BINPATH" "$ARTIFACTS/pandoc"
strip "$ARTIFACTS/pandoc"

echo "Checking that the binary is statically linked..."
file "$ARTIFACTS/pandoc" | grep "statically linked"

echo "Checking that the binary has +lua and +server support..."
"$ARTIFACTS/pandoc" --version | grep -q '+server +lua'

echo "Checking that the binary has data files baked in..."
strings "$ARTIFACTS/pandoc" | grep -q '\$title\$'

make_deb() {
  VERSION=$("$ARTIFACTS"/pandoc --version | awk '{print $2; exit;}')
  REVISION=${REVISION:-1}
  DEBVER=$VERSION-$REVISION
  BASE=pandoc-$DEBVER-$ARCHITECTURE
  DIST=$WORK/$BASE
  DEST=$DIST/usr
  COPYRIGHT=$DEST/share/doc/pandoc/copyright

  mkdir -p "$DEST/bin"
  mkdir -p "$DEST/share/man/man1"
  mkdir -p "$DEST/share/doc/pandoc"

  find "$DIST" -type d -exec chmod 755 {} \;
  cp "$ARTIFACTS/pandoc" "$DEST/bin/"
  pushd "$DEST/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  popd
  for manpage in pandoc.1 pandoc-lua.1 pandoc-server.1
  do
    cp "$WORK/pandoc-cli/man/$manpage" "$DEST/share/man/man1/$manpage"
    gzip -9 "$DEST/share/man/man1/$manpage"
  done
  cp $WORK/COPYRIGHT "$COPYRIGHT"
  echo "" >> "$COPYRIGHT"

  INSTALLED_SIZE=$(du -k -s "$DEST" | awk '{print $1}')
  mkdir "$DIST/DEBIAN"
  perl -pe "s/VERSION/$DEBVER/" $WORK/linux/control.in | \
    perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
    perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
    > "$DIST/DEBIAN/control"

  # we limit compression to avoid OOM error
  fakeroot dpkg-deb -Zgzip -z9 --build "$DIST"
  rm -rf "$DIST"
  cp "$BASE.deb" "$ARTIFACTS/"
  echo "Created $BASE.deb"
}

# Make tarball for pandoc
make_tarball() {
  TARGET=pandoc-$VERSION
  pushd "$ARTIFACTS"
  rm -rf "$TARGET"
  mkdir "$TARGET"
  mkdir "$TARGET/bin" "$TARGET/share" "$TARGET/share/man" "$TARGET/share/man/man1"
  cp $WORK/pandoc-cli/man/pandoc.1 $WORK/pandoc-cli/man/pandoc-server.1 $WORK/pandoc-cli/man/pandoc-lua.1 "$TARGET/share/man/man1"
  gzip -9 "$TARGET"/share/man/man1/*.1
  mv pandoc "$TARGET/bin"
  pushd "$TARGET/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  popd

  tar cvzf "$TARGET-linux-$ARCHITECTURE.tar.gz" "$TARGET"
  echo "Created $TARGET-linux-$ARCHITECTURE.tar.gz"
  rm -r "$TARGET"
  popd
}

echo "Making debian package..."
make_deb

echo "Making tarball..."
make_tarball

echo "Finished!"
exit 0

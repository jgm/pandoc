#!/bin/bash
set -e

ROOT="$(pwd)"

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64)  ARCHITECTURE=amd64;;
  i686)    ARCHITECTURE=i386;;
  i386)    ARCHITECTURE=i386;;
  aarch64) ARCHITECTURE=arm64;;
  armv6l | armv7l) ARCHITECTURE=armhf;;
  *)       ARCHITECTURE=unknown;;
esac

ARTIFACTS="$ROOT/linux/artifacts"
echo "Creating $ARTIFACTS directory"
mkdir -p $ARTIFACTS

# This is our sentinel that tells us when we're done.
rm -f $ARTIFACTS/DONE

clean_up() {
  echo "All done!" > "$ARTIFACTS/DONE"
}
trap clean_up EXIT

echo "Copying and stripping pandoc binary"
cp "$ROOT/pandoc" "$ARTIFACTS/pandoc"
strip "$ARTIFACTS/pandoc"

echo "Performing sanity checks on binary..."
# Confirm that we have static builds
file "$ARTIFACTS/pandoc" | grep "statically linked"

# Confirm that it has +lua and +server support
"$ARTIFACTS/pandoc" --version | grep -q '+server +lua'

# Confirm that it has data files baked in:
strings "$ARTIFACTS/pandoc" | grep -q '\$title\$'

make_deb() {
  VERSION=$("$ARTIFACTS"/pandoc --version | awk '{print $2; exit;}')
  REVISION=${REVISION:-1}
  DEBVER=$VERSION-$REVISION
  BASE=pandoc-$DEBVER-$ARCHITECTURE
  DIST=$ROOT/$BASE
  DEST=$DIST/usr
  COPYRIGHT=$DEST/share/doc/pandoc/copyright

  mkdir -p "$DEST/bin"
  mkdir -p "$DEST/share/man/man1"
  mkdir -p "$DEST/share/doc/pandoc"

  find "$DIST" -type d -exec chmod 755 {} \;
  cp "$ARTIFACTS/pandoc" "$DEST/bin/"
  pushdir "$DEST/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  popdir
  for manpage in pandoc.1 pandoc-lua.1 pandoc-server.1
  do
    cp $ROOT/man/$manpage "$DEST/share/man/man1/$manpage"
    gzip -9 "$DEST/share/man/man1/$manpage"
  done
  cp $ROOT/COPYRIGHT "$COPYRIGHT"
  echo "" >> "$COPYRIGHT"

  INSTALLED_SIZE=$(du -k -s "$DEST" | awk '{print $1}')
  mkdir "$DIST/DEBIAN"
  perl -pe "s/VERSION/$DEBVER/" $ROOT/linux/control.in | \
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
  pushdir "$ARTIFACTS"
  rm -rf "$TARGET"
  mkdir "$TARGET"
  mkdir "$TARGET/bin" "$TARGET/share" "$TARGET/share/man" "$TARGET/share/man/man1"
  cp $ROOT/man/pandoc.1 $ROOT/man/pandoc-server.1 $ROOT/man/pandoc-lua.1 "$TARGET/share/man/man1"
  gzip -9 "$TARGET"/share/man/man1/*.1
  mv pandoc "$TARGET/bin"
  pushdir "$TARGET/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  popdir

  tar cvzf "$TARGET-linux-$ARCHITECTURE.tar.gz" "$TARGET"
  echo "Created $TARGET-linux-$ARCHITECTURE.tar.gz"
  rm -r "$TARGET"
  popdir
}

echo "Making debian package..."
make_deb

echo "Making tarball..."
make_tarball

exit 0

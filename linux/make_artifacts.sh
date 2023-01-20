#!/bin/bash
set -e

CABALOPTS="-f-export-dynamic -fembed_data_files --enable-executable-static -j4"
GHCOPTS="-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread"

MACHINE=$(uname -m)
case "$MACHINE" in
  x86_64)  ARCHITECTURE=amd64;;
  i686)    ARCHITECTURE=i386;;
  i386)    ARCHITECTURE=i386;;
  aarch64) ARCHITECTURE=arm64;;
  armv6l | armv7l) ARCHITECTURE=armhf;;
  *)       ARCHITECTURE=unknown;;
esac

ARTIFACTS="${ARTIFACTS:-/artifacts}"

# build binaries

cabal --version
ghc --version

cabal update
cabal clean
cabal build $CABALOPTS --ghc-options="$GHCOPTS" all
cabal test $CABALOPTS --ghc-options="$GHCOPTS" all

# Copy executable to ARTIFACTS
find dist-newstyle -name 'pandoc' -type f -perm /400 -exec cp {} "$ARTIFACTS"/ \;

# Strip executable
strip "$ARTIFACTS/pandoc"

# Confirm that we have static builds
file "$ARTIFACTS/pandoc" | grep "statically linked"

# Confirm that it has +lua and +server support
"./$ARTIFACTS/pandoc --version" | grep -q '+server +lua'

# Confirm that it has data files baked in:
strings "./$ARTIFACTS/pandoc" | grep -q '\$title\$'

make_deb() {
  VERSION=$("$ARTIFACTS"/pandoc --version | awk '{print $2; exit;}')
  REVISION=${REVISION:-1}
  DEBVER=$VERSION-$REVISION
  BASE=pandoc-$DEBVER-$ARCHITECTURE
  DIST=/mnt/$BASE
  DEST=$DIST/usr
  COPYRIGHT=$DEST/share/doc/pandoc/copyright

  cd /mnt
  mkdir -p "$DEST/bin"
  mkdir -p "$DEST/share/man/man1"
  mkdir -p "$DEST/share/doc/pandoc"

  find "$DIST" -type d -exec chmod 755 {} \;
  cp "$ARTIFACTS/pandoc" "$DEST/bin/"
  cd "$DEST/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  cd /mnt
  for manpage in pandoc.1 pandoc-lua.1 pandoc-server.1
  do
    cp /mnt/man/$manpage "$DEST/share/man/man1/$manpage"
    gzip -9 "$DEST/share/man/man1/$manpage"
  done
  cp /mnt/COPYRIGHT "$COPYRIGHT"
  echo "" >> "$COPYRIGHT"

  INSTALLED_SIZE=$(du -k -s "$DEST" | awk '{print $1}')
  mkdir "$DIST/DEBIAN"
  perl -pe "s/VERSION/$DEBVER/" /mnt/linux/control.in | \
    perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
    perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
    > "$DIST/DEBIAN/control"

  # we limit compression to avoid OOM error
  fakeroot dpkg-deb -Zgzip -z9 --build "$DIST"
  rm -rf "$DIST"
  cp "$BASE.deb" "$ARTIFACTS/"
}

# Make tarball for pandoc
make_tarball() {
  TARGET=pandoc-$VERSION
  cd "$ARTIFACTS"
  rm -rf "$TARGET"
  mkdir "$TARGET"
  mkdir "$TARGET/bin" "$TARGET/share" "$TARGET/share/man" "$TARGET/share/man/man1"
  cp /mnt/man/pandoc.1 /mnt/man/pandoc-server.1 /mnt/man/pandoc-lua.1 "$TARGET/share/man/man1"
  gzip -9 "$TARGET/share/man/man1/*.1"
  mv pandoc "$TARGET/bin"
  cd "$TARGET/bin"
  ln -s pandoc pandoc-server
  ln -s pandoc pandoc-lua
  cd "$ARTIFACTS"

  tar cvzf "$TARGET-linux-$ARCHITECTURE.tar.gz" "$TARGET"
  rm -r "$TARGET"
}

make_deb
make_tarball

exit 0

set -e

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

# This is our sentinel that tells us when we're done.
rm -f $ARTIFACTS/DONE

clean_up() {
  echo "All done!" > "$ARTIFACTS/DONE"
}
trap clean_up EXIT

# build binaries

cabal --version
ghc --version

cabal update
cabal clean
cabal install -j4 -fserver -f-export-dynamic -fembed_data_files --enable-executable-static --ghc-options '-j4 +RTS -A256m -RTS -split-sections -optc-Os -optl=-pthread' --install-method=copy --installdir=$ARTIFACTS
ls $ARTIFACTS

# make deb for EXE
make_deb() {
  VERSION=`$ARTIFACTS/$EXE --version | awk '{print $2; exit;}'`
  REVISION=${REVISION:-1}
  DEBVER=$VERSION-$REVISION
  BASE=$EXE-$DEBVER-$ARCHITECTURE
  DIST=/mnt/$BASE
  DEST=$DIST/usr
  COPYRIGHT=$DEST/share/doc/$EXE/copyright

  cd /mnt
  mkdir -p $DEST/bin
  mkdir -p $DEST/share/man/man1
  mkdir -p $DEST/share/doc/$EXE

  find $DIST -type d | xargs chmod 755
  cp $ARTIFACTS/$EXE $DEST/bin/
  strip $DEST/bin/$EXE
  cp /mnt/man/$EXE.1 $DEST/share/man/man1/$EXE.1
  gzip -9 $DEST/share/man/man1/$EXE.1

  cp /mnt/COPYRIGHT $COPYRIGHT
  echo "" >> $COPYRIGHT

  INSTALLED_SIZE=$(du -k -s $DEST | awk '{print $1}')
  mkdir $DIST/DEBIAN
  perl -pe "s/VERSION/$DEBVER/" /mnt/linux/$EXE.control.in | \
    perl -pe "s/ARCHITECTURE/$ARCHITECTURE/" | \
    perl -pe "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
    > $DIST/DEBIAN/control

  # we limit compression to avoid OOM error
  fakeroot dpkg-deb -Zgzip -z9 --build $DIST
  rm -rf $DIST
  cp $BASE.deb $ARTIFACTS/
}

# Make tarball for EXE
make_tarball() {
  TARGET=$EXE-$VERSION
  cd $ARTIFACTS
  rm -rf $TARGET
  mkdir $TARGET
  mkdir $TARGET/bin $TARGET/share $TARGET/share/man $TARGET/share/man/man1
  cp /mnt/man/$EXE.1 $TARGET/share/man/man1
  mv $EXE $TARGET/bin
  strip $TARGET/bin/$EXE
  gzip -9 $TARGET/share/man/man1/$EXE.1

  tar cvzf $TARGET-linux-$ARCHITECTURE.tar.gz $TARGET
  rm -r $TARGET
}

for EXE in pandoc pandoc-server
do
  make_deb
  make_tarball
done

exit 0

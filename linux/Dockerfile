# USE ALPINE LINUX
FROM alpine:3.11
RUN apk --no-cache add \
      alpine-sdk \
      bash \
      ca-certificates \
      cabal \
      dpkg \
      fakeroot \
      ghc \
      git \
      gmp-dev \
      xz \
      zlib-dev \
      zlib-static
RUN mkdir -p /usr/src/ && \
    git clone https://github.com/jgm/pandoc /usr/src/pandoc
WORKDIR /usr/src/pandoc
CMD cabal --version && \
    ghc --version && \
    git pull && \
    git checkout -b work $TREE && \
    cabal new-update && \
    cabal new-clean && \
    cabal new-configure --enable-tests -f-export-dynamic -fstatic -fembed_data_files -fbibutils --ghc-options '-optc-Os -optl=-pthread -optl=-static -fPIC -split-sections' . pandoc-citeproc && \
    cabal new-build . pandoc-citeproc && \
    cabal new-test -j1 . pandoc-citeproc && \
    for f in $(find dist-newstyle -name 'pandoc*' -type f -perm +400); do cp $f /artifacts/; done && \
    bash linux/make_deb.sh && \
    bash linux/make_tarball.sh

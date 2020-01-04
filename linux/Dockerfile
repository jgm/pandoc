# USE ALPINE LINUX
FROM alpine:3.10
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev bash dpkg fakeroot cabal
RUN mkdir -p /usr/src/
WORKDIR /usr/src/
RUN git clone https://github.com/jgm/pandoc
WORKDIR /usr/src/pandoc
RUN cabal update && \
    cabal install cabal-install-2.4.0.0 && \
    cp $HOME/.cabal/bin/cabal /usr/bin/
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

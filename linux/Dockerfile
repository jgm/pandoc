# USE ALPINE LINUX
FROM alpine
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev bash dpkg fakeroot
# GRAB A RECENT BINARY OF STACK
# Note that 1.6.5 is the last one with a static binary...
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64-static.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/local/bin '*/stack'
# COMPRESS WITH UPX
ADD https://github.com/lalyos/docker-upx/releases/download/v3.91/upx /usr/local/bin/upx
ENV PATH="/usr/local/bin:${PATH}"
RUN chmod 755 /usr/local/bin/upx
RUN ulimit -n 4096
RUN stack config set system-ghc --global true
RUN stack setup --resolver lts-12
RUN mkdir -p /usr/src/
WORKDIR /usr/src/
RUN git clone https://github.com/jgm/pandoc
WORKDIR /usr/src/pandoc
CMD git pull && \
    git checkout -b work $TREE && \
    stack install \
      --flag 'pandoc:static' \
      --flag 'pandoc:embed_data_files' \
      --flag 'pandoc-citeproc:static' \
      --flag 'pandoc-citeproc:embed_data_files' \
      --flag 'hslua:-export-dynamic' \
      --ghc-options '-O2 -optc-Os -optl=-pthread -optl=-static -fPIC' \
      --local-bin-path /artifacts \
      pandoc pandoc-citeproc && \
    bash linux/make_deb.sh && \
    bash linux/make_tarball.sh

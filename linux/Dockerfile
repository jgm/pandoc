# USE ALPINE LINUX
FROM alpine
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc gmp-dev zlib-dev bash dpkg fakeroot
# GRAB A RECENT BINARY OF STACK
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v1.9.1/stack-1.9.1-linux-x86_64-static.tar.gz | tar xvz
RUN cp stack-1.9.1-linux-x86_64-static/stack /usr/local/bin/stack
RUN chmod +x /usr/local/bin/stack
ENV PATH="/usr/local/bin:${PATH}"
RUN ulimit -n 4096
RUN stack config set system-ghc --global true
RUN stack setup --resolver lts-12.12
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

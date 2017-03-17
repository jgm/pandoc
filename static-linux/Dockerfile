# USE ALPINE LINUX
FROM alpine:edge
RUN apk update
# INSTALL BASIC DEV TOOLS, GHC, GMP & ZLIB
RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN apk update
RUN apk add alpine-sdk git ca-certificates ghc cabal stack zlib-dev
RUN stack update
RUN stack config set system-ghc --global true
RUN mkdir -p /etc/stack
RUN echo "build: { split-objs: true }" > /etc/stack/config.yaml
RUN mkdir -p /usr/src/
WORKDIR /usr/src/
RUN git clone https://github.com/jgm/pandoc
WORKDIR /usr/src/pandoc
RUN stack install --local-bin-path /usr/bin hsb2hs
RUN stack install --only-dependencies --flag 'pandoc:embed_data_files' --test --ghc-options '-O2 -optc-Os -optl-static -fPIC'
CMD git pull
CMD stack install --local-bin-path /artifacts --flag 'pandoc:embed_data_files' --test --ghc-options '-O2 -optc-Os -optl-static -fPIC'

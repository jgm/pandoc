FROM debian:jessie-slim
RUN apt-get update
RUN apt-get -y install git ca-certificates zlib1g-dev curl \
    dpkg fakeroot sed gawk grep bash xz-utils make build-essential clang \
    libgmp-dev
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /usr/bin '*/stack'
RUN mkdir -p /etc/stack
#RUN echo "build: { split-objs: true }" > /etc/stack/config.yaml
RUN mkdir -p /usr/src/
WORKDIR /usr/src/
RUN git clone https://github.com/jgm/pandoc
WORKDIR /usr/src/pandoc
RUN stack setup
RUN stack install --stack-yaml stack.pkg.yaml --only-dependencies \
    --flag 'pandoc:embed_data_files' \
    pandoc pandoc-citeproc
CMD git pull && \
    git checkout -b work $TREE && \
    ARTIFACTS=/artifacts sh make_linux.sh

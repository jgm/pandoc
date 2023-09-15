ARG BUILD_ON_IMAGE=glcr.b-data.ch/ghc/ghc-musl
ARG GHC_VERSION=latest
ARG HLS_VERSION
ARG STACK_VERSION

ARG HLS_GHC_VERSION=${HLS_VERSION:+$GHC_VERSION}
ARG HLS_SFX=/${HLS_GHC_VERSION:-all}/hls:${HLS_VERSION:-none}

ARG STACK_VERSION_OVERRIDE=${STACK_VERSION:-none}

FROM ${BUILD_ON_IMAGE}:${GHC_VERSION} as files

RUN mkdir /files

COPY conf /files
COPY scripts /files

## Ensure file modes are correct
RUN find /files -type d -exec chmod 755 {} \; \
  && find /files -type f -exec chmod 644 {} \; \
  && find /files/usr/local/bin -type f -exec chmod 755 {} \;

FROM glcr.b-data.ch/commercialhaskell/ssi:${STACK_VERSION_OVERRIDE} as ssi

FROM ${BUILD_ON_IMAGE}${HLS_SFX} as hls

FROM glcr.b-data.ch/ndmitchell/hlsi:latest as hlsi

FROM docker.io/koalaman/shellcheck:stable as sci

FROM ${BUILD_ON_IMAGE}:${GHC_VERSION}

COPY --from=files /files /

RUN sysArch="$(uname -m)" \
  ## Ensure that common CA certificates
  ## and OpenSSL libraries are up to date
  && apk upgrade --no-cache ca-certificates openssl-dev \
  ## Install pip
  && apk add --no-cache py3-pip \
  ## Install terminal multiplexers
  && apk add --no-cache screen tmux \
  ## Install yamllint
  && apk add --no-cache yamllint \
  ## Install hadolint
  && case "$sysArch" in \
    x86_64) tarArch="x86_64" ;; \
    aarch64) tarArch="arm64" ;; \
    *) echo "error: Architecture $sysArch unsupported"; exit 1 ;; \
  esac \
  && apiResponse="$(curl -sSL \
    https://api.github.com/repos/hadolint/hadolint/releases/latest)" \
  && downloadUrl="$(echo "$apiResponse" | grep -e \
    "browser_download_url.*Linux-$tarArch\"" | cut -d : -f 2,3 | tr -d \")" \
  && echo "$downloadUrl" | xargs curl -sSLo /usr/local/bin/hadolint \
  && chmod 755 /usr/local/bin/hadolint

## Update environment
ARG USE_ZSH_FOR_ROOT
ARG SET_LANG
ARG SET_TZ

ENV TZ=${SET_TZ:-$TZ} \
    LANG=${SET_LANG:-$LANG}

  ## Change root's shell to ZSH
RUN if [ -n "$USE_ZSH_FOR_ROOT" ]; then \
    apk add --no-cache zsh shadow; \
    fix-chsh.sh; \
    chsh -s /bin/zsh; \
  fi \
  ## Update timezone if needed
  && if [ "$TZ" != "" ]; then \
    apk add --no-cache tzdata; \
    echo "Setting TZ to $TZ"; \
    ln -snf "/usr/share/zoneinfo/$TZ" /etc/localtime \
      && echo "$TZ" > /etc/timezone; \
  fi \
  ## Add/Update locale if needed
  && if [ "$LANG" != "C.UTF-8" ]; then \
    if [ -n "$LANG" ]; then \
      apk add --no-cache musl-locales musl-locales-lang; \
    fi; \
    sed -i "s/LANG=C.UTF-8/LANG=$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LANG:-C.UTF-8/LANG:-$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LC_COLLATE=C/LC_COLLATE=$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LC_COLLATE:-C/LC_COLLATE:-$LANG/" /etc/profile.d/*locale.sh; \
  fi

## Copy binaries as late as possible to avoid cache busting
## Install Stack
COPY --from=ssi /usr/local /usr/local
## Install HLS
COPY --from=hls /usr/local /usr/local
## Install HLint
COPY --from=hlsi /usr/local /usr/local
## Install ShellCheck
COPY --from=sci --chown=root:root /bin/shellcheck /usr/local/bin

ARG HLS_VERSION
ARG STACK_VERSION

ARG STACK_VERSION_OVERRIDE=${STACK_VERSION}

ENV HLS_VERSION=${HLS_VERSION} \
    STACK_VERSION=${STACK_VERSION_OVERRIDE:-$STACK_VERSION}

RUN if [ "${GHC_VERSION%.*}" = "9.2" ]; then \
    if [ -f /usr/local/bin/stack ]; then \
      mv -f /usr/local/bin/stack /usr/bin/; \
    fi \
  fi

FROM ubuntu:latest AS build

ARG DEBIAN_FRONTEND=noninteractive

# install dependencies
RUN \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential

# install ghcup gpg keys
RUN \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C && \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01 && \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 && \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF

# install ghcup
RUN if [ "$TARGETPLATFORM" = "linux/amd64" ]; \
    then curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup; \
    else curl https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup > /usr/bin/ghcup; \
    fi; \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

# install GHC and cabal
ARG GHC=9.10.1
ARG CABAL=3.12.1.0

RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

WORKDIR /opt/aoc
COPY ./aoc2024.cabal ./
# Download and build deps
RUN cabal update
RUN cabal build --dependencies-only -j
COPY . ./
RUN cabal install --install-method=copy --installdir=./out
# --ghc-options="-static"

FROM ubuntu:latest
COPY --from=build /opt/aoc/out/aoc /usr/bin/aoc
RUN chmod +x /usr/bin/aoc
ENTRYPOINT [ "/usr/bin/aoc", "--serve" ]

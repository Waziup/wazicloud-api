#Build stage
# Ubuntu focal universe ships GHC 8.8.1, the compiler lts-16.18 (stack.yaml)
# expects, so --system-ghc uses the apt GHC instead of downloading one.
FROM ubuntu:20.04 AS build

ENV DEBIAN_FRONTEND=noninteractive

# liblzma-dev: the lzma Haskell package needs lzma.h and the lzma C library.
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential ca-certificates curl ghc git libbsd-dev libffi-dev \
        libgmp-dev liblzma-dev libncurses-dev libtinfo-dev libtinfo5 pkg-config \
        xz-utils zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# fpco/stack-build:lts-16.18 shipped a stack from ~2020 which cannot read
# Hackage's current TUF metadata and falls back to a retired mirror that answers
# with an XML error page:
#   Selected mirror https://s3.amazonaws.com/hackage.fpcomplete.com/
#   Could not deserialize <repo>/root.json: ... unexpected "<"
# Replace it with a current stack (stack 3.x needs GHC >= 8.4).
RUN curl -fsSL \
        https://github.com/commercialhaskell/stack/releases/download/v3.11.1/stack-3.11.1-linux-x86_64-bin \
        -o /usr/local/bin/stack \
    && chmod +x /usr/local/bin/stack \
    && stack --version

# Do not reuse a STACK_ROOT baked into old stack images or mounted by CI hosts:
# it may hold the package index and root.json cached by the broken stack.
ENV STACK_ROOT=/opt/stack-root

COPY stack.yaml                    /opt/waziup/stack.yaml
COPY waziup.cabal                  /opt/waziup/waziup.cabal
COPY keycloak-hs/stack.yaml        /opt/waziup/keycloak-hs/stack.yaml
COPY keycloak-hs/keycloak-hs.cabal /opt/waziup/keycloak-hs/keycloak-hs.cabal
COPY orion-hs/stack.yaml           /opt/waziup/orion-hs/stack.yaml
COPY orion-hs/orion-hs.cabal       /opt/waziup/orion-hs/orion-hs.cabal

WORKDIR /opt/waziup
RUN stack build --only-dependencies --system-ghc --fast

COPY src/         /opt/waziup/src
COPY main/        /opt/waziup/main
COPY test/        /opt/waziup/test
COPY migrate/     /opt/waziup/migrate
COPY orion-hs/    /opt/waziup/orion-hs/
COPY keycloak-hs/ /opt/waziup/keycloak-hs
RUN stack build --system-ghc --fast waziup:waziup-servant --copy-bins

# Deploy stage
# A bare `FROM ubuntu` resolves to 24.04, which no longer ships libtinfo5, so a
# binary built against focal (GHC 8.8.1) dies at startup with:
#   error while loading shared libraries: libtinfo.so.5: cannot open shared ...
# Pin the deploy image to the same release as the build stage.
FROM ubuntu:20.04

WORKDIR /opt/waziup
ENV PATH /usr/bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends \
        netbase ca-certificates locales libgmp10 libtinfo5 \
    && rm -rf /var/lib/apt/lists/*

# Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL en_US.UTF-8

# --copy-bins may also copy shared libraries next to the executable, keep them
# on the default PATH.
COPY --from=build /root/.local/bin /usr/local/bin/
COPY data /opt/waziup/data

CMD ["waziup-servant"]

EXPOSE 8081

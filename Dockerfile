#Build stage
FROM fpco/stack-build:lts-13.5 as build

COPY stack.yaml                    /opt/waziup/stack.yaml
COPY waziup.cabal                  /opt/waziup/waziup.cabal
COPY keycloak-hs/stack.yaml        /opt/waziup/keycloak-hs/stack.yaml
COPY keycloak-hs/keycloak-hs.cabal /opt/waziup/keycloak-hs/keycloak-hs.cabal
COPY orion-hs/stack.yaml           /opt/waziup/orion-hs/stack.yaml
COPY orion-hs/orion-hs.cabal       /opt/waziup/orion-hs/orion-hs.cabal
COPY aesonbson/                    /opt/waziup/aesonbson
COPY twitter-conduit/              /opt/waziup/twitter-conduit

WORKDIR /opt/waziup
RUN stack build --only-dependencies --system-ghc --fast

COPY src/         /opt/waziup/src
COPY orion-hs/    /opt/waziup/orion-hs/
COPY keycloak-hs/ /opt/waziup/keycloak-hs
RUN stack build --system-ghc --fast

# Deploy stage
FROM ubuntu


WORKDIR /opt/waziup
COPY --from=build /opt/waziup/.stack-work/install/x86_64-linux/lts-13.5/8.6.3/bin/waziup-servant .
COPY data /opt/waziup/data
ENV PATH /usr/bin:$PATH
RUN apt-get update && apt-get install -y netbase ca-certificates locales 

# Set the locale
RUN locale-gen en_US.UTF-8  
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

CMD /opt/waziup/waziup-servant 

EXPOSE 8081

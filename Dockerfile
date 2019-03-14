#Build stage
FROM fpco/stack-build:lts-13.5 as build

COPY stack.yaml   /opt/waziup/stack.yaml
COPY waziup.cabal /opt/waziup/waziup.cabal
COPY aesonbson/ /opt/waziup/aesonbson
COPY servant-flatten/ /opt/waziup/servant-flatten
COPY keycloak-hs/ /opt/waziup/keycloak-hs
COPY orion-hs/ /opt/waziup/orion-hs
COPY twitter-conduit/ /opt/waziup/twitter-conduit

WORKDIR /opt/waziup
RUN stack build --only-dependencies --system-ghc --fast

COPY src/ /opt/waziup/src
RUN stack build --system-ghc --fast

# Deploy stage
FROM ubuntu

WORKDIR /opt/waziup
COPY --from=build /opt/waziup/.stack-work/install/x86_64-linux/lts-13.5/8.6.3/bin/waziup-servant .
COPY data /opt/waziup/data
ENV PATH /usr/bin:$PATH
RUN apt-get update && apt-get install -y netbase ca-certificates 

CMD /opt/waziup/waziup-servant 

EXPOSE 8081

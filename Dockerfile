#Build stage
FROM fpco/stack-build:lts-16.18 as build

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
FROM ubuntu

WORKDIR /opt/waziup
ENV PATH /usr/bin:$PATH
RUN apt-get update && apt-get install -y netbase ca-certificates locales 

# Set the locale
RUN locale-gen en_US.UTF-8  
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

COPY --from=build /root/.local/bin/waziup-servant .
COPY data /opt/waziup/data

CMD /opt/waziup/waziup-servant 

EXPOSE 8081

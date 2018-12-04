FROM haskell:8.4

#RUN apk add stack --update-cache --repository http://dl-3.alpinelinux.org/alpine/edge/testing/ --allow-untrusted

COPY stack.yaml   /opt/waziup/stack.yaml
COPY waziup.cabal /opt/waziup/waziup.cabal
COPY aesonbson/ /opt/waziup/aesonbson
COPY servant-flatten/ /opt/waziup/servant-flatten

RUN apt-get update
RUN apt-get --assume-yes install liblzma-dev

WORKDIR /opt/waziup
RUN ls
RUN stack setup
RUN stack install --only-dependencies 

COPY . /opt/waziup
RUN stack install  

ENV PATH /usr/bin:$PATH
CMD stack exec --system-ghc -- waziup-servant 

EXPOSE 8081

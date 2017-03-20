FROM debian:8.7

RUN apt-get update

RUN apt-get install -y ghc \
  && apt-get install -y cabal-install

RUN cabal update

RUN cabal install network-2.6.3.1 \
  && cabal install containers-0.5.10.1 \
  && cabal install split-0.2.3.1

WORKDIR /code/

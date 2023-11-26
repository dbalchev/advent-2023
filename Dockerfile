FROM haskell:9.2.8-buster

RUN cabal update && cabal install haskell-language-server

#!/bin/bash

cabal install --only-dependencies --enable-tests --with-gcc=g++
cabal configure --enable-tests   && cabal build --with-gcc=g++


if (! cabal test --with-gcc=g++) then
    cat dist/test/*.log
    exit 1
fi

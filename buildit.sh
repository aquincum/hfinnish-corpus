#!/bin/bash

cabal install --only-dependencies --enable-tests;
cabal configure --enable-tests && cabal build


if (! cabal test) then
    cat dist/test/*.log
    exit 1
fi

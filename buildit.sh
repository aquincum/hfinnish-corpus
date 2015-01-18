#!/bin/bash

cabal install --only-dependencies --enable-tests --gcc-options -### --gcc-options -lstdc++
cabal configure --enable-tests  --gcc-options -### --gcc-options -lstdc++ && cabal build  --gcc-options -### --gcc-options -lstdc++


if (! cabal test --gcc-options -### -gcc-options -lstdc++) then
    cat dist/test/*.log
    exit 1
fi

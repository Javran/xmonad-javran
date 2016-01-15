#!/bin/sh
mkdir -p _shake
cabal exec -- ghc --make ShakeBuild.hs -rtsopts -with-rtsopts=-I0 \
    -outputdir=_shake -o _shake/build \
    && _shake/build "$@"

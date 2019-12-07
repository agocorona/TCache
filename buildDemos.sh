#!/bin/bash

# This is a crude way to build all the demo files placing them into the
# demos/bin folder and removing the build artifacts afterwards

stack build

rm -rf ./demos/bin/*

for fullfile in ./demos/*.hs; do
  filename=$(basename -- "$fullfile")
  binname="${filename%.*}"
  echo "$filename -> $binname"
  rm -rf ./demos/.build/Main.o
  stack exec ghc -- -O2 -threaded -with-rtsopts=-N -outputdir demos/.build $fullfile -o demos/bin/$binname
done

#  rm -rf ./demos/.build

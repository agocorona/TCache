#!/bin/bash

# This is a crude way to build all the demo files placing them into the
# demos/bin folder and removing the build artifacts afterwards

rm -rf ./demos/.build
rm -rf ./demos/bin/*

for fullfile in ./demos/*.hs; do
  filename=$(basename -- "$fullfile")
  binname="${filename%.*}"
  echo "$filename -> $binname"
  stack exec ghc -- -outputdir demos/.build $fullfile -o demos/bin/$binname
done

rm -rf ./demos/.build

#!/bin/zsh

setopt extendedglob

stack build || exit 1

for d in \
    ../07/*/* \
    ../08/*/*; do
  stack exec vm-translator $d
  ../../tools/CPUEmulator.sh $d/*.tst~*VME.tst
done

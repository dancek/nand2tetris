#!/bin/zsh

stack build || exit 1

for f in ../07/**/*.vm ../08/ProgramFlow/**/*.vm; do
  base=$(dirname $f)/$(basename $f .vm)
  echo $base.asm
  stack exec vm-translator < $f > $base.asm
  ../../tools/CPUEmulator.sh $base.tst
done

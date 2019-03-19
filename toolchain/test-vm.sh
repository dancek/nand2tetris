#!/bin/zsh

setopt extendedglob

stack build || exit 1

for d in ../08/FunctionCalls/*~SimpleFunction; do
  echo $d
  stack exec vm-translator $d
  ../../tools/CPUEmulator.sh $d/*.tst~*VME.tst
done

for f in \
    ../08/FunctionCalls/SimpleFunction/SimpleFunction.vm \
    ../07/**/*.vm \
    ../08/ProgramFlow/**/*.vm; do
  base=$(dirname $f)/$(basename $f .vm)
  echo $base.asm
  stack exec vm-translator < $f > $base.asm
  ../../tools/CPUEmulator.sh $base.tst
done

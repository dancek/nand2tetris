#!/bin/zsh
set -e

stack build

for f in ../07/**/*.vm; do
  echo $f
  stack exec vm-translator < $f > $(dirname $f)/$(basename $f .vm).asm
done

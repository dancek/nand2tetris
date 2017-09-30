#!/bin/sh

stack build

mkdir -p out
for asm in ref/asm/*.asm
do
    name=$(basename $asm .asm)
    stack exec assembler < $asm > out/$name.hack
    diff -q out/$name.hack ref/asm/$name.hack
done

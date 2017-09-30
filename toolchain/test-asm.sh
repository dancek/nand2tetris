#!/bin/sh

for asm in ref/asm/*.asm
do
    stack exec assembler < $asm | diff -q - ref/asm/$(basename $asm .asm).hack
done

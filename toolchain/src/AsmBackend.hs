module AsmBackend where

{-
Generate assembly code from VM AST.
-}

import VMAST

vmCodegen = Nothing

pushValue :: MemorySegment -> Integer -> [String]
pushValue ms i = loadValue ms i ++ pushRegD

-- Load value from given memory segment to the D register
loadValue :: MemorySegment -> Integer -> [String]
loadValue MConstant i = [
  "@" ++ show i,
  "D=A"]
loadValue memseg i = [
  "@" ++ segmentSymbol memseg,
  "D=A",
  "@" ++ show i,
  "A=A+D",
  "D=M"]

segmentSymbol MLocal = "LCL"
segmentSymbol MArgument = "ARG"
segmentSymbol MThis = "THIS"
segmentSymbol MThat = "THAT"

-- Push register D content
pushRegD :: [String]
pushRegD = [
  "@SP",
  "A=M",
  "M=D",
  "@SP",
  "M=M+1"]

popValue :: MemorySegment -> Integer -> [String]
popValue memseg i = [
  -- addr = seg + i
  "@" ++ segmentSymbol memseg,
  "D=A",
  "@" ++ show i,
  "D=A+D",
  -- *SP = addr
  "@SP",
  "A=M",
  "M=D",
  -- SP--
  "@SP",
  "M=M-1",
  -- *addr = *SP, ie. *(SP+1) = *SP
  "A=M",
  "D=M",
  "A=A+1",
  "M=D"]


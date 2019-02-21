module AsmBackend where

{-
Generate assembly code from VM AST.
-}

import VMAST

type AsmInstruction = String

vmCodegen :: Program -> [AsmInstruction]
vmCodegen = concatMap code

code :: Command -> [AsmInstruction]
code (CMemory (CPush ms i)) = pushValue ms i
code (CMemory (CPop ms i)) = popValue ms i
code (CArithmetic cmd) = arithmetic cmd

pushValue :: MemorySegment -> Integer -> [AsmInstruction]
pushValue ms i = loadValue ms i ++ pushRegD

-- Load value from given memory segment to the D register
loadValue :: MemorySegment -> Integer -> [AsmInstruction]
loadValue MConstant i = [
  "@" ++ show i,
  "D=A"]
loadValue MTemp i = [
  "@" ++ show (5 + i),
  "D=M"]
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
pushRegD :: [AsmInstruction]
pushRegD = [
  "@SP",
  "A=M",
  "M=D",
  "@SP",
  "M=M+1"]

popValue :: MemorySegment -> Integer -> [AsmInstruction]
popValue MTemp i = [
  -- SP--
  "@SP",
  "M=M-1",
  -- D = *SP
  "A=M",
  "D=M",
  -- tmp[i] = D
  "@" ++ show (5+i),
  "M=D"]
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

arithmetic :: ArithmeticCommand -> [AsmInstruction]
arithmetic CAdd = stackTopTwo "+"
arithmetic CSub = stackTopTwo "-"
arithmetic CAnd = stackTopTwo "&"
arithmetic COr = stackTopTwo "|"

arithmetic CNeg = stackTop "-"
arithmetic CNot = stackTop "!"

arithmetic CEq = testTopTwo "JEQ"
arithmetic CGt = testTopTwo "JGT"
arithmetic CLt = testTopTwo "JLT"


-- Manipulate stack top value
stackTop operator = [
  "@SP",
  "A=M-1",
  "M=" ++ operator ++ "M"]

-- Decrement stack pointer, put old top in D and current in M. Then apply operator.
stackTopTwo operator = [
  "@SP",
  "M=M-1",
  "A=M",
  "D=M",
  "A=A-1",
  "M=M" ++ operator ++ "D"]

-- run logical operation on top two
testTopTwo jump = [
  "@SP",
  "M=M-1",
  "A=M",
  "D=M",
  "A=A-1",
  "D=M-D",
  "@T" ++ testLabel,
  "D;" ++ jump,
  "@F" ++ testLabel,
  "D=0;JMP",
  "(T" ++ testLabel ++ ")",
  "D=-1",
  "(F" ++ testLabel ++ ")",
  "@SP",
  "A=M-1",
  "M=D"]

testLabel = "0" -- FIXME: make this stateful
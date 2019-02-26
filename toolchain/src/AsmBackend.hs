module AsmBackend where

{-
Generate assembly code from VM AST.
-}

import Control.Monad.Trans.State

import VMAST

-- memory related constants
tmpBaseAddr = 5
staticNamespace = "Static" -- TODO: use filename?

type AsmInstruction = String
type CodegenState = State Int

vmCodegen :: Program -> [AsmInstruction]
vmCodegen prog = evalState (statefulCodegen prog) 0

statefulCodegen :: Program -> CodegenState [AsmInstruction]
statefulCodegen [] = return []
statefulCodegen (cmd:cmds) = do
  n <- get
  put (n+1)
  rest <- statefulCodegen cmds
  return $ (code n cmd) ++ rest

code :: Int -> Command-> [AsmInstruction]
code _ (CMemory (CPush ms i)) = pushValue ms i
code _ (CMemory (CPop ms i)) = popValue ms i
code n (CArithmetic cmd) = arithmetic n cmd
code _ (CBranching cmd) = branching cmd

pushValue :: MemorySegment -> Integer -> [AsmInstruction]
pushValue ms i = loadValue ms i ++ pushRegD

-- Load value from given memory segment to the D register
loadValue :: MemorySegment -> Integer -> [AsmInstruction]
loadValue MTemp i = loadAddress $ show (5 + i)
loadValue MStatic i = loadAddress $ staticNamespace ++ "." ++ show i
loadValue MPointer 0 = loadAddress $ segmentSymbol MThis
loadValue MPointer 1 = loadAddress $ segmentSymbol MThat
loadValue MConstant i = [
  "@" ++ show i,
  "D=A"]
loadValue memseg i = [
  "@" ++ segmentSymbol memseg,
  "D=M",
  "@" ++ show i,
  "A=A+D",
  "D=M"]

loadAddress addr = [
  "@" ++ addr,
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
popValue MTemp i = popAddress $ show $ tmpBaseAddr + i
popValue MStatic i = popAddress $ staticNamespace ++ "." ++ show i
popValue MPointer 0 = popAddress $ segmentSymbol MThis
popValue MPointer 1 = popAddress $ segmentSymbol MThat
popValue memseg i = [
  -- addr = seg + i
  "@" ++ segmentSymbol memseg,
  "D=M",
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
  "A=M",
  "M=D"]

popAddress addr = [
  -- SP--
  "@SP",
  "M=M-1",
  -- D = *SP
  "A=M",
  "D=M",
  -- addr = D
  "@" ++ addr,
  "M=D"]


arithmetic :: Int -> ArithmeticCommand -> [AsmInstruction]
arithmetic _ CAdd = stackTopTwo "+"
arithmetic _ CSub = stackTopTwo "-"
arithmetic _ CAnd = stackTopTwo "&"
arithmetic _ COr = stackTopTwo "|"

arithmetic _ CNeg = stackTop "-"
arithmetic _ CNot = stackTop "!"

arithmetic n CEq = testTopTwo n "JEQ"
arithmetic n CGt = testTopTwo n "JGT"
arithmetic n CLt = testTopTwo n "JLT"


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
testTopTwo n jump =
  let label = show n in [
    "@SP",
    "M=M-1",
    "A=M",
    "D=M",
    "A=A-1",
    "D=M-D",
    "@T" ++ label,
    "D;" ++ jump,
    "@F" ++ label,
    "D=0;JMP",
    "(T" ++ label ++ ")",
    "D=-1",
    "(F" ++ label ++ ")",
    "@SP",
    "A=M-1",
    "M=D"]

branching :: BranchingCommand -> [AsmInstruction]
branching (CLabel label) = ["(" ++ label ++ ")"]
branching (CGoto label) = [
  "@" ++ label,
  "0;JMP"]
branching (CIfGoto label) = [
  -- SP--
  "@SP",
  "M=M-1",
  -- D = *SP
  "A=M",
  "D=M",
  -- jump if D
  "@" ++ label,
  "D;JNE"]

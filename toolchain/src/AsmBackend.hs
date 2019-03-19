module AsmBackend where

{-
Generate assembly code from VM AST.
-}

import Control.Monad.Trans.State

import VMAST

-- memory related constants
tmpBaseAddr = 5

type AsmInstruction = String
type CodegenState = State Int

vmMain :: [AsmInstruction]
vmMain =
  [ "@261"
  , "D=A"
  , "@SP"
  , "M=D"
  , "@" ++ functionLabel "Sys.init"
  , "0;JMP"]

vmCodegen :: String -> Program -> [AsmInstruction]
vmCodegen filename prog = evalState (statefulCodegen filename prog) 0

statefulCodegen :: String -> Program -> CodegenState [AsmInstruction]
statefulCodegen _ [] = return []
statefulCodegen filename (cmd:cmds) = do
  n <- get
  put (n+1)
  rest <- statefulCodegen filename cmds
  return $
    ["// " ++ show cmd] ++ -- for debugging
    (code filename n cmd) ++ rest

code :: String -> Int -> Command-> [AsmInstruction]
code f _ (CMemory (CPush ms i)) = pushValue f ms i
code f _ (CMemory (CPop ms i)) = popValue f ms i
code _ n (CArithmetic cmd) = arithmetic n cmd
code _ _ (CBranching cmd) = branching cmd
code _ n (CFun cmd) = functionCmd n cmd

pushValue :: String -> MemorySegment -> Integer -> [AsmInstruction]
pushValue f ms i = loadValue f ms i ++ pushRegD

-- Load value from given memory segment to the D register
loadValue :: String -> MemorySegment -> Integer -> [AsmInstruction]
loadValue _ MTemp i = loadAddress $ show (tmpBaseAddr + i)
loadValue f MStatic i = loadAddress $ f ++ "$static." ++ show i
loadValue _ MPointer 0 = loadAddress $ segmentSymbol MThis
loadValue _ MPointer 1 = loadAddress $ segmentSymbol MThat
loadValue _ MConstant i = [
  "@" ++ show i,
  "D=A"]
loadValue _ memseg i = [
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

popValue :: String -> MemorySegment -> Integer -> [AsmInstruction]
popValue _ MTemp i = popAddress $ show $ tmpBaseAddr + i
popValue f MStatic i = popAddress $ f ++ "$static." ++ show i
popValue _ MPointer 0 = popAddress $ segmentSymbol MThis
popValue _ MPointer 1 = popAddress $ segmentSymbol MThat
popValue _ memseg i = [
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

functionCmd :: Int -> FunctionCommand -> [AsmInstruction]
functionCmd linenum (CCall f nArgs) = (
  let returnLabel = "return_" ++ show linenum in
    -- push returnLabel
    [ "@" ++ returnLabel
    , "D=A"
    ] ++ pushRegD
    -- push LCL, ARG, THIS, THAT
    ++ pushSegmentPointer MLocal
    ++ pushSegmentPointer MArgument
    ++ pushSegmentPointer MThis
    ++ pushSegmentPointer MThat
    -- ARG = SP - 5 - nArgs
    ++ [ "@SP"
       , "D=M"
       , "@" ++ show (5 + nArgs)
       , "D=D-A"
       , "@" ++ segmentSymbol MArgument
       , "M=D"
    -- LCL = SP
       , "@SP"
       , "D=M"
       , "@" ++ segmentSymbol MLocal
       , "M=D"
    -- goto f
       , "@" ++ functionLabel f
       , "0;JMP"
    -- label
       , "(" ++ returnLabel ++ ")"
       ]
  )

functionCmd _ (CFunction f nVars) = (
  [ "(" ++ functionLabel f ++ ")" ]
  ++ concat (replicate (fromIntegral nVars) (pushValue "" MConstant 0))
  )

functionCmd _ CReturn =
  -- use TEMP[6] and TEMP[7] for temp vars
  let endFrame = show $ tmpBaseAddr + 6
      retAddr  = show $ tmpBaseAddr + 7 in (
    -- endFrame = LCL
    [ "@" ++ segmentSymbol MLocal
    , "D=M"
    , "@" ++ endFrame
    , "M=D"
    -- retAddr = *(endFrame - 5)
    , "@5"
    , "A=D-A"
    , "D=M"
    , "@" ++ retAddr
    , "M=D"
    ] ++
    -- *ARG = pop()
    popValue "" MArgument 0 ++
    -- SP = ARG+1
    [ "@" ++ segmentSymbol MArgument
    , "D=M+1"
    , "@SP"
    , "M=D"
    ] ++
    -- reset state to what it was before call
    loadSegmentPointer MThat endFrame ++
    loadSegmentPointer MThis endFrame ++
    loadSegmentPointer MArgument endFrame ++
    loadSegmentPointer MLocal endFrame ++
    -- goto retAddr
    [ "@" ++ retAddr
    , "A=M"
    , "0;JMP"
    ]
  )

pushSegmentPointer :: MemorySegment -> [AsmInstruction]
pushSegmentPointer memseg = [
  "@" ++ segmentSymbol memseg,
  "D=M"] ++ pushRegD

loadSegmentPointer :: MemorySegment -> String -> [AsmInstruction]
-- NOTE: decrements *endFrame
loadSegmentPointer memseg endFrame =
  [ "@" ++ endFrame
  , "M=M-1"
  , "A=M"
  , "D=M"
  , "@" ++ segmentSymbol memseg
  , "M=D"
  ]

functionLabel f = "function_" ++ f

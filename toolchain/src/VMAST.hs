module VMAST where

data Command =
  CArithmetic ArithmeticCommand |
  CMemory MemoryCommand |
  CBranching BranchingCommand |
  CFun FunctionCommand deriving (Show)

data ArithmeticCommand =
  CAdd | CSub | CNeg |
  CEq | CGt | CLt |
  CAnd | COr | CNot deriving (Show)

data MemoryCommand =
  CPush MemorySegment Integer |
  CPop MemorySegment Integer deriving (Show)

data BranchingCommand =
  CLabel Label |
  CGoto Label |
  CIfGoto Label deriving (Show)

data FunctionCommand =
  CCall Function Integer |
  CFunction Function Integer |
  CReturn deriving (Show)

data MemorySegment =
  MArgument | MLocal | MStatic | MConstant |
  MThis | MThat | MPointer | MTemp deriving (Show)

type Program = [Command]
type Label = String
type Function = String

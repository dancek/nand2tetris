module VMAST where

data Command =
  CArithmetic ArithmeticCommand |
  CMemory MemoryCommand deriving (Show)

data ArithmeticCommand =
  CAdd | CSub | CNeg |
  CEq | CGt | CLt |
  CAnd | COr | CNot deriving (Show)

data MemoryCommand =
  CPush MemorySegment Integer |
  CPop MemorySegment Integer deriving (Show)

data MemorySegment =
  MArgument | MLocal | MStatic | MConstant |
  MThis | MThat | MPointer | MTemp deriving (Show)

type Program = [Command]

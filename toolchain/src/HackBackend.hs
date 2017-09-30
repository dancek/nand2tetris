module HackBackend (codegen) where

{- 
Generate "machine code" for the Hack machine as specified in the Nand2Tetris
materials. Basically supports the Haskell data representation (AST.hs) of
the assembly language.

In practice, we output the instructions one per line as plain text 0/1 bits.
That's what the provided tools expect.
-}
import Text.Printf

import AST

type MachineInstruction = String

codegen :: Program -> [MachineInstruction]
codegen = fmap code

code :: Instruction -> MachineInstruction
code (AInstr (ANum n)) = printf "0%015b" n
code (CInstr dest comp jump) = "111" ++ compBits comp ++ destBits dest ++ jumpBits jump
code _ = "unknown_instruction"

destBits Nothing = "000"
destBits (Just regs) = [bit RegA, bit RegD, bit RegM]
    where bit r = (if r `elem` regs then '1' else '0')

jumpBits Nothing = "000"
jumpBits (Just jmp) = case jmp of
    Jlt -> "100"
    Jle -> "110"
    Jmp -> "111"
    Jge -> "011"
    Jgt -> "001"
    Jeq -> "010"
    Jne -> "101"

-- Instruction table. This is ugly but there's no simple logic to it.
compBits :: CComp -> String
compBits (CValue Zero)              = "0101010"
compBits (CValue One)               = "0111111"
compBits (CValue MinusOne)          = "0111010"
compBits (CReg RegD)                = "0001100"
compBits (CReg RegA)                = "0110000"
compBits (CReg RegM)                = "1110000"
compBits (CUnary Not RegD)          = "0001101"
compBits (CUnary Not RegA)          = "0110001"
compBits (CUnary Not RegM)          = "1110001"
compBits (CUnary Negate RegD)       = "0001111"
compBits (CUnary Negate RegA)       = "0110011"
compBits (CUnary Negate RegM)       = "1110011"
compBits (CUnary Incr RegD)         = "0011111"
compBits (CUnary Incr RegA)         = "0110111"
compBits (CUnary Incr RegM)         = "1110111"
compBits (CUnary Decr RegD)         = "0001110"
compBits (CUnary Decr RegA)         = "0110010"
compBits (CUnary Decr RegM)         = "1110010"
compBits (CBinary Add RegD RegA)    = "0000010"
compBits (CBinary Add RegA RegD)    = "0000010"
compBits (CBinary Add RegD RegM)    = "1000010"
compBits (CBinary Add RegM RegD)    = "1000010"
compBits (CBinary Sub RegD RegA)    = "0010011"
compBits (CBinary Sub RegD RegM)    = "1010011"
compBits (CBinary Sub RegA RegD)    = "0000111"
compBits (CBinary Sub RegM RegD)    = "1000111"
compBits (CBinary And RegD RegA)    = "0000000"
compBits (CBinary And RegA RegD)    = "0000000"
compBits (CBinary And RegD RegM)    = "1000000"
compBits (CBinary And RegM RegD)    = "1000000"
compBits (CBinary Or RegD RegA)     = "0010101"
compBits (CBinary Or RegA RegD)     = "0010101"
compBits (CBinary Or RegD RegM)     = "1010101"
compBits (CBinary Or RegM RegD)     = "1010101"

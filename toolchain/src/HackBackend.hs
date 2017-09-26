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
code (AInstr (ANum n)) = printf "1%015b" n
code (CInstr dest comp jump) = "111" ++ compBits comp ++ destBits dest ++ jumpBits jump
code _ = "unknown_instruction"

-- FIXME: get correct computation bits
compBits _ = "0000000"

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
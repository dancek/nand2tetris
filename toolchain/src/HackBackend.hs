module HackBackend (codegen) where

{- 
Generate "machine code" for the Hack machine as specified in the Nand2Tetris
materials. Basically supports the Haskell data representation (AST.hs) of
the assembly language.

In practice, we output the instructions one per line as plain text 0/1 bits.
That's what the provided tools expect.
-}
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (find, nub)

import AST

type MachineInstruction = String
type SymbolTable = [(String, Int)]

codegen :: Program -> [MachineInstruction]
codegen prog = catMaybes $ fmap (code $ symbolTable prog) prog


-- SYMBOL TABLE

baseSymbols :: SymbolTable
baseSymbols = [ ("SP", 0),
                ("LCL", 1),
                ("ARG", 2),
                ("THIS", 3),
                ("THAT", 4),
                ("SCREEN", 16384),
                ("KBD", 24576)]

registers :: SymbolTable
registers = [("R" ++ show n, n) | n <- [0..15]]

labels :: Program -> SymbolTable
labels prog = let
    -- instruction positions (don't increment counter for labels)
    instrPos :: Int -> Program -> [(Instruction, Int)]
    instrPos _ [] = []
    instrPos n (Label l : rest) = (Label l, n) : instrPos n rest
    instrPos n (instr : rest) = (instr, n) : instrPos (n+1) rest
    in [(l, pos) | (Label l, pos) <- instrPos 0 prog]

-- vars are the symbols that aren't labels or reserved symbols
variables :: Program -> SymbolTable -> SymbolTable
variables prog tbl = let
    refs = [(ref, lookup ref tbl) | AInstr (ARef ref) <- prog]
    vars = nub [x | (x, Nothing) <- refs]
    in zip vars [0x0010..]

-- all pieces of the symbol table, combined
symbolTable :: Program -> SymbolTable
symbolTable prog = let
    tbl = baseSymbols ++ registers ++ labels prog
    varTbl = variables prog tbl
    in tbl ++ varTbl


-- MACHINE CODE

-- translate a single instruction
code :: SymbolTable -> Instruction -> Maybe MachineInstruction
code _ (AInstr (ANum n)) = Just $ printf "0%015b" n
code tbl (AInstr (ARef ref)) = case lookup ref tbl of
    Just n -> Just $ printf "0%015b" n
    Nothing -> Just $ "unknown label " ++ ref   -- FIXME: proper error handling
code _ (CInstr dest comp jump) = Just $ "111" ++ compBits comp ++ destBits dest ++ jumpBits jump
code _ (Label _) = Nothing


destBits :: CDest -> String
destBits Nothing = "000"
destBits (Just regs) = [bit RegA, bit RegD, bit RegM]
    where bit r = (if r `elem` regs then '1' else '0')

jumpBits :: CJump -> String
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

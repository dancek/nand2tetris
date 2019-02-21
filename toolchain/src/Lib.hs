module Lib (assembler) where

import Text.Megaparsec
import Data.Void

import AsmParser (asmParser)
import HackBackend (asmCodegen)
import VMParser (vmParser)
import AsmBackend (vmCodegen)

assembler :: String -> Either (ParseErrorBundle String Void) String
assembler asm = case runParser asmParser "" asm of
    Left e          -> Left e
    Right result    -> Right $ unlines $ asmCodegen result

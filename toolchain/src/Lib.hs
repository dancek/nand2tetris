module Lib
    ( assembler
    , parser
    , codegen
    ) where

import Text.Megaparsec
import Data.Void

import AsmParser (parser)
import HackBackend (codegen)

assembler :: String -> Either (ParseErrorBundle String Void) String
assembler asm = case runParser parser "" asm of
    Left e          -> Left e
    Right result    -> Right $ unlines $ codegen result

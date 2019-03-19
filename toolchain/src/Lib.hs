module Lib (assembler, vmTranslator, vmMain) where

import Text.Megaparsec
import Data.Void

import AsmParser (asmParser)
import HackBackend (asmCodegen)
import VMParser (vmParser)
import AsmBackend (vmCodegen, vmMain)

type ParsingError = (ParseErrorBundle String Void)
type Compiler = String -> Either ParsingError String

assembler :: Compiler
assembler = buildCompiler asmParser asmCodegen

vmTranslator :: String -> Compiler
vmTranslator filename = buildCompiler vmParser (vmCodegen filename)

buildCompiler :: Parsec Void String program ->
                 (program -> [String]) ->
                 Compiler
buildCompiler parser codegen sourceCode =
    case runParser parser "" sourceCode of
        Left e          -> Left e
        Right result    -> Right $ unlines $ codegen result

module VMParser (vmParser) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import LineLexing
import VMAST

type Parser = Parsec Void String

vmParser :: Parser Program
vmParser = between scn eof commandSeq

commandSeq :: Parser Program
commandSeq = sepEndBy command scn

command :: Parser Command
command = CArithmetic <$> arithmeticCommand <|>
  CMemory <$> memoryCommand <|>
  CBranching <$> branchingCommand <|>
  CFun <$> functionCommand

arithmeticCommand :: Parser ArithmeticCommand
arithmeticCommand = symbolToData [
  ("add", CAdd),
  ("sub", CSub),
  ("neg", CNeg),
  ("eq" , CEq),
  ("gt" , CGt),
  ("lt" , CLt),
  ("and", CAnd),
  ("or" , COr),
  ("not", CNot)]

memoryCommand :: Parser MemoryCommand
memoryCommand =
  symbolToData [
    ("push", CPush),
    ("pop", CPop)]
  <*> symbolToData [
    ("argument", MArgument),
    ("local", MLocal),
    ("static", MStatic),
    ("constant", MConstant),
    ("this", MThis),
    ("that", MThat),
    ("pointer", MPointer),
    ("temp", MTemp)]
  <*> integer

branchingCommand :: Parser BranchingCommand
branchingCommand =
  symbolToData [
    ("label", CLabel),
    ("goto", CGoto),
    ("if-goto", CIfGoto)]
  <*> labelName

labelName :: Parser Label
labelName = lexeme $ many (upperChar <|> char '_')

functionCommand :: Parser FunctionCommand
functionCommand =
  symbolToData [("return", CReturn)]
  <|> symbolToData [
    ("function", CFunction),
    ("call", CCall)]
  <*> functionName
  <*> integer

functionName :: Parser Function
functionName = lexeme $ many (letterChar <|> char '_' <|> char '.' <|> char '$')

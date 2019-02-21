module VMParser (parser) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import LineLexing
import VMAST

type Parser = Parsec Void String

parser :: Parser Program
parser = between scn eof commandSeq

commandSeq :: Parser Program
commandSeq = sepEndBy command scn

command :: Parser Command
command = CArithmetic <$> arithmeticCommand <|>
  CMemory <$> memoryCommand

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

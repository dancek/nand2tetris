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
arithmeticCommand = ar <$>
  (symbol "add"
  <|> symbol "sub"
  <|> symbol "neg"
  <|> symbol "eq"
  <|> symbol "gt"
  <|> symbol "lt"
  <|> symbol "and"
  <|> symbol "or"
  <|> symbol "not")

ar "add" = CAdd
ar "sub" = CSub
ar "neg" = CNeg
ar "eq" = CEq
ar "gt" = CGt
ar "lt" = CLt
ar "and" = CAnd
ar "or" = COr
ar "not" = CNot

memoryCommand :: Parser MemoryCommand
memoryCommand = mcmd <$> (symbol "push" <|> symbol "pop")
  <*> (mseg <$> (symbol "argument"
                <|> symbol "local"
                <|> symbol "static"
                <|> symbol "constant"
                <|> symbol "this"
                <|> symbol "that"
                <|> symbol "pointer"
                <|> symbol "temp"))
  <*> integer

mcmd "push" = CPush
mcmd "pop" = CPop

mseg "argument" = MArgument
mseg "local" = MLocal
mseg "static" = MStatic
mseg "constant" = MConstant
mseg "this" = MThis
mseg "that" = MThat
mseg "pointer" = MPointer
mseg "temp" = MTemp

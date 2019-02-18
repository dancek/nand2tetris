module AsmParser (parser) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AsmAST
import LineLexing

type Parser = Parsec Void String


identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many restChar)
    where
        firstChar = letterChar
        restChar = (alphaNumChar <|> char '_' <|> char '.' <|> char '$')


parser :: Parser Program
parser = between scn eof instructionSeq

instructionSeq :: Parser Program
instructionSeq = sepEndBy instruction scn

instruction :: Parser Instruction
instruction = aInstr <|> cInstr <|> AsmParser.label

aInstr :: Parser Instruction
aInstr = AInstr <$> (symbol "@" *> (aNum <|> aRef))

aNum :: Parser AValue
aNum = ANum <$> integer

aRef :: Parser AValue
aRef = ARef <$> identifier


cInstr :: Parser Instruction
cInstr = CInstr <$> optional cDest <*> cComp <*> optional cJump

cDest :: Parser [Reg]
cDest = try (some reg <* symbol "=")

reg :: Parser Reg
reg = regFromName <$> (symbol "A" <|> symbol "M" <|> symbol "D")

regFromName :: String -> Reg
regFromName "A" = RegA
regFromName "M" = RegM
regFromName "D" = RegD

cComp :: Parser CComp
cComp = unaryOp <|> binaryOp <|> cValue <|> cReg

cValue :: Parser CComp
cValue = CValue <$> rawNumber <$> (symbol "-1" <|> symbol "0" <|> symbol "1")

rawNumber :: String -> RawNumber
rawNumber "-1" = MinusOne
rawNumber "0" = Zero
rawNumber "1" = One

cReg :: Parser CComp
cReg = fmap CReg reg

unaryOp :: Parser CComp
unaryOp = negOp <|> notOp <|> incrOp <|> decrOp

prefixOp :: String -> UnaryOp -> Parser CComp
prefixOp sym op = CUnary op <$> (symbol sym *> reg)

postfixOp :: String -> String -> UnaryOp -> Parser CComp
postfixOp sym1 sym2 op = try (CUnary op <$> (reg <* symbol sym1 <* symbol sym2))

negOp = try $ prefixOp "-" Negate  -- try needed because "-1" also exists
notOp = prefixOp "!" Not
incrOp = postfixOp "+" "1" Incr
decrOp = postfixOp "-" "1" Decr

binaryOp :: Parser CComp
binaryOp = addOp <|> subOp <|> andOp <|> orOp

binOp :: String -> BinaryOp -> Parser CComp
binOp sym op = try (CBinary op <$> reg <* symbol sym <*> reg)

addOp = binOp "+" Add
subOp = binOp "-" Sub
andOp = binOp "&" And
orOp  = binOp "|" Or

cJump :: Parser Jmp
cJump = jmp <$> (symbol ";" *> count 3 upperChar)

jmp "JLT" = Jlt
jmp "JLE" = Jle
jmp "JEQ" = Jeq
jmp "JGE" = Jge
jmp "JGT" = Jgt
jmp "JNE" = Jne
jmp "JMP" = Jmp

label :: Parser Instruction
label = AsmAST.Label <$> (symbol "(" *> identifier <* symbol ")")

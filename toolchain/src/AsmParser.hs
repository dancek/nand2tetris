module AsmParser (parser) where

import Data.Functor (void)
import Data.Void    
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

type Parser = Parsec Void String


lineComment  = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

-- two space consumers: scn takes any space including newlines, sc doesn't take newline
scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space lineSpace lineComment blockComment
    where
        lineSpace = void $ takeWhile1P Nothing isLineSpace
        isLineSpace ' '  = True
        isLineSpace '\t' = True
        isLineSpace _    = False

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal


parser :: Parser Program
parser = between scn eof instructionSeq

instructionSeq :: Parser Program
instructionSeq = sepEndBy instruction scn

instruction :: Parser Instruction
instruction = aInstr <|> cInstr

aInstr :: Parser Instruction
aInstr = do
    symbol "@"
    v <- integer    -- FIXME: symbols
    return (AInstr (ANum v))

    -- aInstr = (AInstr . Anum) <$> symbol "@" *> integer

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
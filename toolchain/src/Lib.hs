module Lib
    ( parser
    ) where

import Data.Char (isSpace)
import Data.Functor (void)
import Data.Maybe (Maybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Printf
import qualified Text.Megaparsec.Char.Lexer as L

{-
A parser for the nand2tetris assembly language. The language contains two instructions, A and C:

A: @value
C: dest=comp;jump

The value for the A instruction is a non-negative decimal integer or a symbol.
For C, either dest or jump can be omitted.

op      ::= a | c
a       ::= "@" value
value   ::= <number> | <symbol>
c       ::= dest? comp jump?
dest    ::= reg+ "="
reg     ::= "A" | "M" | "D"
comp    ::= num | reg | unary | sum | and | or
num     ::= "0" | "1" | "-1"
unary   ::= ("-" | "!") reg
sum     ::= reg ("+" | "-") (reg | "1")
and     ::= reg "&" reg
or      ::= reg "|" reg
jump    ::= ';' jmp
jmp     ::= "JLT" | "JLE" | "JEQ" | "JGT" | "JGE" | "JNE" | "JMP"

-}

data Reg = RegA | RegM | RegD deriving (Show)
data RawNumber = MinusOne | Zero | One deriving (Show)

data Instruction
    = AInstr AValue
    | CInstr CDest CComp CJump
    deriving (Show)

data AValue = ANum Integer | ASym String deriving (Show)

type CDest = Maybe [Reg]
data CComp
    = CValue RawNumber 
    | CReg Reg 
    | CUnary UnaryOp Reg
    | CBinary BinaryOp Reg Reg
    deriving (Show)
type CJump = Maybe Jmp

-- consider R+1 and R-1 unary
data UnaryOp = Negate | Not | Incr | Decr deriving (Show)
data BinaryOp = Add | Sub | And | Or deriving (Show)

data Jmp = Jlt | Jle | Jeq | Jgt | Jge | Jne | Jmp deriving (Show)


data Program = Seq [Instruction] deriving (Show)

type Parser = Parsec Void String


lineComment  = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

-- two space consumers: scn takes any space including newlines, sc doesn't take newline
scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space lineSpace lineComment blockComment
    where
        lineSpace = (void $ takeWhile1P Nothing isLineSpace)
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
instructionSeq = Seq <$> sepEndBy instruction scn

instruction :: Parser Instruction
instruction = aInstr <|> cInstr

aInstr :: Parser Instruction
aInstr = do
    symbol "@"
    v <- integer    -- FIXME: symbols
    return (AInstr (ANum v))

cInstr :: Parser Instruction
cInstr = do
    dest <- optional cDest
    comp <- cComp
    jump <- optional cJump
    return $ CInstr dest comp jump

cDest :: Parser [Reg]
cDest = do
    dest <- some reg
    symbol "="
    return $ dest

reg :: Parser Reg
reg = do
    r <- symbol "A" <|> symbol "M" <|> symbol "D"
    return $ regFromName r

regFromName :: String -> Reg
regFromName "A" = RegA
regFromName "M" = RegM
regFromName "D" = RegD

cComp :: Parser CComp
cComp = try (unaryOp <|> binaryOp <|> cValue <|> cReg)

cValue :: Parser CComp
cValue = do
    v <- symbol "-1" <|> symbol "0" <|> symbol "1"
    return $ CValue $ rawNumber v

rawNumber :: String -> RawNumber
rawNumber "-1" = MinusOne
rawNumber "0" = Zero
rawNumber "1" = One

cReg :: Parser CComp
cReg = fmap CReg reg

unaryOp :: Parser CComp
unaryOp = try (negOp <|> notOp <|> incrOp <|> decrOp)

prefixOp :: String -> UnaryOp -> Parser CComp
prefixOp sym op = do
    symbol sym
    r <- reg
    return $ CUnary op r

postfixOp :: String -> String -> UnaryOp -> Parser CComp
postfixOp sym1 sym2 op = do
    r <- reg
    symbol sym1
    symbol sym2
    return $ CUnary op r

negOp = prefixOp "-" Negate
notOp = prefixOp "!" Not
incrOp = postfixOp "+" "1" Incr
decrOp = postfixOp "-" "1" Decr

binaryOp :: Parser CComp
binaryOp = try (addOp <|> subOp <|> andOp <|> orOp)

binOp :: String -> BinaryOp -> Parser CComp
binOp sym op = do
    a <- reg
    symbol sym
    b <- reg
    return $ CBinary op a b

addOp = binOp "+" Add
subOp = binOp "-" Sub
andOp = binOp "&" And
orOp  = binOp "|" Or

cJump :: Parser Jmp
cJump = do
    symbol ";"
    jumpType <- count 3 upperChar
    return $ jmp jumpType

jmp "JLT" = Jlt
jmp "JLE" = Jle
jmp "JEQ" = Jeq
jmp "JGE" = Jge
jmp "JGT" = Jgt
jmp "JNE" = Jne
jmp "JMP" = Jmp
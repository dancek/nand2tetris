module AST where
    
{-
Parser data types for the nand2tetris assembly language. The language contains two instructions, A and C:

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

data Reg = RegA | RegM | RegD deriving (Show, Eq)
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


type Program = [Instruction]

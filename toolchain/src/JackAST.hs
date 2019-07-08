module JackAST where

data JackClass =
    JackClass String [ClassVarDec] [SubroutineDec]
    deriving (Eq, Show)

data ClassVarDec =
    StaticDec VarDec |
    FieldDec VarDec
    deriving (Eq, Show)

data VarDec =
    VarDec Type [String]
    deriving (Eq, Show)
type ParamDec = VarDec
type LocalVarDec = VarDec

data Type = IntType |
    CharType |
    BooleanType |
    VoidType |
    ClassType String
    deriving (Eq, Show)

data SubroutineDec =
    SubroutineDec Type String [ParamDec] SubroutineBody
    deriving (Eq, Show)

data SubroutineBody =
    SubroutineBody [LocalVarDec] [Statement]
    deriving (Eq, Show)

data Statement =
    ReturnStatement
    deriving (Eq, Show)

data Expression =
    IntegerConstant Integer |
    StringConstant String |
    VariableName String |
    ThisExpression |
    ArrayElement String Expression |
    SubroutineCall String [Expression] |
    UnaryExpression UnaryOp Expression |
    BinaryExpression BinaryOp Expression Expression |
    ParenExpression Expression
    deriving (Eq, Show)

type UnaryOp = Char
type BinaryOp = Char

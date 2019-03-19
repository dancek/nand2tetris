module JackParser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-----------------------------------------------------------
-- LEXING

lineComment  = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: String -> Parser String
symbol = L.symbol scn

integer :: Parser Integer
integer = lexeme L.decimal

symbolToData :: [(String, a)] -> Parser a
symbolToData = foldr1 (<|>) . map (\(k, v) -> (const v <$> symbol k))


-----------------------------------------------------------
-- PARSING

jackParser :: Parser JackClass
jackParser = between scn eof jackClass

-- TODO: forbid identifiers from starting with number
identifier :: Parser String
identifier = lexeme $ many (alphaNumChar <|> char '_')

jackClass :: Parser JackClass
jackClass = symbolToData [("class", JackClass)]
    <*> identifier
    <* symbol "{"
    <*> many classVarDec
    -- <*> many subroutineDec
    <* symbol "}"

classVarDec :: Parser ClassVarDec
classVarDec = symbolToData [
        ("static", StaticDec),
        ("field", FieldDec)]
    <*> varDec

varDec :: Parser VarDec
varDec = VarDec
    <$> typeToData
    <*> identifier
    <* symbol ";"
    -- TODO: multiple declarations

typeToData = symbolToData [
    ("int", IntType),
    ("char", CharType),
    ("boolean", BooleanType)]
    -- TODO: ClassType

-- TODO: move to JackAST.hs
data JackClass = JackClass String [ClassVarDec] deriving (Eq, Show)
data ClassVarDec =
    StaticDec VarDec |
    FieldDec VarDec deriving (Eq, Show)
data VarDec = VarDec Type String deriving (Eq, Show)
data Type = IntType |
    CharType |
    BooleanType |
    ClassType String deriving (Eq, Show)

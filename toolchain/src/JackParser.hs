module JackParser where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import JackAST

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

wrapParser :: Parser a -> Parser a
wrapParser = between scn eof

-- Main parser
jackParser :: Parser JackClass
jackParser = wrapParser jackClass

-- TODO: forbid identifiers from starting with number
identifier :: Parser String
identifier = lexeme $ many (alphaNumChar <|> char '_')


jackClass :: Parser JackClass
jackClass = symbolToData [("class", JackClass)]
    <*> identifier
    <* symbol "{"
    <*> many classVarDec
    <*> many subroutineDec
    <* symbol "}"

classVarDec :: Parser ClassVarDec
classVarDec = symbolToData [
        ("static", StaticDec),
        ("field", FieldDec)]
    <*> varDec

varDec :: Parser VarDec
varDec = VarDec
    <$> typeToData
    <*> commaSeparatedIdentifiers
    <* symbol ";"

typeToData = symbolToData [
    ("int", IntType),
    ("char", CharType),
    ("boolean", BooleanType),
    ("void", VoidType)]
    <|> ClassType <$> identifier

commaSeparatedIdentifiers :: Parser [String]
commaSeparatedIdentifiers = sepBy1 identifier (symbol ",")


subroutineDec :: Parser SubroutineDec
subroutineDec = SubroutineDec
    <$ symbol "method"
    <*> typeToData
    <*> identifier
    <* symbol "("
    <*> many paramDef
    <* symbol ")"
    <* symbol "{"
    <*> subroutineBody
    <* symbol "}"

paramDef = varDec

subroutineBody :: Parser SubroutineBody
subroutineBody = SubroutineBody
    <$> many methodVarDec
    <*> many statement

methodVarDec = symbol "var" *> varDec

statement :: Parser Statement
statement =
    ( ReturnStatement <$ symbol "return"
    ) <* symbol ";"

expression :: Parser Expression
expression = ThisExpression <$ symbol "this"
    <|> StringConstant <$ char '\"' <*> manyTill L.charLiteral (char '\"')
    <|> IntegerConstant <$> integer

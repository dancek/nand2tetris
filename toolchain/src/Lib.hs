module Lib
    ( assembler
    ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Printf
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"


assembler :: Parser String
assembler = line

line :: Parser String
line = aInstr <|> cInstr

aInstr :: Parser String
aInstr = do 
    char '@'
    n <- some digitChar
    return $ printf "%016b" (read n :: Integer)

cInstr :: Parser String
cInstr = do
    dest <- optional cDest
    comp <- cComp
    jump <- optional cJump
    return $ comp

cDest = do
    dest <- some (char 'A' <|> char 'M' <|> char 'D')
    char '='
    return $ dest

cComp = some alphaNumChar

cJump = do
    char ';'
    jumpType <- some alphaNumChar
    return $ jumpType
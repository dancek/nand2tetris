module Lib
    ( assembler
    ) where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Printf

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
cInstr = cDest <|> cJump

cDest = do
    dest <- some (char 'A' <|> char 'M' <|> char 'D')
    char '='
    return $ dest

cJump = do
    some alphaNumChar
    char ';'
    jumpType <- some alphaNumChar
    return $ jumpType
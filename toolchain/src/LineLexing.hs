module LineLexing (sc, scn, lexeme, symbol, integer, symbolToData) where

import Data.Functor (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

symbolToData :: [(String, a)] -> Parser a
symbolToData = foldr1 (<|>) . map (\(k, v) -> (const v <$> symbol k))

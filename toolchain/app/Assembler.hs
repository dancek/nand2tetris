module Main where

import System.IO
import Text.Megaparsec

import Lib

main :: IO ()
main = do
    input <- getContents
    case assembler input of
        Left e -> hPutStr stderr $ parseErrorPretty' input e
        Right r -> putStr r
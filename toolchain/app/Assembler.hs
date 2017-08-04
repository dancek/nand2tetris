module Main where

import Text.Megaparsec
import System.Environment

import Lib

main :: IO ()
main = do
    input <- fmap head getArgs
    parseTest assembler input

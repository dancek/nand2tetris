module Main where

import Text.Megaparsec

import Lib

main :: IO ()
main = do
    input <- getContents
    parseTest' parser input

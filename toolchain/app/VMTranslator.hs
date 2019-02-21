module Main where

import System.IO
import Text.Megaparsec

import Lib

main :: IO ()
main = do
    input <- getContents
    case vmTranslator input of
        Left e -> hPutStr stderr $ errorBundlePretty e
        Right r -> putStr r

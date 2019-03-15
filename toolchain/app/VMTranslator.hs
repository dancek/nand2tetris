module Main where

import Data.List
import System.IO
import System.Environment
import System.Directory
import Text.Megaparsec

import Lib

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> getContents >>= compile
        1 -> compileFile $ head args

compileFile :: String -> IO ()
compileFile f = do
    isFile <- doesFileExist f
    if isFile then readFile f >>= compile
    else do
        allFiles <- listDirectory f
        let files = filter (isSuffixOf ".vm") allFiles in
            mapM_ compileFile $ map (\x -> f ++ "/" ++ x) files


compile input =
    case vmTranslator input of
        Left e -> hPutStr stderr $ errorBundlePretty e
        Right r -> putStr r

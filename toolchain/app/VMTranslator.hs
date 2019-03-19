module Main where

import Data.List
import Data.Maybe
import System.IO
import System.Environment
import System.Directory
import Text.Megaparsec

import Lib

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> do
            input <- getContents
            output <- compile "stdin" input
            putStr output
        1 -> compilePath $ head args

compilePath :: String -> IO ()
compilePath f = do
    isFile <- doesFileExist f
    if isFile then do
        input <- readFile f
        compiled <- (compile f input)
        writeFile (getSingleOutputFilename f) compiled
    else do
        allFiles <- listDirectory f
        let files = filter (isSuffixOf ".vm") allFiles in do
            outputs <- withCurrentDirectory f $
                mapM (\x -> readFile x >>= compile x) files
            let code = unlines (vmMain ++ outputs) in
                withCurrentDirectory f $ writeFile (getDirOutputFilename f) code

compile :: String -> String -> IO String
compile sourceFilename input =
    case vmTranslator sourceFilename input of
        Left e -> do
            hPutStr stderr $ errorBundlePretty e
            return ""
        Right r -> return r

getSingleOutputFilename :: String -> String
getSingleOutputFilename f =
    if ".vm" `isSuffixOf` f then
        take ((length f) - 3) f ++ ".asm"
    else f ++ ".asm"

getDirOutputFilename :: String -> String
getDirOutputFilename f =
    (last $ filter (not . null) $ splitByDelimiter '/' f "") ++ ".asm"

splitByDelimiter :: Eq a => a -> [a] -> [a] -> [[a]]
splitByDelimiter _ [] [] = []
splitByDelimiter _ [] word = [word]
splitByDelimiter d (x:xs) word =
    if x == d then [word] ++ splitByDelimiter d xs []
    else splitByDelimiter d xs (word ++ [x])

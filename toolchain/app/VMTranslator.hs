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
        0 -> getContents >>= (compile Nothing)
        1 -> compileFile $ head args

compileFile :: String -> IO ()
compileFile f = do
    isFile <- doesFileExist f
    if isFile then readFile f >>= (compile $ Just f)
    else do
        allFiles <- listDirectory f
        let files = filter (isSuffixOf ".vm") allFiles in
            withCurrentDirectory f $ mapM_ compileFile files

compile :: Maybe String -> String -> IO ()
compile sourceFilename input =
    case vmTranslator (fromMaybe "stdin" sourceFilename) input of
        Left e -> hPutStr stderr $ errorBundlePretty e
        Right r -> case sourceFilename of
            Just f -> let outfile = (getFilename f) in
                putStrLn outfile >> writeFile outfile r
            Nothing -> putStr r

getFilename :: String -> String
getFilename f =
    if ".vm" `isSuffixOf` f then
        take ((length f) - 3) f ++ ".asm"
    else f ++ ".asm"

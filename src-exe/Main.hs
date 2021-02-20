module Main where
import System.Environment
import Compiler

main = do
    args <- getArgs
    fileContents <- mapM readFile args
    mapM putStrLn (parse fileContents)

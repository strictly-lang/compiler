module Main where
import System.Environment ( getArgs )
import Compiler ( parse )

main = do
    args <- getArgs
    fileContents <- mapM readFile args
    let parsedContent = map readFramelessFile fileContents
    return True

readFramelessFile fileName = do
    fileContent <- readFile fileName
    return (parse fileContent);
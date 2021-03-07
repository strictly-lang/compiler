module Main where
import System.Environment ( getArgs )
import Compiler ( parse )

main :: IO [()]
main = do
    args <- getArgs
    fileContents <- mapM readFile args
    let parsedContent = map parse fileContents
    let linedContent = map unlines parsedContent;
    mapM putStrLn linedContent

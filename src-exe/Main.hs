module Main where

import Compiler.Main (getJs, parse)
import Control.Exception (Exception, throwIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)

data NoString = NoString deriving (Show)

instance Exception NoString

main = do
  args <- getArgs
  compiledContent <- mapM readFramelessFile args
  mapM_ putStrLn compiledContent
  return True

readFramelessFile fileName = do
  cwd <- System.Directory.getCurrentDirectory
  fileContent <- readFile fileName
  maybeToIO (getJs cwd (cwd ++ fileName) (parse fileContent))

maybeToIO :: Maybe String -> IO String
maybeToIO Nothing = throwIO NoString
maybeToIO (Just x) = return x
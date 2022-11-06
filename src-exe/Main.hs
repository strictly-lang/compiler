module Main where

import Compiler (compile)
import Control.Exception (Exception, throwIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)

data NoString = NoString deriving (Show)

instance Exception NoString

main = do
  args <- getArgs
  compiledContent <- mapM readFramelessFile args
  mapM_ putStrLn compiledContent

readFramelessFile :: FilePath -> IO String
readFramelessFile fileName = do
  cwd <- System.Directory.getCurrentDirectory
  fileContent <- readFile fileName
  case compile fileName fileContent of
    Left parseError -> error parseError
    Right (targetPath, compiledContent) ->
      return compiledContent

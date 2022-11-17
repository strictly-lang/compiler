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
  fileContent <- readFile fileName
  cwd <- System.Directory.getCurrentDirectory
  case relativePath cwd (normalizePath cwd fileName) of
    Just normalizedPath ->
      case compile normalizedPath fileContent of
        Left parseError -> error parseError
        Right (targetPath, compiledContent) ->
          return compiledContent
    Nothing ->
      error ("The " ++ fileName ++ "is outside of the current working directory" ++ cwd)

normalizePath :: String -> String -> String
normalizePath cwd filePath@('/' : _) = filePath
normalizePath cwd filePath = cwd ++ "/" ++ filePath

relativePath :: String -> String -> Maybe String
relativePath [] (a : as) = Just as
relativePath (p : ps) (a : as)
  | p == a = relativePath ps as
  | otherwise = Nothing
relativePath _ [] = Nothing
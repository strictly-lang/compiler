module Main where

import Compiler.Main (getJs, parse)
import Control.Exception (Exception, throwIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

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
  case parse fileContent of
    Left parseError -> error (errorBundlePretty parseError)
    -- Right parsedContent ->
    --   case getJs cwd (normalizePath cwd fileName) parsedContent of
    --     (Just result) -> return result
    --     Nothing -> error "Compile Error"

    Right parsedContent -> error (show parsedContent)

normalizePath :: String -> String -> String
normalizePath cwd filePath@('/' : _) = filePath
normalizePath cwd filePath = cwd ++ "/" ++ filePath

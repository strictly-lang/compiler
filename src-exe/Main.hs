module Main where

import Control.Exception (Exception, throwIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Compiler.Main (parse, getJs)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)


data NoString = NoString deriving (Show)

instance Exception NoString

main = do
  args <- getArgs
  compiledContent <- mapM readFramelessFile args
  mapM_ (putStrLn . snd) compiledContent

  return (all fst compiledContent)

readFramelessFile :: FilePath -> IO (Bool, String)
readFramelessFile fileName = do
  cwd <- System.Directory.getCurrentDirectory
  fileContent <- readFile fileName
  case parse fileContent of
    Left parseError -> return (False, errorBundlePretty parseError)
    Right parsedContent -> 
      case getJs cwd (normalizePath cwd fileName) parsedContent of
        (Just result) -> return (True, result)
        Nothing -> return  (False, "Compile Error" )
    -- Right parsedContent -> return (True, show parsedContent)

normalizePath :: String -> String -> String
normalizePath cwd filePath@('/' : _) = filePath
normalizePath cwd filePath = cwd ++ "/" ++ filePath

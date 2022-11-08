module Parser.Main where

import Control.Monad.State.Strict (runState)
import Data.Void (Void)
import Parser.Root (rootParser)
import Parser.Types
import Text.Megaparsec (State, eof, many, parse)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Error

parse :: String -> Either String [ASTRootNode]
parse = do
  result <- Text.Megaparsec.parse parseRoot' ""
  case result of
    Left parseError ->
      return (Left (errorBundlePretty parseError))
    Right ast ->
      return (Right ast)

parseRoot' :: Parser [ASTRootNode]
parseRoot' = do
  many (many eol *> (rootParser <* many eol)) <* eof
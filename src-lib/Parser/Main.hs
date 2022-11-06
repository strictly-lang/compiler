module Parser.Main where

import Control.Monad.State.Strict (runState)
import Data.Void (Void)
import Parser.Root (rootParser)
import Parser.Types
import Text.Megaparsec (State, eof, many, parse)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Error

parse = Text.Megaparsec.parse parseRoot' ""

parseRoot' :: Parser [ASTRootNode]
parseRoot' = many (many eol *> (rootParser <* many eol)) <* eof
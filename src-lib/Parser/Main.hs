module Parser.Main where

import Data.Void (Void)
import Parser.Types
import Parser.Types.Root (rootParser)
import Text.Megaparsec (Parsec, eof, many, parse)
import Text.Megaparsec.Char (eol)
import Types

parseRoot = parse parseRoot' ""

parseRoot' :: Parser [Root]
parseRoot' = many (many eol *> (rootParser <* eol)) <* eof
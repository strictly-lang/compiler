module Parser.Main (parseRoot) where

import Control.Applicative ((<|>))
import Parser.Model.Base (modelParser)
import Parser.View.Base (viewParser)
import Text.Megaparsec (eof, many, parse)
import Parser.Util.Base (indentParser)

import Types

parseRoot = parse parseRoot' ""

parseRoot' :: Parser [Root]
parseRoot' = many (indentParser 0 (viewParser <|> modelParser)) <* eof

module Parser.Main (parseRoot) where

import Control.Applicative ((<|>))
import Parser.Model.Base (modelParser)
import Parser.View.Base (viewParser)
import Text.Megaparsec (eof, many, parse)
import Types

parseRoot = parse parseRoot' ""

parseRoot' :: Parser [Root]
parseRoot' = many (viewParser <|> modelParser) <* eof

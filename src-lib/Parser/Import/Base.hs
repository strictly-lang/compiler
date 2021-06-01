module Parser.Import.Base (importParser) where

import Control.Applicative ((<|>))
import Parser.Util.Base (identityParser, sc)
import Text.Megaparsec (between, some)
import Text.Megaparsec.Char (char, letterChar, space1, string)
import Types

importParser :: Parser Root
importParser = do
  _ <- string "import" <* space1
  path <- pathParser <* sc
  imports <- between (char '(') (char ')') (some identityParser)
  return (Import path imports)

pathParser :: Parser String
pathParser = do
  some (letterChar <|> char '/' <|> char '.')
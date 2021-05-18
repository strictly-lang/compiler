module Parser.Model.Base (modelParser) where

import Text.Megaparsec (some)
import Text.Megaparsec.Char (letterChar, string, space)
import Types
import Parser.Util.Base (optionsParser, optionParser)

modelParser :: Parser Root
modelParser = do
  _ <- string "model"
  _ <- string ":"
  name <- some letterChar
  options <- optionsParser 0 optionParser
  return (Model name [])

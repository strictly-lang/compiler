module Parser.Model.Base (modelParser) where

import Text.Megaparsec.Char (string)
import Types

modelParser :: Parser Root
modelParser = do
  _ <- string "model"
  return Model
module Parser.Model.Base (modelParser) where

import Parser.Util.Base (optionsParser, rightHandSideFunctionParser, sc)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (char, letterChar, string)
import Types

modelParser :: Parser Root
modelParser = do
  _ <- string "model"
  _ <- string ":"
  name <- some letterChar
  options <- optionsParser 0 (modelOptionParser rightHandSideFunctionParser)
  return (Model name [])

modelOptionParser :: Parser a -> Parser (Option a)
modelOptionParser rightHandSideParser = do
  attributeName <- some letterChar <* sc
  _ <- char '=' <* sc
  rightHandSide <- rightHandSideParser
  return (attributeName, rightHandSide)
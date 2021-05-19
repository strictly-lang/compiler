module Parser.Model.Base (modelParser) where

import Parser.Util.Base (optionsParser, rightHandSideFunctionParser, sc, identityParser)
import Text.Megaparsec.Char (char, string)
import Types

modelParser :: Parser Root
modelParser = do
  _ <- string "model"
  _ <- string ":"
  name <- identityParser
  options <- optionsParser 0 (modelOptionParser rightHandSideFunctionParser)
  return (Model name [])

modelOptionParser :: Parser a -> Parser (Option a)
modelOptionParser rightHandSideParser = do
  attributeName <- identityParser <* sc
  _ <- char '=' <* sc
  rightHandSide <- rightHandSideParser
  return (attributeName, rightHandSide)
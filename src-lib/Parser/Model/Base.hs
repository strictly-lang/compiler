module Parser.Model.Base (modelParser) where

import Control.Applicative (optional)
import Parser.Util.Base (identityParser, mergeOptions, optionsParser, rightHandSideFunctionParser, sc)
import Text.Megaparsec.Char (char, string)
import Types

modelParser :: Parser Root
modelParser = do
  _ <- string "model"
  _ <- string ":"
  name <- identityParser
  options <- optionsParser 0 (modelOptionParser rightHandSideFunctionParser)
  return (Model name (mergeOptions options))

modelOptionParser :: Parser a -> Parser (Option (Bool, a))
modelOptionParser rightHandSideParser = do
  attributeName <- identityParser
  isGenerator <- optional (char '*')
  _ <- sc
  _ <- char '=' <* sc
  rightHandSide <- rightHandSideParser
  return (attributeName, (maybeToBool isGenerator, rightHandSide))

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = True
maybeToBool Nothing = False
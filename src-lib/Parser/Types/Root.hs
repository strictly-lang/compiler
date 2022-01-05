module Parser.Types.Root where

import Parser.Types
import Parser.Util (assignParser, uppercaseIdentifierParser)
import Text.Megaparsec (between, many, optional, sepBy)
import Text.Megaparsec.Char (char, space, space1, string)
import Types

dataParser :: Parser Root
dataParser = do
  name <- string "data" *> space1 *> uppercaseIdentifierParser <* space
  dataDeclarations <- between (assignParser <* space) (char ';' <* space) ((algebraicDataTypeParser <* space) `sepBy` (char '|' <* space))
  return (RootDataDeclaration name dataDeclarations)

algebraicDataTypeParser :: Parser DataDeclaration
algebraicDataTypeParser = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (char '(')
  parameters <-
    case hasParameter of
      Just _ -> do many algebraicDataTypeParser <* char ')'
      Nothing -> do return []
  return (DataDeclaration (name, parameters))

rootParser = dataParser
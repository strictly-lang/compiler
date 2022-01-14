module Parser.Types.Root where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.Statement
import Parser.Util (assignParser, lowercaseIdentifierParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (between, many, optional, sepBy)
import Text.Megaparsec.Char (char, space, space1, string)
import Types

dataParser :: Parser Root
dataParser = do
  name <- string "data " *> sc *> uppercaseIdentifierParser <* sc
  dataDeclarations <- between (assignParser <* sc) (char ';' <* sc) ((algebraicDataTypeParser <* sc) `sepBy` (char '|' <* sc))
  return (RootDataDeclaration name dataDeclarations)

algebraicDataTypeParser :: Parser (String, [String])
algebraicDataTypeParser = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (char '(')
  parameters <-
    case hasParameter of
      Just _ -> do many uppercaseIdentifierParser <* char ')'
      Nothing -> do return []
  return (name, parameters)

assignmentParser :: Parser Root
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  RootAssignment name <$> expressionParser

rootParser :: Parser Root
rootParser = dataParser <|> assignmentParser
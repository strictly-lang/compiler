module Parser.Types.Root where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.Statement
import Parser.Util (assignParser, blockParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, sc, statementTerminationParser, uppercaseIdentifierParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, optional, sepBy)
import Text.Megaparsec.Char (char, space, space1, string)
import Types

dataParser :: Parser Root
dataParser = do
  name <- string "data " *> sc *> uppercaseIdentifierParser <* sc
  dataDeclarations <- between assignParser statementTerminationParser ((algebraicDataTypeParser 0 <* sc) `sepBy` (char '|' <* sc))
  return (RootDataDeclaration name dataDeclarations)

algebraicDataTypeParser :: IndentationLevel -> Parser DataDeclaration
algebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (lookAhead listOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser listOpenParser listCloseParser algebraicDataTypeParser indentationLevel
      Nothing -> do return []
  return (DataDeclaration (name, parameters))

assignmentParser :: Parser Root
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  RootAssignment name <$> expressionParser 0

rootParser :: Parser Root
rootParser = dataParser <|> assignmentParser
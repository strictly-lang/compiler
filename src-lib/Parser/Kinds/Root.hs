module Parser.Kinds.Root where

import Control.Applicative ((<|>))
import Parser.Kinds.Statement
import Parser.Types
import Parser.Util (assignParser, blockParser, functionCallCloseParser, functionCallOpenParser, lowercaseIdentifierParser, sc, statementTerminationParser, typeDefinitionParser, uppercaseIdentifierParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, optional, sepBy)
import Text.Megaparsec.Char (char, space, space1, string)
import Types

rootParser :: Parser Root
rootParser = dataParser <|> typeDeclarationParser <|> assignmentParser

dataParser :: Parser Root
dataParser = do
  name <- string "data " *> sc *> uppercaseIdentifierParser <* sc
  dataDeclarations <- blockParser assignParser statementTerminationParser algebraicDataTypeParser 0
  return (RootDataDeclaration name dataDeclarations)

algebraicDataTypeParser :: IndentationLevel -> Parser DataDeclaration
algebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser functionCallOpenParser functionCallCloseParser algebraicDataTypeParser indentationLevel
      Nothing -> do return []
  return (DataDeclaration (name, parameters))

assignmentParser :: Parser Root
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  RootAssignment name <$> expressionParser 0

typeDeclarationParser :: Parser Root
typeDeclarationParser = do
  name <- string "type " *> sc *> uppercaseIdentifierParser <* sc <* assignParser
  typeDefinition <- typeDefinitionParser 0
  _ <- statementTerminationParser
  return (RootTypeAlias name typeDefinition)

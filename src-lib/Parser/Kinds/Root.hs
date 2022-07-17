module Parser.Kinds.Root where

import Control.Applicative ((<|>))
import Parser.Kinds.Statement
import Parser.Types
import Parser.Util (assignParser, blockParser, functionCallCloseParser, functionCallOpenParser, lowercaseIdentifierParser, sc, statementTerminationParser, typeAssignParser, typeDefinitionParser, uppercaseIdentifierParser)
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

algebraicDataTypeParser :: IndentationLevel -> Parser (String, [TypeDefinition])
algebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser functionCallOpenParser functionCallCloseParser typeDefinitionParser indentationLevel
      Nothing -> do return []
  return (name, parameters)

assignmentParser :: Parser Root
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc
  kind <- Left <$> typeAssignParser <|> Right <$> assignParser
  case kind of
    Left _ -> RootTypeAssignment name <$> typeDefinitionParser 0
    Right _ -> RootAssignment name <$> expressionParser 0

typeDeclarationParser :: Parser Root
typeDeclarationParser = do
  name <- string "type " *> sc *> uppercaseIdentifierParser <* sc <* assignParser
  typeDefinition <- typeDefinitionParser 0
  _ <- statementTerminationParser
  return (RootTypeAlias name typeDefinition)
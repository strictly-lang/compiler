module Parser.Root (rootParser) where

import Control.Applicative (Alternative (some), (<|>))
import Parser.Statement
import Parser.Types
import Parser.Util (assignParser, blockParser, eol', functionCallCloseParser, functionCallOpenParser, lowercaseIdentifierParser, sc, statementTerminationParser, typeAssignParser, typeDefinitionParser, uppercaseIdentifierParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, optional, sepBy)
import Text.Megaparsec.Char (char, letterChar, space, space1, string)

rootParser :: Parser ASTStatement
rootParser = dataParser <|> typeDeclarationParser <|> assignmentParser

dataParser :: Parser ASTStatement
dataParser = do
  name <- string "data " *> sc *> uppercaseIdentifierParser <* sc
  dataDeclarations <- blockParser assignParser statementTerminationParser algebraicDataTypeParser 0
  return (ASTStatementAlgebraicDataTypeDeclaration name dataDeclarations)

algebraicDataTypeParser :: IndentationLevel -> Parser (String, [ASTTypeDeclaration])
algebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser functionCallOpenParser functionCallCloseParser typeDefinitionParser indentationLevel
      Nothing -> do return []
  return (name, parameters)

typeDeclarationParser :: Parser ASTStatement
typeDeclarationParser = do
  name <- string "type " *> sc *> uppercaseIdentifierParser <* sc <* assignParser
  typeDefinition <- typeDefinitionParser 0
  _ <- statementTerminationParser
  return (ASTStatementTypeDeclaration name typeDefinition)

assignmentParser :: Parser ASTStatement
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc
  kind <- Left <$> typeAssignParser <|> Right <$> assignParser
  case kind of
    Left _ -> ASTStatementVariableTypeAssignment name <$> typeDefinitionParser 0
    Right _ -> ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable name) <$> expressionParser 0

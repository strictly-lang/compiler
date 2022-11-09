module Parser.Root (rootParser) where

import Control.Applicative (Alternative (some), (<|>))
import Parser.Statement
import Parser.Types
import Parser.Util (assignParser, blockParser, eol', functionCallCloseParser, functionCallOpenParser, functionMacroCloseParser, functionMacroOpenParser, lowercaseIdentifierParser, sc, statementTerminationParser, typeAssignParser, typeDefinitionParser, uppercaseIdentifierParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, optional, sepBy)
import Text.Megaparsec.Char (char, letterChar, space, space1, string)

rootParser :: Parser ASTRootNode
rootParser = dataParser <|> typeDeclarationParser <|> macroParser <|> assignmentParser

dataParser :: Parser ASTRootNode
dataParser = do
  name <- string "data " *> sc *> uppercaseIdentifierParser <* sc
  dataDeclarations <- blockParser assignParser statementTerminationParser algebraicDataTypeParser 0
  return (ASTRootNodeAlgebraicDataTypeDeclaration name dataDeclarations)

algebraicDataTypeParser :: IndentationLevel -> Parser (String, [ASTTypeDeclaration])
algebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser functionCallOpenParser functionCallCloseParser typeDefinitionParser indentationLevel
      Nothing -> do return []
  return (name, parameters)

typeDeclarationParser :: Parser ASTRootNode
typeDeclarationParser = do
  name <- string "type " *> sc *> uppercaseIdentifierParser <* sc <* assignParser
  typeDefinition <- typeDefinitionParser 0
  _ <- statementTerminationParser
  return (ASTRootTypeAssignment name typeDefinition)

macroParser :: Parser ASTRootNode
macroParser = do
  ASTMacro <$> between functionMacroOpenParser functionMacroCloseParser (some letterChar)

assignmentParser :: Parser ASTRootNode
assignmentParser = do
  name <- lowercaseIdentifierParser <* sc
  kind <- Left <$> typeAssignParser <|> Right <$> assignParser
  case kind of
    Left _ -> ASTRootTypeAssignment name <$> typeDefinitionParser 0
    Right _ -> ASTRootAssignment name <$> expressionParser 0

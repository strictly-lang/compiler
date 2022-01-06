module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, delimiterParser, indentationParser, lowercaseIdentifierParser, sc)
import Text.Megaparsec (between, many, sepBy)
import Text.Megaparsec.Char (char, string)
import Types

statementParser :: Parser Statement
statementParser = Expression <$> expressionParser

expressionParser :: Parser Expression
expressionParser = functionDefinitionParser <|> recordParser

recordParser :: Parser Expression
recordParser = do
  RightHandSideRecord <$> between (char '{' <* sc) (char '}' <* sc) (indentationParser recordOptionParser)

recordOptionParser :: Parser (String, Maybe String, Expression)
recordOptionParser = do
  key <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  value <- expressionParser
  return (key, Nothing, value)

functionDefinitionParser :: Parser Expression
functionDefinitionParser = do
  parameters <- between (char '/' <* sc) (string "->" <* sc) (leftHandSideParser `sepBy` delimiterParser)
  RightHandSideFunctionDefinition parameters
    <$> indentationParser statementParser

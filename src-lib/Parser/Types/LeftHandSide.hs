module Parser.Types.LeftHandSide where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get)
import Parser.Types
import Parser.Util (delimiterParser, lowercaseIdentifierParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (between, lookAhead, optional, sepBy)
import Text.Megaparsec.Char (char)
import Types

leftHandSideParser :: Parser LeftHandSide
leftHandSideParser = leftHandSideAlgebraicDataTypeParser <|> leftHandSideRecordParser <|> leftHandSideVariableParser

leftHandSideAlgebraicDataTypeParser :: Parser LeftHandSide
leftHandSideAlgebraicDataTypeParser = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead (char '('))
  parameters <- case hasParameter of
    Just _ -> do
      indentationLevel <- get
      between (char '(' <* sc) (char ')' <* sc) (leftHandSideParser `sepBy` delimiterParser (indentationLevel + 1))
    Nothing -> do return []
  return (LeftHandSideAlgebraicDataType name parameters)

leftHandSideRecordParser :: Parser LeftHandSide
leftHandSideRecordParser = do
  destructuredProperties <- between (char '{' <* sc) (char '}' <* sc) (leftHandSideRecordEntityParser `sepBy` (char ',' <* sc))
  return (LeftHandSideRecord destructuredProperties)

leftHandSideRecordEntityParser :: Parser (String, Maybe LeftHandSide)
leftHandSideRecordEntityParser = do
  propertyName <- lowercaseIdentifierParser <* sc
  hasAlias <- optional (char '=' <* sc)

  case hasAlias of
    Just _ -> do
      alias <- leftHandSideParser
      return (propertyName, Just alias)
    Nothing -> do
      return (propertyName, Nothing)

leftHandSideVariableParser :: Parser LeftHandSide
leftHandSideVariableParser = do
  identifier <- lowercaseIdentifierParser
  isAlias <- optional (char '@')

  case isAlias of
    Just _ -> do LeftHandSideAlias identifier <$> leftHandSideParser
    Nothing -> do return (LeftHandSideVariable identifier)
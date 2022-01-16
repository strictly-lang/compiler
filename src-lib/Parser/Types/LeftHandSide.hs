module Parser.Types.LeftHandSide where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get)
import Parser.Types
import Parser.Util (blockParser, lowercaseIdentifierParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, optional)
import Text.Megaparsec.Char (char)
import Types

leftHandSideParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideParser indentationLevel = leftHandSideAlgebraicDataTypeParser indentationLevel <|> leftHandSideRecordParser indentationLevel <|> leftHandSideVariableParser indentationLevel

leftHandSideAlgebraicDataTypeParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideAlgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead (char '('))
  parameters <- case hasParameter of
    Just _ -> do
      blockParser (char '(' *> sc) (char ')' *> sc) leftHandSideParser indentationLevel
    Nothing -> do return []
  return (LeftHandSideAlgebraicDataType name parameters)

leftHandSideRecordParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideRecordParser indentationLevel = do
  destructuredProperties <- blockParser (char '{' <* sc) (char '}' <* sc) leftHandSideRecordEntityParser indentationLevel
  return (LeftHandSideRecord destructuredProperties)

leftHandSideRecordEntityParser :: IndentationLevel -> Parser (String, Maybe LeftHandSide)
leftHandSideRecordEntityParser indentationLevel = do
  propertyName <- lowercaseIdentifierParser <* sc
  hasAlias <- optional (char '=' <* sc)

  case hasAlias of
    Just _ -> do
      alias <- leftHandSideParser indentationLevel
      return (propertyName, Just alias)
    Nothing -> do
      return (propertyName, Nothing)

leftHandSideVariableParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideVariableParser indentationLevel = do
  identifier <- lowercaseIdentifierParser
  isAlias <- optional (char '@')

  case isAlias of
    Just _ -> do LeftHandSideAlias identifier <$> (leftHandSideParser indentationLevel <* sc)
    Nothing -> do
      _ <- sc
      return (LeftHandSideVariable identifier)
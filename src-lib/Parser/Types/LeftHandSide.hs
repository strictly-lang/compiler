module Parser.Types.LeftHandSide where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Util (assignParser, blockParser, functionCalldCloseParser, functionCalldOpenParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, recordCloseParser, recordOpenParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, optional)
import Text.Megaparsec.Char (char)
import Types

leftHandSideParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideParser indentationLevel = leftHandSideListParser indentationLevel <|> leftHandSideAlgebraicDataTypeParser indentationLevel <|> leftHandSideRecordParser indentationLevel <|> leftHandSideVariableParser indentationLevel

leftHandSideAlgebraicDataTypeParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideAlgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCalldOpenParser)
  parameters <- case hasParameter of
    Just _ -> do
      blockParser functionCalldOpenParser functionCalldCloseParser leftHandSideParser indentationLevel
    Nothing -> do return []
  return (LeftHandSideAlgebraicDataType name parameters)

leftHandSideListParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideListParser indentationLevel = do
  LeftHandSideList <$> blockParser listOpenParser listCloseParser leftHandSideParser indentationLevel

leftHandSideRecordParser :: IndentationLevel -> Parser LeftHandSide
leftHandSideRecordParser indentationLevel = do
  destructuredProperties <- blockParser recordOpenParser recordCloseParser leftHandSideRecordEntityParser indentationLevel
  return (LeftHandSideRecord destructuredProperties)

leftHandSideRecordEntityParser :: IndentationLevel -> Parser (String, Maybe LeftHandSide)
leftHandSideRecordEntityParser indentationLevel = do
  propertyName <- lowercaseIdentifierParser
  hasAlias <- optional assignParser

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
    Just _ -> do LeftHandSideAlias identifier <$> leftHandSideParser indentationLevel
    Nothing -> do
      _ <- sc
      return (LeftHandSideVariable identifier)
module Parser.LeftHandSide where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Util (assignParser, baseOfParser, blockParser, functionCallCloseParser, functionCallOpenParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, recordCloseParser, recordOpenParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, optional)
import Text.Megaparsec.Char (char)

leftHandSideParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideParser indentationLevel = leftHandSideHoleParser indentationLevel <|> leftHandSideListParser indentationLevel <|> leftHandSideAlgebraicDataTypeParser indentationLevel <|> leftHandSideRecordParser indentationLevel <|> leftHandSideVariableParser indentationLevel

leftHandSideHoleParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideHoleParser indentationLevel = do
  _ <- char '_' *> sc
  return ASTLeftHandSideHole

leftHandSideAlgebraicDataTypeParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideAlgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <- case hasParameter of
    Just _ -> do
      blockParser functionCallOpenParser functionCallCloseParser leftHandSideParser indentationLevel
    Nothing -> do return []
  return (ASTLeftHandSideAlgebraicDataType name parameters)

leftHandSideListParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideListParser indentationLevel = do
  ASTLeftHandSideList <$> blockParser listOpenParser listCloseParser leftHandSideParser indentationLevel

leftHandSideRecordParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideRecordParser indentationLevel = do
  destructuredProperties <- blockParser recordOpenParser recordCloseParser leftHandSideRecordEntityParser indentationLevel
  return (ASTLeftHandSideRecord destructuredProperties)

leftHandSideRecordEntityParser :: IndentationLevel -> Parser (String, Maybe ASTLeftHandSide)
leftHandSideRecordEntityParser indentationLevel = do
  propertyName <- lowercaseIdentifierParser
  hasAlias <- optional assignParser

  case hasAlias of
    Just _ -> do
      alias <- leftHandSideParser indentationLevel
      return (propertyName, Just alias)
    Nothing -> do
      return (propertyName, Nothing)

leftHandSideVariableParser :: IndentationLevel -> Parser ASTLeftHandSide
leftHandSideVariableParser indentationLevel = do
  identifier <- lowercaseIdentifierParser
  isAlias <- optional (char '@')

  case isAlias of
    Just _ -> do ASTLeftHandSideAlias identifier <$> leftHandSideParser indentationLevel
    Nothing -> do
      _ <- sc
      return (ASTLeftHandSideVariable identifier)
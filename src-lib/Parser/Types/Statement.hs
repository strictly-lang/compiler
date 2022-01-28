module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, baseOfParser, blockParser, eol', functionCallCloseParser, functionCallOpenParser, indentationParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, numberParser, recordCloseParser, recordOpenParser, sc, statementTerminationParser, streamParser, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, manyTill, optional, some, try)
import Text.Megaparsec.Char (char, lowerChar, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

statementParser :: IndentationLevel -> Parser Statement
statementParser indentationLevel =
  letParser indentationLevel
    <|> (Expression <$> expressionParser indentationLevel)

-----------------------
-- Statement-Parsers --
-----------------------

letParser :: IndentationLevel -> Parser Statement
letParser indentationLevel = do
  _ <- string "let " <* sc
  leftHandSide <- leftHandSideParser indentationLevel
  kind <- Left <$> assignParser <|> Right <$> streamParser
  expression <- expressionParser indentationLevel

  result <- case kind of
    Left _ ->
      return (VariableAssignment leftHandSide expression)
    Right _ ->
      return (Stream leftHandSide expression)
  _ <- statementTerminationParser

  return result

------------------------
-- Expression-Parsers --
------------------------

expressionParser :: IndentationLevel -> Parser Expression
expressionParser indentationLevel = do
  expression <- expressionParser' indentationLevel

  nested <- optional (char '.')

  result <- case nested of
    Just _ -> do
      nextExpressionPart <- expressionParser indentationLevel
      return (expression : nextExpressionPart)
    Nothing -> do
      return [expression]

  hasFunctionCall <- optional (lookAhead functionCallOpenParser)

  result' <- case hasFunctionCall of
    Just _ -> do
      functionCall <- RightHandSideFunctionCall result <$> blockParser functionCallOpenParser functionCallCloseParser expressionParser indentationLevel
      return [functionCall]
    Nothing -> do
      return result

  operator <- optional operatorParser

  case operator of
    Just operator -> do
      nextExpression <- expressionParser indentationLevel
      return [RightHandSideOperator operator result nextExpression]
    Nothing -> do
      return result

expressionParser' :: IndentationLevel -> Parser Expression'
expressionParser' indentationLevel = do
  rightHandSideFunctionDefinitionParser indentationLevel
    <|> rightHandSideConditionParser indentationLevel
    <|> rightHandSideRecordParser indentationLevel
    <|> rightHandSideNumberParser indentationLevel
    <|> rightHandSideMixedTextParser indentationLevel
    <|> rightHandSideListParser indentationLevel
    <|> rightHandSideAgebraicDataTypeParser indentationLevel
    <|> rightHandSideVariableParser indentationLevel
    <|> rightHandSideHostParser indentationLevel

rightHandSideConditionParser :: IndentationLevel -> Parser Expression'
rightHandSideConditionParser indentationLevel = do
  _ <- string "if " *> sc
  condition <- expressionParser indentationLevel
  _ <- string "then" *> sc *> eol'
  thenCase <- some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))
  _ <- optional eol' *> indentationParser (\indentationLevel -> do string "else" *> sc) indentationLevel
  elseCase <- some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))

  return (RightHandSideCondition condition thenCase elseCase)

rightHandSideAgebraicDataTypeParser :: IndentationLevel -> Parser Expression'
rightHandSideAgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (lookAhead listOpenParser)
  parameters <- case hasParameter of
    Just _ -> do
      blockParser listOpenParser listCloseParser expressionParser indentationLevel
    Nothing -> do return []
  return (RightHandSideAlgebraicDataType name parameters)

rightHandSideRecordParser :: IndentationLevel -> Parser Expression'
rightHandSideRecordParser indentationLevel = do
  RightHandSideRecord <$> recordParser indentationLevel

recordParser :: IndentationLevel -> Parser Record
recordParser indentationLevel = do
  properties <- blockParser recordOpenParser (lookAhead (recordCloseParser <|> baseOfParser)) recordOptionParser indentationLevel

  hasSource <- lookAhead (optional baseOfParser)

  case hasSource of
    Just _ -> do
      basedOn <- blockParser baseOfParser recordCloseParser expressionParser indentationLevel
      return (properties, basedOn)
    Nothing -> do
      _ <- recordCloseParser
      return (properties, [])

recordOptionParser :: IndentationLevel -> Parser (String, Maybe String, Expression)
recordOptionParser indentationLevel = do
  key <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  value <- expressionParser indentationLevel
  return (key, Nothing, value)

rightHandSideFunctionDefinitionParser :: IndentationLevel -> Parser Expression'
rightHandSideFunctionDefinitionParser indentationLevel = do
  parameters <- blockParser (char '\\' <* sc) (string "->" <* sc) leftHandSideParser indentationLevel

  hasEol' <- optional eol'

  functionBody <- case hasEol' of
    Just _ -> do
      some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))
    Nothing -> do
      result <- statementParser indentationLevel
      return [result]
  return (RightHandSideFunctionDefinition parameters functionBody)

rightHandSideListParser :: IndentationLevel -> Parser Expression'
rightHandSideListParser indentationLevel = do
  entities <- blockParser listOpenParser (lookAhead (listCloseParser <|> baseOfParser)) expressionParser indentationLevel

  hasSource <- lookAhead (optional baseOfParser)

  RightHandSideList entities
    <$> case hasSource of
      Just _ -> do
        blockParser baseOfParser listCloseParser statementParser indentationLevel
      Nothing -> do
        _ <- listCloseParser
        return []

rightHandSideNumberParser :: IndentationLevel -> Parser Expression'
rightHandSideNumberParser indentationLevel = do
  from <- numberParser

  hasRange <- optional (string "..")

  case hasRange of
    Just _ -> do
      RightHandSideRange from <$> optional numberParser
    Nothing ->
      return (RightHandSideNumber from)

rightHandSideMixedTextParser :: IndentationLevel -> Parser Expression'
rightHandSideMixedTextParser indentationLevel = do
  RightHandSideString
    <$> (char '\"' *> (dynamicTextParser indentationLevel <|> staticTextParser indentationLevel) `manyTill` char '"')

staticTextParser :: IndentationLevel -> Parser RightHandSideString
staticTextParser indentationLevel = do
  text <- charLiteral `manyTill` lookAhead (string "\"" <|> string "${")
  return (RightHandSideStringStatic text)

dynamicTextParser :: IndentationLevel -> Parser RightHandSideString
dynamicTextParser indentationLevel = do
  value <- string "${" *> expressionParser indentationLevel <* char '}'

  return (RightHandSideStringDynamic value)

rightHandSideVariableParser :: IndentationLevel -> Parser Expression'
rightHandSideVariableParser indentationLevel = do RightHandSideVariable <$> lowercaseIdentifierParser

rightHandSideHostParser :: IndentationLevel -> Parser Expression'
rightHandSideHostParser indentationLevel = do
  _ <- char '$'
  hostName <- some lowerChar

  hasOptions <- optional (lookAhead recordOpenParser)

  record <- case hasOptions of
    Just _ -> recordParser indentationLevel
    Nothing -> do return ([], [])

  return (RightHandSideHost hostName record []) -- add children parser

---------------------
-- Operator Parser --
---------------------
operatorParser :: Parser Operator
operatorParser =
  do
    ( equalParser
        <|> unequalParser
        <|> concatParser
        <|> plusParser
        <|> minusParser
        <|> multiplyParser
        <|> divisionParser
        <|> moduloParser
      )
      <* sc

equalParser :: Parser Operator
equalParser = do
  _ <- string "=="
  return Equal

unequalParser :: Parser Operator
unequalParser = do
  _ <- string "!="
  return Unequal

concatParser :: Parser Operator
concatParser = do
  _ <- string "++"
  return Concat

plusParser :: Parser Operator
plusParser = do
  _ <- string "+"
  return Plus

minusParser :: Parser Operator
minusParser = do
  _ <- string "-"
  return Unequal

multiplyParser :: Parser Operator
multiplyParser = do
  _ <- string "*"
  return Multiply

divisionParser :: Parser Operator
divisionParser = do
  _ <- string "/"
  return Division

moduloParser :: Parser Operator
moduloParser = do
  _ <- string "%"
  return Modulo

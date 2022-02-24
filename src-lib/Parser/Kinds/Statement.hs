module Parser.Kinds.Statement where

import Control.Applicative ((<|>))
import Parser.Kinds.LeftHandSide (leftHandSideParser)
import Parser.Types
import Parser.Util (assignParser, baseOfParser, blockParser, functionBodyParser, functionCallCloseParser, functionCallOpenParser, functionDefinitionParser, indentationParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, numberParser, recordCloseParser, recordOpenParser, sc, statementTerminationParser, streamParser, typeAssignParser, typeDefinitionParser, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, many, manyTill, optional, some, try)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, string)
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
    <|> rightHandSideMatchParser indentationLevel
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
  _ <- string "then" *> sc
  thenCase <- some (indentationParser statementParser (indentationLevel + 1))
  _ <- indentationParser (\indentationLevel -> do string "else" *> sc) indentationLevel
  elseCase <- some (indentationParser statementParser (indentationLevel + 1))

  return (RightHandSideCondition condition thenCase elseCase)

rightHandSideMatchParser :: IndentationLevel -> Parser Expression'
rightHandSideMatchParser indentationLevel = do
  _ <- string "match " *> sc
  matchTarget <- expressionParser indentationLevel
  cases <- many (indentationParser caseParser (indentationLevel + 1))

  return (RightHandSideMatch matchTarget cases)

caseParser :: IndentationLevel -> Parser (LeftHandSide, [Statement])
caseParser indentationLevel = do
  _ <- string "case " *> sc
  RightHandSideFunctionDefinition [patterns] statements <- rightHandSideFunctionDefinitionParser indentationLevel
  return (patterns, statements)

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
      basedOn <- blockParser baseOfParser recordCloseParser statementParser indentationLevel
      return (properties, basedOn)
    Nothing -> do
      _ <- recordCloseParser
      return (properties, [])

recordOptionParser :: IndentationLevel -> Parser (String, RecordValue)
recordOptionParser indentationLevel = do
  key <- lowercaseIdentifierParser

  hasCondition <- optional (char '?' *> sc)

  condition <- case hasCondition of
    Just _ -> do
      Just <$> some letterChar <* sc
    Nothing -> do
      return Nothing

  kind <- Left <$> assignParser <|> Right <$> typeAssignParser

  value <- case kind of
    Left _ ->
      RecordExpression condition <$> expressionParser indentationLevel
    Right _ ->
      RecordType <$> typeDefinitionParser indentationLevel

  return (key, value)

rightHandSideFunctionDefinitionParser :: IndentationLevel -> Parser Expression'
rightHandSideFunctionDefinitionParser indentationLevel = do
  parameters <- blockParser functionDefinitionParser functionBodyParser leftHandSideParser indentationLevel

  hasEol <- lookAhead (optional eol)

  functionBody <- case hasEol of
    Just _ -> do
      some (indentationParser statementParser (indentationLevel + 1))
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

  children <- many (indentationParser statementParser (indentationLevel + 1))
  return (RightHandSideHost hostName record children)

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

module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, baseOfParser, blockParser, eol', functionCalldCloseParser, functionCalldOpenParser, indentationParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, recordCloseParser, recordOpenParser, sc, streamParser, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, manyTill, optional, some, try)
import Text.Megaparsec.Char (char, string)
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

  case kind of
    Left _ ->
      return (VariableAssignment leftHandSide expression)
    Right _ ->
      return (Stream leftHandSide expression)

------------------------
-- Expression-Parsers --
------------------------

expressionParser :: IndentationLevel -> Parser Expression
expressionParser indentationLevel = do
  expression <- expressionParser' indentationLevel

  nested <- optional (char '.')

  result <- case nested of
    Just _ -> do
      nextSessionPart <- expressionParser indentationLevel
      return (expression : nextSessionPart)
    Nothing -> do
      return [expression]
  operator <- optional operatorParser

  case operator of
    Just operator -> do
      nextExpression <- expressionParser indentationLevel
      return [RightHandSideOperator operator result nextExpression]
    Nothing -> do
      return result

expressionParser' :: IndentationLevel -> Parser Expression'
expressionParser' indentationLevel = do
  functionDefinitionParser indentationLevel
    <|> conditionParser indentationLevel
    <|> recordParser indentationLevel
    <|> mixedTextParser indentationLevel
    <|> listParser indentationLevel
    <|> agebraicDataTypeParser indentationLevel
    <|> variableParser indentationLevel

conditionParser :: IndentationLevel -> Parser Expression'
conditionParser indentationLevel = do
  _ <- string "if " *> sc
  condition <- expressionParser indentationLevel
  _ <- string "then" *> sc *> eol'
  thenCase <- some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))
  _ <- optional eol' *> indentationParser (\indentationLevel -> do string "else" *> sc) indentationLevel
  elseCase <- some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))

  return (RightHandSideCondition condition thenCase elseCase)

agebraicDataTypeParser :: IndentationLevel -> Parser Expression'
agebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (lookAhead functionCalldOpenParser)
  parameters <- case hasParameter of
    Just _ -> do
      blockParser functionCalldOpenParser functionCalldCloseParser expressionParser indentationLevel
    Nothing -> do return []
  return (RightHandSideAlgebraicDataType name parameters)

recordParser :: IndentationLevel -> Parser Expression'
recordParser indentationLevel = do
  properties <- blockParser recordOpenParser (lookAhead (recordCloseParser <|> baseOfParser)) recordOptionParser indentationLevel

  hasSource <- lookAhead (optional baseOfParser)

  RightHandSideRecord properties
    <$> case hasSource of
      Just _ -> do
        blockParser baseOfParser recordCloseParser statementParser indentationLevel
      Nothing -> do
        _ <- recordCloseParser
        return []

recordOptionParser :: IndentationLevel -> Parser (String, Maybe String, Expression)
recordOptionParser indentationLevel = do
  key <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  value <- expressionParser indentationLevel
  return (key, Nothing, value)

functionDefinitionParser :: IndentationLevel -> Parser Expression'
functionDefinitionParser indentationLevel = do
  parameters <- blockParser (char '\\' <* sc) (string "->" <* sc) leftHandSideParser indentationLevel

  hasEol' <- optional eol'

  functionBody <- case hasEol' of
    Just _ -> do
      some (try (optional eol' *> indentationParser statementParser (indentationLevel + 1)))
    Nothing -> do
      result <- statementParser indentationLevel
      return [result]
  return (RightHandSideFunctionDefinition parameters functionBody)

listParser :: IndentationLevel -> Parser Expression'
listParser indentationLevel = do
  entities <- blockParser listOpenParser (lookAhead (listCloseParser <|> baseOfParser)) expressionParser indentationLevel

  hasSource <- lookAhead (optional baseOfParser)

  RightHandSideList entities
    <$> case hasSource of
      Just _ -> do
        blockParser baseOfParser listCloseParser statementParser indentationLevel
      Nothing -> do
        _ <- listCloseParser
        return []

mixedTextParser :: IndentationLevel -> Parser Expression'
mixedTextParser indentationLevel = do
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

variableParser :: IndentationLevel -> Parser Expression'
variableParser indentationLevel = do RightHandSideVariable <$> lowercaseIdentifierParser

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

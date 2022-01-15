module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get)
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, blockParser, delimiterParser, indentationParser, lowercaseIdentifierParser, sc, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, many, manyTill, optional, sepBy, sepBy1, some, try)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

statementParser :: Parser Statement
statementParser =
  letParser
    <|> (Expression <$> expressionParser)

-----------------------
-- Statement-Parsers --
-----------------------

letParser :: Parser Statement
letParser = do
  _ <- string "let " <* sc
  leftHandSide <- leftHandSideParser
  _ <- assignParser *> sc
  expression <- expressionParser

  VariableAssignment leftHandSide <$> expressionParser

------------------------
-- Expression-Parsers --
------------------------

expressionParser :: Parser Expression
expressionParser = do
  expression <- expressionParser'

  nested <- optional (char '.')

  result <- case nested of
    Just _ -> do
      nextSessionPart <- expressionParser
      return (expression : nextSessionPart)
    Nothing -> do
      return [expression]
  operator <- optional operatorParser

  case operator of
    Just operator -> do
      nextExpression <- expressionParser
      return [RightHandSideOperator operator result nextExpression]
    Nothing -> do
      return result

expressionParser' :: Parser Expression'
expressionParser' = do
  functionDefinitionParser
    <|> conditionParser
    <|> recordParser
    <|> (RightHandSideString <$> (mixedTextParser <* sc))
    <|> listParser
    <|> agebraicDataTypeParser
    <|> variableParser

conditionParser :: Parser Expression'
conditionParser = do
  _ <- string "if " *> sc
  condition <- expressionParser
  _ <- string "then" *> sc *> eol
  indentationLevel <- get
  thenCase <- some (indentationParser (indentationLevel + 1) *> statementParser) <* eol
  _ <- indentationParser indentationLevel *> string "else" *> sc *> eol
  elseCase <- some (indentationParser (indentationLevel + 1) *> statementParser) <* eol

  return (RightHandSideCondition condition thenCase elseCase)

agebraicDataTypeParser :: Parser Expression'
agebraicDataTypeParser = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead (char '('))
  parameters <- case hasParameter of
    Just _ -> do
      indentationLevel <- get
      blockParser (char '(' *> sc) (char ')' *> sc) expressionParser
    Nothing -> do return []
  return (RightHandSideAlgebraicDataType name parameters)

recordParser :: Parser Expression'
recordParser = do
  indentationLevel <- get
  properties <- blockParser (char '{' *> sc) (lookAhead (char '}' <|> char '|')) recordOptionParser

  hasSource <- optional (char '|' <* sc)

  source <-
    case hasSource of
      Just _ -> do
        statementParser `sepBy1` delimiterParser (indentationLevel + 1)
      Nothing -> do
        return []

  _ <- char '}' <* sc

  return (RightHandSideRecord properties source)

recordOptionParser :: Parser (String, Maybe String, Expression)
recordOptionParser = do
  key <- lowercaseIdentifierParser <* sc <* assignParser <* sc
  value <- expressionParser
  return (key, Nothing, value)

functionDefinitionParser :: Parser Expression'
functionDefinitionParser = do
  indentationLevel <- get
  parameters <- blockParser (char '/' <* sc) (string "->" <* sc) leftHandSideParser

  hasEol <- optional eol

  functionBody <- case hasEol of
    Just _ -> do
      _ <- indentationParser (indentationLevel + 1)
      statementParser `sepBy1` try (eol *> indentationParser (indentationLevel + 1))
    Nothing -> do
      result <- statementParser
      return [result]
  return (RightHandSideFunctionDefinition parameters functionBody)

listParser :: Parser Expression'
listParser = do
  indentationLevel <- get
  entities <- blockParser (char '[' *> sc) (lookAhead (char ']' <|> char '|')) expressionParser

  hasSource <- optional (char '|' <* sc)

  source <-
    case hasSource of
      Just _ -> do
        statementParser `sepBy1` delimiterParser (indentationLevel + 1)
      Nothing -> do
        return []

  _ <- char ']' <* sc

  return (RightHandSideList entities source)

mixedTextParser :: Parser [RightHandSideString]
mixedTextParser =
  do char '\"'
    *> (dynamicTextParser <|> staticTextParser) `manyTill` char '"'

staticTextParser :: Parser RightHandSideString
staticTextParser = do
  text <- charLiteral `manyTill` lookAhead (string "\"" <|> string "${")
  return (RightHandSideStringStatic text)

dynamicTextParser :: Parser RightHandSideString
dynamicTextParser = do
  value <- string "${" *> expressionParser <* char '}'

  return (RightHandSideStringDynamic value)

variableParser :: Parser Expression'
variableParser = do RightHandSideVariable <$> (lowercaseIdentifierParser <* sc)

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

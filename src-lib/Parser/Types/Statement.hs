module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get)
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, delimiterParser, indentationParser, lowercaseIdentifierParser, sc)
import Text.Megaparsec (between, lookAhead, many, manyTill, optional, sepBy, sepBy1, try)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

statementParser :: Parser Statement
statementParser = Expression <$> expressionParser

expressionParser :: Parser Expression
expressionParser =
  functionDefinitionParser
    <|> recordParser
    <|> (RightHandSideString <$> (mixedTextParser <* sc))
    <|> listParser
    <|> variableParser

recordParser :: Parser Expression
recordParser = do
  indentationLevel <- get
  properties <- between (char '{' *> sc *> optional (try (eol *> indentationParser (indentationLevel + 1)))) (lookAhead (char '}' <|> char '|')) (recordOptionParser `sepBy` delimiterParser (indentationLevel + 1))

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

functionDefinitionParser :: Parser Expression
functionDefinitionParser = do
  indentationLevel <- get
  parameters <- between (char '/' <* sc) (string "->" <* sc) (leftHandSideParser `sepBy` delimiterParser (indentationLevel + 1))

  hasEol <- optional eol

  functionBody <- case hasEol of
    Just _ -> do
      _ <- indentationParser (indentationLevel + 1)
      statementParser `sepBy1` try (eol *> indentationParser (indentationLevel + 1))
    Nothing -> do
      result <- statementParser
      return [result]
  return (RightHandSideFunctionDefinition parameters functionBody)

listParser :: Parser Expression
listParser = do
  indentationLevel <- get
  entities <- between (char '[' <* sc) (lookAhead (char ']' <|> char '|')) (expressionParser `sepBy` delimiterParser (indentationLevel + 1))

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

variableParser :: Parser Expression
variableParser = do RightHandSideVariable <$> (lowercaseIdentifierParser <* sc)

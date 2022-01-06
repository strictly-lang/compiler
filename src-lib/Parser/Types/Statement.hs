module Parser.Types.Statement where

import Control.Applicative ((<|>))
import Parser.Types
import Parser.Types.LeftHandSide (leftHandSideParser)
import Parser.Util (assignParser, delimiterParser, indentationParser, lowercaseIdentifierParser, sc)
import Text.Megaparsec (between, lookAhead, many, manyTill, optional, sepBy, sepBy1)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

statementParser :: Parser Statement
statementParser = Expression <$> expressionParser

expressionParser :: Parser Expression
expressionParser =
  functionDefinitionParser
    <|> recordParser
    <|> (RightHandSideString <$> mixedTextParser)
    <|> listParser

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

listParser :: Parser Expression
listParser = do
  entities <- between (char '[' <* sc) (lookAhead (char ']' <|> char '|')) (expressionParser `sepBy` (char ',' <* sc))

  hasSource <- optional (char '|' <* sc)

  source <-
    case hasSource of
      Just _ -> do
        statementParser `sepBy1` (char ',' <* sc)
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
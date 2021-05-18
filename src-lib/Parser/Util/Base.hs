module Parser.Util.Base (indentParser, expressionParser, rightHandSideParser, mixedTextParser, optionsParser, optionParser) where

import Control.Applicative (Alternative (many), optional, (<|>))
import Text.Megaparsec (MonadParsec (lookAhead, try), between, manyTill, sepBy, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, space, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

-- indentParser :: Pos  -> Parser a -> Parser a
-- indentParser position parser = do
--     _ <- indentGuard space GT position
--     parser

indentParser :: IndentationLevel -> Parser a -> Parser a
indentParser indentationLevel parser = do
  _ <- string (replicate indentationLevel '\t')
  parser

mixedTextParser :: Parser [MixedText]
mixedTextParser =
  do char '\"'
    *> (dynamicTextParser <|> staticTextParser) `manyTill` char '"'

staticTextParser :: Parser MixedText
staticTextParser = do
  text <- charLiteral `manyTill` lookAhead (string "\"" <|> string "${")
  return (StaticText text)

dynamicTextParser :: Parser MixedText
dynamicTextParser = do
  value <- string "${" *> rightHandSideParser <* char '}'

  return (DynamicText value)

expressionParser :: Parser Expression
expressionParser = do
  leftHandSide <- leftHandSideParser
  operator <- operatorParser
  rightHandSide <- rightHandSideParser

  return (Expression (leftHandSide, operator, rightHandSide))

leftHandSideTupleParser :: Parser LeftHandSide
leftHandSideTupleParser = do
  tuples <- between (char '(') (char ')') (sepBy leftHandSideParser (char ',' <* space))
  return (LeftTuple tuples)

leftHandSideVariableParser :: Parser LeftHandSide
leftHandSideVariableParser = do
  hasHole <- optional (char '_')
  case hasHole of
    Just _ -> do
      _ <- space
      return (LeftVariable Nothing)
    Nothing ->
      do
        variable <- some letterChar
        _ <- space
        return (LeftVariable (Just variable))

leftHandSideParser :: Parser LeftHandSide
leftHandSideParser = (leftHandSideTupleParser <|> leftHandSideVariableParser) <* space

feedOperatorParser :: Parser Operator
feedOperatorParser = do
  _ <- string "<-"
  return FeedOperator

operatorParser :: Parser Operator
operatorParser = feedOperatorParser <* space

rightHandSideVariableParser :: Parser RightHandSide
rightHandSideVariableParser = do
  variableName <- some letterChar `sepBy` char '.'
  return (Variable variableName)

rightHandSideTextParser :: Parser RightHandSide
rightHandSideTextParser = do MixedTextValue <$> mixedTextParser

rightHandSideParser :: Parser RightHandSide
rightHandSideParser = rightHandSideNumberParser <|> rightHandSideTextParser <|> try rightHandSideVariableParser <|> rightHandSideFunctionParser

rightHandSideNumberParser :: Parser RightHandSide
rightHandSideNumberParser = do
  value <- some digitChar
  return (Number (read value))

rightHandSideFunctionParser :: Parser RightHandSide
rightHandSideFunctionParser = do
  arguments <- try (leftHandSideParser `sepBy1` space1)
  _ <- string "->"
  _ <- space
  FunctionDefinition arguments <$> rightHandSideParser

optionsParser :: IndentationLevel -> Parser a -> Parser [a]
optionsParser indentationLevel optionParser = do
  hasOptions <- optional (between (char '{' *> eol) (indentParser indentationLevel (char '}')) (many (indentParser (indentationLevel + 1) (optionParser <* eol))))
  _ <- eol
  case hasOptions of
    Just options -> do
      return options
    Nothing -> return []

optionParser :: Parser Option
optionParser = do
  attributeName <- some letterChar <* space
  _ <- char '='
  _ <- space
  rightHandSide <- rightHandSideParser
  return (attributeName, rightHandSide)
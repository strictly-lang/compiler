module Parser.Util.Base (indentParser, expressionParser, rightHandSideParser, mixedTextParser) where

import Control.Applicative (Alternative (many), (<|>))
import Text.Megaparsec (MonadParsec (eof, lookAhead), Pos, atEnd, between, manyTill, sepBy, sepBy1, some)
import Text.Megaparsec.Char (char, eol, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (charLiteral, indentGuard)
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
  variable <- some letterChar
  _ <- space
  return (LeftVariable variable)

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
  variableName <- some letterChar `sepBy1` char '.'
  return (Variable variableName)

rightHandSideTextParser :: Parser RightHandSide
rightHandSideTextParser = do MixedTextValue <$> mixedTextParser

rightHandSideParser :: Parser RightHandSide
rightHandSideParser = rightHandSideVariableParser <|> rightHandSideTextParser

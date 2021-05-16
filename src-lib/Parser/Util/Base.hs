module Parser.Util.Base (indentParser, expressionParser, rightHandSideParser, mixedTextParser) where

import Control.Applicative (Alternative (many), (<|>))
import Text.Megaparsec (MonadParsec (eof, lookAhead), Pos, atEnd, between, manyTill, sepBy, sepBy1)
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
  do
    text <- char '\"' *> (dynamicTextParser <|> staticTextParser) `manyTill` char '"'
    _ <- eol
    return text

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

leftHandTupleParser :: Parser LeftHandSide
leftHandTupleParser = do
  tuples <- between (char '(') (char ')') (sepBy leftHandSideParser (char ',' <* space))
  return (LeftTuple tuples)

leftHandVariableParser :: Parser LeftHandSide
leftHandVariableParser = do 
    variable <- many letterChar
    -- _ <- space
    return (LeftVariable variable)

leftHandSideParser :: Parser LeftHandSide
leftHandSideParser = (leftHandTupleParser <|> leftHandVariableParser )<* space

feedOperatorParser :: Parser Operator
feedOperatorParser = do
    _ <- string "<-"
    return FeedOperator

operatorParser :: Parser Operator
operatorParser = feedOperatorParser <* space

rightHandSideParser :: Parser RightHandSide
rightHandSideParser = do
  variableName <- many letterChar `sepBy1` char '.'
  return (Variable variableName)

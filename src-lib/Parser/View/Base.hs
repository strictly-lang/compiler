module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative (optional, (<|>))
import Parser.Util.Base (expressionParser, indentParser, mixedTextParser, rightHandSideParser, optionsParser, optionParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, manyTill, sepBy1, some)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, newline, space, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral, indentLevel, symbol)
import Types

viewParser :: Parser Root
viewParser = do
  _ <- string "view"
  _ <- newline
  viewContent <- some (viewContentParser 1)
  return (View viewContent)

viewContentParser :: IndentationLevel -> Parser ViewContent
viewContentParser indentationLevel = indentParser indentationLevel (hostParser indentationLevel <|> helperParser indentationLevel <|> textParser)

hostParser :: IndentationLevel -> Parser ViewContent
hostParser indentationLevel = do
  hostElement <- some lowerChar
  options <- optionsParser indentationLevel optionParser
  children <- many (viewContentParser (indentationLevel + 1))
  return (Host hostElement options children)

hostOptionParser :: Parser Option
hostOptionParser = do
  attributeName <- some letterChar <* space
  _ <- char '='
  rightHandSide <- rightHandSideParser
  return (attributeName, rightHandSide)

helperParser :: IndentationLevel -> Parser ViewContent
helperParser indentationLevel = do
  _ <- char '#'
  eachParser indentationLevel <|> ifParser indentationLevel

ifParser :: IndentationLevel -> Parser ViewContent
ifParser indentationLevel = do
  _ <- string "if"
  _ <- space1
  rightHandSide <- rightHandSideParser
  _ <- eol
  children <- many (viewContentParser (indentationLevel + 1))
  elseChildren <- indentParser indentationLevel (elseParser 1)

  return (Condition rightHandSide children elseChildren)

elseParser :: IndentationLevel -> Parser [ViewContent]
elseParser indentationLevel = do
  _ <- string "#else"
  _ <- eol
  many (viewContentParser (indentationLevel + 1))

eachParser :: IndentationLevel -> Parser ViewContent
eachParser indentationLevel = do
  _ <- string "each"
  _ <- space1
  option <- expressionParser
  _ <- eol
  children <- many (viewContentParser (indentationLevel + 1))
  elseChildren <- indentParser indentationLevel (elseParser 1)

  return (Each [option] children elseChildren)

textParser :: Parser ViewContent
textParser = do MixedText <$> (mixedTextParser <* eol)

module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative (optional, (<|>))
import Parser.Util.Base (expressionParser, indentParserRepeat, indentParser, mixedTextParser, optionsParser, rightHandSideValueParser, sc)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, manyTill, sepBy1, some)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, newline, space1, string)
import Text.Megaparsec.Char.Lexer (charLiteral, indentLevel, symbol)
import Types

viewParser :: Parser Root
viewParser = do
   _ <- string "view"
   _ <- newline
   View <$> viewContentParser 1

viewContentParser :: IndentationLevel -> Parser [ViewContent]
viewContentParser indentationLevel = indentParserRepeat indentationLevel (hostParser indentationLevel <|> helperParser indentationLevel <|> textParser)

hostParser :: IndentationLevel -> Parser ViewContent
hostParser indentationLevel = do
  hostElement <- some lowerChar
  options <- optionsParser indentationLevel hostOptionParser
  children <- viewContentParser (indentationLevel + 1)
  return (Host hostElement options children)

hostOptionParser :: Parser (Option RightHandSideValue)
hostOptionParser = do
  attributeName <- some letterChar <* sc
  _ <- char '='
  rightHandSide <- rightHandSideValueParser
  return (attributeName, rightHandSide)

helperParser :: IndentationLevel -> Parser ViewContent
helperParser indentationLevel = do
  _ <- char '#'
  eachParser indentationLevel <|> ifParser indentationLevel

ifParser :: IndentationLevel -> Parser ViewContent
ifParser indentationLevel = do
  _ <- string "if" <* sc
  rightHandSide <- rightHandSideValueParser
  _ <- eol
  children <- viewContentParser (indentationLevel + 1)
  elseChildren <- elseParser indentationLevel

  return (Condition rightHandSide children elseChildren)

elseParser :: IndentationLevel -> Parser [ViewContent]
elseParser indentationLevel = do
  _ <- indentParser indentationLevel (string "#else" <* eol)
  viewContentParser (indentationLevel + 1)

eachParser :: IndentationLevel -> Parser ViewContent
eachParser indentationLevel = do
  _ <- string "each" <* space1
  option <- expressionParser rightHandSideValueParser
  _ <- eol
  children <- viewContentParser (indentationLevel + 1)
  elseChildren <- elseParser indentationLevel

  return (Each [option] children elseChildren)

textParser :: Parser ViewContent
textParser = do MixedText <$> (mixedTextParser <* eol)

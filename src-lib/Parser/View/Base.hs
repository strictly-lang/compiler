module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative (optional, (<|>))
import Parser.Util.Base (expressionParser, indentParser, indentParserRepeat, mixedTextParser, optionsParser, rightHandSideFunctionParser, rightHandSideValueParser, sc, identityParser)
import Text.Megaparsec (MonadParsec (lookAhead), between, many, manyTill, sepBy1, some)
import Text.Megaparsec.Char (char, eol, lowerChar, newline, space1, string)
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

hostOptionParser :: Parser (Option RightHandSide)
hostOptionParser = do
  isEvent <- optional (string "on")
  attributeName <- identityParser
  _ <- sc *> char '=' <* sc
  case isEvent of
    Just _ -> do
      functionDefinition <- rightHandSideFunctionParser
      return (attributeName, functionDefinition)
    Nothing -> do
      rightHandSide <- rightHandSideValueParser
      return (attributeName, RightHandSideValue rightHandSide)

helperParser :: IndentationLevel -> Parser ViewContent
helperParser indentationLevel = do
  _ <- char '#'
  eachParser indentationLevel <|> ifParser indentationLevel <|> modelParser indentationLevel

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

modelParser :: IndentationLevel -> Parser ViewContent
modelParser indentationLevel = do
  _ <- string "model" <* space1
  option <- expressionParser rightHandSideValueParser
  _ <- eol
  children <- viewContentParser (indentationLevel + 1)
  return (ViewModel option children)
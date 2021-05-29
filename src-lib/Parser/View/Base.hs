module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative (optional, (<|>))
import Parser.Util.Base (expressionParser, identityParser, indentParser, indentParserRepeat, leftHandSideParser, mergeOptions, mixedTextParser, optionsParser, rightHandSideFunctionParser, rightHandSideParser, rightHandSideValueParser, sc)
import Text.Megaparsec (between, many, manyTill, sepBy1, some)
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
  return (Host hostElement (mergeOptions options) children)

hostOptionParser :: Parser (Option RightHandSide)
hostOptionParser = do
  attributeName <- identityParser
  _ <- sc *> char '=' <* sc
  rightHandSide <- rightHandSideParser
  return (attributeName, rightHandSide)

helperParser :: IndentationLevel -> Parser ViewContent
helperParser indentationLevel = do
  _ <- char '#'
  eachParser indentationLevel <|> ifParser indentationLevel <|> modelParser indentationLevel <|> matchParser indentationLevel

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

matchParser :: IndentationLevel -> Parser ViewContent
matchParser indentationLevel = do
  _ <- string "match" <* space1
  value <- rightHandSideValueParser
  _ <- eol
  children <- indentParserRepeat (indentationLevel + 1) (caseParser (indentationLevel + 1))
  return (Match value children)

caseParser :: IndentationLevel -> Parser Case
caseParser indentationLevel = do
  _ <- string "#case" <* space1
  match <- leftHandSideParser

  children <- viewContentParser (indentationLevel + 1)

  return (Case match children)
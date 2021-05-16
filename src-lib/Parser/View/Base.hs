module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative ((<|>))
import Parser.Util.Base (indentParser)
import Text.Megaparsec (MonadParsec (lookAhead), many, manyTill, sepBy1, some)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, newline, string)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

viewParser :: Parser Root
viewParser = do
  _ <- string "view"
  _ <- newline
  viewContent <- some (viewContentParser 1)
  return (View viewContent)

viewContentParser :: IndentationLevel -> Parser ViewContent
viewContentParser indentationLevel = indentParser indentationLevel (hostParser indentationLevel <|> mixedTextParser)

hostParser :: IndentationLevel -> Parser ViewContent
hostParser indentationLevel = do
  hostElement <- some lowerChar
  _ <- eol
  children <- many (viewContentParser (indentationLevel + 1))
  return (Host hostElement children [])

mixedTextParser :: Parser ViewContent
mixedTextParser =
  do
    text <- char '\"' *> (dynamicTextParser <|> staticTextParser) `manyTill` char '"'
    _ <- eol
    return (Text text)

staticTextParser :: Parser MixedText
staticTextParser = do
  text <- charLiteral `manyTill` lookAhead (string "\"" <|> string "${")
  return (StaticText text)

dynamicTextParser :: Parser MixedText
dynamicTextParser = do
  value <- string "${" *> rightHandSideParser <* char '}'

  return (DynamicText value)

rightHandSideParser :: Parser RightHandSide
rightHandSideParser = do
  variableName <- many letterChar `sepBy1` char '.'
  return (Variable variableName)
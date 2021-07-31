module Parser.Style.Base (styleParser) where

import Control.Applicative (Alternative (some), optional, (<|>))
import Parser.Util.Base (identityParser, indentParserRepeat, optionsParser, rightHandSideValueParser, sc)
import Text.Megaparsec (sepBy1)
import Text.Megaparsec.Char (char, space1, string)
import Types

styleParser :: Parser Root
styleParser = do
  _ <- string "style"
  let indentationLevel = 1
  Style <$> indentParserRepeat indentationLevel (styleContentParser indentationLevel)

styleContentParser :: Int -> Parser StyleContent
styleContentParser indentationLevel = do
  selector <- some selectorParser
  options <- optionsParser indentationLevel styleOptionParser
  return (StyleContent (concat selector, options))

selectorParser :: Parser String
selectorParser = do
  prefix <- optional (((: []) <$> char '.') <|> ((: []) <$> char '#') <|> some (char ' '))
  identiyName <- identityParser

  case prefix of
    Just prefix -> do
      return (prefix ++ identiyName)
    Nothing -> do
      return identiyName

styleOptionParser :: Parser (Option RightHandSideValue)
styleOptionParser = do
  attributeName <- identityParser `sepBy1` char '-'
  _ <- sc *> char '=' <* sc
  rightHandSide <- rightHandSideValueParser
  return (concat attributeName, rightHandSide)
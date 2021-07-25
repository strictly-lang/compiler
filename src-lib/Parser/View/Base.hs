module Parser.View.Base (viewParser, viewContentParser) where

import Control.Applicative (optional, (<|>))
import Data.List
import Parser.Util.Base (identityParser, indentParser, indentParserRepeat, leftHandSideParser, mergeOptions, mixedTextParser, optionsParser, rightHandSideFeedParser, rightHandSideFunctionParser, rightHandSideParser, rightHandSideValueParser, sc)
import Text.Megaparsec (sepBy1, some)
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
  (hostElement, importPath) <- hostElementeParser
  options <- optionsParser indentationLevel hostOptionParser
  children <- viewContentParser (indentationLevel + 1)
  return (Host (HostElement (hostElement, mergeOptions options, children)) importPath)

hostElementeParser :: Parser (String, Maybe Import)
hostElementeParser = hostElementRelativeComponentParser <|> hostElementParser

hostElementRelativeComponentParser :: Parser (String, Maybe Import)
hostElementRelativeComponentParser = do
  _ <- char '.'
  _ <- char '-'
  componentName <- componentNameParser

  return (intercalate "-" componentName, Just (Import ("./" ++ intercalate "/" componentName ++ ".sly", [])))

hostElementParser :: Parser (String, Maybe Import)
hostElementParser = do
  elementName <- componentNameParser
  return (intercalate "-" elementName, if length elementName > 1 then Just (Import ("/" ++ intercalate "/" elementName ++ ".sly", [])) else Nothing)

componentNameParser :: Parser [String]
componentNameParser = some lowerChar `sepBy1` char '-'

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
  option <- rightHandSideFeedParser
  _ <- eol
  children <- viewContentParser (indentationLevel + 1)
  elseChildren <- elseParser indentationLevel

  return (Each option children elseChildren)

textParser :: Parser ViewContent
textParser = do MixedText <$> (mixedTextParser <* eol)

modelParser :: IndentationLevel -> Parser ViewContent
modelParser indentationLevel = do
  _ <- string "model" <* space1
  option <- rightHandSideFeedParser
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
module Parser.Util.Base (feedParser, mixedTextParser, optionsParser, rightHandSideFunctionParser, rightHandSideValueParser, sc, indentParserRepeat, indentParser, identityParser, typeParser, mergeOptions, rightHandSideParser, leftHandSideParser) where

import Control.Applicative (Alternative (many), optional, (<|>))
import Text.Megaparsec (MonadParsec (lookAhead), between, manyTill, sepBy, sepBy1, some)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, lowerChar, string, upperChar)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Types

-- indentParser :: Pos  -> Parser a -> Parser a
-- indentParser position parser = do
--     _ <- indentGuard space GT position
--     parser

indentParser :: IndentationLevel -> Parser a -> Parser a
indentParser indentationLevel parser = do
  _ <- many eol -- Empty lines should not break indentation-handling
  _ <- string (replicate indentationLevel '\t')
  parser

indentParserRepeat :: IndentationLevel -> Parser a -> Parser [a]
indentParserRepeat indentationLevel parser = do
  many (indentParser indentationLevel parser)

identityParser :: Parser String
identityParser = do
  firstChar <- lowerChar
  rest <- many letterChar
  return (firstChar : rest)

typeParser :: Parser String
typeParser = do
  firstChar <- upperChar
  rest <- many letterChar
  return (firstChar : rest)

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
  value <- string "${" *> rightHandSideValueParser <* char '}'

  return (DynamicText value)

leftHandSideTupleParser :: Parser LeftHandSide
leftHandSideTupleParser = do
  tuples <- between (char '<') (char '>') (leftHandSideParser `sepBy` (char ',' <* sc))
  return (LeftTuple tuples)

leftHandSideHoleParser :: Parser LeftHandSide
leftHandSideHoleParser = do
  _ <- char '_'
  return LeftHole

leftHandSideVariableParser :: Parser LeftHandSide
leftHandSideVariableParser = do
  variable <- identityParser <* sc
  hasAlias <- optional (char '@')

  ( case hasAlias of
      Just _ -> do LeftAlias variable <$> leftHandSideParser
      Nothing -> do return (LeftVariable variable)
    )

leftHandSideTypeParser :: Parser LeftHandSide
leftHandSideTypeParser = do
  typeName <- typeParser
  hasOptions <- optional (lookAhead (char '('))
  case hasOptions of
    Just _ -> do
      values <- between (char '(' <* sc) (char ')') (leftHandSideParser `sepBy` (char ',' <* sc))
      return (LeftType typeName values)
    Nothing -> do
      return (LeftType typeName [])

leftHandSideRecordParser :: Parser LeftHandSide
leftHandSideRecordParser = do
  destructuredProperties <- between (char '{' <* sc) (char '}' <* sc) (leftHandSideRecordEntityParser `sepBy` (char ',' <* sc))
  return (LeftRecord destructuredProperties)

leftHandSideListParser :: Parser LeftHandSide
leftHandSideListParser = do
  leftEntities <- between (char '[' <* sc) (lookAhead (char ']' <|> char '|') <* sc) (leftHandSideParser `sepBy` (char ',' <* sc))

  hasRest <- optional (char '|' <* sc)

  rest <- case hasRest of
    Just _ -> do
      Just <$> leftHandSideParser
    Nothing -> do
      return Nothing

  _ <- char ']' <* sc

  return (LeftList leftEntities rest)

leftHandSideRecordEntityParser :: Parser (String, Maybe LeftHandSide)
leftHandSideRecordEntityParser = do
  propertyName <- identityParser <* sc
  hasAlias <- optional (char '=' <* sc)

  case hasAlias of
    Just _ -> do
      alias <- leftHandSideParser
      return (propertyName, Just alias)
    Nothing -> do
      return (propertyName, Nothing)

leftHandSideParser :: Parser LeftHandSide
leftHandSideParser = (leftHandSideHoleParser <|> leftHandSideTupleParser <|> leftHandSideVariableParser <|> leftHandSideTypeParser <|> leftHandSideRecordParser <|> leftHandSideListParser) <* sc

rightHandSideOperatorParser :: Parser RightHandSideOperator
rightHandSideOperatorParser = do
  ( rightHandSideOperatorEqualParser
      <|> rightHandSideOperatorUnequalParser
      <|> rightHandSideOperatorPlusParser
      <|> rightHandSideOperatorMinusParser
      <|> rightHandSideOperatorMultiplyParser
      <|> rightHandSideOperatorDivisionParser
      <|> rightHandSideOperatorModuloParser
    )
    <* sc

rightHandSideOperatorEqualParser :: Parser RightHandSideOperator
rightHandSideOperatorEqualParser = do
  _ <- string "=="
  return Equal

rightHandSideOperatorUnequalParser :: Parser RightHandSideOperator
rightHandSideOperatorUnequalParser = do
  _ <- string "!="
  return Unequal

rightHandSideOperatorPlusParser :: Parser RightHandSideOperator
rightHandSideOperatorPlusParser = do
  _ <- char '+'
  return Plus

rightHandSideOperatorMinusParser :: Parser RightHandSideOperator
rightHandSideOperatorMinusParser = do
  _ <- char '-'
  return Minus

rightHandSideOperatorMultiplyParser :: Parser RightHandSideOperator
rightHandSideOperatorMultiplyParser = do
  _ <- char '*'
  return Multiply

rightHandSideOperatorDivisionParser :: Parser RightHandSideOperator
rightHandSideOperatorDivisionParser = do
  _ <- char '/'
  return Division

rightHandSideOperatorModuloParser :: Parser RightHandSideOperator
rightHandSideOperatorModuloParser = do
  _ <- char '%'
  return Modulo

rightHandSideValueRecordParser :: Parser RightHandSideValue
rightHandSideValueRecordParser = do
  entities <- between (char '{' <* sc) (lookAhead (char '}' <|> char '|')) (rightHandSideValueRecordEntityParser `sepBy` (char ',' <* sc))

  hasSource <- optional (char '|' <* sc)

  source <-
    case hasSource of
      Just _ -> do
        Just <$> rightHandSideValueParser
      Nothing -> do
        return Nothing

  _ <- char '}' <* sc

  return (RightHandSideRecord entities source)

rightHandSideValueListParser :: Parser RightHandSideValue
rightHandSideValueListParser = do
  entities <- between (char '[' <* sc) (lookAhead (char ']' <|> char '|')) (rightHandSideValueParser `sepBy` (char ',' <* sc))

  hasSource <- optional (char '|' <* sc)

  source <-
    case hasSource of
      Just _ -> do
        rightHandSideValueListSourceOrFilterParser `sepBy1` (char ',' <* sc)
      Nothing -> do
        return []

  _ <- char ']' <* sc

  return (RightHandSideList entities source)

rightHandSideValueListSourceOrFilterParser :: Parser ListSourceOrFilter
rightHandSideValueListSourceOrFilterParser = do
  (ListSource <$> feedParser rightHandSideValueParser) <|> (Filter <$> rightHandSideValueParser)

feedParser :: Parser a -> Parser (LeftHandSide, a)
feedParser parser = do
  _ <- char '\\' <* sc
  leftHandSide <- leftHandSideParser
  _ <- string "<-" <* sc
  rightHandSide <- parser
  return (leftHandSide, rightHandSide)

rightHandSideValueRecordEntityParser :: Parser (String, RightHandSideValue)
rightHandSideValueRecordEntityParser = do
  name <- identityParser
  _ <- sc <* char '=' <* sc
  value <- rightHandSideValueParser

  return (name, value)

rightHandSideValueVariableParser :: Parser RightHandSideValue
rightHandSideValueVariableParser = do
  variableName <- Variable <$> (identityParser `sepBy1` char '.')
  hasFunctionCall <- optional (char '(')

  case hasFunctionCall of
    Just _ -> do
      -- When syntax-error inside arguments, endless loop occures
      arguments <- manyTill rightHandSideValueParser (char ')')
      return (FunctionCall variableName arguments)
    Nothing -> return variableName

rightHandSideValueTextParser :: Parser RightHandSideValue
rightHandSideValueTextParser = do MixedTextValue <$> mixedTextParser

rightHandSideValueTypeParser :: Parser RightHandSideValue
rightHandSideValueTypeParser = do
  typeName <- typeParser
  hasOptions <- optional (lookAhead (char '('))
  case hasOptions of
    Just _ -> do
      values <- between (char '(' <* sc) (char ')') (rightHandSideValueParser `sepBy` (char ',' <* sc))
      return (RightHandSideType typeName values)
    Nothing -> do
      return (RightHandSideType typeName [])

rightHandSideValueParser :: Parser RightHandSideValue
rightHandSideValueParser = do
  rightHandSideValue <- (rightHandSideValueTypeParser <|> rightHandSideValueNumberParser <|> rightHandSideValueTextParser <|> rightHandSideValueRecordParser <|> rightHandSideValueVariableParser <|> rightHandSideValueListParser <|> rightHandSideValueTupleParser) <* sc
  optionalOperator <- optional rightHandSideOperatorParser

  case optionalOperator of
    Just operator -> do
      RightHandSideOperation operator rightHandSideValue <$> rightHandSideValueParser
    Nothing -> return rightHandSideValue

rightHandSideValueTupleParser :: Parser RightHandSideValue
rightHandSideValueTupleParser = do
  tuples <- between (char '<') (char '>') (rightHandSideValueParser `sepBy` (char ',' <* sc))
  return (Tuple tuples)

rightHandSideValueNumberParser :: Parser RightHandSideValue
rightHandSideValueNumberParser = do
  value <- some digitChar
  return (Number (read value))

rightHandSideFunctionParser :: Parser RightHandSide
rightHandSideFunctionParser = do
  _ <- char '/' <* sc
  arguments <- leftHandSideParser `sepBy` (char ',' <* sc) <* string "->" <* sc
  FunctionDefinition arguments <$> rightHandSideValueParser

rightHandSideParser :: Parser RightHandSide
rightHandSideParser = rightHandSideFunctionParser <|> (RightHandSideValue <$> rightHandSideValueParser)

optionsParser :: IndentationLevel -> Parser a -> Parser [a]
optionsParser indentationLevel optionValueParser = do
  hasOptions <- optional (between (char '{' *> eol) (indentParser indentationLevel (char '}')) (indentParserRepeat (indentationLevel + 1) (optionValueParser <* eol)))
  _ <- eol
  case hasOptions of
    Just options -> do
      return options
    Nothing -> return []

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()

mergeOptions :: [Option a] -> [Option [a]]
mergeOptions [] = []
mergeOptions [(optionName, optionValue)] = [(optionName, [optionValue])]
mergeOptions ((firstOptionName, firstOptionValue) : secondOption@(secondOptionName, secondOptionValue) : restOptions)
  | firstOptionName == secondOptionName =
    let (_, secondOptionMerge) : restOptions' = mergeOptions (secondOption : restOptions)
     in (firstOptionName, firstOptionValue : secondOptionMerge) : restOptions'
  | otherwise = (firstOptionName, [firstOptionValue]) : mergeOptions (secondOption : restOptions)
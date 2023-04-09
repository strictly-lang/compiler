module Parser.Statement (statementParser, expressionParser) where

import Control.Applicative ((<|>))
import Parser.LeftHandSide (leftHandSideParser)
import Parser.Types
import Parser.Util (assignParser, baseOfParser, blockParser, functionBodyParser, functionCallCloseParser, functionCallOpenParser, functionDefinitionParser, indentationParser, listCloseParser, listOpenParser, lowercaseIdentifierParser, numberParser, recordCloseParser, recordOpenParser, sc, statementTerminationParser, streamParser, typeAssignParser, typeDefinitionParser, uppercaseIdentifierParser)
import Text.Megaparsec (lookAhead, many, manyTill, optional, some, try)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, string)
import Text.Megaparsec.Char.Lexer (charLiteral)

statementParser :: IndentationLevel -> Parser ASTStatement
statementParser indentationLevel =
  letParser indentationLevel
    <|> (ASTExpression <$> expressionParser indentationLevel)

-----------------------
-- Statement-Parsers --
-----------------------

letParser :: IndentationLevel -> Parser ASTStatement
letParser indentationLevel = do
  _ <- string "let " <* sc
  leftHandSide <- leftHandSideParser indentationLevel
  kind <- Left <$> assignParser <|> Right <$> streamParser
  expression <- expressionParser indentationLevel

  result <- case kind of
    Left _ ->
      return (ASTStatementVariableExpressionAssignment leftHandSide expression)
    Right _ ->
      return (ASTStream leftHandSide expression)
  _ <- statementTerminationParser

  return result

------------------------
-- Expression-Parsers --
------------------------

expressionParser :: IndentationLevel -> Parser ASTExpression
expressionParser indentationLevel = do
  result <- expressionParser' indentationLevel

  operator <- optional operatorParser

  case operator of
    Just operator -> do
      nextExpression <- expressionParser indentationLevel
      return [ASTExpressionOperator operator result nextExpression]
    Nothing -> do
      return result

expressionParser' :: IndentationLevel -> Parser ASTExpression
expressionParser' indentationLevel = do
  expression <- expressionParser'' indentationLevel

  nested <- optional (char '.')

  result <- case nested of
    Just _ -> do
      nextExpressionPart <- expressionParser' indentationLevel
      return (expression : nextExpressionPart)
    Nothing -> do
      return [expression]

  hasFunctionCall <- optional (lookAhead functionCallOpenParser)

  case hasFunctionCall of
    Just _ -> do
      functionCall <- ASTExpressionFunctionCall <$> blockParser functionCallOpenParser functionCallCloseParser expressionParser indentationLevel
      return (result ++ [functionCall])
    Nothing -> do
      return result

expressionParser'' :: IndentationLevel -> Parser ASTExpression'
expressionParser'' indentationLevel = do
  rightHandSideFunctionDefinitionParser indentationLevel
    <|> rightHandSideConditionParser indentationLevel
    <|> rightHandSideMatchParser indentationLevel
    <|> rightHandSideRecordParser indentationLevel
    <|> rightHandSideNumberParser indentationLevel
    <|> rightHandSideMixedTextParser indentationLevel
    <|> rightHandSideListParser indentationLevel
    <|> rightHandSideAgebraicDataTypeParser indentationLevel
    <|> rightHandSideVariableParser indentationLevel
    <|> rightHandSideHostParser indentationLevel

rightHandSideConditionParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideConditionParser indentationLevel = do
  _ <- string "if " *> sc
  condition <- expressionParser indentationLevel
  _ <- string "then" *> sc
  thenCase <- some (indentationParser statementParser (indentationLevel + 1))
  _ <- indentationParser (\indentationLevel -> do string "else" *> sc) indentationLevel
  elseCase <- some (indentationParser statementParser (indentationLevel + 1))

  return (ASTExpressionCondition condition thenCase elseCase)

rightHandSideMatchParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideMatchParser indentationLevel = do
  _ <- string "match " *> sc
  matchTarget <- expressionParser indentationLevel
  cases <- many (indentationParser caseParser (indentationLevel + 1))

  return (ASTExpressionMatch matchTarget cases)

caseParser :: IndentationLevel -> Parser (ASTLeftHandSide, [ASTStatement])
caseParser indentationLevel = do
  _ <- string "case " *> sc
  ASTExpressionFunctionDeclaration [patterns] statements <- rightHandSideFunctionDefinitionParser indentationLevel
  return (patterns, statements)

rightHandSideAgebraicDataTypeParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideAgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <- case hasParameter of
    Just _ -> do
      blockParser functionCallOpenParser functionCallCloseParser expressionParser indentationLevel
    Nothing -> do return []
  return (ASTExpressionAlgebraicDataType name parameters)

rightHandSideRecordParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideRecordParser indentationLevel = do
  ASTExpressionRecord <$> recordParser indentationLevel

recordParser :: IndentationLevel -> Parser ASTRecord
recordParser indentationLevel = do
  ungroupedProperties <- recordOptionGrouper <$> blockParser recordOpenParser (lookAhead (recordCloseParser <|> baseOfParser)) recordOptionParser indentationLevel
  hasSource <- lookAhead (optional baseOfParser)

  case hasSource of
    Just _ -> do
      basedOn <- blockParser baseOfParser recordCloseParser statementParser indentationLevel
      return (ungroupedProperties, basedOn)
    Nothing -> do
      _ <- recordCloseParser
      return (ungroupedProperties, [])

type UngroupedOption = (String, Either ASTTypeDeclaration (Maybe String, ASTExpression))

recordOptionParser :: IndentationLevel -> Parser UngroupedOption
recordOptionParser indentationLevel = do
  key <- lowercaseIdentifierParser

  hasCondition <- optional (char '?' *> sc)

  condition <- case hasCondition of
    Just _ -> do
      Just <$> some letterChar <* sc
    Nothing -> do
      return Nothing

  kind <- Left <$> assignParser <|> Right <$> typeAssignParser

  value <- case kind of
    Left _ -> do
      expressionResult <- expressionParser indentationLevel
      return (Right (condition, expressionResult))
    Right _ ->
      Left <$> typeDefinitionParser indentationLevel

  return (key, value)

recordOptionGrouper :: [UngroupedOption] -> [GroupedRecordOption]
recordOptionGrouper [] = []
recordOptionGrouper ((currentUngroupedOptionName, Left typeDeclaration) : restUngroupedOptions) =
  let nextGroupedOptions = recordOptionGrouper restUngroupedOptions
   in case nextGroupedOptions of
        [] -> [(currentUngroupedOptionName, (Just typeDeclaration, []))]
        ((nextGroupedOptionName, (nextGroupedOptionTypeDelcaration, expressions)) : restNextGroupedOptions) ->
          if nextGroupedOptionName == currentUngroupedOptionName
            then
              ( case nextGroupedOptionTypeDelcaration of
                  Nothing -> (nextGroupedOptionName, (Just typeDeclaration, expressions))
                  (Just _) -> error ("Record contains two typedeclarations for " ++ currentUngroupedOptionName)
              )
                : restNextGroupedOptions
            else (currentUngroupedOptionName, (Just typeDeclaration, [])) : nextGroupedOptions
recordOptionGrouper ((currentUngroupedOptionName, Right expression) : restUngroupedOptions) =
  let nextGroupedOptions = recordOptionGrouper restUngroupedOptions
   in case nextGroupedOptions of
        [] -> [(currentUngroupedOptionName, (Nothing, [expression]))]
        ((nextGroupedOptionName, (nextGroupedOptionTypeDelcaration, expressions)) : restNextGroupedOptions) ->
          if nextGroupedOptionName == currentUngroupedOptionName
            then
              ( case nextGroupedOptionTypeDelcaration of
                  Nothing -> (nextGroupedOptionName, (Nothing, expressions))
                  (Just _) -> error ("Record contains a typedeclarations in the middle for " ++ currentUngroupedOptionName)
              )
                : restNextGroupedOptions
            else (currentUngroupedOptionName, (Nothing, [expression])) : nextGroupedOptions

rightHandSideFunctionDefinitionParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideFunctionDefinitionParser indentationLevel = do
  parameters <- blockParser functionDefinitionParser functionBodyParser leftHandSideParser indentationLevel

  hasEol <- lookAhead (optional eol)

  functionBody <- case hasEol of
    Just _ -> do
      some (indentationParser statementParser (indentationLevel + 1))
    Nothing -> do
      result <- statementParser indentationLevel
      return [result]
  return (ASTExpressionFunctionDeclaration parameters functionBody)

rightHandSideListParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideListParser indentationLevel = do
  entities <- blockParser listOpenParser (lookAhead (listCloseParser <|> baseOfParser)) expressionParser indentationLevel

  hasSource <- lookAhead (optional baseOfParser)

  ASTExpressionList entities
    <$> case hasSource of
      Just _ -> do
        blockParser baseOfParser listCloseParser statementParser indentationLevel
      Nothing -> do
        _ <- listCloseParser
        return []

rightHandSideNumberParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideNumberParser indentationLevel = do
  from <- numberParser

  hasRange <- optional (string "..")

  case hasRange of
    Just _ -> do
      ASTExpressionRange from <$> optional numberParser
    Nothing ->
      return (ASTExpressionNumber from)

rightHandSideMixedTextParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideMixedTextParser indentationLevel = do
  ASTExpressionString
    <$> (char '\"' *> (dynamicTextParser indentationLevel <|> staticTextParser indentationLevel) `manyTill` char '"')

staticTextParser :: IndentationLevel -> Parser ASTString
staticTextParser indentationLevel = do
  text <- charLiteral `manyTill` lookAhead (string "\"" <|> string "${")
  return (ASTStringStatic text)

dynamicTextParser :: IndentationLevel -> Parser ASTString
dynamicTextParser indentationLevel = do
  value <- string "${" *> expressionParser indentationLevel <* char '}'

  return (ASTStringDynamic value)

rightHandSideVariableParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideVariableParser indentationLevel = do ASTExpressionVariable <$> lowercaseIdentifierParser

rightHandSideHostParser :: IndentationLevel -> Parser ASTExpression'
rightHandSideHostParser indentationLevel = do
  _ <- char '$'
  hostName <- some lowerChar

  hasOptions <- optional (lookAhead recordOpenParser)

  record <- case hasOptions of
    Just _ -> recordParser indentationLevel
    Nothing -> do return ([], [])

  children <- many (indentationParser statementParser (indentationLevel + 1))
  return (ASTExpressionHost hostName record children)

---------------------
-- Operator Parser --
---------------------
operatorParser :: Parser Operator
operatorParser =
  do
    ( equalParser
        <|> unequalParser
        <|> concatParser
        <|> plusParser
        <|> minusParser
        <|> multiplyParser
        <|> divisionParser
        <|> moduloParser
      )
      <* sc

equalParser :: Parser Operator
equalParser = do
  _ <- string "=="
  return "equal"

unequalParser :: Parser Operator
unequalParser = do
  _ <- string "!="
  return "unequal"

concatParser :: Parser Operator
concatParser = do
  _ <- string "++"
  return "concat"

plusParser :: Parser Operator
plusParser = do
  _ <- string "+"
  return "plus"

minusParser :: Parser Operator
minusParser = do
  _ <- string "-"
  return "unequal"

multiplyParser :: Parser Operator
multiplyParser = do
  _ <- string "*"
  return "multiply"

divisionParser :: Parser Operator
divisionParser = do
  _ <- string "/"
  return "division"

moduloParser :: Parser Operator
moduloParser = do
  _ <- string "%"
  return "modulo"

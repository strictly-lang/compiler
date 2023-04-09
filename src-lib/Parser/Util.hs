module Parser.Util where

import Control.Applicative ((<|>))
import Parser.Types
import Text.Megaparsec (MonadParsec (lookAhead), many, optional, some, try)
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, lowerChar, space, string, upperChar)
import Text.Megaparsec.Char.Lexer

hole' :: Parser ()
hole' = do
  return ()

uppercaseIdentifierParser :: Parser String
uppercaseIdentifierParser = do
  firstChar <- upperChar
  rest <- many (letterChar <|> char '\'')
  _ <- sc
  return (firstChar : rest)

lowercaseIdentifierParser :: Parser String
lowercaseIdentifierParser = do
  firstChar <- lowerChar
  rest <- many (letterChar <|> char '\'')
  _ <- sc

  return (firstChar : rest)

numberParser :: Parser Int
numberParser = do
  result <- some digitChar

  decimalPlaces <- optional (try (char '.' *> some digitChar))

  result <- case decimalPlaces of
    Just decimalPlacesValue ->
      return (read (result ++ "." ++ decimalPlacesValue))
    Nothing ->
      return (read result)

  _ <- sc
  return result

blockParser :: Parser begin -> Parser end -> (IndentationLevel -> Parser a) -> IndentationLevel -> Parser [a]
blockParser beginParser endParser contentParser indentationLevel = do
  _ <- beginParser
  blockParser' True endParser contentParser indentationLevel

blockParser' :: Bool -> Parser end -> (IndentationLevel -> Parser a) -> IndentationLevel -> Parser [a]
blockParser' firstEntry endParser contentParser indentationLevel = do
  isEnd <- optional (endParser <|> try (optional delimiterParser *> indentationParser (const endParser) indentationLevel))

  case isEnd of
    Just _ -> return []
    Nothing -> do
      content <- indentationParser contentParser (indentationLevel + 1) <|> ((if firstEntry then hole' else delimiterParser) *> (contentParser indentationLevel <|> indentationParser contentParser (indentationLevel + 1)))

      nextContent <- blockParser' False endParser contentParser indentationLevel
      return (content : nextContent)

indentationParser :: (IndentationLevel -> Parser a) -> IndentationLevel -> Parser a
indentationParser contentParser indentationLevel = do
  try (eol' *> string (replicate indentationLevel '\t')) *> contentParser indentationLevel

eol' :: Parser ()
eol' = do
  _ <- some eol
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()

----------
-- Type --
----------

typeDefinitionParser :: IndentationLevel -> Parser ASTTypeDeclaration
typeDefinitionParser indentationLevel = do
  typeValue <- typeAlgebraicDataTypeParser indentationLevel <|> typeFunctionTypeParser indentationLevel <|> typeRecordTypeParser indentationLevel

  typeListParser typeValue

typeListParser :: ASTTypeDeclaration -> Parser ASTTypeDeclaration
typeListParser typeValue = do
  hasList <- optional (lookAhead listOpenParser)

  case hasList of
    Just _ -> do
      _ <- listOpenParser <* listCloseParser
      typeListParser (ASTTypeDeclarationAlgebraicDataType "List" [typeValue])
    Nothing ->
      return typeValue

typeAlgebraicDataTypeParser :: IndentationLevel -> Parser ASTTypeDeclaration
typeAlgebraicDataTypeParser indentationLevel = do
  name <- uppercaseIdentifierParser <* sc
  hasParameter <- optional (lookAhead functionCallOpenParser)
  parameters <-
    case hasParameter of
      Just _ -> do blockParser functionCallOpenParser functionCallCloseParser typeDefinitionParser indentationLevel
      Nothing -> do return []

  hasList <- optional (lookAhead listOpenParser)
  return (ASTTypeDeclarationAlgebraicDataType name parameters)

typeFunctionTypeParser :: IndentationLevel -> Parser ASTTypeDeclaration
typeFunctionTypeParser indentationLevel = do
  parameters <- blockParser functionDefinitionParser functionBodyParser typeDefinitionParser indentationLevel
  ASTTypeDeclarationFunction parameters <$> typeDefinitionParser indentationLevel

typeRecordTypeParser :: IndentationLevel -> Parser ASTTypeDeclaration
typeRecordTypeParser indentationLevel = do
  ASTTypeDeclarationRecord <$> blockParser recordOpenParser recordCloseParser typeRecordEntryParser indentationLevel

typeRecordEntryParser :: IndentationLevel -> Parser (String, ASTTypeDeclaration)
typeRecordEntryParser indentationLevel = do
  property <- lowercaseIdentifierParser
  _ <- typeAssignParser
  typeValue <- typeDefinitionParser indentationLevel
  return (property, typeValue)

------------
-- Tokens --
------------

statementTerminationParser :: Parser ()
statementTerminationParser = do
  _ <- char ';' *> sc
  return ()

assignParser :: Parser ()
assignParser = do
  _ <- char '=' *> sc
  return ()

typeAssignParser :: Parser ()
typeAssignParser = do
  _ <- char ':' *> sc
  return ()

streamParser :: Parser ()
streamParser = do
  _ <- string "<-" *> sc
  return ()

delimiterParser :: Parser ()
delimiterParser = do
  _ <- char ',' *> sc

  return ()

baseOfParser :: Parser ()
baseOfParser = do
  _ <- char '|' *> sc

  return ()

functionDefinitionParser :: Parser ()
functionDefinitionParser = do
  _ <- char '\\' <* sc

  return ()

functionBodyParser :: Parser ()
functionBodyParser = do
  _ <- string "->" <* sc

  return ()

functionCallOpenParser :: Parser ()
functionCallOpenParser = do
  _ <- char '(' *> sc

  return ()

functionCallCloseParser :: Parser ()
functionCallCloseParser = do
  _ <- char ')' *> sc

  return ()

listOpenParser :: Parser ()
listOpenParser = do
  _ <- char '[' *> sc

  return ()

listCloseParser :: Parser ()
listCloseParser = do
  _ <- char ']' *> sc

  return ()

recordOpenParser :: Parser ()
recordOpenParser = do
  _ <- char '{' *> sc

  return ()

recordCloseParser :: Parser ()
recordCloseParser = do
  _ <- char '}' *> sc

  return ()

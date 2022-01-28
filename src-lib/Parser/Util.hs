module Parser.Util where

import Control.Applicative ((<|>))
import Parser.Types
import Text.Megaparsec (between, many, optional, some, try)
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

  case decimalPlaces of
    Just decimalPlacesValue ->
      return (read (result ++ "." ++ decimalPlacesValue))
    Nothing ->
      return (read result)

blockParser :: Parser begin -> Parser end -> (IndentationLevel -> Parser a) -> IndentationLevel -> Parser [a]
blockParser beginParser endParser contentParser indentationLevel = do
  _ <- beginParser
  blockParser' True endParser contentParser indentationLevel

blockParser' :: Bool -> Parser end -> (IndentationLevel -> Parser a) -> IndentationLevel -> Parser [a]
blockParser' firstEntry endParser contentParser indentationLevel = do
  isEnd <- optional endParser

  case isEnd of
    Just _ -> return []
    Nothing -> do
      newline <-
        if firstEntry
          then Right <$> eol' <|> (Left <$> hole')
          else Right <$> eol' <|> (Right <$> try (delimiterParser *> eol')) <|> (Left <$> delimiterParser)

      contentContainer <- case newline of
        Right _ -> do
          (Left <$> indentationParser (const endParser) indentationLevel) <|> (Right <$> indentationParser contentParser (indentationLevel + 1))
        Left _ -> do
          (Left <$> endParser) <|> (Right <$> contentParser indentationLevel)

      case contentContainer of
        Right content -> do
          nextContent <- blockParser' False endParser contentParser indentationLevel
          return (content : nextContent)
        Left _ -> do
          return []

indentationParser :: (IndentationLevel -> Parser a) -> IndentationLevel -> Parser a
indentationParser contentParser indentationLevel = do
  try (string (replicate indentationLevel '\t') *> contentParser indentationLevel)

eol' :: Parser ()
eol' = do
  _ <- some eol
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()

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

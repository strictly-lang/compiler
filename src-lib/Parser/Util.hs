module Parser.Util where

import Control.Applicative ((<|>))
import Parser.Types
import Text.Megaparsec (between, many, optional, some, try)
import Text.Megaparsec.Char (char, eol, letterChar, lowerChar, space, string, upperChar)
import Text.Megaparsec.Char.Lexer

assignParser :: Parser ()
assignParser = do
  _ <- char '=' *> sc
  return ()

hole' :: Parser ()
hole' = do
  return ()

uppercaseIdentifierParser :: Parser String
uppercaseIdentifierParser = do
  firstChar <- upperChar
  rest <- many (letterChar <|> char '\'')

  return (firstChar : rest)

lowercaseIdentifierParser :: Parser String
lowercaseIdentifierParser = do
  firstChar <- lowerChar
  rest <- many (letterChar <|> char '\'')

  return (firstChar : rest)

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
          else Right <$> eol' <|> (Right <$> try (char ',' *> sc *> eol')) <|> (Left <$> (char ',' *> sc *> hole'))

      content <- case newline of
        Right _ -> do
          indentationParser contentParser (indentationLevel + 1)
        Left _ -> do
          contentParser indentationLevel

      nextContent <- blockParser' False endParser contentParser indentationLevel
      return (content : nextContent)

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
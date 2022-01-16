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
  isEol <- optional eol'

  case isEol of
    Just _ -> do indentationParser (blockParser' endParser contentParser indentationLevel) (indentationLevel + 1)
    Nothing -> do
      blockParser' endParser contentParser indentationLevel indentationLevel

blockParser' :: Parser end -> (IndentationLevel -> Parser a) -> IndentationLevel -> IndentationLevel -> Parser [a]
blockParser' endParser contentParser outerIndentationLevel innerIndentationLevel = do
  isEnd <- optional endParser

  case isEnd of
    Just _ -> return []
    Nothing -> do
      content <- contentParser innerIndentationLevel

      isEnd <- optional endParser

      case isEnd of
        Just _ -> return [content]
        Nothing -> do
          newline <- Right <$> eol' <|> (Right <$> try (char ',' *> sc *> eol')) <|> (Left <$> (char ',' *> sc *> hole'))

          nextContent <- case newline of
            Right _ -> do
              indentationParser (blockParser' endParser contentParser outerIndentationLevel) innerIndentationLevel
            Left _ ->
              blockParser' endParser contentParser outerIndentationLevel innerIndentationLevel
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
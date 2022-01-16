module Parser.Util where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (get, put)
import Parser.Types
import Text.Megaparsec (between, many, optional, sepBy, some, try)
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

blockParser :: Parser begin -> Parser end -> Parser a -> Parser [a]
blockParser beginParser endParser contentParser = do
  indentationLevel <- get
  _ <- beginParser
  isEol <- optional eol'

  case isEol of
    Just _ -> do indentationParser (indentationLevel + 1) *> blockParser' indentationLevel endParser contentParser
    Nothing -> do
      blockParser' indentationLevel endParser contentParser

blockParser' :: IndentationLevel -> Parser end -> Parser a -> Parser [a]
blockParser' indentationLevel endParser contentParser = do
  isEnd <- optional endParser

  case isEnd of
    Just _ -> return []
    Nothing -> do
      content <- contentParser

      isEnd <- optional endParser

      case isEnd of
        Just _ -> return [content]
        Nothing -> do
          newline <- Right <$> eol' <|> (Right <$> try (char ',' *> sc *> eol')) <|> (Left <$> (char ',' *> sc *> hole'))

          nextContent <- case newline of
            Right _ -> do
              indentationParser (indentationLevel + 1) *> blockParser' indentationLevel endParser contentParser
            Left _ ->
              blockParser' indentationLevel endParser contentParser
          return (content : nextContent)

indentationParser :: IndentationLevel -> Parser ()
indentationParser indentationLevel = do
  _ <- string (replicate indentationLevel '\t')
  _ <- put indentationLevel
  return ()

eol' :: Parser ()
eol' = do
  _ <- some eol
  return ()

sc :: Parser ()
sc = do
  _ <- many (char ' ')
  return ()
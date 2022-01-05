module Parser.Util where

import Parser.Types
import Text.Megaparsec (many)
import Text.Megaparsec.Char (char, letterChar, space, string, upperChar)

assignParser :: Parser ()
assignParser = do
  _ <- string "=" *> space
  return ()

uppercaseIdentifierParser :: Parser String
uppercaseIdentifierParser = do
  firstChar <- upperChar
  rest <- many letterChar
  primes <- many (char '\'')

  return (firstChar : rest ++ primes)

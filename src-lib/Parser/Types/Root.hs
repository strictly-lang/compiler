module Parser.Types.Root where

import Parser.Types
import Parser.Util (assignParser, uppercaseIdentifierParser)
import Text.Megaparsec.Char (space, space1, string)
import Types

dataParser :: Parser Root
dataParser = do
  name <- string "data" *> space1 *> uppercaseIdentifierParser <* space <* assignParser
  return (RootDataDeclaration name [])

rootParser = dataParser
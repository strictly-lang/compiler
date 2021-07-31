module Parser.Main (parseRoot) where

import Control.Applicative ((<|>))
import Parser.Import.Base (importParser)
import Parser.Model.Base (modelParser)
import Parser.Style.Base (styleParser)
import Parser.Util.Base (indentParserRepeat)
import Parser.View.Base (viewParser)
import Text.Megaparsec (eof, many, parse)
import Types

parseRoot = parse parseRoot' ""

parseRoot' :: Parser [Root]
parseRoot' = indentParserRepeat 0 (importParser <|> viewParser <|> modelParser <|> styleParser) <* eof

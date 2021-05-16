module Parser.Util.Base (indentParser) where
import Text.Megaparsec (Pos, MonadParsec (eof), atEnd)
import Text.Megaparsec.Char (string, eol, space)
import Text.Megaparsec.Char.Lexer (indentGuard)
import Control.Applicative ((<|>))

import Types

-- indentParser :: Pos  -> Parser a -> Parser a
-- indentParser position parser = do
--     _ <- indentGuard space GT position
--     parser

indentParser :: IndentationLevel   -> Parser a -> Parser a
indentParser indentationLevel parser = do
    _ <- string (replicate indentationLevel '\t')
    parser

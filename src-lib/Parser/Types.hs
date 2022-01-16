module Parser.Types where

import Control.Monad.State.Strict
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type IndentationLevel = Int

type ParserState = IndentationLevel

type Parser = Parsec Void String

module Parser.Types where

import Control.Monad.State.Strict
import Data.Void (Void)
import Text.Megaparsec (ParsecT)

type IndentationLevel = Int

type ParserState = IndentationLevel

type Parser = ParsecT Void String (State ParserState)

module Parser.Types.LeftHandSide where

import Parser.Types
import Parser.Util (lowercaseIdentifierParser)
import Types

leftHandSideParser :: Parser LeftHandSide
leftHandSideParser = leftHandSideVariableParser

leftHandSideVariableParser :: Parser LeftHandSide
leftHandSideVariableParser = LeftHandSideVariable <$> lowercaseIdentifierParser
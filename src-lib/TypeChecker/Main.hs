module TypeChecker.Main where

import Parser.Types (ASTStatement)
import TypeChecker.Types

merge :: TypeHandler a => [a] -> [ASTStatement] -> Bool
merge typeHandler expressions = True
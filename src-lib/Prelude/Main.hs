module Prelude.Main where

import Parser.Types (AST)
import Prelude.Types

emit :: [Macro] -> String -> AST -> Either String (String, String)
emit (macro : macros) filePath ast = Right (filePath, macro ast)
emit [] _ _ = Left "could not find any emitting macros"
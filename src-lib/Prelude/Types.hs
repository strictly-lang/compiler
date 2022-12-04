module Prelude.Types where

import Parser.Types (AST)

type Macro = String -> AST -> String

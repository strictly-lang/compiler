module Compiler.Javascript (getJs) where

import Types

type Path = String

getJs :: Path -> [Expr NodeTuple] -> String
getJs path exprs = ""
module Compiler.Javascript (getJs) where

import Types
import Compiler.Util

type AbsolutePath = String
type ComponentPath  = String

getJs :: AbsolutePath -> ComponentPath -> [Expr NodeTuple] -> Maybe String
getJs absolutePath componentPath exprs = pathToComponent absolutePath componentPath
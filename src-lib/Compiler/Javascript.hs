module Compiler.Javascript (getJs) where

import Compiler.Util
import Types

type AbsolutePath = String

type ComponentPath = String

getJs :: AbsolutePath -> ComponentPath -> [Expr Root] -> Maybe String
getJs absolutePath componentPath exprs = pathToComponent absolutePath componentPath

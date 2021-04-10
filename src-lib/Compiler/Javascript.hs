module Compiler.Javascript (getJs) where

import Compiler.Util ( pathToComponent )
import Types
import Compiler.Types.Root ( compileRoot )

type AbsolutePath = String

type ComponentPath = String

getJs :: AbsolutePath -> ComponentPath -> [Expr Root] -> Maybe String
getJs absolutePath componentPath exprs = do
    componentName <- pathToComponent absolutePath componentPath
    Just (unlines (map (compileRoot componentName exprs) exprs))

module Compiler.Javascript (getJs) where

import Compiler.Types.Root (compileRoot)
import Compiler.Util (pathToComponent)
import Types

type AbsolutePath = String

type ComponentPath = String

getJs :: AbsolutePath -> ComponentPath -> [Root] -> Maybe String
getJs absolutePath componentPath exprs = do
  componentName <- pathToComponent absolutePath componentPath
  Just (unlines (map (compileRoot componentName exprs) exprs))

module Compiler.Main where

import Compiler.Types.Root (compileRoot)
import Compiler.Util (pathToComponent)
import Parser.Main (parseRoot)
import Types

type AbsolutePath = String

type ComponentPath = String

getJs :: AbsolutePath -> ComponentPath -> [Root] -> Maybe String
getJs absolutePath componentPath exprs = do
  componentName <- pathToComponent absolutePath componentPath
  Just (compileRoot componentName exprs)

parse = parseRoot

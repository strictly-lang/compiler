module Compiler.Main where

import Emitter.Types.Root (compileRoot)
import Emitter.Util (pathToComponentName)
import Parser.Main (parseRoot)
import Types

type AbsolutePath = String

type ComponentPath = String

getJs :: AbsolutePath -> ComponentPath -> [Root] -> Maybe String
getJs absolutePath componentPath exprs = do
  componentName <- pathToComponentName absolutePath componentPath
  Just (compileRoot componentName exprs)

parse = parseRoot

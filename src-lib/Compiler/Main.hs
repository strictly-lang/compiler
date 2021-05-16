module Compiler.Main (parse, getWasm, getWat, getJs) where

import Parser.Main (parseRoot)
import Compiler.Javascript (getJs)


parse = parseRoot
getWasm = putStrLn "getWasm"

getWat = putStrLn "getWat"

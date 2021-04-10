module Compiler.Main (parse, getWasm, getWat, getJs) where

import Compiler.Javascript (getJs)
import Parser.Main (parse)

getWasm = putStrLn "getWasm"

getWat = putStrLn "getWat"

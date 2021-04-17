module Compiler.Main (parse, getWasm, getWat, getJs, tokenize) where

import Compiler.Javascript (getJs)
import Parser.Main (parse)
import Tokenizer.Main (tokenize)

getWasm = putStrLn "getWasm"

getWat = putStrLn "getWat"

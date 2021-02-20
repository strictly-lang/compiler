module Compiler (parse, getWasm, getWat, getJs) where

parse fileContent = getAst fileContent

getAst fileContent = fileContent;

getWasm = putStrLn "getWasm"

getWat = putStrLn "getWat"

getJs = putStrLn "getJs"
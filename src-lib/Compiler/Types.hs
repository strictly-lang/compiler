module Compiler.Types where

data Code = Ln String | Ind [Code] |Br
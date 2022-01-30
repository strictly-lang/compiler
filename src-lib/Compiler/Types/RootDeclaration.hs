module Compiler.Types.RootDeclaration where

import Compiler.Types (Code (..))
import Types

algebraicDataTypeConstructor :: [(String, [String])] -> [Code]
algebraicDataTypeConstructor [] = []
algebraicDataTypeConstructor ((name, parameters) : adts) =
  [ Ln ("function " ++ name ++ "(...parameters) {"),
    Ind
      [ Ln "this.parameters = parameters;"
      ],
    Ln "}",
    Br
  ]
    ++ algebraicDataTypeConstructor adts
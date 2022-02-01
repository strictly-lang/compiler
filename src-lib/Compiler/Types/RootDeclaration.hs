module Compiler.Types.RootDeclaration where

import Compiler.Types (Code (..))
import Types

algebraicDataTypeConstructor :: [(String, [String])] -> [Code]
algebraicDataTypeConstructor [] = []
algebraicDataTypeConstructor ((name, parameters) : adts) =
  ( if null parameters
      then [Ln ("function " ++ name ++ "() {}")]
      else
        [ Ln ("function " ++ name ++ "(...parameters) {"),
          Ind
            [ Ln "this.parameters = parameters;"
            ],
          Ln "}"
        ]
  )
    ++ algebraicDataTypeConstructor adts
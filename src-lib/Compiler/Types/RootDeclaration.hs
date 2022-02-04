module Compiler.Types.RootDeclaration where

import Compiler.Types (AppStateMonad, Code (..))
import Types

algebraicDataTypeConstructor :: [(String, [String])] -> AppStateMonad [Code]
algebraicDataTypeConstructor [] = do return []
algebraicDataTypeConstructor ((name, parameters) : adts) =
  do
    next <- algebraicDataTypeConstructor adts
    return
      ( ( if null parameters
            then [Ln ("function " ++ name ++ "() {}")]
            else
              [ Ln ("function " ++ name ++ "(...parameters) {"),
                Ind
                  [ Ln "this.parameters = parameters;"
                  ],
                Ln "}"
              ]
        )
          ++ [Br]
          ++ next
      )

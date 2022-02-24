module Emitter.Kinds.RootDeclaration where

import Emitter.Types (AppStateMonad, Code (..))
import Types

algebraicDataTypeConstructor :: [DataDeclaration] -> AppStateMonad [Code]
algebraicDataTypeConstructor [] = do return []
algebraicDataTypeConstructor (DataDeclaration (name, parameters) : adts) =
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

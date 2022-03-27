module Emitter.Kinds.RootDeclaration where

import Emitter.Types (AppStateMonad, Code (..), VariableStack)
import Types

algebraicDataTypeConstructor :: [DataDeclaration] -> AppStateMonad ([Code], VariableStack)
algebraicDataTypeConstructor [] = do return ([], [])
algebraicDataTypeConstructor (DataDeclaration (name, parameters) : adts) =
  do
    (next, variableStack) <- algebraicDataTypeConstructor adts
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
          ++ next,
        variableStack
      )

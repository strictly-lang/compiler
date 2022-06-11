module Emitter.Kinds.RootDeclaration where

import Data.List (intercalate)
import Emitter.Kinds.Expression (toTypedExpression)
import Emitter.Types
import Types

algebraicDataTypeTypeHandler :: String -> [TypeDefinition] -> TypeHandler
algebraicDataTypeTypeHandler name parameterTypes stack typeDefinition (Right [[RightHandSideAlgebraicDataType name' untypedParameterExpressions]])
  | name == name' =
    Just
      ( do
          let stackHandler =
                StackHandler
                  { runPrimitive =
                      do
                        parameterStackHandlers <- mapM (\(typeDefinition, parameterExpression) -> toTypedExpression stack (Just typeDefinition) [parameterExpression]) (zip parameterTypes untypedParameterExpressions)
                        parameterResult <- mapM runPrimitive parameterStackHandlers
                        return
                          ( concatMap fst parameterResult,
                            [ Ln ("new " ++ name ++ "(")
                            ]
                              ++ intercalate [Ln ", "] (map snd parameterResult)
                              ++ [Ln ")"]
                          ),
                    runFunctionApplication = \_ -> error "no function application implemented in stringreference",
                    runProperty = \_ -> error "no property access implemented",
                    runViewStream = \_ -> error "no streaming",
                    runResolvedType = typeDefinition
                  }
          return
            stackHandler
      )
algebraicDataTypeTypeHandler _ _ _ _ _ = Nothing

algebraicDataTypeConstructor :: [(String, [TypeDefinition])] -> AppStateMonad ([Code], Stack)
algebraicDataTypeConstructor [] = do return ([], [])
algebraicDataTypeConstructor ((name, parameterTypes) : adts) =
  do
    (next, variableStack) <- algebraicDataTypeConstructor adts
    return
      ( ( if null parameterTypes
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
        StackType (algebraicDataTypeTypeHandler name parameterTypes) : variableStack
      )

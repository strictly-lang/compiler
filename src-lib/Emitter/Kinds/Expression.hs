module Emitter.Kinds.Expression where

import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

toTypedExpression :: VariableStack -> TypeDefinition -> UntypedExpression -> AppStateMonad (TypedExpression, [Variable])
toTypedExpression variableStack (TypeFunction parameterTypeDefinitions returnTypeDefinition) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideFunctionDefinition untypedParameters untypedBody -> do
      return
        ( TypedExpression
            { runPrimitive = \foo -> do return [],
              runView = \parameters ->
                let variableStack' = addToVariableStack variableStack (zip untypedParameters parameters)
                 in render variableStack' untypedBody
            },
          []
        )
    _ ->
      error "nope"
toTypedExpression variableStack typeDefinition untypedExpression =
  error (show typeDefinition ++ " " ++ show untypedExpression)

addToVariableStack :: VariableStack -> [(LeftHandSide, ([Variable], TypedExpression))] -> VariableStack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, (_, _)) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, (place, typedExpression)) : restNewVariables) = (name, place, typedExpression) : addToVariableStack variableStack restNewVariables

-- view

render :: VariableStack -> [Statement] -> Parent -> Predecessor -> AppStateMonad ViewResult
render variableStack [] parent predecessor = do
  return
    ViewResult {runViewCreate = [], runViewUpdate = [], runViewUnmount = [], runViewDelete = []}
render variableStack ((UntypedExpression untypedExpression) : restUntypedBody) parent predecessor = do
  typedResult <- toTypedExpression variableStack (TypeAlgebraicDataType "String" []) untypedExpression
  return
    ( ViewResult
        { runViewCreate = [],
          runViewUpdate = [],
          runViewUnmount = [],
          runViewDelete = []
        }
    )
render variableStack untypedBody parent predecessor = error "mep"

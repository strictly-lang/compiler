module Emitter.Kinds.Expression where

import Emitter.Kinds.View (render)
import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

toTypedExpression :: TypeDefinition -> UntypedExpression -> (TypedExpression, [Variable])
toTypedExpression (TypeFunction parameterTypeDefinitions returnTypeDefinition) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideFunctionDefinition untypedParameters untypedBody ->
      ( TypedExpression
          { runType = \foo -> do return [],
            runView = \variableStack parameters ->
              let variableStack' = addToVariableStack variableStack (zip untypedParameters parameters)
               in render variableStack' untypedBody
          },
        []
      )
    _ ->
      error "nope"

addToVariableStack :: VariableStack -> [(LeftHandSide, ([Variable], TypedExpression))] -> VariableStack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, (_, _)) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, (place, typedExpression)) : restNewVariables) = (name, place, typedExpression) : addToVariableStack variableStack restNewVariables
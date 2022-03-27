module Emitter.Kinds.Expression where

import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable)
import Types

toTypedExpression :: [Variable] -> TypeDefinition -> UntypedExpression -> (TypedExpression, [Variable])
toTypedExpression place (TypeFunction parameterTypeDefinitions returnTypeDefinition) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideFunctionDefinition untypedParameters untypedBody ->
      (TypedExpression {runType = \foo bar -> []}, [])
    _ ->
      error "nope"

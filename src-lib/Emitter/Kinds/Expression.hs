{-# LANGUAGE LambdaCase #-}

module Emitter.Kinds.Expression where

import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, variableToString)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

toTypedExpression :: VariableStack -> TypeDefinition -> UntypedExpression -> AppStateMonad (TypedExpression, [Variable])
toTypedExpression variableStack (TypeFunction parameterTypeDefinitions returnTypeDefinition) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideFunctionDefinition untypedParameters untypedBody -> do
      return
        ( TypedExpression
            { runPrimitive = do return [],
              runView = \parameters ->
                let variableStack' = addToVariableStack variableStack (zip untypedParameters parameters)
                 in render variableStack' untypedBody,
              runFunctionApplication = \_ -> error "no function application implemented",
              runProperty = \_ -> error "no property access implemented"
            },
          []
        )
    _ ->
      error "nope"
toTypedExpression variableStack typeDefinition@(TypeAlgebraicDataType "String" []) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideString strings -> do
      return
        ( TypedExpression
            { runPrimitive = do
                result <-
                  mapM
                    ( \case
                        RightHandSideStringStatic static -> do
                          return [Ln static]
                        RightHandSideStringDynamic untypedExpression -> do
                          (typedExpression, dependencies) <- toTypedExpression variableStack typeDefinition untypedExpression
                          runPrimitive typedExpression
                    )
                    strings
                return (Ln "\"" : concat result ++ [Ln "\""]),
              runView = \_ -> error "no view access implemented",
              runFunctionApplication = \_ -> error "no function application implemented",
              runProperty = \_ -> error "no property access implemented"
            },
          []
        )
    _ ->
      error "nope"
toTypedExpression variableStack typeDefinition untypedExpression =
  error (show typeDefinition ++ " - " ++ show untypedExpression)

addToVariableStack :: VariableStack -> [(LeftHandSide, ([Variable], TypedExpression))] -> VariableStack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, (_, _)) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, (place, typedExpression)) : restNewVariables) = (name, place, typedExpression) : addToVariableStack variableStack restNewVariables

-- view

render :: VariableStack -> [Statement] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult
render variableStack [] scope parent siblings = do
  return
    ViewResult {runViewCreate = [], runViewUpdate = [], runViewUnmount = [], runViewDelete = []}
render variableStack ((UntypedExpression untypedExpression) : restUntypedBody) scope parent siblings = do
  (typedResult, dependencies) <- toTypedExpression variableStack (TypeAlgebraicDataType "String" []) untypedExpression
  exprId <- getFreshExprId
  let textElement = scope ++ nameToVariable "text" exprId
  textContent <- runPrimitive typedResult

  return
    ( ViewResult
        { runViewCreate =
            Ln (variableToString textElement ++ " = document.createTextNode(") :
            textContent
              ++ [ Ln ");",
                   Br
                 ]
              ++ appendElement parent siblings textElement,
          runViewUpdate = [],
          runViewUnmount = [],
          runViewDelete = [],
          runSiblings = siblings ++ [textElement]
        }
    )
render variableStack untypedBody scope parent siblings = error "mep"

appendElement :: Parent -> [Sibling] -> [Variable] -> [Code]
appendElement parent [] element =
  Ln (variableToString parent ++ ".append(") : [Ln (variableToString element), Ln ");", Br]
appendElement parent siblings element =
  let lastSibling = last siblings
   in ( Ln (variableToString lastSibling ++ ".after(") : [Ln (variableToString element), Ln ");", Br]
      )
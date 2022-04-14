{-# LANGUAGE LambdaCase #-}

module Emitter.Kinds.Expression where

import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, variableToString)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

type Prefix = [Code]

toTypedExpression :: TypeDefinition -> UntypedExpression -> AppStateMonad TypedExpression
toTypedExpression = toTypedExpression' []

toTypedExpression' :: Prefix -> TypeDefinition -> UntypedExpression -> AppStateMonad TypedExpression
toTypedExpression' [] typeDefinition@(TypeFunction parameterTypeDefinitions returnTypeDefinition) (untypedExpression : restUntypedExpression) =
  case untypedExpression of
    RightHandSideFunctionDefinition untypedParameters untypedBody -> do
      return
        ( TypedExpression
            { runPrimitive = \variableStack -> do return ([], []),
              runView = \variableStack parameters ->
                let variableStack' = addToVariableStack variableStack (zip untypedParameters parameters)
                 in render variableStack' untypedBody,
              runFunctionApplication = \_ -> error "no function application implemented",
              runProperty = \_ -> error "no property access implemented",
              runResolvedType = const typeDefinition
            }
        )
    _ ->
      error "nope"
toTypedExpression' prefix typeDefinition@(TypeAlgebraicDataType "String" []) untypedExpression =
  return
    ( TypedExpression
        { runPrimitive = \variableStack ->
            do
              result <-
                case untypedExpression of
                  [RightHandSideString strings] -> do
                    mapM
                      ( \case
                          RightHandSideStringStatic static -> do
                            return ([], [Ln static])
                          RightHandSideStringDynamic untypedExpression -> do
                            (typedExpression) <- toTypedExpression' prefix typeDefinition untypedExpression
                            runPrimitive typedExpression variableStack
                      )
                      strings
                  (RightHandSideVariable variableName) : rest ->
                    error (show (length variableStack))
              return (concatMap fst result, Ln "\"" : concatMap snd result ++ [Ln "\""]),
          runView = \_ -> error "no view access implemented",
          runFunctionApplication = \_ -> error "no function application implemented",
          runProperty = \_ -> error "no property access implemented",
          runResolvedType = const typeDefinition
        }
    )
toTypedExpression' prefix typeDefinition untypedExpression =
  error (show prefix ++ show typeDefinition ++ " - " ++ show untypedExpression)

addToVariableStack :: VariableStack -> [(LeftHandSide, TypedExpression)] -> VariableStack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, _) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, (typedExpression)) : restNewVariables) = (name, typedExpression) : addToVariableStack variableStack restNewVariables

-- view

render :: VariableStack -> [Statement] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult
render variableStack [] scope parent siblings = do
  return
    ViewResult {runViewCreate = [], runViewUpdate = [], runViewUnmount = [], runViewDelete = [], runSiblings = siblings}
render variableStack ((UntypedExpression [RightHandSideHost elementName properties children]) : restUntypedBody) scope parent siblings = do
  exprId <- getFreshExprId
  let hostElement = scope ++ nameToVariable "element" exprId
  childrenResult <- render variableStack children scope hostElement []
  siblingResult <- render variableStack restUntypedBody scope parent (siblings ++ [hostElement])

  return
    ( ViewResult
        { runViewCreate =
            [ Ln (variableToString hostElement ++ " = document.createElement(\"" ++ elementName ++ "\");"),
              Br
            ]
              ++ appendElement parent siblings hostElement
              ++ runViewCreate childrenResult
              ++ runViewCreate siblingResult,
          runViewUpdate =
            runViewUpdate childrenResult
              ++ runViewUpdate siblingResult,
          runViewUnmount =
            runViewUnmount childrenResult
              ++ runViewUnmount siblingResult,
          runViewDelete =
            runViewDelete childrenResult
              ++ runViewDelete siblingResult,
          runSiblings = runSiblings siblingResult
        }
    )
render variableStack ((UntypedExpression untypedExpression) : restUntypedBody) scope parent siblings = do
  typedResult <- toTypedExpression (TypeAlgebraicDataType "String" []) untypedExpression
  exprId <- getFreshExprId
  let textElement = scope ++ nameToVariable "text" exprId
  (dependencies, textContent) <- runPrimitive typedResult variableStack
  siblingResult <- render variableStack restUntypedBody scope parent (siblings ++ [textElement])

  return
    ( ViewResult
        { runViewCreate =
            Ln (variableToString textElement ++ " = document.createTextNode(") :
            textContent
              ++ [ Ln ");",
                   Br
                 ]
              ++ appendElement parent siblings textElement
              ++ runViewCreate siblingResult,
          runViewUpdate = runViewUpdate siblingResult,
          runViewUnmount = runViewUnmount siblingResult,
          runViewDelete = runViewDelete siblingResult,
          runSiblings = runSiblings siblingResult
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
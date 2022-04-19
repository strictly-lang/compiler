{-# LANGUAGE LambdaCase #-}

module Emitter.Kinds.Expression where

import Control.Monad.State.Lazy (get)
import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, slashToCamelCase, slashToDash, variableToString)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

type Prefix = [Code]

stringHandler :: TypeHandler
stringHandler stack typeDefinition@(TypeAlgebraicDataType "String" []) =
  Just
    ( \untypedExpression ->
        StackHandler
          { runPrimitive =
              do
                let prefix = []
                result <-
                  case untypedExpression of
                    RightHandSideString strings -> do
                      mapM
                        ( \case
                            RightHandSideStringStatic static -> do
                              return ([], [Ln static])
                            RightHandSideStringDynamic untypedExpression -> do
                              (TypedExpression typedExpression) <- toTypedExpression' prefix stack typeDefinition untypedExpression
                              runPrimitive typedExpression
                        )
                        strings
                    (RightHandSideVariable variableName) ->
                      error (show (length stack))
                return (concatMap fst result, Ln "\"" : concatMap snd result ++ [Ln "\""]),
            runView = \_ -> error "no view access implemented",
            runFunctionApplication = \_ -> error "no function application implemented",
            runProperty = \_ -> error "no property access implemented",
            runResolvedType = typeDefinition
          }
    )
stringHandler _ _ = Nothing

componentHandler stack typeDefinition@(TypeFunction [_, _] (TypeAlgebraicDataType "View" [])) =
  Just
    ( \untypedExpression ->
        case untypedExpression of
          RightHandSideFunctionDefinition parameters body ->
            StackHandler
              { runPrimitive =
                  do
                    appState <- get
                    exprId <- getFreshExprId
                    let scope = [DotNotation "this"]
                    let componentName' = componentName appState
                    let unscopedMounted = nameToVariable "mounted" exprId
                    let unscopedProperties = nameToVariable "properties" exprId
                    let scopedMounted = scope ++ unscopedMounted
                    let scopedProperties = scope ++ unscopedProperties
                    -- let typedProperties = typedOrigin scopedProperties propertyTypes

                    view <- render stack body [] (scope ++ [DotNotation "shadowRoot"]) []

                    return
                      ( [],
                        [ Ln ("class " ++ slashToCamelCase componentName' ++ " extends HTMLElement {"),
                          Ind
                            [ Ln (variableToString unscopedMounted ++ " = false;"),
                              Br,
                              Ln (variableToString unscopedProperties ++ " = {};"),
                              Br,
                              Ln "connectedCallback() {",
                              Ind
                                ( [ Ln "this.attachShadow({mode: 'open'});",
                                    Br,
                                    Ln (variableToString scopedMounted ++ " = true;"),
                                    Br
                                  ]
                                    ++ runViewCreate view
                                ),
                              Br,
                              Ln "}"
                            ],
                          Ln "}",
                          Br,
                          Br,
                          Ln ("customElements.define(\"" ++ slashToDash componentName' ++ "\", " ++ slashToCamelCase componentName' ++ ");"),
                          Br
                        ]
                      ),
                runView = \_ -> error "no view access implemented",
                runFunctionApplication = \_ -> error "no function application implemented",
                runProperty = \_ -> error "no property access implemented",
                runResolvedType = typeDefinition
              }
    )
componentHandler _ _ = Nothing

prelude :: [StackEntry]
prelude =
  [ StackType stringHandler,
    StackType componentHandler
  ]

toTypedExpression :: Stack -> TypeDefinition -> UntypedExpression -> AppStateMonad TypedExpression
toTypedExpression = toTypedExpression' []

toTypedExpression' :: Prefix -> Stack -> TypeDefinition -> UntypedExpression -> AppStateMonad TypedExpression
toTypedExpression' prefix stack typeDefinition (untypedExpression : restUntypedExpression) = do
  return (TypedExpression (findType stack typeDefinition untypedExpression))

findType :: Stack -> TypeDefinition -> (UntypedExpression' -> StackHandler)
findType stack = findType' (stack, stack)

findType' :: (Stack, Stack) -> TypeDefinition -> (UntypedExpression' -> StackHandler)
findType' ([], _) typeDefinition = error ("no corresponding type found " ++ show typeDefinition)
findType' ((StackType stackEntry) : nextStack, allStack) typeDefinition =
  case stackEntry allStack typeDefinition of
    Just result -> result
    Nothing -> findType' (nextStack, allStack) typeDefinition
findType' (_ : nextStack, allStack) typeDefinition = findType' (nextStack, allStack) typeDefinition

addToVariableStack :: Stack -> [(LeftHandSide, TypedExpression)] -> Stack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, _) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, (typedExpression)) : restNewVariables) = (StackValue (name, typedExpression)) : addToVariableStack variableStack restNewVariables

-- view

render :: Stack -> [Statement] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult
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
  TypedExpression typedResult <- toTypedExpression variableStack (TypeAlgebraicDataType "String" []) untypedExpression
  exprId <- getFreshExprId
  let textElement = scope ++ nameToVariable "text" exprId
  (dependencies, textContent) <- runPrimitive typedResult
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